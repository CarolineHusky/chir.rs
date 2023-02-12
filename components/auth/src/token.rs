//! Module for issuing and validating tokens

use std::{collections::HashSet, sync::Arc};

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use axum::{
    extract::{FromRequestParts, Path, State},
    headers::Authorization,
    http::{request::Parts, StatusCode},
    response::{IntoResponse, Response},
    Json, RequestPartsExt, TypedHeader,
};
use chrono::{DateTime, Months, SecondsFormat, Utc};
use headers::authorization::Bearer;
use once_cell::sync::Lazy;
use pasetors::{
    claims::{Claims, ClaimsValidationRules},
    keys::{Generate, SymmetricKey},
    local,
    token::UntrustedToken,
    version4::V4,
    Local,
};
use redis::cmd;
use serde::{Deserialize, Serialize};
use serde_json::json;
use sqlx::{query, query_as, PgPool};
use tracing::{error, info};

use crate::{id_generator::generate_id_urlsafe, kv::ensure_kv, models::UserSession, ServiceState};

/// Loads or Creates the PASETO key
///
/// # Errors
/// This function returns an error if loading or generating the PASETO key fails.
pub async fn get_or_create_paseto_key(db: &PgPool) -> Result<SymmetricKey<V4>> {
    let key = ensure_kv(db, b"paseto/key", || {
        SymmetricKey::generate()
            .map(|v| v.as_bytes().to_vec())
            .map_err(Into::into)
    })
    .await?;
    SymmetricKey::from(&key[..]).map_err(Into::into)
}

/// Extractor for an authenticated user
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuthenticatedUser {
    /// ID of the authenticated user
    id: String,
    /// Scopes that the user can access
    scopes: HashSet<String>,
    /// Session ID of the session
    session_id: String,
}

impl AuthenticatedUser {
    /// Logs out a session
    ///
    /// After this call, the session is no longer valid
    ///
    /// # Errors
    /// This function returns an error if logging out fails
    #[allow(clippy::missing_panics_doc)]
    #[allow(clippy::panic)]
    pub async fn logout(&self, state: &Arc<ServiceState>) -> Result<()> {
        info!("{} logged out", self.session_id);
        query!(
            "DELETE FROM auth_user_sessions WHERE user_id = $1 AND jti = $2",
            &self.id,
            &self.session_id
        )
        .execute(&state.database)
        .await?;

        state.invalidate_user_session(&self.session_id).await?;
        Ok(())
    }

    /// Removes a scope from a session
    ///
    /// After this call, the session will no longer have access to a specific scope
    ///
    /// # Errors
    /// This function returns an error if removing the scope fails
    #[allow(clippy::missing_panics_doc)]
    #[allow(clippy::panic)]
    pub async fn remove_scope(&self, state: &Arc<ServiceState>, scope_name: &str) -> Result<()> {
        query!(
            r#"
            DELETE FROM auth_session_scopes
                USING auth_user_sessions
                WHERE auth_user_sessions.jti = $1
                    AND auth_session_scopes.jti = $1
                    AND auth_session_scopes.scope = $2
                    AND auth_user_sessions.user_id = $3
            "#,
            &self.session_id,
            &scope_name,
            &self.id
        )
        .execute(&state.database)
        .await?;

        Ok(())
    }

    /// Return whether or not the user has granted a scope
    pub fn has_scope(&self, scope: impl AsRef<str>) -> bool {
        self.scopes.contains(scope.as_ref())
    }

    /// Returns the user ID
    #[must_use]
    pub fn id(&self) -> &str {
        &self.id
    }
}

impl ServiceState {
    /// Attempts to load a cached token from redis
    ///
    /// # Errors
    /// This function will return an error if connection to redis fails or
    pub async fn get_cached_token(self: &Arc<Self>, jti: &str) -> Result<AuthenticatedUser> {
        let mut conn = self.redis.get().await?;
        let value: String = cmd("GET")
            .arg(format!("auth/session:{jti}"))
            .query_async(&mut conn)
            .await?;
        let sess = serde_json::from_str(&value)?;
        Ok(sess)
    }

    /// List sessions of a user
    ///
    /// # Errors
    /// This function will return an error if the list couldn’t be fetched
    #[allow(clippy::missing_panics_doc)]
    #[allow(clippy::panic)]
    pub async fn list_user_sessions(self: &Arc<Self>, user_id: &str) -> Result<Vec<String>> {
        let res = query!(
            "SELECT jti FROM auth_user_sessions WHERE user_id = $1",
            user_id
        )
        .fetch_all(&self.database)
        .await?;
        Ok(res.into_iter().map(|r| r.jti).collect::<Vec<_>>())
    }

    /// Attempts to cache a token to redis
    ///
    /// # Errors
    /// This function will return an error if connection to redis fails
    pub async fn try_cache_token(
        self: &Arc<Self>,
        jti: &str,
        user: &AuthenticatedUser,
    ) -> Result<()> {
        let mut conn = self.redis.get().await?;
        let encoded = serde_json::to_string(user)?;
        cmd("SET")
            .arg(format!("auth/session:{jti}"))
            .arg(encoded)
            .arg("NX")
            .arg("EX")
            .arg(3600)
            .query_async(&mut conn)
            .await?;
        Ok(())
    }

    /// Constructs a user session from the JTI and the expected user ID
    ///
    /// # Errors
    /// This function will return an error if the token is invalid or the database connection fails
    pub async fn get_session_for_user(
        self: &Arc<Self>,
        jti: &str,
        user: &str,
    ) -> Result<AuthenticatedUser> {
        let sess = self.get_user_session(jti).await?;
        if sess.id() != user {
            return Err(anyhow!(
                "Session for user {} is not for user {}",
                user,
                sess.id()
            ));
        }
        Ok(sess)
    }

    /// Constructs a user session from the JTI
    ///
    /// # Errors
    /// This function will return an error if the token is invalid or the database connection fails
    pub async fn get_user_session(self: &Arc<Self>, jti: &str) -> Result<AuthenticatedUser> {
        if let Ok(sess) = self.get_cached_token(jti).await {
            return Ok(sess);
        }
        let token = get_token_info(self, jti).await?;
        let scopes = get_token_scopes(self, jti).await?;
        let sess = AuthenticatedUser {
            id: token.user_id,
            scopes,
            session_id: jti.to_string(),
        };
        self.try_cache_token(jti, &sess).await.ok();
        Ok(sess)
    }

    /// Invalidates the session cache for a specific user
    ///
    /// # Errors
    /// This function will return an error if redis access failed
    pub async fn invalidate_user_session(self: &Arc<Self>, jti: &str) -> Result<()> {
        let mut conn = self.redis.get().await?;
        cmd("DEL")
            .arg(format!("auth/session:{jti}"))
            .query_async(&mut conn)
            .await?;
        Ok(())
    }

    /// Issues a token to a user
    ///
    /// # Errors
    /// This function returns an error if the database connection fails
    #[allow(clippy::missing_panics_doc)]
    #[allow(clippy::panic)]
    pub async fn issue_token(self: &Arc<Self>, user: &str, scopes: &[String]) -> Result<String> {
        let jti = generate_id_urlsafe();
        let now = Utc::now();
        let expire = now.checked_add_months(Months::new(1)).unwrap_or(now);
        let expire_paseto = expire.to_rfc3339_opts(SecondsFormat::Secs, true);

        let mut claims = Claims::new()?;
        claims.issuer("https://auth.chir.rs/")?;
        claims.subject(user)?;
        claims.expiration(&expire_paseto)?;
        claims.token_identifier(&jti)?;

        let token = local::encrypt(&self.paseto_key, &claims, None, None)?;

        let mut tx = self.database.begin().await?;

        query!(
            r#"
                INSERT INTO auth_user_sessions
                    (jti, user_id, exp_at, reauth_after)
                VALUES ($1, $2, $3, $4)
            "#,
            jti,
            user,
            expire,
            None::<DateTime<Utc>>
        )
        .execute(&mut tx)
        .await?;

        for scope in scopes {
            query!(
                r#"
                    INSERT INTO auth_session_scopes
                        (jti, scope)
                    VALUES ($1, $2)
                "#,
                jti,
                scope
            )
            .execute(&mut tx)
            .await?;
        }
        tx.commit().await?;

        Ok(token)
    }
}

/// The response returned on error
#[must_use]
pub fn on_error_response() -> Response {
    (
        StatusCode::UNAUTHORIZED,
        Json(json!({
            "title": "Unauthorized",
            "status": 401
        })),
    )
        .into_response()
}

/// Maps the error
pub fn on_error(e: impl std::fmt::Debug) -> Response {
    error!("{e:?}");
    on_error_response()
}

/// The response returned on error
pub fn on_server_error_response() -> Response {
    let incident_id = generate_id_urlsafe();
    error!("Incident ID: {incident_id}");
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(json!({
            "title": "Internal Server Error",
            "status": 500,
            "incident_id": incident_id.to_string()
        })),
    )
        .into_response()
}

/// Maps the error
pub fn on_server_error(e: impl std::fmt::Debug) -> Response {
    error!("{e:?}");
    on_server_error_response()
}

/// Returns session information given a jti
///
/// # Errors
/// This function returns an error if the token doesn’t exist
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::panic)]
async fn get_token_info(state: &Arc<ServiceState>, token_jti: &str) -> Result<UserSession> {
    let res = query_as!(
        UserSession,
        r#"
            SELECT * FROM auth_user_sessions
            WHERE jti = $1
            LIMIT 1
        "#,
        token_jti
    )
    .fetch_one(&state.database)
    .await?;
    Ok(res)
}

/// Returns available scopes for a token
///
/// # Errors
/// This function returns an error if the connection to the database fails
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::panic)]
async fn get_token_scopes(state: &Arc<ServiceState>, token_jti: &str) -> Result<HashSet<String>> {
    let res = query!(
        r#"
            SELECT scope FROM auth_session_scopes
            WHERE jti = $1
        "#,
        token_jti
    )
    .fetch_all(&state.database)
    .await?;
    Ok(res.into_iter().map(|v| v.scope).collect())
}

/// The validation rules for token validation
static VALIDATION_RULES: Lazy<ClaimsValidationRules> = Lazy::new(|| {
    let mut validation_rules = ClaimsValidationRules::new();
    validation_rules.validate_issuer_with("https://auth.chir.rs/");
    validation_rules
});

/// Extension trait for the [`Claims`] type
trait ClaimsExt {
    /// Attempts to read a string claim
    ///
    /// # Errors
    /// This function returns an error if the claim does not exist or is not a string
    fn get_string_claim(&self, key: &str) -> Result<&str>;
}

impl ClaimsExt for Claims {
    fn get_string_claim(&self, key: &str) -> Result<&str> {
        self.get_claim(key).map_or_else(
            || Err(anyhow!("claim {} not found", key)),
            |val| {
                val.as_str()
                    .map_or_else(|| Err(anyhow!("claim {} is not a string", key)), Ok)
            },
        )
    }
}

#[async_trait]
impl FromRequestParts<Arc<ServiceState>> for AuthenticatedUser {
    type Rejection = Response;
    async fn from_request_parts(
        parts: &mut Parts,
        state: &Arc<ServiceState>,
    ) -> Result<Self, Self::Rejection> {
        let TypedHeader(Authorization(token)) = parts
            .extract::<TypedHeader<Authorization<Bearer>>>()
            .await
            .map_err(IntoResponse::into_response)?;

        let untrusted_token =
            UntrustedToken::<Local, V4>::try_from(token.token()).map_err(on_error)?;
        let trusted_token = local::decrypt(
            &state.paseto_key,
            &untrusted_token,
            &VALIDATION_RULES,
            None,
            None,
        )
        .map_err(on_error)?;
        let claims = trusted_token
            .payload_claims()
            .ok_or_else(on_error_response)?;

        let jti = claims.get_string_claim("jti").map_err(on_error)?;

        let sess = state.get_user_session(jti).await.map_err(on_error)?;
        if sess.id != claims.get_string_claim("sub").map_err(on_error)? {
            return Err(on_error_response());
        }

        Ok(sess)
    }
}

/// Validates a token and returns information about the token
#[allow(clippy::unused_async)]
pub async fn validate(user: AuthenticatedUser) -> Json<AuthenticatedUser> {
    Json(user)
}

/// Deletes the current session
///
/// # Errors
/// This function returns an error if the user cannot be logged out.
pub async fn logout(
    state: State<Arc<ServiceState>>,
    user: AuthenticatedUser,
) -> Result<(), Response> {
    user.logout(&state).await.map_err(on_server_error)
}

/// Checks if the token has a scope
///
/// # Errors
/// This function returns an error if the user is authenticated or does not have the needed scope
#[allow(clippy::unused_async)]
pub async fn validate_scope(
    user: AuthenticatedUser,
    Path(scope): Path<String>,
) -> Result<(), Response> {
    if user.has_scope(scope) {
        Ok(())
    } else {
        Err(on_error_response())
    }
}

/// Removes a scope from the current user
///
/// # Errors
/// This function returns 500 if revoking the scope fails
pub async fn remove_scope(
    state: State<Arc<ServiceState>>,
    user: AuthenticatedUser,
    Path(scope_name): Path<String>,
) -> Result<(), Response> {
    user.remove_scope(&state, &scope_name)
        .await
        .map_err(on_server_error)
}
