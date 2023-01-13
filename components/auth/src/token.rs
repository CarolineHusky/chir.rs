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
use chrono::{Months, SecondsFormat, Utc};
use diesel::prelude::*;
use diesel_async::{
    pooled_connection::deadpool::Pool as DatabasePool, AsyncPgConnection, RunQueryDsl,
};
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
use tracing::{error, info};
use uuid::Uuid;

use crate::{
    kv::ensure_kv,
    models::{SessionScope, UserSession},
    schema::{auth_session_scopes, auth_user_sessions},
    ServiceState,
};

/// Loads or Creates the PASETO key
///
/// # Errors
/// This function returns an error if loading or generating the PASETO key fails.
pub async fn get_or_create_paseto_key(
    db: &DatabasePool<AsyncPgConnection>,
) -> Result<SymmetricKey<V4>> {
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
    pub async fn logout(&self, state: &Arc<ServiceState>) -> Result<()> {
        use crate::schema::auth_user_sessions::dsl::{auth_user_sessions, jti};
        let mut db = state.database.get().await?;
        info!("{} logged out", self.session_id);
        diesel::delete(auth_user_sessions.filter(jti.eq(&self.session_id)))
            .execute(&mut db)
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
    pub async fn remove_scope(&self, state: &Arc<ServiceState>, scope_name: &str) -> Result<()> {
        use crate::schema::auth_session_scopes::dsl::{auth_session_scopes, jti, scope};
        let mut db = state.database.get().await?;
        diesel::delete(
            auth_session_scopes
                .filter(jti.eq(&self.session_id))
                .filter(scope.eq(&scope_name)),
        )
        .execute(&mut db)
        .await?;
        state.invalidate_user_session(&self.session_id).await?;
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
    pub async fn list_user_sessions(self: &Arc<Self>, user_id_str: &str) -> Result<Vec<String>> {
        use crate::schema::auth_user_sessions::dsl::{auth_user_sessions, jti, user_id};
        let mut db = self.database.get().await?;
        let res = auth_user_sessions
            .select(jti)
            .filter(user_id.eq(&user_id_str))
            .load(&mut db)
            .await?;
        Ok(res.into_iter().collect())
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
            session_id: jti.to_owned(),
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
    pub async fn issue_token(self: &Arc<Self>, user: &str, scopes: &[String]) -> Result<String> {
        let jti = Uuid::new_v4().to_string();
        let mut db = self.database.get().await?;
        let now = Utc::now();
        let expire = now.checked_add_months(Months::new(1)).unwrap_or(now);
        let expire_paseto = expire.to_rfc3339_opts(SecondsFormat::Secs, true);

        let mut claims = Claims::new()?;
        claims.issuer("https://auth.chir.rs/")?;
        claims.subject(user)?;
        claims.expiration(&expire_paseto)?;
        claims.token_identifier(&jti)?;

        let token = local::encrypt(&self.paseto_key, &claims, None, None)?;

        let db_session = UserSession {
            jti: jti.clone(),
            user_id: user.to_owned(),
            exp_at: expire,
            reauth_after: None,
        };

        diesel::insert_into(auth_user_sessions::table)
            .values(db_session)
            .execute(&mut db)
            .await?;

        diesel::insert_into(auth_session_scopes::table)
            .values(
                scopes
                    .iter()
                    .map(|v| SessionScope {
                        jti: jti.clone(),
                        scope: v.clone(),
                    })
                    .collect::<Vec<_>>(),
            )
            .execute(&mut db)
            .await?;

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
    let incident_uuid = Uuid::new_v4();
    error!("Incident UUID: {incident_uuid:?}");
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(json!({
            "title": "Internal Server Error",
            "status": 500,
            "incident_uuid": incident_uuid.to_string()
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
#[allow(clippy::wildcard_imports)]
async fn get_token_info(state: &Arc<ServiceState>, token_jti: &str) -> Result<UserSession> {
    use crate::schema::auth_user_sessions::dsl::*;
    let mut db = state.database.get().await?;
    let res = auth_user_sessions
        .filter(jti.eq(token_jti))
        .first(&mut db)
        .await?;
    Ok(res)
}

/// Returns available scopes for a token
///
/// # Errors
/// This function returns an error if the connection to the database fails
#[allow(clippy::wildcard_imports)]
async fn get_token_scopes(state: &Arc<ServiceState>, token_jti: &str) -> Result<HashSet<String>> {
    use crate::schema::auth_session_scopes::dsl::*;
    let mut db = state.database.get().await?;
    let res = auth_session_scopes
        .select(scope)
        .filter(jti.eq(token_jti))
        .load(&mut db)
        .await?;
    Ok(res.into_iter().collect())
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

        let sess = state
            .get_user_session(claims.get_string_claim("jti").map_err(on_error)?)
            .await
            .map_err(on_error)?;
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
