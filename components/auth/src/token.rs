//! Module for issuing and validating tokens

use std::{collections::HashSet, sync::Arc};

use async_trait::async_trait;
use axum::{
    extract::{FromRequestParts, State},
    headers::Authorization,
    http::{request::Parts, StatusCode},
    response::{IntoResponse, Response},
    Json, RequestPartsExt, TypedHeader,
};
use chrono::{Months, SecondsFormat, Utc};
use deadpool_redis::Pool;
use diesel::prelude::*;
use diesel_async::RunQueryDsl;
use headers::authorization::Bearer;
use rand::{distributions::Alphanumeric, thread_rng, Rng};
use redis::cmd;
use rusty_paseto::prelude::{
    ExpirationClaim, IssuerClaim, Key, Local, PasetoBuilder, PasetoParser, PasetoSymmetricKey,
    SubjectClaim, TokenIdentifierClaim, V4,
};
use serde::{Deserialize, Serialize};
use tracing::error;

use crate::{models::UserSession, schema::auth_user_sessions, ServiceState};
use anyhow::{anyhow, Result};

/// Attempts to load the paseto key from redis
///
/// # Errors
/// This function returns an error if loading the PASETO key fails.
async fn load_paseto_key(redis: &Pool) -> Result<PasetoSymmetricKey<V4, Local>> {
    let mut conn = redis.get().await?;
    let value: Vec<u8> = cmd("GET")
        .arg(&["paseto/key"])
        .query_async(&mut conn)
        .await?;
    if value.len() != 32 {
        if !value.is_empty() {
            cmd("DEL").arg("paseto/key").query_async(&mut conn).await?;
        }
        return Err(anyhow!("Invalid paseto key"));
    }
    let key: Key<32> = Key::from(&value[..]);
    Ok(PasetoSymmetricKey::from(key))
}

/// Saves a PASETO key to redis
///
/// # Errors
/// This function returns an error if the database already has a PASETO key stored.
async fn store_paseto_key(redis: &Pool, key: &PasetoSymmetricKey<V4, Local>) -> Result<()> {
    let mut conn = redis.get().await?;
    cmd("SET")
        .arg("paseto/key")
        .arg(key.as_ref())
        .arg("NX")
        .query_async(&mut conn)
        .await?;
    Ok(())
}

/// Loads or Creates the PASETO key
pub async fn get_or_create_paseto_key(redis: &Pool) -> PasetoSymmetricKey<V4, Local> {
    loop {
        if let Ok(key) = load_paseto_key(redis).await {
            return key;
        }
        if let Ok(new_key) = Key::try_new_random() {
            let paseto_key = PasetoSymmetricKey::from(new_key);
            if store_paseto_key(redis, &paseto_key).await.is_ok() {
                return paseto_key;
            }
        }
    }
}

/// Extractor for an authenticated user
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuthenticatedUser {
    /// ID of the authenticated user
    id: String,
    /// Scopes that the user can access
    scopes: HashSet<String>,
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
        };
        self.try_cache_token(jti, &sess).await.ok();
        Ok(sess)
    }
}

/// Supported and required PASETO claims
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct StandardClaims {
    /// Issuer of the token
    pub iss: String,
    /// Subject of the token
    pub sub: String,
    /// Expiration time of the token
    pub exp: String,
    /// Time where the token starts being valid at
    pub nbf: String,
    /// Time the token has been issued at
    pub iat: String,
    /// The Token Identifier
    pub jti: String,
}

/// The response returned on error
fn on_error_response() -> Response {
    (StatusCode::UNAUTHORIZED, "Unauthorized").into_response()
}

/// Maps the error
fn on_error(e: impl std::fmt::Debug) -> Response {
    error!("{e:?}");
    on_error_response()
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

        let token = token.token();
        let json = {
            let mut parser: PasetoParser<'_, V4, Local> = PasetoParser::default();
            parser.parse(token, &state.paseto_key).map_err(on_error)?
        };
        let claims: StandardClaims = serde_json::value::from_value(json).map_err(on_error)?;

        if claims.iss != "https://auth.chir.rs/" {
            return Err(on_error_response());
        }

        let sess = state
            .get_user_session(&claims.jti)
            .await
            .map_err(on_error)?;
        if sess.id != claims.sub {
            return Err(on_error_response());
        }

        Ok(sess)
    }
}

/// Issues a token to a user
///
/// # Errors
/// This function returns an error if the database connection fails
async fn issue_token(state: &Arc<ServiceState>, user: &str) -> Result<String> {
    let jti: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(16)
        .map(char::from)
        .collect();
    let mut db = state.database.get().await?;
    let now = Utc::now();
    let expire = now.checked_add_months(Months::new(1)).unwrap_or(now);
    let expire_paseto = expire.to_rfc3339_opts(SecondsFormat::Secs, true);
    let token = PasetoBuilder::<V4, Local>::default()
        .set_claim(ExpirationClaim::try_from(expire_paseto)?)
        .set_claim(IssuerClaim::from("https://auth.chir.rs/"))
        .set_claim(SubjectClaim::from(user))
        .set_claim(TokenIdentifierClaim::from(&jti[..]))
        .build(&state.paseto_key)?;
    let db_session = UserSession {
        jti,
        user_id: user.to_owned(),
        exp_at: expire,
        reauth_after: None,
    };

    diesel::insert_into(auth_user_sessions::table)
        .values(db_session)
        .execute(&mut db)
        .await?;

    Ok(token)
}

/// Debug endpoint for creating a dummy token
///
/// # Errors
/// This function returns an error if the token cannot be created.
pub async fn test_issue(State(state): State<Arc<ServiceState>>) -> Result<String, Response> {
    issue_token(&state, "https://lotte.chir.rs/")
        .await
        .map_err(on_error)
}

/// Validates a token and returns information about the token
#[allow(clippy::unused_async)]
pub async fn validate(user: AuthenticatedUser) -> Json<AuthenticatedUser> {
    Json(user)
}
