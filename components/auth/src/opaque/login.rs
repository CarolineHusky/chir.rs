//! OPAQUE login code

use std::sync::Arc;

use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use base64::{engine::general_purpose, Engine as _};
use chir_rs_auth_model::{
    LoginStep3Request, LoginStep3Response, LoginStep4Request, LoginStep4Response,
    LoginStep5Request, LoginStep5Response,
};
use diesel::prelude::*;
use diesel_async::RunQueryDsl;
use opaque_ke::{
    CredentialFinalization, CredentialRequest, Identifiers, ServerLogin,
    ServerLoginStartParameters, ServerRegistration,
};
use redis::cmd;
use sha2::{Digest, Sha256};
use uuid::Uuid;

use crate::{token::on_error, ServiceState};

use super::CipherSuite;

/// Creates identifiers for binding a request to the user and server
const fn mk_identifiers(user_name: &str) -> Identifiers<'_> {
    Identifiers {
        client: Some(user_name.as_bytes()),
        server: Some(b"https://auth.chir.rs/"),
    }
}

impl ServiceState {
    /// Retrieves the OPAQUE registration for the user
    ///
    /// # Errors
    /// This function returns an error if the access fails
    pub async fn get_registration_for_user(
        self: &Arc<Self>,
        user_id: &str,
    ) -> Result<Option<ServerRegistration<CipherSuite>>> {
        use crate::schema::auth_users::dsl::{auth_users, id, password_file};
        let mut db = self.database.get().await?;
        let res_serialized = auth_users
            .select(password_file)
            .filter(id.eq(user_id))
            .first::<Vec<u8>>(&mut db)
            .await
            .optional()?;

        if let Some(serialized) = res_serialized {
            Ok(Some(ServerRegistration::deserialize(
                serialized.as_slice(),
            )?))
        } else {
            Ok(None)
        }
    }

    /// Starts an OPAQUE login
    ///
    /// # Errors
    /// This function returns an error if the login fails
    pub async fn start_opaque_login(
        self: &Arc<Self>,
        continuation_token: &str,
        credential_request: &[u8],
    ) -> Result<(Vec<u8>, String)> {
        let mut conn = self.redis.get().await?;
        let user_id: String = cmd("GETDEL")
            .arg(format!("login/step2:{continuation_token}"))
            .query_async(&mut conn)
            .await?;

        let credential_request = CredentialRequest::<CipherSuite>::deserialize(credential_request)?;
        let password_file = self.get_registration_for_user(&user_id).await?;
        let server_login_start_params = ServerLoginStartParameters {
            context: None,
            identifiers: mk_identifiers(&user_id),
        };
        let login = ServerLogin::<CipherSuite>::start(
            &mut rand::thread_rng(),
            &self.server_setup,
            password_file,
            credential_request,
            user_id.as_bytes(),
            server_login_start_params,
        )?;
        let new_state = (login.state.serialize().to_vec(), user_id);
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        cmd("SET")
            .arg(format!("login/step3:{req_uuid}"))
            .arg(serde_json::to_string(&new_state)?)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok((login.message.serialize().to_vec(), req_uuid))
    }

    /// Completes an OPAQUE login
    ///
    /// # Errors
    /// This function returns an error if the authentication was unsuccessful.
    pub async fn finish_opaque_login(
        self: &Arc<Self>,
        continuation_token: &str,
        login_response: &[u8],
        code_challenge: &str,
        scopes: &[String],
    ) -> Result<String> {
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("login/step3:{continuation_token}"))
            .query_async(&mut conn)
            .await?;
        let (server_login_serialized, user_id): (Vec<u8>, String) =
            serde_json::from_str(&state_json)?;
        let server_login = ServerLogin::<CipherSuite>::deserialize(&server_login_serialized)?;
        let credential_finalization = CredentialFinalization::deserialize(login_response)?;
        // TODO: do something with the session key maybe
        server_login.finish(credential_finalization)?;
        let token = self.issue_token(&user_id, scopes).await?;
        let new_state = (code_challenge, token);
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        cmd("SET")
            .arg(format!("login/step4:{req_uuid}"))
            .arg(serde_json::to_string(&new_state)?)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok(req_uuid)
    }

    /// Retrieves a token from the access code
    ///
    /// # Errors
    /// This function returns an error if the authentication was unsuccessful.
    pub async fn get_access_token(
        self: &Arc<Self>,
        access_code: &str,
        code_verifier: &str,
    ) -> Result<String> {
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("login/step4:{access_code}"))
            .query_async(&mut conn)
            .await?;
        let (code_challenge, token): (String, _) = serde_json::from_str(&state_json)?;
        let mut hasher = Sha256::new();
        hasher.update(code_verifier.as_bytes());
        let digest = hasher.finalize();
        let received_challenge = general_purpose::URL_SAFE_NO_PAD.encode(digest);
        if code_challenge != received_challenge {
            anyhow::bail!("Invalid challenge");
        }
        Ok(token)
    }
}

/// Performs the third step of the authentication process.
///
/// # Errors
/// Returns an error if the authentication process fails.
pub async fn step_3(
    state: State<Arc<ServiceState>>,
    Json(request): Json<LoginStep3Request>,
) -> Result<Json<LoginStep3Response>, Response> {
    let (credential_response, next_token) = state
        .start_opaque_login(&request.continuation_token, &request.credential_request)
        .await
        .map_err(on_error)?;

    Ok(Json(LoginStep3Response {
        credential_response,
        next_token,
    }))
}

/// Performs the fourth step of the authentication process.
///
/// # Errors
/// Returns an error if the authentication process fails.
pub async fn step_4(
    state: State<Arc<ServiceState>>,
    Json(request): Json<LoginStep4Request>,
) -> Result<Json<LoginStep4Response>, Response> {
    let next_token = state
        .finish_opaque_login(
            &request.continuation_token,
            &request.credential_finalization,
            &request.code_challenge,
            &request.scopes,
        )
        .await
        .map_err(on_error)?;
    Ok(Json(LoginStep4Response { next_token }))
}

/// Performs the fifth step of the authentication process.
///
/// # Errors
/// Returns an error if the authentication process fails.
pub async fn step_5(
    state: State<Arc<ServiceState>>,
    Json(request): Json<LoginStep5Request>,
) -> Result<Json<LoginStep5Response>, Response> {
    let access_token = state
        .get_access_token(&request.access_code, &request.code_verifier)
        .await
        .map_err(on_error)?;
    Ok(Json(LoginStep5Response { access_token }))
}
