//! Registration code for webauthn

use std::sync::Arc;

use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use chir_rs_auth_model::{
    RegistrationStep3Request, RegistrationStep3Response, RegistrationStep4Request,
};
use diesel_async::RunQueryDsl;
use redis::cmd;
use uuid::Uuid;
use webauthn_rs::prelude::{
    AuthenticatorAttachment, CreationChallengeResponse, RegisterPublicKeyCredential,
    SecurityKeyRegistration,
};

use crate::{
    models::{AuthenticatorInsert, User},
    token::on_error,
    ServiceState,
};

impl ServiceState {
    /// Starts webauthn token registration
    ///
    /// # Errors
    /// This function returns an error if the user isn’t currently registering
    pub async fn start_webauthn_registration(
        self: &Arc<Self>,
        continuation_token: &str,
    ) -> Result<(CreationChallengeResponse, String)> {
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("registration/step2:{continuation_token}"))
            .query_async(&mut conn)
            .await?;
        let state: User = serde_json::from_str(&state_json)?;
        let user_uuid = super::uuid_for_user_id(&state.id);
        let (registration, webauthn_state) = super::WEBAUTHN.start_securitykey_registration(
            user_uuid,
            &state.id,
            &state.id,
            None,
            None,
            Some(AuthenticatorAttachment::CrossPlatform),
        )?;
        let new_state = (state, webauthn_state);
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        cmd("SET")
            .arg(format!("registration/step3:{req_uuid}"))
            .arg(serde_json::to_string(&new_state)?)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok((registration, req_uuid))
    }

    /// Completes the registration
    ///
    /// # Errors
    /// Function returns an error if registering the authenticator failed
    pub async fn finish_webauthn_registration(
        self: &Arc<Self>,
        continuation_token: &str,
        registration: RegisterPublicKeyCredential,
    ) -> Result<()> {
        use crate::schema::{auth_authenticators, auth_users};
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("registration/step3:{continuation_token}"))
            .query_async(&mut conn)
            .await?;
        let (user, registration_state): (User, SecurityKeyRegistration) =
            serde_json::from_str(&state_json)?;
        let security_key =
            super::WEBAUTHN.finish_securitykey_registration(&registration, &registration_state)?;
        let mut db = self.database.get().await?;
        diesel::insert_into(auth_users::table)
            .values(&user)
            .execute(&mut db)
            .await?;
        let authenticator = AuthenticatorInsert {
            user_id: user.id,
            webauthn_registration: serde_json::to_string(&security_key)?,
        };
        diesel::insert_into(auth_authenticators::table)
            .values(&authenticator)
            .execute(&mut db)
            .await?;
        Ok(())
    }
}

/// Route for the third registration step
///
/// # Errors
/// this function returns an error if registration fails, or if the client isn’t authorized to request a registration
pub async fn step_3(
    state: State<Arc<ServiceState>>,
    Json(request): Json<RegistrationStep3Request>,
) -> Result<Json<RegistrationStep3Response>, Response> {
    let (challenge, next_token) = state
        .start_webauthn_registration(&request.continuation_token)
        .await
        .map_err(on_error)?;
    Ok(Json(RegistrationStep3Response {
        challenge,
        next_token,
    }))
}

/// Route for the final registration step
///
/// # Errors
/// this function returns an error if registration fails, or if the client isn’t authorized to request a registration
pub async fn step_4(
    state: State<Arc<ServiceState>>,
    Json(request): Json<RegistrationStep4Request>,
) -> Result<(), Response> {
    state
        .finish_webauthn_registration(&request.continuation_token, request.registration)
        .await
        .map_err(on_error)
}
