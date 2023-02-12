//! Registration code for webauthn

use std::sync::Arc;

use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use chir_rs_auth_model::{
    RegistrationStep3Request, RegistrationStep3Response, RegistrationStep4Request,
};
use redis::cmd;
use sqlx::query;
use webauthn_rs::prelude::{
    AuthenticatorAttachment, CreationChallengeResponse, RegisterPublicKeyCredential,
    SecurityKeyRegistration,
};

use crate::{id_generator::generate_id_urlsafe, models::User, token::on_error, ServiceState};

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
        let req_id = generate_id_urlsafe();
        cmd("SET")
            .arg(format!("registration/step3:{req_id}"))
            .arg(serde_json::to_string(&new_state)?)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok((registration, req_id))
    }

    /// Completes the registration
    ///
    /// # Errors
    /// Function returns an error if registering the authenticator failed
    #[allow(clippy::missing_panics_doc)]
    #[allow(clippy::panic)]
    pub async fn finish_webauthn_registration(
        self: &Arc<Self>,
        continuation_token: &str,
        registration: RegisterPublicKeyCredential,
    ) -> Result<()> {
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("registration/step3:{continuation_token}"))
            .query_async(&mut conn)
            .await?;
        let (user, registration_state): (User, SecurityKeyRegistration) =
            serde_json::from_str(&state_json)?;
        let security_key =
            super::WEBAUTHN.finish_securitykey_registration(&registration, &registration_state)?;
        let mut tx = self.database.begin().await?;

        query!(
            r#"
            INSERT INTO auth_users
                (id, password_file, activated)
            VALUES
                ($1, $2, $3)
        "#,
            user.id,
            user.password_file,
            user.activated
        )
        .execute(&mut tx)
        .await?;

        query!(
            r#"
                INSERT INTO auth_authenticators
                    (id, user_id, webauthn_registration)
                VALUES
                    ($1, $2, $3)
            "#,
            security_key.cred_id().0,
            user.id,
            serde_json::to_value(&registration)?
        )
        .execute(&mut tx)
        .await?;
        tx.commit().await?;
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
