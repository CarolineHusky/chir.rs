//! Registration code for OPAQUE

use std::sync::Arc;

use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use chir_rs_auth_model::{
    RegistrationStep1Request, RegistrationStep1Response, RegistrationStep2Request,
    RegistrationStep2Response,
};
use opaque_ke::{RegistrationRequest, RegistrationUpload, ServerRegistration};
use redis::cmd;
use uuid::Uuid;

use crate::{
    models::User,
    token::{on_error, on_error_response},
    ServiceState,
};

use super::CipherSuite;

impl ServiceState {
    /// Starts registration for a specific user
    ///
    /// The function assumes that the client has supplied the registration token
    ///
    /// # Errors
    /// This function returns an error if the request is malformed
    pub async fn start_opaque_registration(
        self: &Arc<Self>,
        user_id: &str,
        registration_message: &[u8],
    ) -> Result<(String, Vec<u8>)> {
        let server_setup = self.get_opaque_server_setup().await;
        let registration_request = RegistrationRequest::deserialize(registration_message)?;
        let server_registration_start_result = ServerRegistration::<CipherSuite>::start(
            &server_setup,
            registration_request,
            user_id.as_bytes(),
        )?;
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        let mut conn = self.redis.get().await?;
        cmd("SET")
            .arg(format!("registration/step1:{req_uuid}"))
            .arg(user_id)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        let message = server_registration_start_result.message.serialize();

        Ok((req_uuid, message.to_vec()))
    }

    /// Continues the registration process
    ///
    /// # Errors
    /// returns an error if the registration fails
    pub async fn continue_opaque_registration(
        self: &Arc<Self>,
        cont_token: &str,
        registration_message: &[u8],
    ) -> Result<String> {
        let mut conn = self.redis.get().await?;
        let user_id: String = cmd("GETDEL")
            .arg(format!("registration/step1:{cont_token}"))
            .query_async(&mut conn)
            .await?;
        let registration_upload =
            RegistrationUpload::<CipherSuite>::deserialize(registration_message)?;
        let new_password_file = ServerRegistration::<CipherSuite>::finish(registration_upload);
        let user = User {
            id: user_id.clone(),
            password_file: new_password_file.serialize().to_vec(),
            activated: false,
        };
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        cmd("SET")
            .arg(format!("registration/step2:{req_uuid}"))
            .arg(serde_json::to_string(&user)?)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok(req_uuid)
    }
}

/// Route for the first registration step
///
/// # Errors
/// this function returns an error if registration fails, or if the client isn’t authorized to request a registration
pub async fn step_1(
    state: State<Arc<ServiceState>>,
    Json(request): Json<RegistrationStep1Request>,
) -> Result<Json<RegistrationStep1Response>, Response> {
    if request.registration_token != state.registration_key {
        return Err(on_error_response());
    }
    let (next_token, registration_message) = state
        .start_opaque_registration(&request.user_id, &request.registration_message)
        .await
        .map_err(on_error)?;
    Ok(Json(RegistrationStep1Response {
        registration_message,
        next_token,
    }))
}

/// Route for the second registration step
///
/// # Errors
/// this function returns an error if registration fails, or if the client isn’t authorized to request a registration
pub async fn step_2(
    state: State<Arc<ServiceState>>,
    Json(request): Json<RegistrationStep2Request>,
) -> Result<Json<RegistrationStep2Response>, Response> {
    let next_token = state
        .continue_opaque_registration(&request.continuation_token, &request.credential_upload)
        .await
        .map_err(on_error)?;
    Ok(Json(RegistrationStep2Response { next_token }))
}
