//! Registration code for OPAQUE

use std::sync::Arc;

use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use opaque_ke::{RegistrationRequest, ServerRegistration};
use redis::cmd;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{
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
}

/// Request structure for the first registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step1Request {
    /// Token used for registration
    pub registration_token: String,
    /// User ID of choice
    pub user_id: String,
    /// OPAQUE registration start request
    pub registration_message: Vec<u8>,
}

/// Response structure for the first registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step1Response {
    /// OPAQUE registration start response
    pub registration_message: Vec<u8>,
    /// Token used for the next request
    pub next_token: String,
}

/// Route for the first registration step
///
/// # Errors
/// this function returns an error if registration fails, or if the client isn’t authorized to request a registration
pub async fn step_1(
    state: State<Arc<ServiceState>>,
    Json(request): Json<Step1Request>,
) -> Result<Json<Step1Response>, Response> {
    if request.registration_token != state.registration_key {
        return Err(on_error_response());
    }
    let (next_token, registration_message) = state
        .start_opaque_registration(&request.user_id, &request.registration_message)
        .await
        .map_err(on_error)?;
    Ok(Json(Step1Response {
        registration_message,
        next_token,
    }))
}
