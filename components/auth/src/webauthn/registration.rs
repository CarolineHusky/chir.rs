//! Registration code for webauthn

use std::ops::Not;

use anyhow::Result;
use serde::{Deserialize, Serialize};
use webauthn_rs::prelude::{AuthenticatorAttachment, CreationChallengeResponse};

use crate::{models::User, ServiceState};

impl ServiceState {
    pub async fn start_webauthn_registration(
        self: &Arc<Self>,
        continuation_token: &str,
    ) -> Result<(CreationChallengeResponse, String)> {
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("registration/step2:{cont_token}"))
            .query_async(&mut conn)
            .await?;
        let state: User = serde_json::from_str(&state_json)?;
        let user_uuid = super::uuid_for_user_id(&state.user_id);
        let (registration, webauthn_state) = super::WEBAUTHN.start_securitykey_registration(
            user_uuid,
            &state.user_id,
            &state.user_id,
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
}

/// Request structure for the third registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step3Request {
    /// Token returned from the previous step
    pub cont_token: String,
}

/// Response structure for the third registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step3Response {
    /// The registration challenge to sign
    pub challenge: CreationChallengeResponse,
    /// Token used for the next request
    pub next_token: String,
}

/// Route for the third registration step
///
/// # Errors
/// this function returns an error if registration fails, or if the client isn’t authorized to request a registration
pub async fn step_3(
    state: State<Arc<ServiceState>>,
    Json(request): Json<Step3Request>,
) -> Result<Json<Step3Response>, Response> {
    let (challenge, next_token) = state
        .start_webauthn_registration(&request.continuation_token, &request.credential_upload)
        .await
        .map_err(on_error)?;
    Ok(Json(Step3Response {
        challenge,
        next_token,
    }))
}
