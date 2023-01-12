//! The types for the `chir-rs-auth` crate

use serde::{Deserialize, Serialize};
use webauthn_rs::prelude::CreationChallengeResponse;

/// Request structure for the first registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegistrationStep1Request {
    /// Token used for registration
    pub registration_token: String,
    /// User ID of choice
    pub user_id: String,
    /// OPAQUE registration start request
    pub registration_message: Vec<u8>,
}

/// Response structure for the first registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegistrationStep1Response {
    /// OPAQUE registration start response
    pub registration_message: Vec<u8>,
    /// Token used for the next request
    pub next_token: String,
}

/// Request structure for the second registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegistrationStep2Request {
    /// Token used for continuing the registration
    pub continuation_token: String,
    /// OPAQUE credential upload
    pub credential_upload: Vec<u8>,
}

/// Response structure for the second registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegistrationStep2Response {
    /// Token used for the next request
    pub next_token: String,
}

/// Request structure for the third registration step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegistrationStep3Request {
    /// Token returned from the previous step
    pub continuation_token: String,
}

/// Response structure for the third registration step
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RegistrationStep3Response {
    /// The registration challenge to sign
    pub challenge: CreationChallengeResponse,
    /// Token used for the next request
    pub next_token: String,
}
