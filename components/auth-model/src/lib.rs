//! The types for the `chir-rs-auth` crate

use serde::{Deserialize, Serialize};
use webauthn_rs::prelude::{
    CreationChallengeResponse, PublicKeyCredential, RegisterPublicKeyCredential,
    RequestChallengeResponse,
};

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

/// Request structure for the final registration step
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RegistrationStep4Request {
    /// Token returned from the previous step
    pub continuation_token: String,
    /// Registration
    pub registration: RegisterPublicKeyCredential,
}

/// Request structure for the first login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep1Request {
    /// User ID
    pub user_id: String,
}

/// Response structure for the first login step
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LoginStep1Response {
    /// The challenge to sign
    pub challenge: RequestChallengeResponse,
    /// Token used for the next request
    pub next_token: String,
}

/// Request structure for the second login step
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LoginStep2Request {
    /// Token returned from the previous step
    pub continuation_token: String,
    /// The public key credential
    pub credential: PublicKeyCredential,
}

/// Response structure for the second login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep2Response {
    /// Token used for the next request
    pub next_token: String,
}

/// Request structure for the third login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep3Request {
    /// Token returned from the previous step
    pub continuation_token: String,
    /// The OPAQUE credential request
    pub credential_request: Vec<u8>,
}

/// Response structure for the third login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep3Response {
    /// The OPAQUE credential response
    pub credential_response: Vec<u8>,
    /// Token used for the next request
    pub next_token: String,
}

/// Request structure for the fourth login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep4Request {
    /// Token returned from the previous step
    pub continuation_token: String,
    /// The OPAQUE credential finalization
    pub credential_finalization: Vec<u8>,
    /// Code challenge for OAuth
    pub code_challenge: String,
    /// Requested scopes
    pub scopes: Vec<String>,
}

/// Response structure for the fourth login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep4Response {
    /// Token used for the next request
    pub next_token: String,
}

/// Request structure for the final login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep5Request {
    /// Token returned from the previous step
    pub access_code: String,
    /// Code verifier for oauth
    pub code_verifier: String,
}

/// Response structure for the final login step
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LoginStep5Response {
    /// Access token
    pub access_token: String,
}
