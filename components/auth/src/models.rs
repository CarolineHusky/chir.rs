//! Database models for the auth service

use chrono::{DateTime, Utc};
use educe::Educe;
use serde::{Deserialize, Serialize};

/// Database model for local users.
#[derive(Educe, Serialize, Deserialize)]
#[non_exhaustive]
#[educe(Debug)]
pub struct User {
    /// User ID of local users, their URL
    pub id: String,
    /// The OPAQUE password file of the user
    #[educe(Debug(ignore))]
    pub password_file: Vec<u8>,
    /// Whether or not the user has been activated (only activated users can log in)
    pub activated: bool,
}

/// User session
#[derive(Clone, Debug, Serialize, Deserialize)]
#[non_exhaustive]
pub struct UserSession {
    /// the PASETO token ID of the user session
    pub jti: String,
    /// The user the session belongs to
    pub user_id: String,
    /// The last possible time the token can be used at
    pub exp_at: DateTime<Utc>,
    /// When the next reauth happens (None if there are no reauths needed)
    pub reauth_after: Option<DateTime<Utc>>,
}
