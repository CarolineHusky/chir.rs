//! Database models for the auth service

use crate::schema::{auth_authenticators, auth_session_scopes, auth_user_sessions, auth_users};
use chrono::{DateTime, Utc};
use diesel::prelude::*;
use educe::Educe;
use serde::{Deserialize, Serialize};

/// Database model for local users.
#[derive(Queryable, Identifiable, Educe, Insertable, Serialize, Deserialize)]
#[diesel(table_name = auth_users)]
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

/// Authenticator device for a local user
#[derive(Queryable, Identifiable, Associations, Educe, Serialize, Deserialize)]
#[diesel(belongs_to(User))]
#[diesel(table_name = auth_authenticators)]
#[non_exhaustive]
#[educe(Debug)]
pub struct Authenticator {
    /// ID of the authenticator
    pub id: i32,
    /// User ID the authenticator belongs to
    pub user_id: String,
    /// The webauthn registration of the authenticator
    #[educe(Debug(ignore))]
    pub webauthn_registration: String,
}

/// Authenticator device for a local user
#[derive(Insertable, Educe, Serialize, Deserialize)]
#[educe(Debug)]
#[diesel(table_name = auth_authenticators)]
pub struct AuthenticatorInsert {
    /// User ID the authenticator belongs to
    pub user_id: String,
    /// The webauthn registration of the authenticator
    #[educe(Debug(ignore))]
    pub webauthn_registration: String,
}

/// User session
#[derive(Queryable, Identifiable, Associations, Debug, Insertable, Serialize, Deserialize)]
#[diesel(belongs_to(User))]
#[diesel(table_name = auth_user_sessions)]
#[non_exhaustive]
#[diesel(primary_key(jti))]
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

/// Scopes associated with a session
#[derive(Queryable, Identifiable, Associations, Debug, Insertable, Serialize, Deserialize)]
#[diesel(belongs_to(UserSession, foreign_key = jti))]
#[diesel(table_name = auth_session_scopes)]
#[non_exhaustive]
#[diesel(primary_key(jti, scope))]
pub struct SessionScope {
    /// The Session the scope belongs to
    pub jti: String,
    /// The granted scope for the session
    pub scope: String,
}
