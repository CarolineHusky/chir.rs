//! Database models for the auth service
#![allow(single_use_lifetimes)]
use std::borrow::Cow;

use crate::schema::{auth_authenticators, auth_session_scopes, auth_user_sessions, auth_users};
use chrono::{DateTime, FixedOffset};
use diesel::prelude::*;
use educe::Educe;

/// Database model for local users.
#[derive(Queryable, Identifiable, Educe)]
#[diesel(table_name = auth_users)]
#[non_exhaustive]
#[educe(Debug)]
pub struct User<'a> {
    /// User ID of local users, their URL
    pub id: Cow<'a, str>,
    /// The OPAQUE password file of the user
    #[educe(Debug(ignore))]
    pub password_file: Cow<'a, [u8]>,
}

/// Authenticator device for a local user
#[derive(Queryable, Identifiable, Associations, Educe)]
#[diesel(belongs_to(User<'_>))]
#[diesel(table_name = auth_authenticators)]
#[non_exhaustive]
#[educe(Debug)]
pub struct Authenticator<'a> {
    /// ID of the authenticator
    pub id: i32,
    /// User ID the authenticator belongs to
    pub user_id: Cow<'a, str>,
    /// The webauthn registration of the authenticator
    #[educe(Debug(ignore))]
    pub webauthn_registration: Cow<'a, str>,
}

/// User session
#[derive(Queryable, Identifiable, Associations, Debug)]
#[diesel(belongs_to(User<'_>))]
#[diesel(table_name = auth_user_sessions)]
#[non_exhaustive]
#[diesel(primary_key(jti))]
pub struct UserSession<'a> {
    /// the PASETO token ID of the user session
    pub jti: Cow<'a, str>,
    /// The user the session belongs to
    pub user_id: Cow<'a, str>,
    /// The last possible time the token can be used at
    pub exp_at: DateTime<FixedOffset>,
    /// When the next reauth happens (None if there are no reauths needed)
    pub reauth_after: Option<DateTime<FixedOffset>>,
}

/// Scopes associated with a session
#[derive(Queryable, Identifiable, Associations, Debug)]
#[diesel(belongs_to(UserSession<'_>, foreign_key = jti))]
#[diesel(table_name = auth_session_scopes)]
#[non_exhaustive]
#[diesel(primary_key(jti, scope))]
pub struct SessionScope {
    /// The Session the scope belongs to
    pub jti: String,
    /// The granted scope for the session
    pub scope: String,
}
