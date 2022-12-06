use std::borrow::Cow;

use crate::schema::{authenticators, session_scopes, user_sessions, users};
use chrono::{DateTime, FixedOffset};
use diesel::prelude::*;
use educe::Educe;

#[derive(Queryable, Identifiable, Educe)]
#[diesel(table_name = users)]
#[non_exhaustive]
#[educe(Debug)]
pub struct User<'a> {
    pub id: Cow<'a, str>,
    #[educe(Debug(ignore))]
    pub password_file: Cow<'a, [u8]>,
}

#[derive(Queryable, Identifiable, Associations, Educe)]
#[diesel(belongs_to(User<'_>))]
#[diesel(table_name = authenticators)]
#[non_exhaustive]
#[educe(Debug)]
pub struct Authenticator<'a> {
    pub id: i32,
    pub user_id: Cow<'a, str>,
    #[educe(Debug(ignore))]
    pub webauthn_registration: Cow<'a, str>,
}

#[derive(Queryable, Identifiable, Associations, Debug)]
#[diesel(belongs_to(User<'_>))]
#[diesel(table_name = user_sessions)]
#[non_exhaustive]
#[diesel(primary_key(jti))]
pub struct UserSession<'a> {
    pub jti: Cow<'a, str>,
    pub user_id: Cow<'a, str>,
    pub exp_at: DateTime<FixedOffset>,
    pub reauth_after: Option<DateTime<FixedOffset>>,
}

#[derive(Queryable, Identifiable, Associations, Debug)]
#[diesel(belongs_to(UserSession<'_>, foreign_key = jti))]
#[diesel(table_name = session_scopes)]
#[non_exhaustive]
#[diesel(primary_key(jti, scope))]
pub struct SessionScope {
    pub jti: String,
    pub scope: String,
}
