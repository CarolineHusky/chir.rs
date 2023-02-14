//! Support code for webauthn
use once_cell::sync::Lazy;
use uuid::{uuid, Uuid};
use webauthn_rs::{prelude::Url, Webauthn, WebauthnBuilder};

pub mod login;
pub mod registration;

/// The UUID used for calculating the User UUID for webauthn.
const WEBAUTHN_BASE_UUID: Uuid = uuid!("c9e64359-f227-48bd-8cc8-717a1d690d4b");

/// Relying party ID
#[cfg(debug_assertions)]
pub const RELYING_PARTY_ID: &str = "localhost";

/// Relying party ID
#[cfg(not(debug_assertions))]
pub const RELYING_PARTY_ID: &str = "auth.chir.rs";

/// Relying party origin
#[cfg(debug_assertions)]
pub const RELYING_PARTY_ORIGIN: &str = "http://localhost:8080/";

/// Relying party origin
#[cfg(not(debug_assertions))]
pub const RELYING_PARTY_ORIGIN: &str = "https://auth.chir.rs/";

/// The static webauthn structure
#[allow(clippy::expect_used)]
static WEBAUTHN: Lazy<Webauthn> = Lazy::new(|| {
    let rp_origin = Url::parse(RELYING_PARTY_ORIGIN).expect("Valid URL");
    WebauthnBuilder::new(RELYING_PARTY_ID, &rp_origin)
        .expect("Invalid configuration")
        .rp_name("Raccoon Authenticator")
        .build()
        .expect("Invalid configuration")
});

/// Returns the UUID for a given user ID
fn uuid_for_user_id(user_id: &str) -> Uuid {
    Uuid::new_v5(&WEBAUTHN_BASE_UUID, user_id.as_bytes())
}
