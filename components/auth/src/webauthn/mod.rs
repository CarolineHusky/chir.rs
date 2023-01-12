//! Support code for webauthn
use once_cell::sync::Lazy;
use uuid::{uuid, Uuid};
use webauthn_rs::{prelude::Url, Webauthn, WebauthnBuilder};

pub mod registration;

/// The UUID used for calculating the User UUID for webauthn.
const WEBAUTHN_BASE_UUID: Uuid = uuid!("c9e64359-f227-48bd-8cc8-717a1d690d4b");

/// The static webauthn structure
#[allow(clippy::expect_used)]
static WEBAUTHN: Lazy<Webauthn> = Lazy::new(|| {
    let rp_id = "https://auth.chir.rs";
    let rp_origin = Url::parse(rp_id).expect("Valid URL");
    WebauthnBuilder::new(rp_id, &rp_origin)
        .expect("Invalid configuration")
        .rp_name("Raccoon Authenticator")
        .build()
        .expect("Invalid configuration")
});

/// Returns the UUID for a given user ID
fn uuid_for_user_id(user_id: &str) -> Uuid {
    Uuid::new_v5(&WEBAUTHN_BASE_UUID, user_id.as_bytes())
}
