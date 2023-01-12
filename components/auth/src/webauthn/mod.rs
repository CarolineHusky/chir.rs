//! Support code for webauthn
use once_cell::sync::Lazy;
use uuid::{uuid, Uuid};
use webauthn_rs::prelude::Url;

pub mod registration;

const WEBAUTHN_BASE_UUID: Uuid = uuid!("c9e64359-f227-48bd-8cc8-717a1d690d4b");

static WEBAUTHN: Webauthn = Lazy::new(|| {
    let rp_id = "https://auth.chir.rs";
    let rp_origin = Url::parse(rp_id);
    WebauthnBuilder::new(rp_id, &rp_origin)
        .expect("Invalid configuration")
        .rp_name("Raccoon Authenticator")
        .build()
});

fn uuid_for_user_id(user_id: &str) -> Uuid {
    Uuid::new_v5(&WEBAUTHN_BASE_UUID, user_id.as_bytes())
}
