//! OPAQUE support logic

use anyhow::Result;
use opaque_ke::ServerSetup;
use sqlx::PgPool;

use crate::kv::ensure_kv;

pub mod login;
pub mod registration;

pub use chir_rs_auth_model::CipherSuite;

/// Loads the OPAQUE configuration from the database, or creates a new one if it does not exist.
///
/// # Errors
/// This function will return an error if the database could not be accessed.
pub async fn get_opaque_server_setup(db: &PgPool) -> Result<ServerSetup<CipherSuite>> {
    let setup = ensure_kv(db, b"opaque/setup", || {
        let server_setup = ServerSetup::<CipherSuite>::new(&mut rand::thread_rng());
        Ok(server_setup.serialize().to_vec())
    })
    .await?;
    ServerSetup::deserialize(&setup).map_err(Into::into)
}
