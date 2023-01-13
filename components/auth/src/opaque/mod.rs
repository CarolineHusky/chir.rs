//! OPAQUE support logic

use anyhow::Result;
use diesel_async::{pooled_connection::deadpool::Pool as DatabasePool, AsyncPgConnection};
use opaque_ke::ServerSetup;

use crate::kv::ensure_kv;

pub mod login;
pub mod registration;

/// The ciphersuite used by OPAQUE
#[derive(Debug, Copy, Clone)]
pub struct CipherSuite;

impl opaque_ke::CipherSuite for CipherSuite {
    type OprfCs = opaque_ke::Ristretto255;
    type KeGroup = opaque_ke::Ristretto255;
    type KeyExchange = opaque_ke::key_exchange::tripledh::TripleDh;
    type Ksf = argon2::Argon2<'static>;
}

/// Loads the OPAQUE configuration from the database, or creates a new one if it does not exist.
///
/// # Errors
/// This function will return an error if the database could not be accessed.
pub async fn get_opaque_server_setup(
    db: &DatabasePool<AsyncPgConnection>,
) -> Result<ServerSetup<CipherSuite>> {
    let setup = ensure_kv(db, b"opaque/setup", || {
        let server_setup = ServerSetup::<CipherSuite>::new(&mut rand::thread_rng());
        Ok(server_setup.serialize().to_vec())
    })
    .await?;
    ServerSetup::deserialize(&setup).map_err(Into::into)
}
