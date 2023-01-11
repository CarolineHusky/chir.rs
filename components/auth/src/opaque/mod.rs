//! OPAQUE support logic

use std::sync::Arc;

use anyhow::Result;
use opaque_ke::ServerSetup;
use redis::cmd;
use tracing::warn;

use crate::ServiceState;

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

impl ServiceState {
    /// Fetches the server setup info from redis
    async fn fetch_opaque_server_setup(self: &Arc<Self>) -> Result<ServerSetup<CipherSuite>> {
        let mut conn = self.redis.get().await?;

        let value: Vec<u8> = cmd("GET")
            .arg(&["opaque/setup"])
            .query_async(&mut conn)
            .await?;
        Ok(ServerSetup::deserialize(&value)?)
    }

    /// Stores the server setup, returning an error if it already exists
    async fn set_server_setup(
        self: &Arc<Self>,
        server_setup: ServerSetup<CipherSuite>,
    ) -> Result<()> {
        let mut conn = self.redis.get().await?;
        let setup = server_setup.serialize();
        cmd("SET")
            .arg("opaque/setup")
            .arg(setup.as_slice())
            .arg("NX")
            .query_async(&mut conn)
            .await?;
        Ok(())
    }
    /// Loads or generate server setup info
    pub async fn get_opaque_server_setup(self: &Arc<Self>) -> ServerSetup<CipherSuite> {
        loop {
            if let Ok(server_setup) = self.fetch_opaque_server_setup().await {
                return server_setup;
            }
            warn!("No key found. Assuming initial setup");
            let server_setup = ServerSetup::new(&mut rand::thread_rng());
            self.set_server_setup(server_setup).await.ok();
        }
    }
}
