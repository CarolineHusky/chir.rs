//! Authentication Microservice
//!
//! This microservice implements the inner workings of local authentication.
//!
//! Most of the endpoints are only used in the javascript of the web login UI, as for all other cases `IndieAuth` should be used.

use anyhow::Result;
use axum::{routing::get, Router};
use diesel::{
    r2d2::{ConnectionManager, Pool},
    PgConnection,
};
use std::{net::SocketAddr, path::Path, sync::Arc};

use dotenvy::dotenv;
use serde::{Deserialize, Serialize};
pub mod models;
#[allow(missing_docs, clippy::missing_docs_in_private_items)]
pub mod schema;

/// Database configuration
#[derive(Clone, Debug, Deserialize, Serialize)]
#[non_exhaustive]
pub struct DbConfig {
    /// Database URL
    pub db_url: String,
    /// Maximum pool size
    pub max_pool_size: u32,
    /// Minimum idle connections to the database
    pub min_pool_idle: Option<u32>,
}

/// Configuration structure
#[derive(Clone, Debug, Deserialize, Serialize)]
#[non_exhaustive]
pub struct Config {
    /// The database config
    database: DbConfig,
    /// The address to listen on
    listen_addr: SocketAddr,
}

impl Config {
    /// Attempts to load the configuration from a path
    ///
    /// # Errors
    /// An error occurs when the file cannot be read, is malformed, or accesses the network
    pub fn from_file(p: impl AsRef<Path>) -> Result<Self> {
        Ok(serde_dhall::from_file(p).parse()?)
    }
    /// Attempts to load the configuration with the path read from an environment variable
    ///
    /// # Errors
    /// An error occurs when the environment variable `CONFIG_FILE` is not set, or if [`Config::from_file`] fails.
    pub fn from_env() -> Result<Self> {
        Self::from_file(std::env::var("CONFIG_FILE")?)
    }
}

/// Shared state for the running server
#[derive(Debug)]
pub struct State {
    /// The database connection
    database: Pool<ConnectionManager<PgConnection>>,
}

/// Connects to the database
///
/// # Errors
/// returns an error if the connection to the database failed
fn connect_to_database(config: &Config) -> Result<Pool<ConnectionManager<PgConnection>>> {
    let manager = ConnectionManager::<PgConnection>::new(&config.database.db_url);
    Ok(Pool::builder()
        .max_size(config.database.max_pool_size)
        .min_idle(config.database.min_pool_idle)
        .build(manager)?)
}

#[tokio::main]
async fn main() -> Result<()> {
    dotenv().ok();
    tracing_subscriber::fmt::Subscriber::builder()
        .pretty()
        .with_env_filter(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();
    let config = Config::from_env()?;

    let state = Arc::new(State {
        database: connect_to_database(&config)?,
    });

    let app = Router::new().route("/", get(root)).with_state(state);

    axum::Server::bind(&config.listen_addr)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}

/// Returns some information about the service
#[allow(clippy::unused_async)]
async fn root() -> &'static str {
    "This is the authentication service for local users. You probably do not want to access this directly."
}
