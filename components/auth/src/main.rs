//! Authentication Microservice
//!
//! This microservice implements the inner workings of local authentication.
//!
//! Most of the endpoints are only used in the javascript of the web login UI, as for all other cases `IndieAuth` should be used.

use anyhow::Result;
use axum::{
    routing::{delete, get},
    Router,
};
use diesel_async::{
    pooled_connection::{deadpool::Pool as DatabasePool, AsyncDieselConnectionManager},
    AsyncPgConnection,
};
use dotenvy::dotenv;
use educe::Educe;
use pasetors::{keys::SymmetricKey, version4::V4};
use serde::{Deserialize, Serialize};
use std::{net::SocketAddr, path::Path, sync::Arc};
pub mod models;
#[allow(missing_docs, clippy::missing_docs_in_private_items)]
pub mod schema;
pub mod token;

/// Configuration structure
#[derive(Clone, Debug, Deserialize, Serialize)]
#[non_exhaustive]
pub struct Config {
    /// The database config
    database_url: String,
    /// The address to listen on
    listen_addr: SocketAddr,
    /// The redis config
    redis_url: String,
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
#[derive(Educe)]
#[educe(Debug)]
pub struct ServiceState {
    /// The database connection
    #[educe(Debug(ignore))]
    database: DatabasePool<AsyncPgConnection>,
    #[educe(Debug(ignore))]
    /// The redis connection
    redis: deadpool_redis::Pool,
    #[educe(Debug(ignore))]
    /// The PASETO signing key
    paseto_key: SymmetricKey<V4>,
}

/// Connects to the database
///
/// # Errors
/// returns an error if the connection to the database failed
fn connect_to_database(config: &Config) -> Result<DatabasePool<AsyncPgConnection>> {
    let config = AsyncDieselConnectionManager::<AsyncPgConnection>::new(&config.database_url);
    let pool = DatabasePool::builder(config).build()?;
    Ok(pool)
}

#[tokio::main]
async fn main() -> Result<()> {
    dotenv().ok();
    tracing_subscriber::fmt::Subscriber::builder()
        .pretty()
        .with_env_filter(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();
    let config = Config::from_env()?;

    let redis = deadpool_redis::Config::from_url(&config.redis_url)
        .create_pool(Some(deadpool_redis::Runtime::Tokio1))?;

    let state = Arc::new(ServiceState {
        database: connect_to_database(&config)?,
        paseto_key: token::get_or_create_paseto_key(&redis).await,
        redis,
    });

    let app = Router::new()
        .route("/", get(root))
        .route("/get-token", get(token::test_issue))
        .route("/token", get(token::validate))
        .route("/token", delete(token::logout))
        .route("/scopes/:scope", get(token::validate_scope))
        .route("/scopes/:scope", delete(token::remove_scope))
        .with_state(state);

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
