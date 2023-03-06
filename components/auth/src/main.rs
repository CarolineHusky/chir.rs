//! Authentication Microservice
//!
//! This microservice implements the inner workings of local authentication.
//!
//! Most of the endpoints are only used in the javascript of the web login UI, as for all other cases `IndieAuth` should be used.

use anyhow::Result;
use axum::{
    routing::{delete, get, post},
    Router,
};
use dotenvy::dotenv;
use educe::Educe;
use opaque::CipherSuite;
use opaque_ke::ServerSetup;
use pasetors::{keys::SymmetricKey, version4::V4};
use serde::{Deserialize, Serialize};
use sqlx::{postgres::PgPoolOptions, PgPool};
use std::{net::SocketAddr, path::Path, sync::Arc};
use tower_http::{
    cors::CorsLayer,
    services::{ServeDir, ServeFile},
    trace::TraceLayer,
};
use tracing::info;

use crate::id_generator::generate_id_urlsafe;
pub mod id_generator;
pub mod kv;
pub mod models;
pub mod opaque;
pub mod session;
pub mod token;
pub mod webauthn;

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
    /// Asset root path
    asset_path: String,
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
    database: PgPool,
    #[educe(Debug(ignore))]
    /// The redis connection
    redis: deadpool_redis::Pool,
    #[educe(Debug(ignore))]
    /// The PASETO signing key
    paseto_key: SymmetricKey<V4>,
    /// The OPAQUE server setup
    #[educe(Debug(ignore))]
    server_setup: ServerSetup<CipherSuite>,
    /// Registration key, needed for registering
    registration_key: String,
}

/// Migrate the database, blocking
///
/// # Errors
/// This function returns an error if the database could not be migrated.
#[allow(clippy::expect_used)]
async fn migrate_database(pool: &PgPool) -> Result<()> {
    sqlx::migrate!().run(pool).await?;
    Ok(())
}

/// Connects to the database
///
/// # Errors
/// returns an error if the connection to the database failed
async fn connect_to_database(config: &Config) -> Result<PgPool> {
    let pool = PgPoolOptions::new()
        .max_connections(10)
        .connect(&config.database_url)
        .await?;
    migrate_database(&pool).await?;
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

    let database = connect_to_database(&config).await?;

    let state = Arc::new(ServiceState {
        paseto_key: token::get_or_create_paseto_key(&database).await?,
        server_setup: opaque::get_opaque_server_setup(&database).await?,
        database,
        redis,
        registration_key: generate_id_urlsafe(),
    });

    info!("Register with registration key: {}", state.registration_key);

    let app = Router::new()
        .route("/", get(root))
        .route("/token", get(token::validate))
        .route("/token", delete(token::logout))
        .route("/scopes/:scope", get(token::validate_scope))
        .route("/scopes/:scope", delete(token::remove_scope))
        .route("/session", get(session::list_sessions))
        .route("/session/:session", get(session::load_session))
        .route("/session/:session", delete(session::revoke_session))
        .route(
            "/session/:session/:scope",
            delete(session::revoke_session_scope),
        )
        .route("/register/step1", post(opaque::registration::step_1))
        .route("/register/step2", post(opaque::registration::step_2))
        .route("/register/step3", post(webauthn::registration::step_3))
        .route("/register/step4", post(webauthn::registration::step_4))
        .route("/login/step1", post(webauthn::login::step_1))
        .route("/login/step2", post(webauthn::login::step_2))
        .route("/login/step3", post(opaque::login::step_3))
        .route("/login/step4", post(opaque::login::step_4))
        .route("/login/step5", post(opaque::login::step_5))
        // web endpoints
        .nest_service(
            "/web",
            ServeDir::new(&config.asset_path)
                .not_found_service(ServeFile::new(format!("{}/index.html", config.asset_path))),
        )
        .with_state(state)
        .layer(CorsLayer::permissive())
        .layer(TraceLayer::new_for_http());

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
