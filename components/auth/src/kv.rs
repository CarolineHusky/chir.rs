//! Key-Value actions
#![allow(clippy::module_name_repetitions)]

use anyhow::Result;
use sqlx::{query, PgPool};

/// Retrieves a value from the database
///
/// # Errors
/// This function will return an error if the request fails
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::panic)]
pub async fn get_kv(db: &PgPool, key: impl AsRef<[u8]> + Send + Sync) -> Result<Option<Vec<u8>>> {
    let result = query!(
        "SELECT kv_value FROM auth_kv WHERE kv_key = $1",
        key.as_ref()
    )
    .fetch_optional(db)
    .await?;
    Ok(result.map(|v| v.kv_value))
}

/// Inserts or updates a value in the database
///
/// # Errors
/// This function will return an error if the request fails
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::panic)]
pub async fn upsert_kv(
    db: &PgPool,
    key: impl AsRef<[u8]> + Send + Sync,
    value: impl AsRef<[u8]> + Send + Sync,
) -> Result<()> {
    query!(
        r#"
        INSERT INTO auth_kv (kv_key, kv_value)
            VALUES ($1, $2)
        ON CONFLICT (kv_key)
        DO UPDATE SET kv_value = $2
        "#,
        key.as_ref(),
        value.as_ref()
    )
    .execute(db)
    .await?;
    Ok(())
}

/// Inserts a value into the database
///
/// # Errors
/// This function will return an error if the request fails, or the value already exists
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::panic)]
pub async fn insert_kv(
    db: &PgPool,
    key: impl AsRef<[u8]> + Send + Sync,
    value: impl AsRef<[u8]> + Send + Sync,
) -> Result<()> {
    query!(
        r#"
        INSERT INTO auth_kv (kv_key, kv_value)
            VALUES ($1, $2)
        "#,
        key.as_ref(),
        value.as_ref()
    )
    .execute(db)
    .await?;
    Ok(())
}

/// Makes sure that a value exists, and returns it
///
/// # Errors
/// This function will return an error if the request fails, or if the code for the init value returns an error
pub async fn ensure_kv<F>(
    db: &PgPool,
    key: impl AsRef<[u8]> + Send + Sync,
    init: F,
) -> Result<Vec<u8>>
where
    F: Fn() -> Result<Vec<u8>> + Send + Sync,
{
    let key = key.as_ref();
    loop {
        if let Some(v) = get_kv(db, key).await? {
            return Ok(v);
        }
        insert_kv(db, key, init()?).await?;
    }
}
