//! Key-Value actions
#![allow(clippy::module_name_repetitions)]

use anyhow::Result;
use diesel::prelude::*;
use diesel_async::{
    pooled_connection::deadpool::Pool as DatabasePool, AsyncPgConnection, RunQueryDsl,
};

use crate::models::KeyValue;

/// Retrieves a value from the database
///
/// # Errors
/// This function will return an error if the request fails
pub async fn get_kv(
    db: &DatabasePool<AsyncPgConnection>,
    key: impl AsRef<[u8]> + Send + Sync,
) -> Result<Option<Vec<u8>>> {
    use crate::schema::auth_kv::dsl::{auth_kv, kv_key, kv_value};
    let mut db = db.get().await?;
    let result = auth_kv
        .select(kv_value)
        .filter(kv_key.eq(key.as_ref()))
        .first::<Vec<u8>>(&mut db)
        .await
        .optional()?;
    Ok(result)
}

/// Inserts or updates a value in the database
///
/// # Errors
/// This function will return an error if the request fails
pub async fn upsert_kv(
    db: &DatabasePool<AsyncPgConnection>,
    key: impl AsRef<[u8]> + Send + Sync,
    value: impl AsRef<[u8]> + Send + Sync,
) -> Result<()> {
    use crate::schema::auth_kv::dsl::{auth_kv, kv_key, kv_value};
    let val = KeyValue {
        kv_key: key.as_ref().to_vec(),
        kv_value: value.as_ref().to_vec(),
    };
    let mut db = db.get().await?;
    diesel::insert_into(auth_kv)
        .values(&val)
        .on_conflict(kv_key)
        .do_update()
        .set(kv_value.eq(&val.kv_value))
        .execute(&mut db)
        .await?;
    Ok(())
}

/// Inserts a value into the database
///
/// # Errors
/// This function will return an error if the request fails, or the value already exists
pub async fn insert_kv(
    db: &DatabasePool<AsyncPgConnection>,
    key: impl AsRef<[u8]> + Send + Sync,
    value: impl AsRef<[u8]> + Send + Sync,
) -> Result<()> {
    use crate::schema::auth_kv::dsl::auth_kv;
    let val = KeyValue {
        kv_key: key.as_ref().to_vec(),
        kv_value: value.as_ref().to_vec(),
    };
    let mut db = db.get().await?;
    diesel::insert_into(auth_kv)
        .values(&val)
        .execute(&mut db)
        .await?;
    Ok(())
}

/// Makes sure that a value exists, and returns it
///
/// # Errors
/// This function will return an error if the request fails, or if the code for the init value returns an error
pub async fn ensure_kv<F>(
    db: &DatabasePool<AsyncPgConnection>,
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
