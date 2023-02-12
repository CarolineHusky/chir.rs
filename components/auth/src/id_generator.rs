//! Generation logic for time based IDs

use std::sync::atomic::{AtomicU16, Ordering};

use base64::{
    alphabet::Alphabet,
    engine::{general_purpose::NO_PAD, GeneralPurpose},
    Engine as _,
};
use chrono::Utc;
use once_cell::sync::Lazy;
use rand::Rng;

/// Random state variable
/// This is assigned at startup and never changes
static RANDOM_STATE: Lazy<u16> = Lazy::new(|| rand::thread_rng().gen::<u16>());

/// Atomic counter variable
/// Every generation increases this value by one.
static COUNTER: AtomicU16 = AtomicU16::new(0);

/// Generates an integer ID
///
/// The ID is generated as follows:
///
/// bits 127-32: non-leap nanoseconds since 1970-01-01T00:00:00Z
/// bits 31-16: node ID
/// bits 15-0: counter
///
/// # Panics
/// This function will panic if the system clock is before 1970-01-01T00:00:00Z.
pub fn generate_numeric_id() -> u128 {
    let now = Utc::now();
    let timestamp_secs = now.timestamp();
    assert!(timestamp_secs >= 0, "Valid time");
    let timestamp_secs: u64 = timestamp_secs.try_into().unwrap_or_default();
    let timestamp: u128 =
        u128::from(timestamp_secs) * 1_000_000_000 + u128::from(now.timestamp_subsec_nanos());
    let counter = COUNTER.fetch_add(1, Ordering::Relaxed);
    timestamp << 32 | u128::from(*RANDOM_STATE) << 16 | u128::from(counter)
}

/// The encoder for this ID type, sortable through comparing byte values
#[allow(clippy::expect_used)]
static URLSAFE_ENCODER: Lazy<GeneralPurpose> = Lazy::new(|| {
    let alphabet =
        Alphabet::new("-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")
            .expect("valid alphabet");
    GeneralPurpose::new(&alphabet, NO_PAD)
});

/// Generates an urlsafe alphanumeric ID
#[must_use]
pub fn generate_id_urlsafe() -> String {
    let id = generate_numeric_id();
    let id_bytes = id.to_be_bytes();
    URLSAFE_ENCODER.encode(id_bytes)
}
