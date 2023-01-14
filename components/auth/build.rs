//! Build script for auth-chir-rs

/// Build script for auth-chir-rs
fn main() {
    println!("cargo:rerun-if-changed=migrations");
}
