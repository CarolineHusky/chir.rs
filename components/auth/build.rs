//! auth-chir-rs build script

/// Auth-chir-rs build script
fn main() {
    // trigger recompilation when a new migration is added
    println!("cargo:rerun-if-changed=migrations");
}
