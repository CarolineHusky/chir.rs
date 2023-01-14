//! API Calls for managing sessions

use std::sync::Arc;

use axum::{
    extract::{Path, State},
    response::Response,
    Json,
};
use uuid::Uuid;

use crate::{
    token::{on_error_response, on_server_error, AuthenticatedUser},
    ServiceState,
};

/// Lists sessions for the current user
///
/// Requires scope: `https://auth.chir.rs/sessions/read`
///
/// # Errors
///
/// This function returns an error if the user is not allowed to access the route,
/// or if the method fails.
pub async fn list_sessions(
    state: State<Arc<ServiceState>>,
    user: AuthenticatedUser,
) -> Result<Json<Vec<Uuid>>, Response> {
    if !user.has_scope("https://auth.chir.rs/sessions/read") {
        return Err(on_error_response());
    }

    let sessions = state
        .list_user_sessions(user.id())
        .await
        .map_err(on_server_error)?;

    Ok(Json(sessions))
}

/// Loads session information for a session of the current user
///
/// Requires scope: `https://auth.chir.rs/sessions/read`
///
/// # Errors
/// This function returns an error if the user is not allowed to access the route,
/// or if the method fails.
#[allow(clippy::module_name_repetitions)]
pub async fn load_session(
    state: State<Arc<ServiceState>>,
    user: AuthenticatedUser,
    Path(session): Path<Uuid>,
) -> Result<Json<AuthenticatedUser>, Response> {
    if !user.has_scope("https://auth.chir.rs/sessions/read") {
        return Err(on_error_response());
    }
    let session = state
        .get_session_for_user(&session, user.id())
        .await
        .map_err(on_server_error)?;
    Ok(Json(session))
}

/// Deletes a session of the current user
///
/// Requires scope: `https://auth.chir.rs/sessions/write`
///
/// # Errors
/// This function returns an error if the user is not allowed to access the route,
/// or if the method fails.
#[allow(clippy::module_name_repetitions)]
pub async fn revoke_session(
    state: State<Arc<ServiceState>>,
    user: AuthenticatedUser,
    Path(session): Path<Uuid>,
) -> Result<(), Response> {
    if !user.has_scope("https://auth.chir.rs/sessions/write") {
        return Err(on_error_response());
    }
    let session = state
        .get_session_for_user(&session, user.id())
        .await
        .map_err(on_server_error)?;
    session.logout(&state).await.map_err(on_server_error)?;
    Ok(())
}

/// Revokes a scope for a specific session
///
/// Requires scope: `https://auth.chir.rs/sessions/write`
///
/// # Errors
/// This function returns an error if the user is not allowed to access the route,
/// or if the method fails.
pub async fn revoke_session_scope(
    state: State<Arc<ServiceState>>,
    user: AuthenticatedUser,
    Path((session, scope)): Path<(Uuid, String)>,
) -> Result<(), Response> {
    if !user.has_scope("https://auth.chir.rs/sessions/write") {
        return Err(on_error_response());
    }
    let session = state
        .get_session_for_user(&session, user.id())
        .await
        .map_err(on_server_error)?;
    session
        .remove_scope(&state, &scope)
        .await
        .map_err(on_server_error)?;
    Ok(())
}
