//! Webauthn login steps

use std::sync::Arc;

use crate::{token::on_error, ServiceState};
use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use chir_rs_auth_model::{LoginStep1Request, LoginStep1Response};
use diesel::prelude::*;
use diesel_async::RunQueryDsl;
use redis::cmd;
use uuid::Uuid;
use webauthn_rs::prelude::{RequestChallengeResponse, SecurityKey};

use super::WEBAUTHN;

impl ServiceState {
    pub async fn get_webauthn_devices(
        self: &Arc<Self>,
        user: &str,
    ) -> Result<Vec<SecurityKey>, anyhow::Error> {
        use crate::schema::auth_authenticators::dsl::*;
        let mut db = self.database.get().await?;
        let res = auth_authenticators
            .select(webauthn_registration)
            .filter(user_id.eq(user))
            .load(&mut db)
            .await?;
        Ok(res
            .into_iter()
            .map(|x: String| serde_json::from_str::<SecurityKey>(&x))
            .collect::<Result<Vec<SecurityKey>, _>>()?)
    }

    pub async fn start_webauthn_authentication(
        self: &Arc<Self>,
        user_id: &str,
    ) -> Result<(RequestChallengeResponse, String)> {
        let security_keys = self.get_webauthn_devices(user_id).await?;
        let (challenge, state) = WEBAUTHN.start_securitykey_authentication(&security_keys)?;
        let mut conn = self.redis.get().await?;
        let new_state = (state, user_id);
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        cmd("SET")
            .arg(format!("login/step1:{req_uuid}"))
            .arg(serde_json::to_string(&new_state)?)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok((challenge, req_uuid))
    }
}

pub async fn step_1(
    state: State<Arc<ServiceState>>,
    Json(request): Json<LoginStep1Request>,
) -> Result<Json<LoginStep1Response>, Response> {
    let (challenge, next_token) = state
        .start_webauthn_authentication(&request.user_id)
        .await
        .map_err(on_error)?;
    Ok(Json(LoginStep1Response {
        challenge,
        next_token,
    }))
}
