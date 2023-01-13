//! Webauthn login steps

use std::sync::Arc;

use crate::{token::on_error, ServiceState};
use anyhow::Result;
use axum::{extract::State, response::Response, Json};
use chir_rs_auth_model::{
    LoginStep1Request, LoginStep1Response, LoginStep2Request, LoginStep2Response,
};
use diesel::prelude::*;
use diesel_async::RunQueryDsl;
use redis::cmd;
use uuid::Uuid;
use webauthn_rs::prelude::{
    AuthenticationResult, PublicKeyCredential, RequestChallengeResponse, SecurityKey,
};

use super::WEBAUTHN;

impl ServiceState {
    pub async fn get_webauthn_devices(self: &Arc<Self>, user: &str) -> Result<Vec<SecurityKey>> {
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

    pub async fn get_webauthn_device(
        self: &Arc<Self>,
        user: &str,
        auth_id: &[u8],
    ) -> Result<SecurityKey> {
        use crate::schema::auth_authenticators::dsl::*;
        let mut db = self.database.get().await?;
        let res: String = auth_authenticators
            .select(webauthn_registration)
            .filter(user_id.eq(user))
            .filter(id.eq(auth_id))
            .first(&mut db)
            .await?;
        Ok(serde_json::from_str(&res)?)
    }

    pub async fn update_authenticator(
        self: &Arc<Self>,
        user: &str,
        auth_result: AuthenticationResult,
    ) -> Result<()> {
        use crate::schema::auth_authenticators::dsl::*;
        let mut authenticator = self
            .get_webauthn_device(user, auth_result.cred_id().as_ref())
            .await?;
        if authenticator.update_credential(&auth_result) != Some(true) {
            return Ok(());
        }
        let mut db = self.database.get().await?;
        diesel::update(auth_authenticators.find(auth_result.cred_id().as_ref()))
            .set(webauthn_registration.eq(serde_json::to_string(&auth_result)?))
            .execute(&mut db)
            .await?;
        Ok(())
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

    pub async fn finish_webauthn_authentication(
        self: &Arc<Self>,
        continuation_token: &str,
        credentials: PublicKeyCredential,
    ) -> Result<String> {
        let mut conn = self.redis.get().await?;
        let state_json: String = cmd("GETDEL")
            .arg(format!("login/step1:{continuation_token}"))
            .query_async(&mut conn)
            .await?;
        let (state, user_id) = serde_json::from_str(&state_json)?;
        let result = WEBAUTHN.finish_securitykey_authentication(&credentials, &state)?;
        self.update_authenticator(user_id, result).await?; // TODO: verify the counter
        let req_uuid = Uuid::new_v4().as_hyphenated().to_string();
        cmd("SET")
            .arg(format!("login/step2:{req_uuid}"))
            .arg(user_id)
            .arg("EX")
            .arg(300)
            .query_async(&mut conn)
            .await?;
        Ok(req_uuid)
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

pub async fn step_2(
    state: State<Arc<ServiceState>>,
    Json(request): Json<LoginStep2Request>,
) -> Result<Json<LoginStep2Response>, Response> {
    let next_token = state
        .finish_webauthn_authentication(&request.continuation_token, request.credential)
        .await
        .map_err(on_error)?;
    Ok(Json(LoginStep2Response { next_token }))
}
