//! Registration code

use anyhow::Result;
use argon2::Argon2;
use chir_rs_auth_model::{
    CipherSuite, RegistrationStep1Request, RegistrationStep1Response, RegistrationStep2Request,
    RegistrationStep2Response, RegistrationStep3Request, RegistrationStep3Response,
    RegistrationStep4Request,
};
use gloo_net::http::Request;
use once_cell::sync::Lazy;
use opaque_ke::{
    ClientRegistration, ClientRegistrationFinishParameters, Identifiers, RegistrationResponse,
    RegistrationUpload,
};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use wasm_bindgen_futures::JsFuture;
use web_sys::{CredentialCreationOptions, Document, HtmlInputElement, SubmitEvent, Window};
use webauthn_rs_proto::RegisterPublicKeyCredential;
use yew::{html, Component, Context, Html};

use crate::normalize_and_validate_password;

/// The state for the registration process.
#[derive(Debug, Default)]
enum RegisterState {
    /// Registration form
    #[default]
    Register,
    /// Continue state
    Continue(Box<RegistrationStep3Response>),
    /// Error to be displayed
    Error(anyhow::Error),
    /// Waiting state
    Waiting(String),
}

/// Struct for the step 1 results
#[derive(Clone, Debug)]
pub struct Step1Result {
    /// The selected username
    username: String,
    /// The selected password
    password: String,
    /// The response from the first registration step
    step1_response: RegistrationStep1Response,
    /// The OPAQUE client registration
    client_registration: ClientRegistration<CipherSuite>,
}

/// Struct for the key derivation results
#[derive(Clone, Debug)]
pub struct KeyDerivResult {
    /// The continuation token for this request
    continuation_token: String,
    /// The registration upload
    registration_upload: RegistrationUpload<CipherSuite>,
}

/// The message that Register uses
#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum RegisterMsg {
    /// Start the registration process
    Register,
    /// Error occurred
    Error(anyhow::Error),
    /// First step of the registration process completed
    Step1Complete(Step1Result),
    /// Key derivation process completed
    KeyDerivComplete(KeyDerivResult),
    /// The signal for continuing the registration process
    Continue,
    /// Second step of the registration process completed
    Step2Complete(String),
    /// Third step of the registration process completed
    Step3Complete(RegistrationStep3Response),
    /// Webauthn registration completed
    WebauthnComplete((String, RegisterPublicKeyCredential)),
    /// Registration completed
    Finished,
}

impl From<anyhow::Error> for RegisterMsg {
    fn from(value: anyhow::Error) -> Self {
        Self::Error(value)
    }
}

impl<A, B> From<Result<A, B>> for RegisterMsg
where
    A: Into<Self>,
    B: Into<Self>,
{
    fn from(result: Result<A, B>) -> Self {
        match result {
            Ok(v) => v.into(),
            Err(v) => v.into(),
        }
    }
}

/// The registration component
#[derive(Debug, Default)]
pub struct Register {
    /// Current state of the registration
    state: RegisterState,
}

impl Register {
    /// Renderer for the registration form
    fn view_register(ctx: &Context<Self>) -> Html {
        html! {
            <>
                <h1>{"Registration"}</h1>
                <form onsubmit={ ctx.link().callback(|e: SubmitEvent| {
                    gloo_console::log!("Submit pressed");
                    e.prevent_default();
                    RegisterMsg::Register
                })}
                action="javascript:void(0);">
                    <p><label for="registration-token">{ "Registration token: " }</label><input type="text" id="registration-token" /></p>
                    <p><label for="username">{ "Username: " }</label><input type="text" id="username" /></p>
                    <p><label for="password">{ "Password: " }</label><input type="password" id="password" /></p>
                    <p><label for="password2">{ "Password (repeat): " }</label><input type="password" id="password2" /></p>
                    <button type="subscribe">{"Register"}</button>
                    <p id="status" />
                </form>
            </>
        }
    }

    /// Renderer for the continue button
    fn view_continue(ctx: &Context<Self>) -> Html {
        html! {
            <button onclick={ ctx.link().callback(|_| RegisterMsg::Continue) }>{"Continue"}</button>
        }
    }

    /// Renderer for the error
    fn view_error(error: &anyhow::Error) -> Html {
        let format = format!("{error:?}");
        gloo_console::log!(&format);
        html! {
            <p>{&format}</p>
        }
    }

    /// Renderer for the waiting state
    fn view_waiting(msg: &str) -> Html {
        html! {
            <p>{msg}{"..."}</p>
        }
    }

    /// Starts the registration process
    async fn start_registration() -> Result<RegisterMsg> {
        let registration_token = get_value_by_id("registration-token")?;
        let username = get_value_by_id("username")?;
        let password = get_value_by_id("password")?;
        let password2 = get_value_by_id("password2")?;

        if password != password2 {
            anyhow::bail!("Passwords don't match");
        }

        // Normalize the password and check against the password policy
        let password = normalize_and_validate_password(&username, &password).await?;

        let mut rng = rand::thread_rng();

        let client_registration_start_result =
            ClientRegistration::<CipherSuite>::start(&mut rng, password.as_bytes())?;

        let step1_request = RegistrationStep1Request {
            registration_token: registration_token.clone(),
            user_id: username.clone(),
            registration_message: client_registration_start_result
                .message
                .serialize()
                .to_vec()
                .into(),
        };

        let step1_response = Request::post(&format!("{BASE_URL}register/step1"))
            .json(&step1_request)?
            .send()
            .await?
            .json::<RegistrationStep1Response>()
            .await?;

        Ok(RegisterMsg::Step1Complete(Step1Result {
            username,
            password,
            step1_response,
            client_registration: client_registration_start_result.state,
        }))
    }

    /// Derives the key from the password
    ///
    /// # Errors
    /// This function will return an error if the key derivation fails
    fn start_key_derivation(resp: Step1Result) -> Result<RegisterMsg> {
        let registration_response = RegistrationResponse::<CipherSuite>::deserialize(
            &resp.step1_response.registration_message.0,
        )?;
        let client_registration = resp.client_registration.clone();
        let password = resp.password;
        let username = resp.username;

        let registration_params = client_registration_params(&username);
        let mut rng = rand::thread_rng();
        let result = client_registration.finish(
            &mut rng,
            password.as_bytes(),
            registration_response,
            registration_params,
        )?;

        Ok(RegisterMsg::KeyDerivComplete(KeyDerivResult {
            continuation_token: resp.step1_response.next_token,
            registration_upload: result.message,
        }))
    }

    /// Completes the OPAQUE part of the reggistration process
    ///
    /// # Errors
    /// Returns an error if there was an issue completing the OPAQUE part of the registration process
    async fn finish_opaque_registration(res: KeyDerivResult) -> Result<RegisterMsg> {
        let request = RegistrationStep2Request {
            continuation_token: res.continuation_token,
            credential_upload: res.registration_upload.serialize().to_vec().into(),
        };

        let step2_response = Request::post(&format!("{BASE_URL}register/step2"))
            .json(&request)?
            .send()
            .await?
            .json::<RegistrationStep2Response>()
            .await?;

        Ok(RegisterMsg::Step2Complete(step2_response.next_token))
    }

    /// Retrieves a webauthn Challenge
    ///
    /// # Errors
    /// Returns an error if there was an issue retrieving the webauthn challenge
    pub(crate) async fn request_webauthn_challenge(next_token: String) -> Result<RegisterMsg> {
        let step3_request = RegistrationStep3Request {
            continuation_token: next_token,
        };

        let step3_response = Request::post(&format!("{BASE_URL}register/step3"))
            .json(&step3_request)?
            .send()
            .await?
            .json::<RegistrationStep3Response>()
            .await?;

        Ok(RegisterMsg::Step3Complete(step3_response))
    }

    /// Registers a webauthn device
    ///
    /// # Errors
    /// Returns an error if there was an issue registering the webauthn device
    pub(crate) async fn register_webauthn(res: RegistrationStep3Response) -> Result<RegisterMsg> {
        // convert into javascript format
        let c_options: CredentialCreationOptions = res.challenge.into();
        // Create a promise that calls the browsers navigator.credentials.create api.
        let promise = window()?
            .navigator()
            .credentials()
            .create_with_options(&c_options)
            .expect_throw("Unable to create promise");

        let jsval = JsFuture::from(promise)
            .await
            .map_err(|e| anyhow::anyhow!("{e:?}"))?;

        let rpkc = web_sys::PublicKeyCredential::from(jsval);
        let rpkc = RegisterPublicKeyCredential::from(rpkc);

        Ok(RegisterMsg::WebauthnComplete((res.next_token, rpkc)))
    }

    /// Completes the registration process
    ///
    /// # Errors
    /// This function will return an error if there was an issue completing the registration process
    pub(crate) async fn finish_registration(
        res: (String, RegisterPublicKeyCredential),
    ) -> Result<RegisterMsg> {
        let step4_request: RegistrationStep4Request = RegistrationStep4Request {
            continuation_token: res.0,
            registration: res.1,
        };

        Request::post(&format!("{BASE_URL}register/step4"))
            .json(&step4_request)?
            .send()
            .await?;

        Ok(RegisterMsg::Finished)
    }
}

impl Component for Register {
    type Message = RegisterMsg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self::default()
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        match self.state {
            RegisterState::Register => Self::view_register(ctx),
            RegisterState::Error(ref e) => Self::view_error(e),
            RegisterState::Waiting(ref msg) => Self::view_waiting(msg),
            RegisterState::Continue(_) => Self::view_continue(ctx),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let new_state = match (&self.state, msg) {
            (_, RegisterMsg::Error(e)) => RegisterState::Error(e),
            (RegisterState::Register, RegisterMsg::Register) => {
                gloo_console::log!("Starting registration");
                //self.state = RegisterState::Waiting("Starting registration".to_owned());
                ctx.link()
                    .send_future(async { RegisterMsg::from(Self::start_registration().await) });
                RegisterState::Register
            }
            (_, RegisterMsg::Step1Complete(ref res)) => {
                gloo_console::log!("Starting key derivation");
                let res = res.clone();
                ctx.link()
                    .send_future(async { RegisterMsg::from(Self::start_key_derivation(res)) });
                RegisterState::Waiting("Starting key derivation (this can take a while)".to_owned())
            }
            (_, RegisterMsg::KeyDerivComplete(ref res)) => {
                gloo_console::log!("Finished key derivation");
                let res = res.clone();
                ctx.link().send_future(async {
                    RegisterMsg::from(Self::finish_opaque_registration(res).await)
                });
                RegisterState::Waiting("Sending OPAQUE credentials".to_owned())
            }
            (_, RegisterMsg::Step2Complete(ref res)) => {
                gloo_console::log!("Finished OPAQUE registration");
                let res = res.clone();
                ctx.link().send_future(async {
                    RegisterMsg::from(Self::request_webauthn_challenge(res).await)
                });
                RegisterState::Waiting("Requesting webauthn challenge".to_owned())
            }
            (_, RegisterMsg::Step3Complete(ref res)) => {
                gloo_console::log!("Received webauthn challenge");
                RegisterState::Continue(Box::new(res.clone()))
            }
            (RegisterState::Continue(ref res), RegisterMsg::Continue) => {
                let res = (**res).clone();
                ctx.link()
                    .send_future(async { RegisterMsg::from(Self::register_webauthn(res).await) });
                RegisterState::Waiting("Authenticating with your authenticator".to_owned())
            }
            (_, RegisterMsg::WebauthnComplete(ref res)) => {
                gloo_console::log!("Finished webauthn registration");
                let res = res.clone();
                ctx.link()
                    .send_future(async { RegisterMsg::from(Self::finish_registration(res).await) });
                RegisterState::Waiting("Uploading the registration".to_owned())
            }
            (_, RegisterMsg::Finished) => {
                gloo_console::log!("Finished registration");
                RegisterState::Waiting("Done. Check the database".to_owned())
            }
            _ => RegisterState::Error(anyhow::anyhow!("Invalid state")),
        };
        self.state = new_state;
        true
    }
}

/// Returns the value for an HTML input element by ID
///
/// # Errors
/// This function will return an error if the element was not found, or the code is not running in a web browser.
fn get_value_by_id(id: &str) -> Result<String> {
    document()?
        .get_element_by_id(id)
        .ok_or_else(|| anyhow::anyhow!("Element not found: {id}"))?
        .dyn_into::<HtmlInputElement>()
        .map(|e| e.value())
        .map_err(|e| anyhow::anyhow!("Failed to convert element to HtmlInputElement: {e:?}"))
}

/// Returns the document
///
/// # Errors
/// This function will return an error if the code is not running in a web browser
fn document() -> Result<Document> {
    window()?
        .document()
        .ok_or_else(|| anyhow::anyhow!("No document found"))
}

/// Returns the window
///
/// # Errors
/// This function will return an error if the code is not running in a web browser
fn window() -> Result<Window> {
    web_sys::window().ok_or_else(|| anyhow::anyhow!("No window found"))
}

/// Creates identifiers for binding a request to the user and server
const fn mk_identifiers(user_name: &str) -> Identifiers<'_> {
    Identifiers {
        client: Some(user_name.as_bytes()),
        server: Some(b"https://auth.chir.rs/"),
    }
}

/// Base URL for the server
#[cfg(not(debug_assertions))]
const BASE_URL: &str = "https://auth.chir.rs/";

/// Base URL for the server
#[cfg(debug_assertions)]
const BASE_URL: &str = "http://localhost:5621/";

/// Argon2id instance
#[allow(clippy::expect_used)]
static ARGON2: Lazy<Argon2<'static>> = Lazy::new(|| {
    Argon2::new(
        argon2::Algorithm::Argon2id,
        argon2::Version::V0x13,
        argon2::Params::new(
            1_048_576, // 1GiB
            4, 1, None,
        )
        .expect("Accepted Argon2 parameters"),
    )
});

/// Create client registration finish parameters
fn client_registration_params(
    user_name: &str,
) -> ClientRegistrationFinishParameters<'_, 'static, CipherSuite> {
    ClientRegistrationFinishParameters {
        identifiers: mk_identifiers(user_name),
        ksf: Some(&*ARGON2),
    }
}
