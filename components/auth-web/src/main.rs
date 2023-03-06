//! Webapp for the authentication service.
#![allow(clippy::future_not_send)]
use gloo_net::http::Request;
use register::Register;
use sha1::{Digest, Sha1};
use unicode_normalization::UnicodeNormalization;
use yew::{function_component, html, Html};
use yew_router::{prelude::Link, BrowserRouter, Routable, Switch};
use zxcvbn::zxcvbn;

pub mod register;

/// Route list for the auth service webapp
#[derive(Debug, Copy, Clone, Routable, PartialEq, Eq)]
pub enum Route {
    /// Index page, lets you sxct whether to log in or log out
    #[at("/")]
    Home,
    /// Login page
    #[at("/login")]
    Login,
    /// Registration page
    #[at("/register")]
    Register,
    /// Default route, if a 404 occurs
    #[not_found]
    #[at("/404")]
    NotFound,
}

/// Switch between routes
fn switch(routes: Route) -> Html {
    match routes {
        Route::Home => html! {
            <p>
                <h1>{"Log in or sign with auth.chir.rs"}</h1>
                <Link<Route> to={Route::Login}>{"Login"}</Link<Route>> <br />
                <Link<Route> to={Route::Register}>{"Register"}</Link<Route>> <br />
            </p>
        },
        Route::Login => html! { <br /> },
        Route::Register => html! { <Register /> },
        Route::NotFound => html! { <h1>{ "404" }</h1> },
    }
}

/// App entrypoint
#[function_component]
fn App() -> Html {
    html! {
        <BrowserRouter>
            <Switch<Route> render={switch} />
        </BrowserRouter>
    }
}

/// Main entrypoint
fn main() {
    yew::Renderer::<App>::new().render();
}

/// Normalizes a password
#[must_use]
pub fn normalize_password(password: &str) -> String {
    // Normalize the password
    let password = password.trim(); // Remove whitespace from the start or end of the password
    password.nfkc().collect::<String>() // Nist special publication 800-63B: 5.1.1.2 “ the verifier SHOULD apply the Normalization Process for Stabilized Strings using either the NFKC or NFKD normalization”
}

/// Verify if a password matches password policies
///
/// # Errors
/// This function will return an error if the password does not meet the password policy
pub async fn normalize_and_validate_password(
    username: &str,
    password: &str,
) -> anyhow::Result<String> {
    let password = normalize_password(password);

    // Verify that the password matches our requirements
    if password.len() < 8 {
        anyhow::bail!("Password must be at least 8 characters long");
    }

    let strength_estimate = zxcvbn(&password, &[username, "chir.rs", "auth.chir.rs", "chirr"])?;

    if strength_estimate.score() < 3 {
        anyhow::bail!("Password is too weak according to ZXCVBN ({strength_estimate:?})");
    }

    let mut hasher = Sha1::new();
    hasher.update(password.as_bytes());
    let digest = hasher.finalize();
    let mut digest_buf = vec![0; digest.len() * 2];
    let digest = base16ct::upper::encode_str(&digest, &mut digest_buf)
        .map_err(|e| anyhow::anyhow!("Failed to base16 encode: {e}"))?;

    let pwned_passwords = Request::get(&format!(
        "https://api.pwnedpasswords.com/range/{}",
        &digest[..5]
    ))
    .send()
    .await?
    .text()
    .await?;

    for line in pwned_passwords.lines() {
        if line.starts_with(&digest[5..]) {
            anyhow::bail!("Password was used in previous breaches of third party sites");
        }
    }

    Ok(password)
}
