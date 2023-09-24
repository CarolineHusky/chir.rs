async function onSubmit(e: Event) {
  e.preventDefault();
  const username = (document.getElementById("username") as HTMLInputElement)
    .value;
  const signup_token = (
    document.getElementById("signup-token") as HTMLInputElement
  ).value;

  const webauthn = await require("@github/webauthn-json");

  // fetch creation options
  const response = await fetch(
    "/auth/register/start?" +
      new URLSearchParams({
        username,
        signup_token,
      }),
  );
  const creation_options = await response.json();

  const credential = await webauthn.create({ publicKey: creation_options });

  const response2 = await fetch(
    "/auth/register/finish?" +
      new URLSearchParams({
        username,
      }),
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(credential),
    },
  );

  if (response2.status === 200) {
    window.location.href = "/";
  } else {
    alert("Error: " + response2.status);
  }
}

async function main() {
  document.getElementById("signup-form")?.addEventListener("submit", onSubmit);
}
main();
