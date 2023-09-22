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
      }),
  );
  const creation_options = await response.json();

  const credential = await webauthn.create({ publicKey: creation_options });

  console.log(credential);
}

async function main() {
  document.getElementById("signup-form")?.addEventListener("submit", onSubmit);
}
main();
