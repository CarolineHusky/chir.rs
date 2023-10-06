async function onSubmit(e: Event) {
  e.preventDefault();
  const username = (document.getElementById("username") as HTMLInputElement)
    .value;
  const signup_token = (
    document.getElementById("signup-token") as HTMLInputElement
  ).value;

  // fetch creation options
  const response = await fetch(
    "/auth/register/start?" +
      new URLSearchParams({
        username,
        signup_token,
      }),
    {
      headers: {
        Accept: "application/cbor",
      },
    },
  );
  const creation_options_cbor = await response.arrayBuffer();

  const cbor = await import("cbor-x");

  const creation_options = cbor.decode(new Uint8Array(creation_options_cbor));

  const webauthn = await import("@github/webauthn-json");

  const credential = await webauthn.create({ publicKey: creation_options });

  const response2 = await fetch(
    "/auth/register/finish?" +
      new URLSearchParams({
        username,
      }),
    {
      method: "POST",
      headers: {
        "Content-Type": "application/cbor",
      },
      body: cbor.encode(credential),
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
