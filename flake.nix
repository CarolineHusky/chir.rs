{
  description = "chir.rs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:DarkKirb/flake-utils";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    cargo2nix = {
      url = "github:DarkKirb/cargo2nix/release-0.11.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    cargo2nix,
    ...
  } @ inputs:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "riscv64-linux"] (system: let
      overlays = [
        cargo2nix.overlays.default
        (import rust-overlay)
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
      crossPkgs = import nixpkgs {
        inherit system;
        overlays =
          overlays
          ++ [
            (self: super: {
              rust =
                super.rust
                // {
                  toRustTarget = system: let
                    res = super.rust.toRustTarget system;
                  in
                    if res == "wasm32-unknown-wasi"
                    then "wasm32-unknown-unknown"
                    else res;
                };
            })
          ];
        crossSystem = {
          system = "wasm32-wasi";
          useLLVM = true;
        };
        config.allowUnsupportedSystem = true;
      };
      rustPkgs = pkgs.rustBuilder.makePackageSet {
        packageFun = import ./Cargo.nix;
        rustChannel = "nightly";
        rustVersion = "latest";
        packageOverrides = pkgs: pkgs.rustBuilder.overrides.all;
      };
      rustCrossPkgs = crossPkgs.rustBuilder.makePackageSet {
        packageFun = args: let
          cnix = import ./Cargo.nix;
          hostPlatform = args.hostPlatform;
          hostPlatformPatch =
            if hostPlatform.isWasm
            then {
              parsed.kernel.name = "unknown";
              config = "wasm32-unknown-none-unknown";
              isWasi = false;
            }
            else {};
        in
          cnix (args
            // {
              hostPlatform = crossPkgs.lib.recursiveUpdate hostPlatform hostPlatformPatch;
            });
        rustChannel = "nightly";
        rustVersion = "latest";
        packageOverrides = pkgs: pkgs.rustBuilder.overrides.all;
        target = "wasm32-unknown-unknown";
      };
    in rec {
      devShells.default = with pkgs;
        mkShell {
          buildInputs =
            [
              (rust-bin.nightly.latest.default.override {
                extensions = ["rust-src"];
                targets = ["wasm32-unknown-unknown"];
              })
              cargo2nix.packages.${system}.cargo2nix
              statix
              cargo-bloat
              cargo-crev
              cargo-deny
              cargo-edit
              cargo-outdated
              dhall
              dhall-json
              dhall-lsp-server
              gdb
              rnix-lsp
              sqlx-cli
              sqlite
              pkg-config
              openssl
              trunk
              binaryen
              wasm-bindgen-cli
            ]
            ++ (
              if system == "x86_64-linux"
              then [
                cargo-tarpaulin
              ]
              else []
            );
        };
      packages =
        pkgs.lib.mapAttrs (_: v: v {}) rustPkgs.workspace
        // {
          chir-rs-auth-web = pkgs.stdenvNoCC.mkDerivation {
            name = "chir-rs-auth-web";
            src = (rustCrossPkgs.workspace.chir-rs-auth-web {}).bin;
            nativeBuildInputs = [pkgs.wasm-bindgen-cli];
            index_html = ./components/auth-web/index.html;
            buildPhase = ''
              wasm-bindgen --target web ./bin/chir-rs-auth-web.wasm --out-dir $out
            '';
            installPhase = ''
              cp $index_html $out/index.html
            '';
          };
        };

      nixosModules.default = import ./nixos {
        inherit inputs system;
      };
      hydraJobs =
        packages
        // {
          inherit devShells formatter;
        };
      formatter = pkgs.alejandra;
    });
}
