{
  description = "chir.rs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    cargo2nix = {
      url = "github:cargo2nix/cargo2nix";
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
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux"] (system: let
      overlays = [
        cargo2nix.overlays.default
        (import rust-overlay)
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
      rustPkgs = pkgs.rustBuilder.makePackageSet {
        packageFun = import ./Cargo.nix;
        rustChannel = "1.67.0";
        packageOverrides = pkgs: pkgs.rustBuilder.overrides.all;
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
            ]
            ++ (
              if system == "x86_64-linux"
              then [
                cargo-tarpaulin
              ]
              else []
            );
        };
      packages = pkgs.lib.mapAttrs (_: v: v {}) rustPkgs.workspace;

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
