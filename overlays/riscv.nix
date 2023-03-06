nixpkgs: let
  pkgs = import nixpkgs {
    system = "x86_64-linux";
    crossSystem = "riscv64-linux";
  };
in
  self: super: {
    inherit (pkgs) dhall dhall-json dhall-lsp-server;
  }
