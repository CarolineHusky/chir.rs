{
  description = "srid/chir-rs: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    nix-packages.url = "github:DarkKirb/nix-packages";
    nix-packages.inputs.nixpkgs.follows = "nixpkgs";
    microformats2-parser.url = "github:darkkirb/microformats2-parser";
    microformats2-parser.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      perSystem = {
        self',
        system,
        lib,
        config,
        pkgs,
        ...
      }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          # The base package set (this value is the default)
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`
          packages = {
            microformats2-parser.source = inputs.microformats2-parser;
          };

          # Add your package overrides here
          settings = {
            webauthn = {
              broken = false;
              jailbreak = true;
            };
          };

          # Development shell configuration
          devShell = {
            hlsCheck.enable = false;
          };

          # What should haskell-flake add to flake outputs?
          autoWire = []; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.alejandra.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
          programs.dhall.enable = true;
          programs.prettier.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Default package & app.
        packages = rec {
          chir-rs-fe = with pkgs;
            mkYarnPackage {
              name = "chir-rs-fe";
              src = ./web;
              packageJSON = ./web/package.json;
              yarnLock = ./web/yarn.lock;
              yarnNix = ./web/yarn.nix;
              nativeBuildInputs = [fixup_yarn_lock];
              configurePhase = ''
                cp -r $node_modules node_modules
              '';
              buildPhase = "yarn build";
              installPhase = "cp -rv dist $out";
              distPhase = "true";
            };
          chir-rs = pkgs.haskell.lib.justStaticExecutables (config.haskellProjects.default.outputs.packages.chir-rs.package.overrideAttrs (super: {
            postUnpack = ''
              cp -rv ${chir-rs-fe} chir-rs-0.1.0.0/static
              chmod -R +w chir-rs-0.1.0.0/static
              mkdir chir-rs-0.1.0.0/static/img
              for f in ${art-assets}/*; do
                ln -sv $f chir-rs-0.1.0.0/static/img
              done
            '';
          }));
          inherit (inputs.nix-packages.packages.${system}) lotte-art;
          art-assets = pkgs.callPackage ./packages/art-encodes.nix {
            inherit (inputs.nix-packages.packages.${system}) lotte-art;
          };
          default = self'.packages.chir-rs;
        };

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "chir-rs";
          meta.description = "Haskell development environment";
          # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
            rnix-lsp
            stack
            nodejs
            yarn
            yarn2nix
            pcre
            zlib
          ];
        };
        formatter = pkgs.alejandra;
      };
    };
}
