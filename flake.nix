{
  description = "LIGO Nix Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (
        system: let
          lib = nixpkgs.legacyPackages.${system}.lib;

          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              ocaml-overlay.overlays.default
              (import ./nix/overlay.nix)
              (_: prev:
                with prev; {
                  ocamlPackages = ocaml-ng.ocamlPackages_4_14;
                  coqPackages = coqPackages_8_13;
                  # ocamlformat = ocaml-ng.ocamlPackages_4_14.ocamlformat_0_21_0;
                })
            ];
          };

          ligo = pkgs.callPackage ./nix/ligo.nix {};
          ligo-syntaxes = ./tools/vscode/syntaxes;

          fmt = treefmt.lib.evalModule pkgs {
            projectRootFile = "dune-project";

            programs.ocamlformat.enable = true;
            programs.alejandra.enable = true;

            settings.global.excludes = ["_build" "result" ".direnv" "vendors/*" "vendored-dune/*"];
          };
        in {
          packages = {
            ligo = ligo;
            default = ligo;
          };

          devShells = with ligo-webide; rec {
            default = pkgs.mkShell {
              name = "ligo-dev-shell";

              inputsFrom = [ligo];

              buildInputs = with pkgs; [
                alejandra
                shellcheck
                ocamlformat
                ocamlPackages.utop
                ocamlPackages.ocaml-lsp
                ocamlPackages.merlin
              ];

              shellHook = ''
                export MERLIN_PATH="${pkgs.ocamlPackages.merlin}";
              '';
            };
          };

          formatter = fmt.config.build.wrapper;
        }
      );
}
