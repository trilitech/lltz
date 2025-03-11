{
  description = "LLTZ Nix Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
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
                })
            ];
          };

          lltz = pkgs.callPackage ./nix/lltz.nix {};

          fmt = treefmt.lib.evalModule pkgs {
            projectRootFile = "dune-project";

            programs.ocamlformat.enable = true;
            programs.alejandra.enable = true;

            settings.global.excludes = ["_build" "result" ".direnv" "vendors/*" "vendored-dune/*"];
          };
        in {
          packages = {
            lltz = lltz;
            default = lltz;
          };

          devShells = rec {
            default = pkgs.mkShell {
              name = "lltz-dev-shell";

              inputsFrom = [lltz];

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
