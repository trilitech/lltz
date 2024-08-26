{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    opam-repository = {
      flake = false;
      url = "github:ocaml/opam-repository";
    };
    opam2nix = {
      url = "github:vapourismo/opam-nix-integration";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.opam-repository.follows = "opam-repository";
    };
  };

  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [opam2nix.overlays.default];
        };

        opamPackages = pkgs.opamPackages.overrideScope (
          pkgs.lib.composeManyExtensions [
            (final: prev: {
              repository = prev.repository.override {src = opam-repository;};
            })

            (
              final: prev:
                prev.repository.select {
                  opams = [
                    {
                      name = "lltz";
                      src = ./.;
                      opam = ./lltz.opam;
                    }
                  ];

                  packageConstraints = ["fmt" "utop" "ocamlformat" "ocamlformat-rpc" "ocaml-lsp-server"];
                }
            )

            # An override that patches opam packages that are missing dependencies from their opam files
            (
              final: prev: {
                # Add ocp-indent to ocamlformat-lib
                ocamlformat-lib = prev.ocamlformat-lib.overrideAttrs (old: {
                  propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ [prev.ocp-indent];
                });

                # Add fmt to logs
                logs = prev.logs.overrideAttrs (old: {
                  buildInputs = (old.buildInputs or []) ++ [prev.fmt];
                });
              }
            )
          ]
        );
      in {
        packages.default = opamPackages.lltz;
        packages.zarith = opamPackages.zarith;

        devShells.default = pkgs.mkShell {
          name = "lltz-shell";

          inputsFrom = [opamPackages.lltz];

          buildInputs = with pkgs; [alejandra] ++ (with opamPackages; [utop ocamlformat ocamlformat-rpc ocaml-lsp-server]);
        };
      });
}
