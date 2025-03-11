{
  stdenv,
  lib,
  pkgs,
  libiconv,
}: let
  inherit (pkgs) darwin ocamlPackages;
in
  with ocamlPackages;
    buildDunePackage rec {
      pname = "lltz";
      version = "dev";
      src = ./..;

      propagatedBuildInputs =
        [
          ppx_deriving
          ppx_sexp_conv
          ptime
          grace
          sexplib
          logs
          ppx_jane
          ppx_expect
          ppx_yojson_conv
          cmdliner
          hex
          digestif
          zarith
          octez-libs
        ]
        ++ lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.Security
        ];
    }
