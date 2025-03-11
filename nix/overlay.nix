final: prev:
with prev; {
  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope (_: prev:
        with prev; rec {
          octezSource = fetchFromGitLab {
            owner = "ligolang";
            repo = "tezos-ligo";
            rev = "fb4bad17f4d4a8b1df1ba5ea96935f63321e3a30";
            hash = "sha256-FkOR4VqFngdejBCqzwwySINfK/oNklsnIVrvQYVAfyc=";
          };
          buildOctezPackage = {
            pname,
            propagatedBuildInputs ? [],
            nativeBuildInputs ? [],
          }:
            buildDunePackage {
              pname = pname;
              version = "v21-ligo";
              src = octezSource;
              nativeBuildInputs = nativeBuildInputs;
              propagatedBuildInputs =
                [
                  ppxlib
                  logs
                  ppx_repr
                  digestif
                  zarith
                  mtime
                  lwt
                  rusage
                  astring
                  checkseum
                  cmdliner
                  index
                  bigstringaf
                  uri
                  ocamlgraph
                  bheap
                  pure-splitmix
                  bls12-381
                  conduit-lwt-unix
                  qcheck-alcotest
                  asetmap
                  lwt-watcher
                  tezt
                  ppx_expect
                  alcotest-lwt
                  aches
                  hacl-star
                  seqes
                  stdint
                  cohttp
                  camlp-streams
                  secp256k1-internal
                  lwt-canceler
                  lwt-exit
                  magic-mime
                  aches-lwt
                  tar-unix
                  dune-configurator
                  camlzip
                  yaml
                  ppx_import
                  ctypes
                  ctypes-foreign
                  class_group_vdf
                  pprint
                  ocaml-migrate-parsetree-2
                  ocp-ocamlres
                  pyml
                  libiconv
                ]
                ++ propagatedBuildInputs
                ++ lib.optionals stdenv.isDarwin [
                  darwin.apple_sdk.frameworks.Security
                ];
            };
          cohttp = buildDunePackage rec {
            pname = "cohttp";
            version = "5.3.1";
            minimalOCamlVersion = "4.08";
            src = fetchurl {
              url = "https://github.com/mirage/ocaml-cohttp/releases/download/v${version}/cohttp-${version}.tbz";
              hash = "sha256-9eJz08Lyn/R71+Ftsj4fPWzQGkC+ACCJhbxDTIjUV2s=";
            };
            buildInputs = [jsonm ppx_sexp_conv];
            propagatedBuildInputs = [base64 re stringext uri-sexp];
          };

          grace = buildDunePackage rec {
            pname = "grace";
            version = "0.0.2";
            src = fetchFromGitHub {
              owner = "johnyob";
              repo = "grace";
              rev = "d15a6d7d07a2551d1a9934fa79c2cf84c918f990";
              hash = "sha256-jubzimeKs29Y6Di2/kpKEOnNAEzMzVpC5HMLjog4Tlg=";
            };
            propagatedBuildInputs = [core ppx_jane fmt dedent iter core_unix uutf ppx_optcomp];
          };

          tezt = buildDunePackage rec {
            pname = "tezt";
            version = "4.1.0";

            minimalOCamlVersion = "4.12";

            src = fetchFromGitLab {
              owner = "nomadic-labs";
              repo = pname;
              rev = version;
              hash = "sha256-1Cl/GOB+MDPJIl/6600PLTSL+vCYcAZGjedd6hr7rJw=";
            };

            propagatedBuildInputs = [clap ezjsonm lwt re];
          };

          # TODO: odoc-parser and ocamlformat are issues with nix-ocaml
          odoc-parser = prev.odoc-parser.overrideAttrs (prev: {
            propagatedBuildInputs = (prev.propagatedBuildInputs or []) ++ [result];
            postPatch = "";
          });
          ocamlformat_0_21_0 = prev.ocamlformat_0_21_0.overrideAttrs (prev: rec {
            version = "0.21.0";
            tarballName = "ocamlformat-${version}.tbz";
            src = final.fetchurl {
              url = "https://github.com/ocaml-ppx/ocamlformat/releases/download/${version}/${tarballName}";
              sha256 = "sha256-KhgX9rxYH/DM6fCqloe4l7AnJuKrdXSe6Y1XY3BXMy0=";
            };
            propagatedBuildInputs = [csexp];
          });
          octez-src = fetchFromGitLab {
            owner = "ligolang";
            repo = "tezos-ligo";
            rev = "fb4bad17f4d4a8b1df1ba5ea96935f63321e3a30";
            hash = "sha256-FkOR4VqFngdejBCqzwwySINfK/oNklsnIVrvQYVAfyc=";
          };
          octez-rust-deps = buildDunePackage {
            pname = "octez-rust-deps";
            version = "v21-ligo";
            src = octezSource;
            cargoRoot = "src/rust_deps";
            cargoDeps = rustPlatform.importCargoLock {
              lockFile = "${octezSource.outPath}/src/rust_deps/Cargo.lock";
            };
            postPatch = ''
              cd src/rust_deps
              find . -type d -exec chmod u+w {} +
              patchShebangs .
              cd ../..
            '';
            propagatedBuildInputs =
              [libiconv]
              ++ lib.optionals stdenv.isDarwin [
                darwin.apple_sdk.frameworks.Security
              ];
            nativeBuildInputs = [
              rustc
              cargo
              rustPlatform.cargoSetupHook
            ];
          };
          octez-alcotezt = buildOctezPackage {
            pname = "octez-alcotezt";
          };
          octez-internal-libs = buildOctezPackage {
            pname = "octez-internal-libs";
            propagatedBuildInputs = [octez-alcotezt];
          };
          octez-distributed-internal = buildOctezPackage {
            pname = "octez-distributed-internal";
          };
          octez-distributed-lwt-internal = buildOctezPackage {
            pname = "octez-distributed-lwt-internal";
            propagatedBuildInputs = [octez-distributed-internal];
          };
          octez-libs = buildOctezPackage {
            pname = "octez-libs";
            propagatedBuildInputs = [
              octez-rust-deps
              octez-internal-libs
              octez-distributed-lwt-internal
            ];
          };
        });
    });
}
