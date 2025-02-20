#!/usr/bin/env bash
set -e

# Usage: ./toggle_lltz_separate_build.sh set|unset
MODE=${1:-set}

# Define the files (adjust the paths if necessary)
IGNORED_FILE="./vendors/dune"
LIBRARY_FILE="./lib/michelson/optimisations/dune"

if [ "$MODE" = "unset" ]; then
  echo "Disabling changes..."

  # Write the ignored_subdirs stanza to IGNORED_FILE
  cat > "$IGNORED_FILE" <<'EOF'
(ignored_subdirs
  (tezos-ligo grace tezos_utils)
)
EOF

  # Ensure that the library file has "ligo.tezos-utils"
  # (in case it was previously modified)
  sed -i 's/lltz\.tezos-utils/ligo.tezos-utils/g' "$LIBRARY_FILE"

  echo "Changes disabled. LLTZ can now be built as a submodule."
  echo "Run './toggle_lltz_separate_build.sh set' to enable them again."
else
  echo "Enabling changes..."

  # Empty the IGNORED_FILE
  > "$IGNORED_FILE"

  # Replace "ligo.tezos-utils" with "lltz.tezos-utils" in the library file
  sed -i 's/ligo\.tezos-utils/lltz.tezos-utils/g' "$LIBRARY_FILE"

  echo "Changes enabled. LLTZ can now be built separately."
  echo "Run './toggle_lltz_separate_build.sh unset' to disable them again."
fi