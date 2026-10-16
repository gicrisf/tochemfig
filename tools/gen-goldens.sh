#!/usr/bin/env bash
# Regenerate the test corpus and golden output files.
#
# Requires a Python 3 with epam.indigo (provided by the project flake).  When
# run from outside the development shell, the script re-enters it via nix.
set -euo pipefail

cd "$(dirname "$0")/.."

if python3 -c "import indigo" >/dev/null 2>&1; then
  exec python3 tools/gen-goldens.py "$@"
else
  exec nix develop --command python3 tools/gen-goldens.py "$@"
fi
