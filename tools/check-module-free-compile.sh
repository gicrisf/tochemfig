#!/usr/bin/env bash
# Verify that tochemfig byte-compiles correctly even when emacs-indigo's
# dynamic module cannot be loaded at build time.
#
# emacs-indigo defines its `indigo-with-*' resource macros only *after* its
# module has loaded (they live in indigo-mol.el / indigo-iter.el, required from
# inside `indigo-load-module').  So when the module cannot be loaded during
# byte-compilation, the macros are absent and the compiler bakes in unexpanded
# calls, which then fail at runtime with "void-function mol".
#
# This check substitutes a stub `indigo' (the module is never loaded), keeps the
# real emacs-indigo Elisp on the load path, byte-compiles tochemfig in a
# temporary directory, and asserts the macros were expanded.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
INDIGO_EL="$ROOT/emacs-indigo"
EMACS="${EMACS:-emacs}"

if [ ! -f "$INDIGO_EL/indigo-mol.el" ]; then
  echo "check-module-free-compile: $INDIGO_EL/indigo-mol.el not found" >&2
  exit 1
fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/shim" "$tmp/src"

# Stub the `indigo' feature: provides it without loading the dynamic module.
printf "(provide 'indigo)\n" > "$tmp/shim/indigo.el"

cp "$ROOT"/tochemfig-atom.el "$ROOT"/tochemfig-bond.el \
   "$ROOT"/tochemfig-mappings.el "$ROOT"/tochemfig-molecule.el \
   "$ROOT"/tochemfig.el "$tmp/src/"

cd "$tmp/src"
"$EMACS" -Q --batch \
  --eval "(setq load-path (append (list \"$tmp/shim\" \"$INDIGO_EL\" \"$tmp/src\") load-path))" \
  -f batch-byte-compile \
  tochemfig-atom.el tochemfig-bond.el tochemfig-mappings.el \
  tochemfig-molecule.el tochemfig.el 2>"$tmp/stderr" || true

status=0

check_present() {
  local file="$1" symbol="$2"
  if grep -aq "$symbol" "$file"; then
    echo "ok:   $file: contains expansion of $symbol"
  else
    echo "FAIL: $file: missing expansion of $symbol (macro not expanded?)" >&2
    status=1
  fi
}

if [ ! -f tochemfig.elc ] || [ ! -f tochemfig-molecule.elc ]; then
  echo "FAIL: byte-compilation did not produce .elc files" >&2
  status=1
else
  # `indigo-with-molecule' expands to a call to this function.
  check_present tochemfig.elc "indigo-load-molecule-from-string"
  # `indigo-with-atoms-iterator' expands to a call to this function.
  check_present tochemfig-molecule.elc "indigo-iterate-atoms"
fi

if [ "$status" -eq 0 ]; then
  echo "check-module-free-compile: PASS"
else
  echo >&2
  echo "--- compiler output ---" >&2
  cat "$tmp/stderr" >&2
  exit 1
fi
