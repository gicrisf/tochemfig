# SmilesDrawer reference harness

Development-only generator for the SVG parity fixtures.  It is not used by
`chem-mode` at runtime.

## Requirements

- Node.js
- The local `smilesDrawer/` checkout at the repository root
- Dependencies in this directory:

```sh
cd tools/smilesdrawer
npm ci
```

## Generate the fixtures

```sh
cd tools/smilesdrawer
node render.mjs
```

The generator reads `test/fixtures/smilesdrawer/manifest.json` and writes one
JSON file per fixture next to it.  Fixtures contain SmilesDrawer coordinates,
the molecular graph, ring membership, and the normalized SVG primitives emitted
while drawing.

## Compare

From the repository root:

```sh
emacs -Q --batch -L . -L emacs-indigo \
  -l test/chem-svg-parity-test.el \
  --eval '(chem-svg-parity-test--report t)'
```

The report covers all fixtures.  Unknown or intentionally unsupported
SmilesDrawer behaviors are documented as they are found.
