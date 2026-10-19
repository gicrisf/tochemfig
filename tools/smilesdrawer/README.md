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
while drawing.  Every manifest entry must have `name`, `smiles`, non-empty
`features`, and a boolean `raster` flag.  Optional `reference_options`,
`reference_theme`, `chem_options`, and `known_gap` fields are copied into the
generated fixture metadata.  The generator records the oracle commit from the
local checkout.

## Generate reaction fixtures

```sh
cd tools/smilesdrawer
node render-reactions.mjs
```

This reads `test/fixtures/smilesdrawer-reactions/manifest.json` and writes
`test/fixtures/smilesdrawer-reactions/*.json`.  It instruments
`SvgDrawer.draw` while `ReactionDrawer` runs, so each component keeps its
SmilesDrawer graph as well as its final nested-SVG placement.  The fixture also
records the ordered reaction composition: components, plus signs, the arrow,
and any text above or below the arrow.

Reaction fixtures intentionally omit per-component SVG primitives and labels;
those are covered by the molecule parity corpus.  The reaction parity test
compares composition only: component placement, plus/arrow/text boxes, text
content, and viewBox coverage.

The advisory raster report also covers all reaction fixtures.  Three curated
fixtures are hard raster gates: esterification, Diels-Alder, and stereo.
Agent/condition fixtures remain advisory until text rendering is stable enough
for raster thresholds.

## Coverage report

```sh
node tools/smilesdrawer/coverage.mjs
```

This reads only the manifest and reports feature counts, raster-gated
fixtures, missing feature tags, duplicate names, and declared known gaps.

## Raster and layout report

From the repository root, with Inkscape and Pillow available:

```sh
nix develop --command emacs -Q --batch -L . -L emacs-indigo -L test \
  -l tools/svg-raster-report.el
```

This rasterizes each reference and native shared-coordinate SVG with Inkscape,
prints advisory pixel/edge metrics and bond-length variation, and writes
side-by-side images under `/tmp/chem-svg-raster/`.  It is not part of
`eldev test` and has no hard visual gates yet.

## Compare

From the repository root:

```sh
emacs -Q --batch -L . -L emacs-indigo \
  -l test/chem-svg-parity-test.el \
  --eval '(chem-svg-parity-test--report t)'
```

The report covers all fixtures.  Unknown or intentionally unsupported
SmilesDrawer behaviors are documented as they are found.
