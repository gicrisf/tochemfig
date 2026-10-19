# SmilesDrawer SVG parity plan

## Goal

Move `chem-mode`'s SVG exporter toward verifiable parity with
[SmilesDrawer](https://github.com/reymond-group/smilesDrawer) without adding a
Node, browser, or SmilesDrawer dependency at render time.

Parity is split into four levels:

1. **Semantic parity** — the same molecule, bonds, charges, radicals,
   stereochemistry, and ring systems.
2. **Renderer parity** — given the same coordinates, the same SVG primitives
   and style decisions.
3. **Layout parity** — Indigo and SmilesDrawer produce drawings with the same
   structural and geometric properties.
4. **Raster parity** — rendered images look alike.

Semantic and renderer parity are expected to be exact or near-exact. Layout
parity is metric-based. Raster parity is advisory and used for review and
regression detection, not as the primary oracle.

## Status

Renderer parity is complete for the current corpus:

- The development-only reference generator and normalizer live in
  `tools/smilesdrawer/`.
- Forty-four fixtures cover aromatic rings, fused rings, heterocycles,
  acyclic chains, stereochemistry, bridged and spiro rings, large rings,
  charged heteroatoms, pseudo groups with terminal carbons, cumulated double
  bonds, terminal alkynes, small molecules, allenes, and exporter option
  variants.
- `test/chem-svg-parity-test.el` builds test-only IR from SmilesDrawer
  coordinates and runs fast shared-coordinate checks for edge geometry,
  absolute line placement, aromatic circle radii, and determinism.
- The aromatic circle radius now matches SmilesDrawer's
  `apothem - bondSpacing` formula.
- Acyclic double-bond side selection, shortening, and absolute line placement
  now match the butene fixtures.
- Heteroatom label text, left-direction ordering, and atom-centered placement
  now match the shared-coordinate fixtures.
- Attached pseudo-element groups (COOH, NO2, and related terminal heteroatom
  groups) are detected, hidden atoms and edges are omitted, and the group text
  is rendered on the center atom.  All degree-one neighbors, including terminal
  carbons, are folded into the group.  Pseudo labels use SmilesDrawer's
  right/down/left direction rules, including the all-pseudo concatenation case.
- Solid-wedge polygons and dashed-wedge strokes now match SmilesDrawer's
  geometry, including the wide half-width and hashed-wedge step/cap.
- Label mask centers and radii now match SmilesDrawer for text labels.
- Square viewBox, 500x500 default output, text measurement, and text-inclusive
  bounds now match SmilesDrawer.
- Bond colors now match SmilesDrawer's solid/radial/linear gradient rules, and
  labels use per-part tspan colors with the same theme palette.
- Linear carbon atoms now render as SmilesDrawer-style carbon dots, including
  mask circles; sp carbons next to triple bonds do not receive dots.
- An advisory raster and layout report (`tools/svg-raster-report.el`) renders
  reference and native SVGs with Inkscape, compares PNGs, and reports
  bond-length variation.
- Hard raster gates cover seventeen curated shared-coordinate fixtures in
  `test/chem-svg-raster-test.el`.  They skip without Inkscape/Pillow and run in
  the Nix development shell.  The reference SVG is rasterized with the native
  viewBox so drawings with intentionally different text bounds stay aligned.
  The three Batch 7 pseudo-group fixtures are fast-suite only because
  label-only pseudo groups rasterize inconsistently under Inkscape.
- Allene centers now render the carbon label used by SmilesDrawer.
- Renderer parity is complete for the fixture corpus.  Remaining parity work is
  optional layout metrics and exact layout-engine porting.
- Fixture-corpus expansion is planned in
  `docs/smilesdrawer-fixture-plan.md`.

## Reaction composition parity

Reaction support uses a separate corpus so the molecule fixtures stay focused
on renderer parity:

- `tools/smilesdrawer/render-reactions.mjs` instruments `SvgDrawer.draw` while
  `ReactionDrawer` lays out a reaction.  Each fixture records the component
  graphs, their final nested-SVG placement, and the ordered composition
  (reactants, plus signs, arrow, text, products).
- `test/fixtures/smilesdrawer-reactions/` currently covers esterification,
  Diels-Alder, two products, stereo/aromatic input, an agent with conditions,
  and a charged salt.
- `test/chem-svg-reaction-parity-test.el` builds test-only reaction IR from the
  reference graphs and composition boxes, renders it with the native SVG
  exporter, and checks component placements, plus and arrow geometry, text
  positions and content, and viewBox coverage.

Per-component SVG primitives, labels, masks, and gradients are not compared in
the reaction corpus; the molecule parity suite already covers those.  The
reaction viewBox check is containment-based because native text bounds can be
larger than SmilesDrawer's clipped estimates.

The advisory raster report covers all six reaction fixtures.  Three curated
fixtures are hard raster gates: esterification, Diels-Alder, and stereo.  The
agent/condition fixtures stay fast-suite only until text rendering proves
stable enough for raster thresholds.

## Text bounds and viewBox divergence

SmilesDrawer estimates a horizontal label's extent as the first text part's
width multiplied by the number of parts.  That estimate is too small because it
uses the first part's *element* width for every part, while each part's
*display* text can be wider due to charges and subscripts.  For example,
`[NH4+]` is rendered as `N⁺H₄`: the first element (`N`) is about 0.68 em wide,
but the display parts `N⁺` and `H₄` are each about 1.13 em in the deterministic
estimator.  SmilesDrawer clips the trailing `₄`.

Native bounds now measure every part with `chem-exp-svg--text-size` and expand
from the text layout anchor:

- `right`: the right edge uses the sum of all part widths;
- `left`: the left edge uses the sum of all part widths;
- `down`/`up`: the horizontal edge uses the widest part.

Native bounds never shrink relative to the old SmilesDrawer-compatible bounds.
For the affected fixtures the native viewBox is therefore a superset of the
reference viewBox:

- `alanine`
- `alanine_down`
- `aniline`
- `acetate`
- `formaldehyde`
- `nitrobenzene`

`chem-svg-parity-viewbox` checks exact equality for all other fixtures and
containment for this list.  Raster gates rasterize the reference SVG with the
native viewBox so intentionally different bounds do not change the comparison
scale.  The advisory raster report uses the same normalization.

The correction is padding-neutral: `:svg-padding` is unchanged, and the old
bounds remain a floor.  Only the six fixtures above change; the other 38 are
unchanged.  The viewBox side changes by 1.18–2.38% for four of them, 5.99% for
`nitrobenzene`, and 13.00% for the small `acetate` box.  The increase is the
previously clipped text, not extra margin.

## Reference version

- SmilesDrawer `2.4.1` from the local `smilesDrawer/` checkout.
- The checkout is development-only and remains excluded from the repository.
- Reference metadata records the version, checkout commit, theme, and options.
- SmilesDrawer is a test oracle only. Production rendering stays pure Elisp.

## Key observation: randomness

Source inspection of SmilesDrawer 2.4.1 found only one random call:
`SvgWrapper.makeid()`, which generates random SVG IDs. The layout and drawing
algorithms are deterministic for a given input, option set, and environment.

Consequences:

- Random IDs are normalized or generated deterministically in the reference
  harness.
- Repeated renders are used to detect hidden nondeterminism.
- Text measurement is the main environment-dependent risk; the reference
  harness must freeze or normalize text metrics.
- The real parity challenge is layout and rendering differences, not random
  layout.

## Core strategy: separate layout from rendering

### Renderer parity with shared coordinates

Generate a SmilesDrawer reference drawing containing:

- atom graph, element, hydrogens, charge, radical, and stereo
- SmilesDrawer vertex coordinates
- ring membership and ring centers
- edge-to-atom mapping
- normalized SVG primitives: lines, polygons, circles, text, masks, viewBox
- options and reference metadata

Build a temporary `chem-drawing` from that reference in the test suite, using
SmilesDrawer's coordinates. Render it with `chem-exp-svg` and compare primitives
keyed by graph ID.

This removes layout differences and isolates the renderer. It is the highest
signal test and the first implementation milestone.

The test-only IR builder must:

- compute `bond-angles` from coordinates and neighbors
- populate atoms, edges, edge lookup, rings, and adjacency as needed
- preserve stereo and ring membership
- not add SVG-specific fields to the production IR

### Layout parity for the native pipeline

Compare the normal Indigo-built drawing against the SmilesDrawer reference
after graph isomorphism. Compare:

- molecular graph and stereo exactly
- ring cyclic order and fused-ring orientation
- double-bond side and substituent side
- wedge/hash side
- ring regularity and aromatic display choice
- bond-length and angle distributions
- drawing bounds and aspect ratio
- minimum label/bond and bond/bond clearances
- Procrustes RMSD after rotation, reflection, scale, and translation, as an
  advisory score

Layout coordinates do not have to match exactly unless the project later
decides to port SmilesDrawer's layout engine.

## Reference harness

### Generator

Add a development-only Node harness under `tools/smilesdrawer/`:

- `render.mjs` — parse SMILES, render with SmilesDrawer, emit reference JSON.
- `normalize.mjs` — normalize IDs, transforms, number formatting, and
  primitive descriptions.
- `README.md` — pinned version, setup, and regeneration instructions.

Suggested reference shape:

```json
{
  "reference": {
    "name": "smiles-drawer",
    "version": "2.4.1",
    "commit": "...",
    "options": {}
  },
  "smiles": "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
  "atoms": [
    {"id": 0, "element": "C", "x": 0.0, "y": 0.0, "hydrogens": 3, "charge": 0}
  ],
  "edges": [
    {"id": 0, "a": 0, "b": 1, "order": "single", "stereo": "none"}
  ],
  "rings": [
    {"id": 0, "atoms": [1, 2, 3, 4, 5], "aromatic": true, "cx": 0.0, "cy": 0.0}
  ],
  "primitives": {
    "lines": [
      {"edge": 0, "role": "main", "x1": 0.0, "y1": 0.0, "x2": 30.0, "y2": 0.0}
    ],
    "polygons": [],
    "circles": [],
    "labels": []
  },
  "svg": {
    "viewBox": "0 0 500 500",
    "width": 500,
    "height": 500
  }
}
```

### Instrumentation

Rather than inferring graph ownership from SVG element order, instrument the
SmilesDrawer drawing calls:

- tag or record primitives emitted while drawing a specific edge
- record label primitives while drawing a specific atom
- record aromatic circles while drawing a specific ring
- record the current theme and style options

This makes comparisons graph-keyed and robust to element ordering.

### Determinism

- Override `SvgWrapper.makeid()` with a deterministic ID generator, or
  normalize all generated IDs before comparison.
- Freeze text measurement for the reference run, or record measured text
  boxes and compare our anchors/content with tolerance.
- Run each reference render at least twice and assert the normalized output is
  stable.
- If hidden nondeterminism is found, run K renders, canonicalize each, and
  compare the canonical set or distribution.

## Canonicalization

Define a canonical drawing signature for every renderer:

- normalize IDs, whitespace, number precision, and element order
- key primitives by graph IDs rather than DOM order
- use graph isomorphism to map atoms and edges when comparing Indigo and
  SmilesDrawer layouts
- choose a canonical anchor edge and similarity transform (origin, rotation,
  scale)
- canonicalize over graph automorphisms when the molecule is symmetric
- for stochastic output, canonicalize over seeds and choose the smallest
  representative

The signature should be invariant to:

- translation, rotation, reflection, and uniform scale
- atom ordering and SVG element ordering
- random IDs and insignificant numeric formatting

## Test layers

### Fast tests

Run as part of the normal `eldev test` suite:

- 20–50 curated reference fixtures
- semantic parity
- renderer primitives with shared coordinates
- viewBox, label, mask, wedge, circle, and bond-line comparisons
- determinism checks
- no Node requirement; fixtures are committed as compact JSON

Suggested fixtures include caffeine, benzene, naphthalene, indole, bridged
rings, alanine, E/Z examples, charged species, radicals, salts, and
multi-fragment molecules.

### Medium tests

Run on pull requests or nightly:

- full 1000-molecule corpus from `studies/external-corpus/`
- native Indigo layout compared against SmilesDrawer reference
- semantic and structural metrics
- option matrix for themes, aromatic display, labels, stereo, and canvas size
- feature coverage report

### Heavy tests

Run nightly or on demand:

- raster parity for the full corpus
- fuzzing with generated and mutated molecules
- metamorphic transformations
- worst-case visual report with side-by-side and overlay images
- coverage thresholds by molecule class

### Option matrix

At minimum:

- light and dark themes
- aromatic `auto`, `circle`, and `alternating`
- stereochemistry on/off
- CIP and enhanced stereo labels
- show carbons and methyls
- atom numbers, markers, charges, radicals
- fixed and automatic canvas sizes
- font size and bond spacing variations

## Metamorphic tests

Apply transformations to both renderers and compare invariants:

- atom-order permutations of equivalent SMILES
- equivalent resonance and Kekulé forms
- ring-closure order variations
- coordinate rotation, reflection, and scaling
- option changes that should affect only presentation
- repeated rendering

The expected result is not always identical coordinates. It is an identical
canonical graph, embedding, and style signature, or a predictable transform.

## Raster comparison

Use rasterization only after vector parity is understood:

- render both SVGs with a deterministic rasterizer and fixed fonts
- align images using atom centers and graph correspondence
- compare edge maps with chamfer distance
- compare color histograms and local SSIM
- generate side-by-side and overlay reports for the worst failures

Raster metrics are initially advisory. Hard gates are added only for fixtures
where the expected renderer and font behavior are stable.

## Known likely gaps

The first renderer parity run is expected to expose:

- aromatic circle radius: SmilesDrawer uses
  `apothemFromSideLength(bondLength, n) - bondSpacing`; the current exporter
  uses the measured ring apothem minus small margins
- viewBox behavior: SmilesDrawer squares the viewBox when `scale <= 0`; the
  current exporter uses exact rectangular bounds
- bond gradients: SmilesDrawer uses gradients when connected atoms have
  different theme colors
- text metrics and baselines: SmilesDrawer uses `11pt` text, `tspan`-based
  decorations, and alphabetic baselines for horizontal labels
- double-bond side selection for terminal, exocyclic, and fused edges
- terminal label direction, especially diagonal one-bond atoms
- aromatic ring eligibility and bridged-ring fallbacks

Each difference must be classified as intentional or fixed. Intentional
differences must be documented and excluded from strict comparisons.

## Repository layout

Proposed additions:

```text
tools/smilesdrawer/
  render.mjs
  normalize.mjs
  README.md

test/svg-parity-test.el
test/fixtures/smilesdrawer/
  manifest.json
  caffeine.json
  naphthalene.json
  ...
```

- Fast fixtures are committed.
- Full-corpus references are generated locally and cached; they are not
  committed.
- The reference generator skips when the local SmilesDrawer checkout or Node
  environment is absent.
- Normal tests must remain runnable without Node.

## Acceptance criteria

- Semantic parity: 100% across the committed corpus.
- Renderer parity: primitive counts, labels, ring circles, wedges, masks, and
  viewBox match exactly or within documented tolerances.
- Native layout parity: all structural invariants pass; style metrics stay
  within thresholds.
- No new SVG fields in the production IR.
- No production Node/browser dependency.
- Fast tests stay fast; heavy tests are isolated and documented.
- Parity report identifies molecule, option profile, element/edge/ring, and
  expected/actual values for every failure.

## Phases

### Phase 1 — reference harness and shared-coordinate fixtures

- Add the Node reference generator and normalizer.
- Add a test-only IR builder from reference JSON.
- Create 20 hard fixtures.
- Compare renderer primitives and fix the highest-impact differences.

### Phase 2 — corpus and layout metrics

- Expand to the 1000-molecule corpus.
- Add graph isomorphism and canonical drawing signatures.
- Add structural and style metrics by molecule class.
- Add option-matrix tests.

### Phase 3 — metamorphic and fuzz tests

- Add atom-order, resonance, rotation, reflection, and repeated-render tests.
- Add generated and mutated molecules.
- Add coverage thresholds and failure triage.

### Phase 4 — raster review

- Add deterministic rasterization.
- Add graph-aligned image comparison and reports.
- Promote stable fixture classes to hard visual gates.

## Open decisions

- Do we want exact SmilesDrawer visual defaults, including square viewBox and
  gradient bonds, or primarily geometry and drawing logic with our current
  styling?
- Are text metrics and font rendering in scope for strict parity?
- Is Node plus jsdom acceptable as a development/test-only dependency?
- If layout coordinates must match exactly, do we port SmilesDrawer's layout
  engine, or keep layout parity metric-based?

## Licensing

SmilesDrawer is MIT-licensed. If parity work ports additional algorithms or
code beyond the current post-layout drawing approach, include attribution and
the MIT license notice in the appropriate repository notice or source headers.
