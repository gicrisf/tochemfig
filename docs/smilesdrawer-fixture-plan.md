# SmilesDrawer fixture corpus extension plan

## Purpose

This document is a handoff plan for extending the SmilesDrawer reference
fixture corpus used by the SVG renderer parity tests.

The goal is **renderer coverage**, not layout-engine parity and not random
molecule count. A fixture belongs in the corpus only when it exercises a
renderer behavior that is not already covered by an existing fixture.

## Current state

- Implementation status: Batches 0 through 7 are complete.
- Branch: `svg`
- Fixture count: **44**
- Fast suite: **509 tests**
- Hard raster gates: **17 curated fixtures**
- Known fixture gaps: none currently declared
- Deferred follow-up: terminal-carbon `showCarbons` parity for
  methylammonium, and exact pseudo-key handling for bracketed neutral
  terminals (the current IR has no bracket flag). See "Batch 7" below.
- Fixture generator: `tools/smilesdrawer/render.mjs`
- Manifest: `test/fixtures/smilesdrawer/manifest.json`
- Generated fixtures: `test/fixtures/smilesdrawer/<name>.json`
- Fast parity tests: `test/chem-svg-parity-test.el`
- Raster gates: `test/chem-svg-raster-test.el`
- Raster helper: `tools/compare-png.py`
- Advisory report: `tools/svg-raster-report.el`
- Reference version: SmilesDrawer `2.4.1`

The current fixtures are:

```text
benzene
naphthalene
indole
pyridine
pyrrole
caffeine_aromatic
caffeine_kekule
ethanol
alanine
butene_e
butene_z
cyclohexane
bicyclic
nitrobenzene
phenol
aniline
allene
furan
thiophene
imidazole
alanine_down
spiro_decane
adamantane
macrocycle_10
quinoline
purine
cyclohexanone
tetramethylammonium
phosphate
azide
pyridine_n_oxide
two_stereocenters
substituted_allene
terminal_alkyne
naphthalene_kekule
ethyl_cyclohexane
formaldehyde
caffeine_dark
benzene_wide
ethanol_large_font
benzene_zero_padding
sulfonate
acetate
boronic_acid
```

The current fast tests cover:

- edge geometry and absolute line placement
- gradients and solid bond colors
- label text, placement, and per-part colors
- label masks
- carbon dots
- aromatic ring circles
- text-inclusive square viewBox
- solid wedges and hashed wedges
- pseudo-element groups such as `COOH`, `NO2`, and terminal-carbon groups
  (`SO₂O⁻CH₃`, `COO⁻CH₃`)
- pseudo-label directions, including `down` for fully concatenated groups
- allene labels
- deterministic output

The hard raster gates use Inkscape and Pillow when available. They skip in
environments without those tools. Run the gated suite with:

```sh
nix develop --command eldev test
```

The plain suite skips only the raster gate:

```sh
eldev test
```

## Fixture pipeline

### Generator

The generator is development-only and requires:

- Node.js
- the local ignored `smilesDrawer/` checkout at the repository root
- `npm ci` in `tools/smilesdrawer/`

Run:

```sh
cd tools/smilesdrawer
node render.mjs
```

The generator reads `manifest.json` and writes one JSON file per fixture.

Each fixture currently contains:

- `reference`: oracle metadata and options
- `name`
- `smiles`
- `atoms`
- `edges`
- `rings`
- `primitives`: normalized lines, polygons, circles
- `labels`
- `masks`
- `points`
- `gradients`
- `viewBox`, `width`, `height`, `minX`, `minY`
- `svg`: the complete reference SVG

### Fast parity tests

`test/chem-svg-parity-test.el` loads fixture JSON, builds a test-only
`chem-drawing` from SmilesDrawer’s coordinates, renders it with
`chem-exp-svg`, and compares graph-keyed primitives.

The fixtures do not require Node at test time. Only regeneration requires Node.

### Raster gates

`test/chem-svg-raster-test.el` uses a curated name list and hard thresholds for:

- MAE
- RMS
- edge-map error

Reference and native SVGs use the same coordinates, so these gates protect the
renderer, not Indigo layout.

Raster gates currently skip unless both Inkscape and Pillow are available.

## Batch 0 — harness hardening

Complete this before adding molecules. It keeps the corpus maintainable.

### 0.1 Add fixture metadata

Extend manifest entries with optional metadata:

```json
{
  "name": "quinoline",
  "smiles": "c1ccc2ncccc2c1",
  "features": ["fused-heteroaromatic", "ring-circle"],
  "raster": false,
  "reference_theme": "light",
  "chem_options": {}
}
```

Recommended fields:

- `features`: controlled vocabulary of renderer behaviors
- `raster`: include this fixture in hard raster gates
- `reference_theme`: SmilesDrawer theme
- `reference_options`: options passed to SmilesDrawer
- `chem_options`: native exporter overrides for the test builder
- `known_gap`: optional string when a fixture is intentionally not gated

Do not add fixtures without feature tags. The tags are the coverage matrix.

### 0.2 Separate reference-only fields from native options

The current `entry.options` field is passed to SmilesDrawer. It is **not** the
native `chem-exp-svg` option plist.

Add a separate `chem_options` field for native overrides so a fixture can
compare:

- SmilesDrawer reference SVG
- native SVG with the same semantic settings

Only add option variants where the reference and native settings are genuinely
comparable. If there is no SmilesDrawer equivalent, treat the variant as a
native regression test rather than a parity fixture.

### 0.3 Record the Oracle revision

Add the SmilesDrawer checkout commit to each fixture’s `reference` metadata.
The current metadata records version and source but not commit.

Refuse to regenerate if the oracle revision changes without an explicit
baseline update.

### 0.4 Consider SVG storage policy

All fixtures currently embed the complete reference SVG. This simplifies
raster gates but increases fixture size as the corpus grows.

Recommended policy:

- keep `svg` for fixtures marked `raster: true`
- optionally omit `svg` for fast-only fixtures
- keep all structured reference data for fast tests

Decision: all fixtures keep the reference SVG for now. At 44 fixtures the
corpus is small enough to keep the generator and raster report simple. Revisit
if fixture size becomes a problem.

### 0.5 Add a coverage report

Add a small report command that prints:

- fixture count
- feature frequency
- fixtures not on any raster gate
- duplicate feature signatures
- fixtures with `known_gap`

This makes the handoff less dependent on tribal knowledge.

### Batch 0 acceptance

- Existing 21 fixtures still generate.
- Existing fast parity tests pass.
- Raster gates still pass in `nix develop`.
- Metadata is present and validated.
- No fixture JSON is edited by hand.

## Batch 1 — ring systems and aromatic geometry

Goal: exercise ring construction, fused and bridged geometry, shared edges, and
aromatic eligibility beyond simple benzene/naphthalene.

Added 6 fixtures:

| Name | SMILES | Features |
|---|---|---|
| `spiro_decane` | `C1CCC2(CC1)CCCC2` | spiro, ring center, non-fused rings |
| `adamantane` | `C1C2CC3CC1CC(C2)C3` | bridged cage, carbon dots, non-planar projection |
| `macrocycle_10` | `C1CCCCCCCCC1` | large ring, circle eligibility, viewBox |
| `quinoline` | `c1ccc2ncccc2c1` | fused heteroaromatic, shared edge |
| `purine` | `c1ncc2[nH]cnc2n1` | fused heteroaromatic, multiple rings |
| `cyclohexanone` | `O=C1CCCCC1` | exocyclic double bond, ring side choice |

Validation rules:

- Confirm SmilesDrawer parses each SMILES before adding it.
- Confirm native Indigo parses it.
- Reject fixtures whose reference SVG has no drawable primitives.
- Do not add bridged aromatic fallbacks until a concrete molecule is validated;
  these are high-value but easy to mis-specify.

Expected renderer risk:

- aromatic circle eligibility
- shared-edge ring selection
- double-bond side selection on fused/exocyclic edges
- ring center and viewBox bounds
- carbon dots in cage projections

## Batch 2 — heteroatoms, charges, radicals, and groups

Goal: exercise theme colors, gradients, charges, radicals, pseudo groups, and
non-carbon centers.

Implemented 4 fixtures; the remaining candidates below were deferred:

| Candidate name | SMILES | Features |
|---|---|---|
| `tetramethylammonium` | `C[N+](C)(C)C` | positive charge, N label, gradients |
| `sulfonate` | `CS(=O)(=O)[O-]` | negative charge, sulfur, multiple O |
| `phosphate` | `OP(=O)(O)O` | phosphorus, multiple O, hydrogens |
| `azide` | `N=[N+]=[N-]` | charge separation, cumulated double bonds |
| `pyridine_n_oxide` | `[O-][n+]1ccccc1` | aromatic charge, N-oxide, gradients |
| `boronic_acid` | `OB(O)O` | boron, three-coordinate center |
| `methyl_radical` | `C[CH2]` | radical dot text, terminal label |

Validation rules:

- Confirm the renderer supports the feature before adding it.
- Isotopes and metals are currently out of scope unless support is added.
- Multi-fragment salts should be deferred unless the renderer has explicit
  behavior for fragment links and hidden atoms.
- Check that pseudo-element grouping does not unexpectedly hide the center.

Expected renderer risk:

- per-part tspan colors
- charge placement and hydrogen ordering
- radial vs linear gradients
- terminal label direction
- pseudo-group detection

### Batch 2 deferred cases

These candidates exposed SmilesDrawer behavior that the native renderer did not
implement when Batch 2 was written:

- `sulfonate`: **implemented in Batch 7.** The methyl group is now folded into
  the sulfur pseudo label (`SO₂O⁻CH₃`), hiding the S-C bond and carbon.
- `boronic_acid`: **implemented in Batch 7.** The pseudo label is now laid out
  downward (`BO₃H₃`).
- `acetate`: **implemented in Batch 7.** The methyl group is now folded into
  the carboxylate pseudo label (`COO⁻CH₃`).
- `methylammonium`: still deferred. SmilesDrawer labels the terminal methyl as
  `CH₃`; the native renderer needs `showCarbons` support in the parity fixture
  builder.
- `methyl_radical`: still deferred. SmilesDrawer does not expose a radical
  field for the fixture builder.

See "Batch 7 — pseudo-group parity" for the implementation and remaining
caveats.

## Batch 3 — stereochemistry and bond edge cases

Goal: exercise wedge/hash geometry, stereocenter configuration, terminal
labels, cumulated systems, and explicit Kekulé input.

Added 6 fixtures:

| Candidate name | SMILES | Features |
|---|---|---|
| `two_stereocenters` | `C[C@H](O)[C@@H](C)O` | two stereocenters, mixed wedges |
| `substituted_allene` | `CC=C=CC` | allene center, cumulated bonds, label |
| `terminal_alkyne` | `CC#C` | triple bond, terminal label |
| `naphthalene_kekule` | `C1=CC=C2C=CC=CC2=C1` | explicit Kekulé fused ring |
| `ethyl_cyclohexane` | `CCC1CCCCC1` | ring substituent, label direction |
| `formaldehyde` | `C=O` | terminal double bond, minimal label |

Validation rules:

- The allene fixture must confirm whether SmilesDrawer draws a label or a point.
- Explicit Kekulé fixtures may not aromatize in SmilesDrawer; that difference is
  intentional if it reflects the oracle.
- Do not add E/Z labels until the renderer supports them.

Expected renderer risk:

- wedge/hash side and width
- label direction for one-bond atoms
- ring vs acyclic double-bond handling
- allene label/dot selection
- terminal triple bond and label masks

## Batch 4 — option variants

Goal: protect option-specific renderer paths that use the same molecule.

Added 4 variants after Batch 0 metadata:

| Name | Reference setting | Native setting |
|---|---|---|
| `caffeine_dark` | SmilesDrawer `dark` theme | `svg-theme dark` |
| `benzene_wide` | SmilesDrawer width/height | `svg-width`, `svg-height` |
| `ethanol_large_font` | SmilesDrawer `fontSizeLarge` | `svg-font-size` |
| `benzene_zero_padding` | SmilesDrawer `padding: 0` | `svg-padding: 0` |

Deferred variants:

- `showCarbons`: requires applying common drawing options to the test IR.
- stereo off: no directly comparable SmilesDrawer fixture option.
- hydrogen deletion: requires applying common drawing options to the test IR.

Rules:

- Only add a variant when both sides can express the same semantic option.
- If SmilesDrawer has no equivalent, put the variant in a native regression
  test instead of the parity fixture list.
- Do not assume `entry.options` is a native option plist; keep
  `reference_options` and `chem_options` separate.
- Avoid option combinations that only test the same code path. Use one variant
  per distinct path.

Expected renderer risk:

- theme element colors and background
- label visibility and hydrogen handling
- fixed output dimensions vs square viewBox
- stereo on/off paths
- mask and viewBox interaction

## Batch 5 — raster gate expansion

Do this only after the new fast fixtures are green.

Current raster gates: 17.

Added in this pass:

- `spiro_decane`
- `quinoline`
- `tetramethylammonium`
- `two_stereocenters`
- `terminal_alkyne`

The dark-theme fixture is not raster-gated because its background rendering is
not comparable at the current settings. `benzene_wide`, `ethanol_large_font`,
and `benzene_zero_padding` exercise option paths but are validated by the fast
parity suite instead.

Process:

1. Generate the fixture.
2. Run `tools/svg-raster-report.el`.
3. Record the observed MAE, RMS, and edge error.
4. Add a threshold with a small, documented margin.
5. Run `nix develop --command eldev test`.
6. Keep the fixture advisory if its metrics are unstable across runs.

Do not add every fixture to hard raster gates. Raster thresholds are sensitive
to Inkscape version, font availability, anti-aliasing, and Pillow version.

## Batch 6 — coverage audit

Status: complete.

The audit used `tools/smilesdrawer/coverage.mjs` and a review of
`chem-exp-svg.el`.

Results:

- 44 fixtures, all with feature tags
- 17 raster-gated fixtures
- no duplicate names or missing tags
- 20 singleton features, all deliberate specialized cases: amine, boron
  heteroatom, cage, exocyclic double bond, hydroxyl, large ring, multiple
  stereocenters, nitro, the four option variants, oxygen/sulfur/phosphorus
  heteroatoms, positive charge, down-directed pseudo label, spiro, and triple
  bond
- no additional high-value molecule fixtures were needed

Branches not represented by fixtures are handled elsewhere:

- alternating aromatic display and stereo-off are covered by native SVG unit
  tests
- radicals are covered by a native unit test; SmilesDrawer does not expose a
  radical field for the fixture builder
- isotope, metal, E/Z label, and multi-fragment cases remain out of scope;
  reaction schemes are planned separately
- sulfonate, acetate, and boronic acid pseudo-group behavior is covered by
  Batch 7

Suggested audit targets for future changes:

- every branch in `chem-exp-svg--edge-svg`
- every `chem-exp-svg--ring-aromatic-display` outcome
- every label direction
- every atom label color and hydrogen/charge/radical path
- every gradient type
- every mask and point path
- every viewBox branch

## Batch 7 — pseudo-group parity

Status: **implemented**. Three fixtures were added: `sulfonate`, `acetate`, and
`boronic_acid`.

### Why these cases were initially deferred

The native renderer only attaches terminal heteroatoms to a pseudo label.
SmilesDrawer first decides that a center is eligible, then attaches **every
degree-one neighbor**, including terminal carbon and hydrogen. This accounts for
the three deferred molecules:

| Molecule | Behavior before Batch 7 | SmilesDrawer behavior |
|---|---|---|
| `CS(=O)(=O)[O-]` | Left a visible S-C bond; label omitted the methyl group | Hides the methyl carbon; label `SO₂O⁻CH₃` |
| `CC(=O)[O-]` | Left a visible C-C bond; label omitted the methyl group | Hides the methyl carbon; label `COO⁻CH₃` |
| `OB(O)O` | Label text was correct but laid out horizontally | Label `BO₃H₃`, laid out downward |

The former `C[NH3+]` concern is **not** a renderer gap. Indigo places the
methyl to the left of the nitrogen, so native `H₃C` in a left-directed label is
consistent with the native layout. SmilesDrawer places the methyl on the right.
A shared-coordinate fixture would use the reference coordinates and therefore
render `CH₃` correctly once the fixture builder applies `showCarbons`; that
remains deferred.

### Resolved gaps

1. **Terminal-neighbor grouping** — `chem-exp-svg--pseudo-elements` now keeps
   eligibility based on terminal heteroatom count, but attaches and hides all
   degree-one neighbors, including carbon and hydrogen.
2. **Group key ordering** — `chem-exp-svg--attach-pseudo` now omits the numeric
   zero charge from the key, so `0O` sorts before `0O-1` and `O₂` precedes
   `O⁻`.
3. **Pseudo-label direction** — `chem-exp-svg--text-direction-for-state` now
   uses SmilesDrawer's rules:
   - `right` when every non-center atom is covered by a distinct pseudo key
     (`atom-count = raw-pseudo-key-count + 1`)
   - `down` when a pseudo center has no visible neighbors
   - left/right when a visible previous atom remains
4. **Terminal carbon direction** — no renderer change was needed; see the
   layout note above.

### Implementation

1. Attach and hide all degree-one neighbors in
   `chem-exp-svg--pseudo-elements`.
2. Omit the numeric zero charge from the pseudo group key.
3. Apply the no-visible-neighbor and all-pseudo direction rules before the
   existing horizontal direction logic.
4. Add `sulfonate`, `acetate`, and `boronic_acid` fixtures.
5. Add native unit tests for label text, hidden bonds, and direction.
6. Run the advisory raster report.  The three new fixtures are not hard-gated:
   the reference and native SVGs for label-only pseudo groups rasterize
   inconsistently under Inkscape, so the advisory metrics are not a reliable
   gate for them.

### Acceptance examples

- `CS(=O)(=O)[O-]`: no visible S-C line; `SO₂O⁻CH₃`; direction `down`
- `CC(=O)[O-]`: no visible C-C line; `COO⁻CH₃`; direction `right`
- `OB(O)O`: `BO₃H₃`; direction `down`; viewBox matches
- `O=[N+]([O-])c1ccccc1`: remains `N⁺O₂` with direction `right`
- alanine `COOH`: remains `HOOC` with direction `left`

`methyl_radical` cannot become a SmilesDrawer fixture because SmilesDrawer does
not expose radical data; keep it covered by the native SVG unit test.

The empty neutral-charge key matches SmilesDrawer for **unbracketed** neutral
atoms, which covers the three fixtures.  SmilesDrawer uses `0` instead of an
empty charge for *bracketed* neutral atoms.  Indigo does not expose bracket
status and the IR has no bracket field, so exact parity for bracketed neutral
terminal groups would require a source-SMILES metadata field.  That is
deliberately out of scope here.

## What not to add

Do not add fixtures for unsupported features merely to make the count larger:

- isotopes
- metals and coordination compounds
- E/Z text labels
- reaction arrows or reaction schemes
- layout-only parity cases
- huge polymers or structures that make raster tests slow

Record unsupported cases as explicit known gaps or future work instead.

## Test and verification commands

Fast parity tests:

```sh
eldev test -f test/chem-svg-parity-test.el
```

Full suite, raster gates skipped when tools are absent:

```sh
eldev test
```

Full suite with hard raster gates:

```sh
nix develop --command eldev test
```

Advisory report:

```sh
emacs -Q --batch -L . -L emacs-indigo \
  -l test/chem-svg-parity-test.el \
  --eval '(chem-svg-parity-test--report t)'
```

Raster/layout report:

```sh
nix develop --command emacs -Q --batch -L . -L emacs-indigo -L test \
  -l tools/svg-raster-report.el
```

Module-free compile:

```sh
tools/check-module-free-compile.sh
```

Byte compilation:

```sh
eldev compile
```

## Per-batch workflow

For each batch:

1. Update `manifest.json` with names, SMILES, features, and metadata.
2. Run `node tools/smilesdrawer/render.mjs` from `tools/smilesdrawer/`.
3. Inspect generated fixtures for empty primitives or unexpected hidden atoms.
4. Run fast parity tests.
5. Fix renderer bugs or mark an explicit known gap.
6. Run raster gates only for fixtures added to that list.
7. Update `docs/smilesdrawer-parity.md`, `CHANGELOG.org`, and test counts.
8. Run `eldev test`, `eldev compile`, and the module-free compile.
9. Commit the batch separately.

Suggested commit messages:

```text
extend smilesdrawer fixtures with ring systems
extend smilesdrawer fixtures with charged heteroatoms
extend smilesdrawer fixtures with stereo edge cases
add smilesdrawer option-variant fixtures
expand smilesdrawer raster gates
```

## Acceptance criteria

A batch is complete when:

- every new fixture generates successfully
- every new fixture has feature metadata
- fast parity tests pass
- no unintended known gaps were added
- raster gates pass where the fixture is gated
- `eldev test` passes
- `nix develop --command eldev test` passes
- `eldev compile` and `tools/check-module-free-compile.sh` pass
- documentation and changelog are updated
- the batch is committed independently

## Risks and maintenance

- **Oracle drift:** a new SmilesDrawer version can change IDs, layout, labels,
  gradients, or primitive ordering. Pin the checkout and record its commit.
- **Fixture bloat:** full reference SVGs add size. Keep `svg` only where raster
  tests need it if the corpus grows substantially.
- **Raster fragility:** Inkscape/font/Pillow changes can shift metrics. Keep
  raster gates curated and document the environment.
- **False confidence:** shared-coordinate parity proves the renderer, not the
  Indigo layout. Do not describe it as end-to-end visual parity.
- **Manual JSON edits:** never edit generated fixtures by hand. Change the
  manifest or generator and regenerate.

## Out of scope

- Porting SmilesDrawer’s layout engine
- Exact coordinate parity for the Indigo pipeline
- Rasterizing the full 1000-molecule corpus
- Reaction depictions, isotopes, and metals until explicitly supported
