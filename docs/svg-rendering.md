# SVG rendering plan

## Purpose and audience

This is the implementation plan for the pure-Elisp SVG exporter in
`chem-mode`.  It is written for a contributor who understands Emacs Lisp and
the existing drawing pipeline but has not worked on SVG generation before.

The plan assumes the current `chem-mode` IR and exporter architecture.  It is
based on the actual code on the `svg` branch, not on the older local notes in
`svg-plans/`.  The local `svg-plans/` directory remains useful as reference
material, but several of its IR assumptions predate the current canonical-edge
and ring-record implementation.

## Status and relationship to other plans

- Branch: `svg`, created from `chem-mode` at `dfa1ce5`.
- Current baseline on the base commit: 406 ERT tests pass.
- Phase 1 is implemented on this branch: exporter skeleton, options/themes,
  primitives, and exporter tests.
- Phase 2 is implemented on this branch: basic bonds, cross-bond ordering,
  atom labels, charges, radicals, atom numbers, markers, masks, text
  direction, and theme backgrounds.
- Phase 3 is implemented on this branch: aromatic circles, alternating dashed
  inner lines, ring-side double bonds, and fused/bridged fallbacks.
- Phase 4 is implemented on this branch: filled and hashed wedges, either
  fallback, `:svg-show-stereo`, configuration-derived wedge direction, and
  allene input-wedge preservation.
- Phase 5 is implemented on this branch: optional CIP and enhanced-stereo
  labels with combined formatting and XML escaping.
- Phase 6 is implemented on this branch: SVG image insertion, deterministic
  UTF-8 image files, Org/Markdown links, exporter-aware transient groups, and
  the SVG theme selector.
- Phase 7 is implemented on this branch: the 1000-molecule corpus smoke test
  and documentation updates.
- Org Babel integration is implemented in `chem-babel.el`; see
  `chem-mode-plans/org.md`.  The suite currently has 488 tests.
- `chemfig` remains the publication and parity exporter.
- Stereochemistry data (tetrahedral centers, allene detection, CIP, enhanced
  stereo) is already in the IR.
- The chemfig exporter intentionally does not render CIP or enhanced-stereo
  labels.  SVG is the place where those labels can be rendered.
- Org Babel integration lives in the optional `chem-babel.el`; core files do
  not require Org.  This document covers the SVG renderer and the exporter/UI
  hooks used by both Org blocks and interactive commands.
- Do not change chemfig output as part of this work.
- This revision settles the SVG option shape:
  - per-option defaults are `chem-exp-svg-default-*` variables;
  - option keys are `:svg-*`; for example, `chem-exp-svg-default-theme` maps
    to `:svg-theme`;
  - all scalar options are declared in `chem-exp-svg-option-specs`;
  - all palettes are declared in `chem-exp-svg-themes`;
  - background is a theme property; there is no `:svg-background`.

## Current limitations

- No best-wedge edge selection; the input wedge edge is used.
- No E/Z labels, allene aR/aS labels, or isotope labels.
- Text metrics are approximate, and stereochemistry labels do not avoid
  collisions yet.
- SVG IDs are fixed, so multiple SVGs on one page may share IDs.
- `chem-exp-svg-themes` is a palette defcustom; it is not part of the
  `chem-save-options` / `chem-reload-options` surface.
- Org Babel cache cleanup only considers links in the current buffer; shared
  `chem-babel-cache-directory` values need care.

## Goals

- Add a pure-Elisp `svg` exporter registered with `chem-register-exporter`.
- Render the stable, renderer-neutral IR to deterministic SVG.
- Reuse the existing IR geometry, ring records, stereocenter data, and atom
  label decisions.
- Produce valid, self-contained SVG with no external runtime dependency.
- Support the common molecules and drawing features already covered by the
  chemfig exporter:
  - single, double, triple, and aromatic bonds;
  - atom labels, hydrogens, charges, radicals, atom numbers, markers;
  - wedges and hashes;
  - aromatic circles and alternating bond representations;
  - cross bonds;
  - link bonds excluded from drawing.
- Keep output deterministic: identical drawing plus options must produce
  identical bytes.
- Provide a foundation for later Org image results and stereochemistry labels.
- Keep the existing chemfig golden output byte-identical.

## Non-goals for the first SVG milestone

- No Indigo or external toolkit calls in the renderer.
- No TeX, Node, browser, or external converter at render time.
- No changes to the chemfig exporter or its goldens.
- No new SVG-specific fields in the IR.
- Org Babel is implemented separately in `chem-babel.el`; see the Org plan.
- No best-wedge edge selection.  SVG draws the input wedge edge, but derives
  its up/down direction from the stored stereocenter configuration and current
  geometry.
- No allene aR/aS labels; the public Indigo API does not expose them.
- No E/Z labels in v1.  Bond stereo is available, but E/Z priority derivation
  is a separate feature.
- No isotope labels until `emacs-indigo` exposes an isotope accessor.
- No full typographic text layout.  Text metrics are approximate by design.
- No pixel-perfect visual parity with SmilesDrawer or chemfig.  The goal is a
  correct, readable, deterministic drawing.

## Current architecture

The current pipeline is:

```
SMILES / MOL
  -> chem-from-string / chem-from-file
  -> chem--prepare-mol
  -> chem-indigo-build
       -> parse atoms, bonds, stereochemistry, and rings
       -> apply depiction transforms
       -> chem-apply-drawing-options
       -> chem-drawing
  -> chem-render DRAWING EXPORTER OPTIONS
  -> exporter renderer
```

`chem-apply-drawing-options` is the final step inside `chem-indigo-build`
(`chem-indigo.el:660`), not a separate top-level pipeline stage.

An IR returned by `chem-from-*` is already prepared.  Exporters must not
reapply common options.  In particular, the SVG exporter must use the atom
`label` and `number` fields already stored in the IR rather than re-reading
`show-carbons`, `show-methyls`, or `atom-numbers`.

### Current IR

`chem-edge`:

```elisp
id a b
order          ; :single :double :triple :aromatic :query
stereo         ; :none :up :down :either :cis :trans
stereo-center  ; atom at the wide end of a wedge, or nil
link-p
cross-p
marker
decoration     ; :plain or :fancy
```

`chem-stereocenter`:

```elisp
type           ; :abs :or :and :either
group          ; integer; 0 is valid
pyramid        ; four atom indices, -1 for implicit H
cip            ; :none :unknown :s :r :S :R :E :Z, or nil
wedge-edge     ; canonical edge id
configuration  ; +1/-1
```

`chem-allene`:

```elisp
center
terminals
substituents   ; ((SUB SUB) (SUB SUB)); -1 for implicit H
wedge-edges
configuration  ; reserved; nil for now
```

`chem-atom`:

```elisp
idx x y
element hydrogens charge radical
neighbors bond-angles
label          ; display label, or nil for a hidden carbon
number         ; 1-based atom number when requested
marker
stereocenter   ; chem-stereocenter, or nil
allene         ; chem-allene, or nil
```

`chem-ring`:

```elisp
id
atoms          ; ordered atom indices
bonds          ; ordered canonical edge ids
anchor         ; canonical edge id used by chemfig
aromatic-p
sequence       ; source SSSR index
```

The center, circumradius, apothem, regularity, and symmetry are computed by
helpers in `chem-ir.el`; they are not stored as fields.

`chem-drawing`:

```elisp
atoms          ; hash: idx -> chem-atom
edges          ; hash: id -> chem-edge
edge-lookup    ; hash: canonical (a . b) -> chem-edge
adjacency      ; hash: atom idx -> ordered (edge-id . neighbor-idx)
rings          ; list of chem-ring, stable order
entry-atom exit-atom exit-node root
```

There is no Indigo toolkit, no `seen-*` scratch state, and no per-node option
snapshot in the finished drawing.

### Helpers already available

Coordinate and graph helpers:

- `chem-edge-at`
- `chem-get-edge`
- `chem-edge-other-atom`
- `chem-edge-stereo-directed`
- `chem-edges`
- `chem-visible-edges`
- `chem-atom-adjacency`
- `chem-rotate`
- `chem-flip-horizontal`
- `chem-flip-vertical`
- `chem-scale`

Ring helpers:

- `chem-ring-at`
- `chem-ring-center`
- `chem-ring-vertex-radii`
- `chem-ring-circumradius`
- `chem-ring-apothem`
- `chem-ring-regular-p`
- `chem-ring-symmetric-p`
- `chem-rings-for-atom`
- `chem-rings-for-edge`
- `chem-atom-in-ring-p`
- `chem-edge-in-ring-p`
- `chem-ring-side`

Stereochemistry helpers:

- `chem-stereocenter-configuration`
- `chem-stereocenter-wedge-edge`
- `chem-stereocenter-wedge-up-p`
- `chem-stereocenter-other-positions`
- `chem-signed-area`

`chem-stereocenter-wedge-up-p` lives in `chem-geometry.el`.  Use it to derive
the solid/hashed direction from the stored configuration and the current
coordinates, especially after `:flip` or `:flop`.

Do not rederive ring geometry in the SVG exporter.  Use these helpers and cache
per-render results if profiling shows a problem.

### Current tests and validation

- ERT suite: `eldev test`
- Byte compilation: `eldev compile`
- Module-free compile: `tools/check-module-free-compile.sh`
- SVG exporter tests: `test/chem-exp-svg-test.el`
- Org Babel tests: `test/chem-babel-test.el`
- SVG corpus smoke test: included, skips when the studies corpus is absent
- Studies corpus parity: `tools/validate-corpus.el` compare
- Studies graph check: `tools/validate-corpus.el` graph

The SVG exporter is pure Elisp and must compile even when the Indigo dynamic
module is unavailable.

## Design principles

1. **The IR is the contract.**  SVG consumes the IR; it does not read SMILES,
   MOL, Indigo handles, or chemfig output.
2. **No SVG fields in the IR.**  ViewBox, masks, colors, fonts, IDs, and XML are
   local to `chem-exp-svg.el`.
3. **Draw the graph, not the traversal tree.**  SVG iterates canonical visible
   edges.  `chem-node` is shared IR, but its traversal and phantom-node data is
   chemfig-oriented; SVG ignores it.
4. **Prepared geometry.**  Coordinates are already rotated, flipped, flopped,
   and scaled by the common pipeline.  Do not apply a second transform.  Wedge
   up/down direction is still derived from `chem-stereocenter-configuration`
   and the current geometry, not from a second coordinate transform.
5. **Deterministic output.**  Stable ordering, stable IDs, stable number
   formatting, and no timestamps or random values.
6. **No external runtime.**  Pure Elisp string generation.  `libxml` is used
   only in tests for validation.
7. **Incremental rendering.**  Start with a correct simple molecule and add
   bonds, rings, stereo, labels, and UI in separate phases.
8. **Keep chemfig output untouched.**  Do not change the chemfig renderer,
   mappings, parser behavior, goldens, or option semantics.  Generic option
   plumbing in `chem-options.el` and UI grouping may change.

## Files and registration

New file:

```
chem-exp-svg.el
```

It registers the exporter:

```elisp
(chem-register-exporter 'svg #'chem-exp-svg-render
  (list :label "SVG"
        :options chem-exp-svg-option-specs
        :target 'image))
```

The `:target 'image` metadata is forward-looking.  No current UI code reads it;
it is reserved for later image-insertion and Org work.

The renderer entry point:

```elisp
(defun chem-exp-svg-render (drawing options)
  "Render DRAWING to an SVG string using OPTIONS."
  ...)
```

It returns a string.  It does not write files.

Build/configuration updates:

- Add `chem-exp-svg.el` to `Eldev` `eldev-main-fileset`.
- Add `test/chem-exp-svg-test.el` to `Eldev` `eldev-test-fileset`.
- Add `chem-exp-svg.el` to both the copy list and the compile list in
  `tools/check-module-free-compile.sh`.
- Add generic exporter option merging in `chem-options.el`; see
  “Option resolution”.
- Ensure `chem-mode--generate` loads the selected exporter before building
  options, so its spec table is available.
- Require `chem-exp-svg` only through `chem-mode--ensure-exporter` (or an
  explicit `require` from a UI command), not from `chem-ir.el` or
  `chem-indigo.el`.
- The exporter-aware transient groups and SVG suffixes belong in
  `chem-dispatch.el`; see “UI integration”.

## Exporter options

Define `chem-exp-svg-default-*` variables in `chem-exp-svg.el` and declare
them in the centralized `chem-exp-svg-option-specs` table.  Keys use the
`:svg-*` prefix, so `chem-exp-svg-default-X` maps to `:svg-X`.

Starting set:

| Variable | Key | Meaning |
| --- | --- | --- |
| `chem-exp-svg-default-width` | `:svg-width` | number or nil for automatic |
| `chem-exp-svg-default-height` | `:svg-height` | number or nil for automatic |
| `chem-exp-svg-default-padding` | `:svg-padding` | margin in final SVG units |
| `chem-exp-svg-default-scale` | `:svg-scale` | IR-to-SVG multiplier, default 30.0 |
| `chem-exp-svg-default-bond-spacing` | `:svg-bond-spacing` | double/triple offset |
| `chem-exp-svg-default-bond-thickness` | `:svg-bond-thickness` | stroke width |
| `chem-exp-svg-default-short-bond-length` | `:svg-short-bond-length` | fraction for ring double bonds |
| `chem-exp-svg-default-font-family` | `:svg-font-family` | font family string |
| `chem-exp-svg-default-font-size` | `:svg-font-size` | number |
| `chem-exp-svg-default-theme` | `:svg-theme` | theme name symbol |
| `chem-exp-svg-default-aromatic` | `:svg-aromatic` | `auto`, `circle`, or `alternating` |
| `chem-exp-svg-default-show-stereo` | `:svg-show-stereo` | draw wedges and hashes |
| `chem-exp-svg-default-show-cip-labels` | `:svg-show-cip-labels` | default nil |
| `chem-exp-svg-default-show-enhanced-stereo` | `:svg-show-enhanced-stereo` | default nil |

There is no `:svg-background`.  Background is a theme property; see “Themes
and colors”.

### Option resolution

`chem-exp-svg-option-specs` is the single list of scalar SVG options.  Each
spec carries at least `:key`, `:variable`, `:label`, `:type`, and `:group`:

```elisp
(:key :svg-theme
 :variable chem-exp-svg-default-theme
 :label "Theme"
 :type symbol
 :group colors)
```

`chem--build-options` merges, in order:

1. common option specs;
2. the selected exporter's option specs;
3. explicit overrides supplied by the caller.

Precedence is: explicit overrides, then buffer-local variables, then global
defaults.  `chem-mode--generate` calls `chem-mode--ensure-exporter` before
building options so that `chem-exporter-option-specs` is available.

The SVG renderer reads only `:svg-*` keys from the options plist.  It does not
re-read common options or hardcode option values.  A small
`chem-exp-svg--option` helper may fall back to the corresponding
`chem-exp-svg-default-*` variable when a key is absent, for defensive
programmatic calls.

### Centralized tables (B1)

B1 keeps per-option defcustoms and centralizes the definitions:

- `chem-exp-svg-option-specs` — all scalar options, one table;
- `chem-exp-svg-themes` — all palettes, one table;
- `chem-exp-svg-default-theme` — the selected theme.

This mirrors SmilesDrawer's centralized `defaultOptions` and `themes` without
replacing the variable-based UI used by `chem-mode--option-vars`, the
transient, and describe/save/reload.

#### Future B2 option shape

A later refactor may adopt a SmilesDrawer-style single
`chem-exp-svg-default-options` alist with nested `:themes` and a deep merge
similar to `Options.extend`.  That would require path-based option accessors
and changes to `chem-mode--option-vars`, describe/save/reload, and the
transient.  It is explicitly out of scope for v1; keep the B1 shape until the
option/UI machinery supports it cleanly.

### Aromatic option ownership

The current common option for chemfig is `:aromatic-circles` (boolean).  The
SVG plan wants a richer `auto|circle|alternating` policy.

For v1, keep the SVG policy in the SVG exporter as `:svg-aromatic`
(`auto|circle|alternating`) and do not change the common option or chemfig.
Later, when the Org header contract is implemented, decide whether `:aromatic`
should become a common option and how chemfig maps to it.

Do not silently reuse `:aromatic-circles` for SVG if that makes the two
exporters behave differently in ways the user cannot see.

### Themes and colors

Themes are centralized in `chem-exp-svg-themes`, an alist of theme name to a
property plist.  Start with `light` and `dark`; additional palettes can be
added to the same table later.  Example:

```elisp
(defcustom chem-exp-svg-themes
  '((light . (:foreground "#222222"
             :background "#ffffff"
             :default    "#222222"
             :C "#222222" :N "#3498db" :O "#e74c3c"
             :S "#f1c40f" :P "#d35400"
             :F "#27ae60" :CL "#16a085" :BR "#d35400" :I "#8e44ad"
             :B "#e67e22" :SI "#e67e22" :H "#666666"))
    (dark . (:foreground "#ffffff"
            :background "#141414"
            :default    "#ffffff"
            :C "#ffffff" :N "#3498db" :O "#e74c3c"
            :S "#f1c40f" :P "#d35400"
            :F "#27ae60" :CL "#16a085" :BR "#d35400" :I "#8e44ad"
            :B "#e67e22" :SI "#e67e22" :H "#aaaaaa")))
  "Centralized SVG color themes.")
```

`:svg-theme` selects a key in this table.  Element colors are looked up by the
uppercased element symbol as a keyword (`"Cl"` becomes `:CL`).  Missing element
entries fall back to `:default`, then `:foreground`; if `:foreground` is also
missing, fall back to `:C`.

`chem-exp-svg-default-theme` defaults to `light`.  An unknown theme name is a
user error, not a silent fallback.

Background rules:

- `:background` is the only background source.
- a non-nil `:background` emits a background rectangle covering the viewBox;
- `:background nil` means transparent;
- users can add a custom theme with `:background nil` to the centralized table;
- there is no separate background override option.

The theme also controls bond color: draw bond strokes with `:foreground`, and
draw atom labels with the element color.  Keep this rule deterministic and
local to the SVG exporter.

## Coordinate system and canvas

IR coordinates are 2D chemistry coordinates with y up.  SVG uses y down.

Use one coordinate wrapper:

```elisp
x_svg = (x - min-x) * :svg-scale + :svg-padding
y_svg = (max-y - y) * :svg-scale + :svg-padding
```

All geometry options (`:svg-scale`, `:svg-padding`, `:svg-bond-spacing`,
`:svg-bond-thickness`, and `:svg-font-size`) are expressed in SVG user units.
The viewBox is computed from the scaled IR bounds plus `:svg-padding` on each
side.  `:svg-width` and `:svg-height` are output dimensions only; they do not
change the drawing coordinate system.  The default `:svg-scale` is 30.0, which
maps a normalized IR bond length of 1.0 to 30 SVG user units.

Do not apply ``:angle``, ``:flip``, ``:flop``, or ``:bond-scale`` in the
exporter.  Those transforms are already reflected in the IR coordinates.

### Bounds

Compute bounds from:

- atom coordinates;
- atom label boxes (approximate);
- ring circle extents;
- bond stroke widths;
- wedge widths and hash extents;
- the `:svg-padding` margin.

Text bounds are approximate because Emacs does not provide consistent font
metrics across platforms.  Use a deterministic estimate based on
`:svg-font-size` and label length, then add a safety margin.

### ViewBox and dimensions

- Always emit a `viewBox`.
- If `:svg-width` and `:svg-height` are nil, emit no fixed pixel dimensions;
  the SVG scales to its container.
- If only one dimension is supplied, compute the other from the viewBox aspect
  ratio and emit both; do not letterbox.
- If both are supplied, fit inside the requested box and letterbox with
  `preserveAspectRatio="xMidYMid meet"`.
- Add `xmlns="http://www.w3.org/2000/svg"`.

### Number formatting

Use fixed decimal formatting with trailing zeros removed, or an integer when
the value is integral.  Keep one helper for all coordinates and dimensions.
Avoid locale-dependent formatting and avoid raw float printing.

## Drawing order

Recommended order:

1. `<defs>`: masks and reusable definitions.
2. Theme background rectangle when the selected theme's `:background` is
   non-nil.
3. Normal bonds.
4. Ring circles.
5. Cross bonds.
6. Atom labels.
7. Stereochemistry labels and annotations.
8. Optional debug overlay, never in normal output.

Draw cross bonds after normal bonds so they can appear on top.  If a white
outline or halo is needed, add it explicitly rather than relying on the mask.
Apply the label mask to the bond/ring layer only; never mask the text layer
itself.

## Bonds

Iterate `chem-visible-edges`; skip `link-p` edges.  Use canonical `a`/`b`
orientation and derive the visible direction from the atom order, not from the
chemfig tree.

Use `chem-edge-order` and `chem-edge-stereo`; for tetrahedral wedges derive
the solid/hashed direction from the stereocenter configuration.  Do not use
the old chemfig bond-type vocabulary.

### Single bond

Draw a line from atom `a` to atom `b`.

### Double bond

Use the ring membership and ring side:

- If the edge is in a ring, place the second line on the ring side, toward the
  ring center.
- If both atoms are terminal or no ring side is available, draw two symmetric
  parallel lines separated by half `:svg-bond-spacing`.
- Otherwise draw the main line plus one offset line.

Shorten the second line on ring sides using the ring geometry and
`:svg-bond-spacing`.
Use `chem-ring-side`, `chem-rings-for-edge`, and `chem-ring-center`.

For fused rings, select the ring whose center is closest to the bond midpoint;
break ties by ring id.

### Triple bond

Draw three parallel lines.  Offset the outer lines by
`:svg-bond-spacing / 1.5` or a documented equivalent.

### Aromatic bonds

The SVG `:svg-aromatic` policy has three values:

- `circle`: draw a circle for eligible aromatic rings and draw ring bonds as
  single lines.
- `alternating`: draw a solid line and a dashed inner line on the ring side
  for bonds with `order :aromatic`.  If the molecule already has explicit
  alternating single/double orders, draw those normally.
- `auto`: recommend circles for eligible regular/symmetric aromatic rings and
  fall back to alternating/dashed inner lines for distorted or bridged rings.

Do not invent a Kekulé assignment in v1.  Document the limitation.

### Wedges and hashes

Use `chem-edge-stereo`, `chem-edge-stereo-center`, and the stereocenter
configuration.  Do not select a different wedge edge.

- The wide end is always `stereo-center`; the narrow end is the other atom.
- For an edge whose `stereo-center` atom has a `chem-stereocenter` with a
  non-nil `configuration`, derive the solid/hashed direction from the current
  coordinates:

  ```elisp
  (chem-stereocenter-wedge-up-p atoms center edge configuration)
  ```

  Draw a solid wedge when it returns non-nil, a hashed wedge otherwise.
- If there is no stereocenter configuration, for example an allene
  substituent wedge, fall back to `chem-edge-stereo-directed` from
  `stereo-center`.
- `:either`: wavy or dashed line.  A simple dashed line is acceptable for v1.
- `:cis` and `:trans` are double-bond geometry states, not wedge states.  Draw
  the double bond normally.  E/Z labels are out of scope.

This is how flip/flop/rotation preserve configuration.  Do not draw the stored
`:up`/`:down` unchanged after coordinate transforms when configuration is
available.

Respect `:svg-show-stereo`.  If disabled, draw a plain single line even when the
edge carries wedge/hash stereo.

The allene input wedges are ordinary substituent wedges and are drawn by the
same code.  The `chem-allene` record is not needed for basic drawing.

### Cross bonds

`chem-edge-cross-p` marks a bond that should be drawn over another bond.  Draw
cross bonds after normal bonds.  Start without a white halo; add one only if
visual inspection shows it is necessary.  Do not depend on the chemfig tree
overlay.

### Link bonds

Skip `link-p` edges.  They are invisible fragment connectors.

### Bond decoration

`chem-edge-decoration` is chemfig presentation (`:plain` or `:fancy`).  SVG
should ignore it in v1 and draw standard double/triple bonds.  Do not port the
chemfig fancy-bond shortening logic unless a later milestone asks for it.

## Rings and aromatic display

Use `chem-drawing-rings`; do not use the chemfig aromatic-circle pseudo-bond.

### Circle eligibility

A ring is a candidate for an aromatic circle when:

- `chem-ring-aromatic-p` is true;
- the ring is regular or sufficiently symmetric, using
  `chem-ring-regular-p` and `chem-ring-symmetric-p`;
- the ring is large enough for a readable circle;
- the ring does not contain a bridge that would make the circle misleading.

For v1, a simple rule is acceptable:

- draw circles for aromatic five- and six-membered rings that are regular;
- draw circles for larger regular aromatic rings if the geometry looks
  reasonable;
- fall back to alternating/dashed inner lines for distorted or bridged rings.

### Circle geometry

Use `chem-ring-center` and `chem-ring-apothem`.  The circle radius should be
smaller than the apothem by half `:svg-bond-thickness` and a small visual
margin.
Do not reuse the chemfig radius, which is a scaled apothem.

### Double bonds in rings

Use the ring center to choose the inner side.  This is the same geometric
decision needed for aromatic alternating representation.

## Atom labels and text

The IR already contains the shared label decisions:

- `chem-atom-label`: element string, or nil for a hidden carbon;
- `chem-atom-number`: atom number, or nil;
- `chem-atom-hydrogens`;
- `chem-atom-charge`;
- `chem-atom-radical`;
- `chem-atom-marker`.

Do not re-read `show-carbons`, `show-methyls`, or `atom-numbers` in the SVG
exporter.

### Label precedence

- If `number` is non-nil, draw the atom number and suppress hydrogens, charge,
  and radical.  A hidden carbon with a number still draws the number.
- If `label` is nil and there is no number, draw nothing.
- Otherwise draw the element label and its hydrogen/charge/radical annotations.

### Hydrogen, charge, and radical text

Recommended v1:

- 0 hydrogens: element only;
- 1 hydrogen: `H`;
- more than one: `H` plus a Unicode subscript count;
- charge: `+`, `-`, `2+`, `2-` as a superscript;
- radical: one or two dot characters as a superscript.

Unicode subscripts and superscripts are the default.  If a user reports a font
problem, add a tspan fallback option later.  Keep the glyph mapping in one
helper and test it.

### Text direction

Use the mean angle of the atom's visible neighbors
(`chem-atom-bond-angles`):

- terminal atoms or no visible neighbors: horizontal text;
- otherwise choose right, left, up, or down based on the mean angle;
- use `text-anchor` and `dominant-baseline` to keep the text near the atom.

This is a simplified version of SmilesDrawer's text-direction logic.  It does
not need to be perfect in v1, but it must be deterministic.

### Text bounds and masks

Add a black circle to the SVG mask at each visible atom label position.  Apply
the mask to the bond/ring group only; text elements must stay outside the
masked group.  Estimate the radius from `:svg-font-size` and label length.  Use
the same estimate for bounds and masking.

### XML escaping

Escape `&`, `<`, `>`, and quotes in text and attributes.  Use a single helper.
Test it with labels such as `&1` and with marker strings.

### Colors

Use the theme's element color for the label text.  Hidden atoms produce no
text and no color.

## Stereochemistry

### Wedges and hashes

Covered in the bond section.  This is the v1 stereochemistry drawing feature.

### CIP labels

`chem-stereocenter-cip` stores `:R`, `:S`, `:r`, `:s`, `:none`, `:unknown`,
`:E`, `:Z`, or `nil`.  Only `:R`, `:S`, `:r`, and `:s` are stereocenter labels.
`:E` and `:Z` are bond CIP descriptors and are out of scope for v1.

- Add `:svg-show-cip-labels`, default nil.
- Render only `:R`, `:S`, `:r`, and `:s`.  Case is significant.
- Do not render `:none`, `:unknown`, `:E`, or `:Z`.
- Place the label near the stereocenter atom with a deterministic offset.
- Start with a simple offset (for example, above-right) and refine collisions
  later.
- This is SVG-only; do not change chemfig.

### Enhanced stereo labels

`chem-stereocenter-type` and `chem-stereocenter-group` store the enhanced
stereo data.

- Add `:svg-show-enhanced-stereo`, default nil.
- `:abs` produces no enhanced-stereo label.
- `:and` group N produces `&N` (XML-escaped as `&amp;N`).
- `:or` group N produces `orN`.
- `:either` group N produces `eN`.
- Never flatten OR/AND/EITHER to absolute stereochemistry.
- If both CIP and enhanced-stereo labels are enabled, combine them in a
  documented format, for example `R, &1`, with correct XML escaping.

### Allene centers

`chem-allene` detection is available.  Do not derive aR/aS labels; the public
Indigo API does not expose an axial descriptor.  Draw the input wedges normally.
A future renderer can use the allene record for annotations, but not for an
axial CIP label.

### E/Z labels

Bond stereo `:cis`/`:trans` is available.  Do not derive E/Z labels in v1; this
requires substituent priorities and is a separate feature.

## Markers and atom numbers

- Atom numbers are already in `chem-atom-number`.
- Markers are already in `chem-atom-marker` and `chem-edge-marker`.
- Emit markers as SVG `id` attributes on groups or elements, not as visible
  text.
- For a hidden atom with a marker, emit an empty anchored group at the atom
  coordinates to carry the ID; do not draw visible text.
- Skip markers on `link-p` edges; those edges are invisible.
- Prefer a deterministic generated ID and carry the original marker in a
  `data-marker` attribute when sanitization would change it.
- Markers must not affect drawing order.

## XML, IDs, and determinism

### IDs

SVG masks and definitions need IDs.  Use deterministic IDs:

- fixed names for a single SVG, or
- a content-derived prefix for multiple SVGs on the same page.

Do not use `gensym`, random numbers, or timestamps.  If two SVGs from the same
drawing are inserted into one HTML page, they may share IDs; document that a
future `chem-insert-image` can add a unique prefix derived from the output
hash.

### Determinism

The same drawing and options must produce the same bytes.  Avoid:

- hash-table iteration order without sorting;
- random IDs;
- locale-dependent number formatting;
- timestamps;
- system font measurements.

Sort atoms, edges, rings, and labels by stable numeric ids.  Sort mask circles
by atom index.

### XML validity

Use well-formed XML.  Test with `libxml-parse-xml-region` when available, and
fall back to `xml-parse-region`.  Do not use string concatenation that assumes
labels cannot contain XML metacharacters.

## UI integration

Minimal v1:

- `chem-mode--ensure-exporter` already loads `chem-exp-NAME` on demand,
  including `chem-exp-svg` when `chem-default-exporter` is `svg`.
- `chem-options.el` merges the selected exporter's option specs generically;
  do not add another hardcoded SVG option block.
- `chem-render DRAWING 'svg OPTIONS` returns the SVG string.
- `chem-export` and `chem-copy` work with `svg` as source text.  This is not
  the primary Org workflow but is useful for debugging.
- `chem-insert-image`:
  - renders the selected drawing to SVG;
  - writes it to `chem-image-directory` using UTF-8 and a deterministic
    content-hash file name;
  - inserts an Org or Markdown link;
  - does not depend on Org at load time.

Transient menu (`chem-dispatch.el`):

- The SVG groups expose the `:svg-*` options.
- The theme selector sources its choices from `chem-exp-svg-themes`.
- Chemfig options are hidden when the exporter is `svg`.
- SVG options are hidden when the exporter is `chemfig`.
- The exporter selector remains visible.
- Image insertion is a separate action.
- UI tests cover option resolution, buffer-local defaults, and group
  visibility.

Org Babel integration is implemented in `chem-babel.el`; see
`chem-mode-plans/org.md` for the block contract and cache behavior.
`examples/org-playground.org` is the executable tour of the finished
features.

## Testing

### Unit tests

Create `test/chem-exp-svg-test.el`.

- XML escaping of `&`, `<`, `>`, and quotes.
- Deterministic number formatting.
- Unicode sub/superscript helpers.
- Theme lookup, element keyword normalization, and
  `:default`/`:foreground`/`:C` fallback.
- `:svg-*` option resolution, including buffer-local defaults and generic
  exporter-spec merging.
- Bounds and coordinate mapping, including `:svg-scale` and `:svg-padding`.
- SVG ID generation.

### Exporter tests

- `chem-render DRAWING 'svg OPTIONS` returns a string.
- Output parses as XML.
- Output contains an `<svg>` root and a `viewBox`.
- Single bonds produce line elements.
- Double and triple bonds produce the expected number of line elements.
- Atom labels appear as text for heteroatoms and charged/radical atoms.
- Hidden carbons produce no text.
- Atom numbers take precedence over hydrogens/charge/radical.
- Hydrogen, charge, and radical text is present when expected.
- Markers appear as stable IDs, including on hidden atoms.
- Link bonds are absent.
- Cross bonds are present and drawn after normal bonds.
- The theme background produces a rectangle when `:background` is non-nil and
  produces no rectangle when it is nil.
- `:svg-theme` selects the requested theme, and an unknown theme is a user
  error.
- Element colors and the `:default`/`:foreground` fallback appear in output.
- The label mask applies to bonds and rings, not to text.
- Default output is deterministic across repeated renders.
- Terse/compact output, if added, is still valid XML.

### Stereochemistry tests

- Alanine and lactic acid produce wedge/hash polygons or hash lines.
- `:up` and `:down` place the wide end at `stereo-center`.
- Flip, flop, and rotation preserve configuration: the solid/hashed direction
  is derived from `chem-stereocenter-configuration` and current geometry.
- `:either` produces a valid fallback.
- `:svg-show-stereo` nil produces a plain bond.
- CIP labels render only when enabled and only for `:R`, `:S`, `:r`, `:s`.
- `:E`, `:Z`, `:none`, and `:unknown` CIP descriptors are not rendered as
  stereocenter labels.
- Enhanced-stereo labels render only when enabled and for AND/OR/EITHER.
- `:abs` produces no group label.
- Allene input wedges render, but no aR/aS label appears.
- E/Z labels are absent.

### Ring tests

- Benzene with `:svg-aromatic circle` contains a circle and single ring bonds.
- Benzene with `:svg-aromatic alternating` contains dashed inner lines.
- Naphthalene produces deterministic circles or alternating fallback.
- Fused/bridged rings do not crash.
- Ring double bonds are drawn on the ring side.

### Visual tests

Manual, not automated:

- Open the SVG in Emacs `image-mode` or a browser.
- Compare ethanol, benzene, caffeine, alanine, lactic acid, naphthalene, and a
  cross-bond molecule with the chemfig output.
- Check label collision, wedge direction, ring circles, text legibility, and
  light/dark theme backgrounds.

### Corpus tests

The smoke test over the 1000-molecule studies corpus:

- renders every MOL to SVG;
- parses every result as XML;
- verifies deterministic output on a sample;
- records failures with file names;
- does not require visual comparison;
- skips when `studies/external-corpus` is unavailable.

### Regression gates

```sh
eldev test
eldev compile
tools/check-module-free-compile.sh
```

Studies parity and graph checks remain chemfig checks.  The SVG corpus smoke
test is additional.  The module-free compile check must include
`chem-exp-svg.el` in both its copy and compile lists.

## Implementation phases and commits

### Phase 0 — baseline

- Confirm the `svg` branch is based on the clean `chem-mode` point.
- Run `eldev test` and capture the current count.
- Confirm the studies corpus baseline if available.
- No code changes.

### Phase 1 — exporter skeleton and primitives

Commit: `add svg exporter primitives`

Status: complete.

- Add `chem-exp-svg.el`.
- Add `chem-exp-svg-default-*` options, `chem-exp-svg-option-specs`,
  `chem-exp-svg-themes`, and the selected `chem-exp-svg-default-theme`.
- Add generic exporter option merging in `chem-options.el` and ensure the
  exporter is loaded before options are built.
- Add XML escaping, number formatting, coordinate mapping, bounds, and
  element helpers.
- Register `'svg` with a minimal renderer.
- Add unit tests for the helpers, option resolution, and theme lookup.
- Add the new file to `Eldev`, `eldev-test-fileset`, and both module-free
  compile lists.

### Phase 2 — basic molecules

Commit: `render atoms and bonds in svg`

Status: complete.

- Single, double, and triple bonds.
- Cross-bond draw ordering.
- Atom labels, hydrogens, charges, radicals, atom numbers, and hidden-atom
  marker anchors.
- Text direction, masks, and viewBox.
- Theme background rectangle.
- Basic deterministic output.
- Tests for ethanol, benzene, caffeine, simple charged/radical molecules, and
  theme background on/off.

### Phase 3 — rings and aromatic display

Commit: `render rings and aromatic bonds in svg`

Status: complete.

- Aromatic circles using ring geometry.
- Alternating/dashed inner lines.
- Ring-side double bonds.
- Fused/bridged fallback behavior.
- Tests for benzene, naphthalene, and distorted rings.

### Phase 4 — stereochemistry drawing

Commit: `render stereochemistry in svg`

Status: complete.

- Filled wedges, hashed wedges, and either fallback.
- `:svg-show-stereo` option.
- Configuration-derived wedge direction under flip/flop/rotation.
- Allene input-wedge preservation.
- Tests for alanine, lactic acid, bromochloromethane, and two-center molecules.

### Phase 5 — stereochemistry labels

Commit: `add stereochemistry labels to svg`

Status: complete.

- CIP labels (`R`, `S`, `r`, `s`) behind `:svg-show-cip-labels`.
- Enhanced-stereo labels (`&N`, `orN`, `eN`) behind
  `:svg-show-enhanced-stereo`.
- Combined label formatting and XML escaping.
- Tests for CIP and enhanced-stereo fixtures.
- No chemfig changes.

### Phase 6 — UI and image insertion

Commit: `add svg image insertion`

Status: complete.

- Add `chem-insert-image` if absent.
- Add `chem-image-directory` and deterministic UTF-8 file naming.
- Add Org/Markdown link insertion.
- Add exporter-aware transient groups.
- Add the theme selector sourced from `chem-exp-svg-themes`.
- Add UI tests for option resolution, buffer-local defaults, and visibility.
- Keep Org Babel out of this commit.

### Phase 7 — corpus and docs

Commit: `validate and document svg rendering`

Status: complete.

- Corpus smoke test.
- README/CHANGELOG updates.
- Update `docs/svg-rendering.md` with actual status and limitations.
- Keep `docs/stereochemistry.md` unchanged unless a stereochemistry detail
  changes.

## Performance

- The renderer should be linear in atoms, edges, and rings for normal
  molecules.
- Avoid repeated ring scans inside per-bond loops; cache ring membership per
  render if needed.
- Avoid building large intermediate XML trees; direct string assembly is
  acceptable and simpler.
- Text measurement is approximate; do not call font-configuration APIs per
  atom.
- Profile only if a large molecule or the corpus is noticeably slow.

## Risks and pitfalls

1. **Coordinate transforms:** the IR is already transformed.  Applying
   rotation or scaling again is the most likely geometry bug.  Wedge direction
   is a special case: derive it from configuration and current geometry, do
   not apply a second transform.
2. **Y-axis flip:** all coordinates must go through one helper.  Mixing IR
   and SVG coordinates produces mirrored drawings.
3. **Ring side selection:** fused and bridged rings are the hardest geometry.
   Start with a deterministic simple rule and document fallbacks.
4. **Text metrics:** without real font metrics, labels and masks can be
   slightly off.  Use conservative padding.
5. **XML escaping:** `&1` enhanced-stereo labels and marker strings must be
   escaped.
6. **ID collisions:** multiple SVGs on one page need deterministic unique IDs
   or a documented single-SVG assumption.
7. **Wedge direction:** the wide end is `stereo-center`; do not confuse it with
   the canonical edge endpoint.  After flip/flop, do not reuse the stored
   `:up`/`:down`; recompute solid/hashed from configuration and geometry.
8. **Aromatic representation:** do not invent Kekulé bonds.  Use circles or
   dashed inner lines.
9. **Cross bonds:** draw after normal bonds and do not rely on chemfig phantom
   nodes.
10. **Scope creep:** Org Babel sessions/sexpr, E/Z labels, allene aR/aS,
    best-wedge edge selection, and chemfig changes are separate features.
11. **Font/Unicode support:** test subscripts and superscripts with the default
    font; provide a tspan fallback only if needed.
12. **Determinism:** hash-table order and raw float printing are common
    sources of non-reproducible SVG.
13. **Option plumbing:** keep `chem-exp-svg-default-*` variables and `:svg-*`
    keys in sync through `chem-exp-svg-option-specs`; test generic exporter
    option merging so SVG options actually reach the renderer.
14. **Theme lookup:** element symbols are normalized to uppercase keywords and
    fall back to `:default`, `:foreground`, then `:C`; the background is
    theme-owned and must not be reintroduced as a separate `:svg-background`
    option.

## Open questions

1. Which built-in themes ship in v1?  Recommended: `light` and `dark`; more
   palettes can be added to `chem-exp-svg-themes` later.
2. Should small molecules use golden SVG strings, or structural assertions
   only?  Recommended: structural assertions plus a small golden for ethanol
   and benzene.
3. Should isotope labels be added when `emacs-indigo` exposes an isotope
   accessor?  Defer until the API exists.
4. Should multiple SVGs share IDs?  Recommend content-derived prefixes for
   inserted images; fixed IDs are acceptable for a single rendered string.
5. Should `chem-exp-svg-themes` participate in the generic option
   describe/save/reload surface, or remain a palette defcustom edited through
   `customize`?  Recommended: customize-only for v1.
6. Should custom themes be selectable only by name (recommended) or also by an
   inline `:svg-themes` override?  Recommend name-only for v1; add inline
   overrides later if needed.

## Definition of done

- `chem-render DRAWING 'svg OPTIONS` returns valid, deterministic SVG.
- The renderer uses the current IR and does not require Indigo.
- Scalar options resolve through `:svg-*` keys and
  `chem-exp-svg-option-specs`.
- Theme selection works through `chem-exp-svg-themes`; background comes only
  from the theme, with no `:svg-background` option.
- Single, double, triple, and aromatic bonds render.
- Atom labels, hydrogens, charges, radicals, atom numbers, and markers render,
  including markers on hidden atoms.
- Wedges and hashes render with the correct wide end and preserve
  configuration under flip/flop/rotation.
- Cross bonds render after normal bonds; link bonds are omitted.
- Aromatic circles and alternating representations work.
- CIP and enhanced-stereo labels are optional and default off.
- Allene input wedges render; no aR/aS label is produced.
- `chem-insert-image` writes deterministic UTF-8 SVG files and inserts
  Org/Markdown links.
- Transient groups expose only the selected exporter's options.
- XML validity, determinism, and corpus smoke tests pass.
- `eldev test`, `eldev compile`, and module-free compile pass.
- Chemfig output and goldens are unchanged.
- The plan is updated with actual implementation status and limitations.

## Appendix A — current IR cheat sheet

```elisp
;; Edges
(chem-edges drawing)              ; all canonical edges, sorted by id
(chem-visible-edges drawing)      ; excludes link-p
(chem-get-edge drawing a b)
(chem-edge-order edge)
(chem-edge-stereo edge)
(chem-edge-stereo-center edge)
(chem-edge-cross-p edge)
(chem-edge-link-p edge)
(chem-edge-marker edge)
(chem-edge-stereo-directed edge from)

;; Rings
(chem-drawing-rings drawing)
(chem-ring-center drawing ring)
(chem-ring-apothem drawing ring)
(chem-ring-circumradius drawing ring)
(chem-ring-regular-p drawing ring)
(chem-ring-symmetric-p drawing ring)
(chem-ring-aromatic-p ring)
(chem-rings-for-edge drawing edge-id)
(chem-ring-side drawing ring from to)

;; Atoms
(chem-atom-label atom)
(chem-atom-number atom)
(chem-atom-hydrogens atom)
(chem-atom-charge atom)
(chem-atom-radical atom)
(chem-atom-marker atom)
(chem-atom-stereocenter atom)
(chem-atom-allene atom)
(chem-atom-bond-angles atom)

;; Stereochemistry
(chem-stereocenter-cip center)
(chem-stereocenter-type center)
(chem-stereocenter-group center)
(chem-stereocenter-configuration center)
(chem-stereocenter-wedge-edge center)
(chem-stereocenter-wedge-up-p atoms center edge configuration)
(chem-allene-center allene)
(chem-allene-terminals allene)
(chem-allene-substituents allene)
```

## Appendix B — minimal renderer skeleton

```elisp
(defun chem-exp-svg-render (drawing options)
  "Render DRAWING to an SVG string using OPTIONS."
  (let* ((state (chem-exp-svg--make-state drawing options))
         (body (list (chem-exp-svg--background state)
                     (chem-exp-svg--bonds state)
                     (chem-exp-svg--rings state)
                     (chem-exp-svg--atoms state)
                     (chem-exp-svg--stereo-labels state))))
    (chem-exp-svg--document state body)))
```

Options are read with `chem-exp-svg--option`, for example
`(chem-exp-svg--option state :svg-theme)`.  The helper resolves the `:svg-*`
key and may fall back to the corresponding `chem-exp-svg-default-*` variable.
Theme colors come from `chem-exp-svg-themes`.

The real implementation will split these helpers further.  Keep the
entry point small.

## Appendix C — references

- `chem-ir.el` — current stable IR and geometry helpers.
- `chem-geometry.el` — stereocenter configuration and
  `chem-stereocenter-wedge-up-p`.
- `chem-indigo.el` — ingestion, canonical edges, ring records, allene data.
- `chem-options.el` — common options and generic exporter option merging.
- `chem-exp-chemfig.el` — reference renderer for label precedence, rings, and
  stereo behavior; do not copy its tree traversal into SVG.
- `chem-dispatch.el` — transient menu, including the exporter-aware groups and
  theme selector.
- `chem-babel.el` — optional Org Babel integration; see
  `chem-mode-plans/org.md`.
- `test/chem-exp-chemfig-parser.el` — test helper for the chemfig output; not
  used by SVG.
- `svg-plans/` — local historical design notes.  Use them for ideas, but verify
  every IR claim against the current code.
- `smilesDrawer/` — local reference checkout for post-layout drawing
  algorithms.  Port only drawing logic, not the parser or layout engine.
  Include attribution if code is ported closely.
  `smilesDrawer/src/DrawerBase.js` is the reference for the centralized
  `defaultOptions`/`themes` shape, and `smilesDrawer/src/ThemeManager.js` for
  theme color lookup.
- SmilesDrawer license: MIT.  The project itself is MIT.

## Appendix D — validation commands

```sh
eldev test
eldev compile
tools/check-module-free-compile.sh
```

Studies corpus:

```sh
emacs -Q --batch -L . -L emacs-indigo -L test \
  -l tools/validate-corpus.el \
  --eval '(chem-validate-corpus-compare "studies/external-corpus" "/tmp/chem-mode-baseline")'

emacs -Q --batch -L . -L emacs-indigo -L test \
  -l tools/validate-corpus.el \
  --eval '(chem-validate-corpus-graph "studies/external-corpus")'
```

The SVG corpus smoke test lives in `test/chem-exp-svg-test.el` and skips when
the studies corpus is not available.
