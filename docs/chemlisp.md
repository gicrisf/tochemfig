# CheMLisp

> **Status: experimental.** CheMLisp's syntax, public API, and rendered output
> may change without notice.  Do not rely on it as a stable drawing format yet.

CheMLisp is a hand-authorable S-expression drawing language for `chem-mode`.
It compiles directly to the shared `chem-drawing` IR and renders with both the
SVG and chemfig exporters.  It is a source format for **drawings**, not a
replacement for SMILES or MOL.

The compiler module is pure Elisp and builds without the Indigo dynamic module.
The `chemlisp` Org Babel language also works without Indigo; only
SMILES/MOL/RXN blocks load the Indigo ingestion layer on demand.

## Quick start

In Org:

```org
#+begin_src chemlisp :results file
(chem-def hydroxyl ()
  (O))

(molecule
  (C
    (C (hydroxyl))))
#+end_src
```

Press `C-c C-c` to regenerate the SVG.  Use `:renderer chemfig` with
`:results value` or `:results raw` to get chemfig source instead.

Programmatically:

```elisp
(require 'chemlisp)
(require 'chem-exp-svg)

(chemlisp-compile "(molecule (C (C (O))))")      ; => chem-drawing
(chemlisp-render "(molecule (C (C (O))))" 'svg)  ; => SVG string
```

## Drawing forms

A document contains any number of top-level `chem-def` forms and exactly one
drawing form:

```elisp
(chem-def name (params...) body)

(molecule ...)
```

Definitions are collected before expansion, so they may appear before or after
the drawing and may use each other in any order.

### Atoms

```elisp
(C (C (O)))        ; ethanol
(C (C (=O) (O)))   ; acetic acid
```

Each nested form is one bond.  The first atom child continues the main chain;
later children are branches.

```elisp
(C
  (C                 ; continues the trunk
    (C
      (C)))
  (C))               ; branch
```

Use `(C :exit t)` to mark the terminal atom explicitly.

Lowercase atoms are aromatic:

```elisp
(c n o s p b se as)
```

A bond between two aromatic atoms defaults to aromatic; an aromatic/aliphatic
bond defaults to single.  An explicit bond marker overrides the default.

### Bond markers

```elisp
(=O)   ; double
(≡N)   ; triple
(~C)   ; aromatic
(>C)   ; up wedge, narrow end at the parent
(<C)   ; down wedge
(_C)   ; invisible link
```

Equivalent explicit forms:

```elisp
(O :bond :double)
(N :bond :triple)
(C :bond :aromatic)
(C :bond :single :stereo :up)
(C :bond :single :stereo :down)
(C :bond :link)
```

### Attributes

Attributes follow the atom token, before its children:

```elisp
:id c1
:angle 30
:turn -15
:length 1.2
:charge -1
:h 3                  ; :hydrogens 3 is an alias
:radical 1
:show :auto|:always|:never
:exit t
:at (0 0)             ; root only
```

### Closures

Named targets are stable under edits:

```elisp
(c :id c1
  (c :id c2 :angle 0
    (c :id c3 :angle 60
      (c :id c4 :angle 120
        (c :id c5 :angle 180
          (c :id c6 :angle 240
            (close c1)))))))
```

Numeric targets use one-based pre-order atom indices:

```elisp
(close 1)
```

`(-> c1)` is an alias for `(close c1)`.  Forward references are rejected, and a
closure target must be an ancestor of its source atom.  A closure may carry
`:bond` or `:stereo` attributes.

### Geometry

`:angle` is absolute, counterclockwise.  `:turn` is relative to the incoming
direction.  `:length` defaults to `1.0`.

Missing angles use a deterministic skeleton layout.  Because rings need to
close, author ring bonds with explicit angles:

```elisp
(c :id c1
  (c :id c2 :angle 0
    (c :id c3 :angle 60
      (c :id c4 :angle 120
        (c :id c5 :angle 180
          (c :id c6 :angle 240
            (close c1)))))))
```

### Fragments and view

Multiple roots become disconnected fragments:

```elisp
(molecule
  (Na :charge 1)
  (Cl :charge -1))
```

The view wrapper transforms the final drawing:

```elisp
(molecule :rotation -30 :flip-x t :scale 1.1
  (C ...))
```

Supported view keys: `:rotation`, `:flip-x`, `:flip-y`, `:scale`,
`:translation`.

## Definitions

### Values

An empty parameter list defines a reusable value:

```elisp
(chem-def hydroxyl ()
  (O))

(chem-def methyl ()
  (C))

(molecule
  (C
    (C (hydroxyl))
    (methyl)))
```

A use may append children to the definition root:

```elisp
(molecule
  (C (C (hydroxyl (C)))))   ; ethanol, built from O + C
```

Use-site attributes and bond markers apply to the use:

```elisp
(molecule
  (C (hydroxyl :charge -1))
  (C (=hydroxyl)))
```

### Templates

A non-empty parameter list defines a template.  Parameters are holes:

```elisp
(chem-def phenyl (ortho meta para)
  (c :id c1
    (c :id c2 :angle 0
      (c :id c3 :angle 60
        (c :id c4 :angle 120
          (c :id c5 :angle 180
            (c :id c6 :angle 240
              (close c1)))
          (para))
        (meta))
      (ortho))))

(molecule
  (phenyl :para (O)))
```

Positional arguments are allowed:

```elisp
(phenyl nil nil (O))
```

Keyword and positional arguments cannot be mixed.  Missing arguments default to
`nil`, and `nil` removes the hole.  Each parameter must be used exactly once in
the template body.

Reserved use-site attributes on a template call are `:bond`, `:stereo`,
`:angle`, `:turn`, `:length`, `:charge`, `:h`, `:radical`, `:show`, `:exit`,
and `:at`.
Parameter names cannot collide with those keywords.

```elisp
(molecule
  (C (phenyl :para (O) :angle 90)))
```

### Local values

`chem-let` is parallel; `chem-let*` is sequential:

```elisp
(chem-let ((hydroxyl (O))
           (methyl   (C)))
  (molecule
    (C
      (C (hydroxyl))
      (methyl))))

(chem-let* ((methyl (C))
            (ethyl  (C (methyl))))
  (molecule
    (C (ethyl))))
```

Local bindings shadow document definitions.  Local bindings cannot shadow
template parameters in v1.  Each use expands with fresh IDs, so closures inside
a reused value stay local to that instance.

## Scoping and expansion

- Definitions and local bindings expand away before IR construction.
- `chem-def` is source reuse; it does **not** emit chemfig `\definesubmol`.
  The chemfig `:submol-name` option still wraps the whole drawing.
- Atom IDs are scoped to a definition instance.  A caller cannot close to an
  atom inside a used definition, and a definition cannot close to a caller atom.
- Numeric closures refer to one-based pre-order indices in the final expanded
  drawing.  Prefer named IDs inside reusable definitions.
- Definitions, parameters, and binding names cannot collide with element or
  aromatic tokens.  Names such as `a`, `x`, `name`, and `other-name` are fine;
  `b`, `c`, `n`, `o`, `s`, `p` are reserved.

## Labels and hydrogens

Implicit hydrogens are derived from a small valence table.  `:h` overrides them.

- Aromatic carbon infers `H` from its aromatic bond count.
- Aromatic heteroatoms default to no hydrogen; specify `:h` explicitly
  (for example `(n :h 1)` for pyrrole and `(n :h 0)` for pyridine).
- `:show :always` forces an atom label.
- `:show :never` hides the label while keeping the atom and its bonds.

## Common options

CheMLisp uses the same exporter options as the other input formats.  The
following common options apply:

- `:angle`, `:flip`, `:flop` for an extra presentation transform;
- `:show-carbons`, `:show-methyls`, `:atom-numbers` for label policy;
- `:fancy-bonds` for chemfig decorations;
- `:markers` for atom and bond markers;
- `:aromatic-circles` for chemfig aromatic circles;
- all `:svg-*` options for SVG.

Options that describe SMILES/MOL ingestion do not apply:
`:bond-scale`, `:bond-stretch`, `:recalculate-coordinates`, `:hydrogens`,
`:entry-atom`, `:exit-atom`, and `:cross-bond`.  Use CheMLisp geometry, `:h`,
and `:exit` instead.  `:cross` is not implemented in v1.

## Security

The reader uses Lisp `read` with read-time evaluation disabled.  It accepts
symbols, numbers, strings, keywords, and proper lists, and rejects reader
evaluation such as `#.(...)`.  CheMLisp definitions are data forms, not Emacs
Lisp macros, and are never evaluated.

## Current limitations

- Ring geometry is not auto-laid-out; use explicit angles.
- Closures must target an ancestor of their source atom.
- No `chem-flet` / `chem-flet*`.
- No chemfig `\definesubmol` emission from `chem-def`.
- No pseudo groups, hidden atoms, carbon dots, or free-form text labels.
- No reactions, scenes, freeze, writer, or SMILES/MOL conversion yet.
- No isotope attribute.

## Tests

```sh
emacs -Q --batch -L . -L emacs-indigo -L test \
  -l test/chemlisp-test.el -f ert-run-tests-batch-and-exit
```

The compiler also participates in
`tools/check-module-free-compile.sh`, proving it builds without the Indigo
dynamic module.
