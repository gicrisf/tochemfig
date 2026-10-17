# chem-mode Quickstart Guide

Generate LaTeX chemfig code from molecular structures directly in Emacs.

`chem-mode` is a native Emacs Lisp port of the `mol2chemfig` generator: it
uses [emacs-indigo](https://github.com/gicrisf/emacs-indigo) to read and lay
out molecules, and produces chemfig code with no Python or shell dependency.

## Prerequisites

1. **Emacs 29.1+**
2. **emacs-indigo 0.10.1+** (0.11.1 recommended): build the C module first:
   ```bash
   cd emacs-indigo
   ./install.sh
   ```
   If you installed it via Doom/straight, make sure it is not stale
   (`M-x straight-pull-package RET indigo`, or `doom sync -u`); old builds can
   break byte-compilation of chem-mode.
3. **LaTeX** with the `chemfig`/TiKZ packages (to render the output).

## Installation

Add to your Emacs config (`~/.emacs` or `~/.emacs.d/init.el`):

```elisp
;; Add paths
(add-to-list 'load-path "/path/to/tochemfig")
(add-to-list 'load-path "/path/to/tochemfig/emacs-indigo")

;; Load the package
(require 'chem-mode)

;; Optional: context-aware minor mode (the transient is autoloaded)
(require 'chem-mode)
```

## Transient menu (primary interface)

`M-x chem-dispatch` opens the options menu. It
works from any buffer and enables `chem-mode` if needed. If the region or
the text at point looks like a molecule, that is used; otherwise you are
prompted with the current input mode.

| Key   | Action                                             |
|-------|----------------------------------------------------|
| `RET` | Generate and insert                                |
| `C`   | Copy generated code to the kill ring               |
| `F`   | Generate from a MOL file (one-off)                 |
| `A`   | Rotate the next generation only                    |
| `?`   | Show the `C-c m` key bindings                      |

For example, put point on `CCO` and press `RET`, or run `M-x chem-export` to do
the same without opening the menu. Ethanol is inserted as
`-[:330]-[:30,,,1]OH`.

The old one-shot commands (`tochemfig-input-direct`, `tochemfig-input-file`,
`tochemfig-terse`, `tochemfig-rotate`, `tochemfig-fancy-bonds`,
`tochemfig-wrap-chemfig`, ...) have been removed. Use the corresponding menu
option or `M-x chem-export` instead.

## Context-aware mode

Enable `chem-mode` in a buffer to generate chemfig code from the region
or the SMILES string at point. In the wrapping contexts (LaTeX, Org and
Markdown by default) the output is wrapped in `\chemfig{...}` automatically;
wrapping is decided per command, so LaTeX source blocks inside Org work too.

```elisp
(add-hook 'latex-mode-hook #'chem-mode)
(add-hook 'org-mode-hook #'chem-mode)
```

To enable it across all the wrapping contexts at once, use the global mode:

```elisp
(chem-global-mode 1)
```

It only enables the mode in wrapping contexts; set
`chem-global-mode-all-buffers` to `t` to enable it everywhere, and
`chem-mode-wrap-modes` to change which major modes count.

The commands are bound under the `C-c m` prefix:

| Key       | Command                      | Description                           |
|-----------|------------------------------|---------------------------------------|
| `C-c m g` | `chem-export`               | Generate for region / point / prompt  |
| `C-c m o` | `chem-dispatch`         | Transient options menu                |
| `C-c m c` | `chem-copy`             | Copy to the kill ring                 |
| `C-c m d` | `chem-describe-options` | Describe effective options            |

Other useful commands:

| Command                     | Description                                       |
|-----------------------------|---------------------------------------------------|
| `chem-describe-options`| Show effective options (buffer-local vs global)   |
| `chem-save-options`    | Save effective options as the global defaults     |
| `chem-reload-options`  | Reload the saved global defaults                  |
| `chem-reset-options`   | Reset buffer options (`C-u`: global defaults)     |

## Options menu

The menu edits every option live and generates with `RET`. Options are
buffer-local, and are only localized the first time you edit them, so global
default changes still reach untouched buffers. The action keys are `RET`
(generate), `C` (copy), `F` (MOL file), `A` (rotate once) and `?` (bindings).

## Evil / Doom

`chem-mode` is plain Emacs; its `C-c m` bindings work in Evil's normal and
visual states and there is no Evil dependency. Doom users usually make the
transient menu the primary entry point and map the top-level commands under
their leader:

```elisp
(map! :leader
      (:prefix ("C" . "chem")
       :desc "Options menu (RET generate, C copy, F file, A rotate)"
       "o" #'chem-dispatch
       :desc "Generate at point/region" "g" #'chem-export
       :desc "Copy to kill ring" "c" #'chem-copy
       :desc "Describe options" "d" #'chem-describe-options
       :desc "Save options as global defaults" "s" #'chem-save-options
       :desc "Reload saved options" "r" #'chem-reload-options
       :desc "Reset buffer options" "R" #'chem-reset-options))
```

`SPC C o` is the primary entry point. Inside the menu use `RET` to generate,
`C` to copy, `F` for one-off MOL input, `A` to rotate once and `?` for the
bindings. The `g`, `c` and `d` leader keys are shortcuts for the same top-level
commands; `s`, `r` and `R` manage the option defaults. File input and one-off
rotation intentionally have no separate leader bindings: they live in the menu.
The old `C-c m f/t/w/r` shortcuts are gone.

Generating from a visual selection replaces it and exits visual state. The
`*chem options*` buffer is read-only and uses Emacs state, so `q` quits it.
Doom's menu bar is off, so use `SPC C o` / `C-c m o` / which-key.

## Examples

Ethanol:
```
-[:330]-[:30,,,1]OH
```

Acetic acid:
```
-[:90](=[:150]O)-[:30,,,1]OH
```

Glycine:
```
H_2N-[:330,,2]-[:30](=[:330]O)-[:90,,,1]OH
```

Caffeine with wrapper (open `M-x chem-dispatch`, enable `w` if needed,
then `RET` on `CN1C=NC2=C1C(=O)N(C(=O)N2C)C`) produces the fully annotated
structure shown in the README.

## All options

| Variable                                   | Default     | Description                              |
|--------------------------------------------|-------------|------------------------------------------|
| `chem-default-input`                  | `direct`    | Input mode: `direct` or `file`           |
| `chem-exp-chemfig-default-terse`                  | `nil`       | Compact output                           |
| `chem-default-strict`                 | `t`         | Abide by Indigo's structure validation   |
| `chem-exp-chemfig-default-indent`                 | `4`         | Branch indentation                       |
| `chem-default-recalculate-coordinates`| `nil`       | Discard and recalculate coordinates      |
| `chem-default-angle`                  | `0.0`       | Rotation angle (degrees)                 |
| `chem-exp-chemfig-default-relative-angles`        | `nil`       | Relative bond angles                     |
| `chem-default-flip`                   | `nil`       | Mirror horizontally                      |
| `chem-default-flop`                   | `nil`       | Mirror vertically                        |
| `chem-default-show-carbons`           | `nil`       | Show C labels                            |
| `chem-default-show-methyls`           | `nil`       | Show CH3 labels                          |
| `chem-default-hydrogens`              | `keep`      | H handling: `keep`/`add`/`delete`        |
| `chem-default-aromatic-circles`       | `nil`       | Draw circles in aromatic rings           |
| `chem-default-fancy-bonds`            | `nil`       | Enhanced double/triple bond style        |
| `chem-default-atom-numbers`           | `nil`       | Show atom indices                        |
| `chem-default-bond-scale`             | `normalize` | Bond scaling mode: `keep`/`scale`/`normalize` |
| `chem-default-bond-stretch`           | `1.0`       | Bond length factor / target average      |
| `chem-default-markers`                | `nil`       | Atom/bond marker prefix                  |
| `chem-default-cross-bond`             | `nil`       | Bonds to draw crossing over others       |
| `chem-exp-chemfig-default-legacy-lewis`           | `nil`       | Use legacy `\lewis` (ChemFig < 1.6)      |
| `chem-exp-chemfig-default-wrap-chemfig`           | `nil`       | Wrap in `\chemfig{...}`                  |
| `chem-exp-chemfig-default-submol-name`            | `nil`       | Wrap in `\definesubmol{NAME}{...}`       |
| `chem-default-entry-atom`             | `nil`       | First atom of a sub-molecule             |
| `chem-default-exit-atom`              | `nil`       | Last atom of a sub-molecule              |

Set your preferred defaults in your config:

```elisp
(setq chem-default-input 'direct)
(setq chem-exp-chemfig-default-relative-angles t)
(setq chem-default-fancy-bonds t)
(setq chem-exp-chemfig-default-wrap-chemfig t)
(setq chem-default-angle -30.0)
```

## Using in LaTeX

```latex
\documentclass{article}
\usepackage{chemfig}

\begin{document}

% Acetic acid
\chemfig{-[:90](=[:150]O)-[:30,,,1]OH}

\end{document}
```

Some output uses helper macros inherited from `mol2chemfig`: charges
(`\mcfplus`, `\mcfminus`, `\mcfright`, ...), atom numbers (`\mcfatomno`),
aromatic circles (`\mcfcringle`), wavy bonds (`\mcfwavy`) and fancy bonds
(`\dbl`, `\dbr`, `\trpl`, ...). Include mol2chemfig's `mcfmacros` preamble
(or the equivalent definitions) when rendering structures that use them.

## Keybindings (optional)

If you prefer global bindings over the minor mode:

```elisp
(global-set-key (kbd "C-c m g") 'chem-export)          ; region / point / prompt
(global-set-key (kbd "C-c m o") 'chem-dispatch) ; options menu
(global-set-key (kbd "C-c m c") 'chem-copy)
(global-set-key (kbd "C-c m d") 'chem-describe-options)
```
