# tochemfig Quickstart Guide

Generate LaTeX chemfig code from molecular structures directly in Emacs.

`tochemfig` is a native Emacs Lisp port of the `mol2chemfig` generator: it
uses [emacs-indigo](https://github.com/gicrisf/emacs-indigo) to read and lay
out molecules, and produces chemfig code with no Python or shell dependency.

## Prerequisites

1. **Emacs 28.1+**
2. **emacs-indigo**: build the C module first:
   ```bash
   cd emacs-indigo
   ./install.sh
   ```
3. **LaTeX** with the `chemfig`/TiKZ packages (to render the output).

## Installation

Add to your Emacs config (`~/.emacs` or `~/.emacs.d/init.el`):

```elisp
;; Add paths
(add-to-list 'load-path "/path/to/tochemfig")
(add-to-list 'load-path "/path/to/tochemfig/emacs-indigo")

;; Load the package
(require 'tochemfig)

;; Optional: context-aware minor mode and options menu
(require 'tochemfig-mode)
```

## Basic Usage

### From SMILES strings

```
M-x tochemfig-input-direct RET CCO RET
```

Inserts `-[:330]-[:30,,,1]OH` (ethanol).

### From MOL files

```
M-x tochemfig-input-file RET /path/to/molecule.mol RET
```

### Quick generation with configured defaults

```
M-x tochemfig-default RET <SMILES or file> RET
```

## Context-aware mode

Enable `tochemfig-mode` in a buffer to generate chemfig code from the region
or the SMILES string at point. In the wrapping contexts (LaTeX, Org and
Markdown by default) the output is wrapped in `\chemfig{...}` automatically;
wrapping is decided per command, so LaTeX source blocks inside Org work too.

```elisp
(add-hook 'latex-mode-hook #'tochemfig-mode)
(add-hook 'org-mode-hook #'tochemfig-mode)
```

To enable it across all the wrapping contexts at once, use the global mode:

```elisp
(tochemfig-global-mode 1)
```

It only enables the mode in wrapping contexts; set
`tochemfig-global-mode-all-buffers` to `t` to enable it everywhere, and
`tochemfig-mode-wrap-modes` to change which major modes count.

The commands are bound under the `C-c m` prefix:

| Key       | Command                              | Description                           |
|-----------|--------------------------------------|---------------------------------------|
| `C-c m g` | `tochemfig`                          | Generate for region / point / prompt  |
| `C-c m o` | `tochemfig-dispatch`                 | Transient options menu                |
| `C-c m c` | `tochemfig-copy`                     | Copy to the kill ring                 |
| `C-c m f` | `tochemfig-input-file`               | Generate from a MOL file              |
| `C-c m d` | `tochemfig-describe-options`         | Describe effective options            |
| `C-c m t` | `tochemfig-mode-toggle-terse`        | Toggle terse output (buffer-local)    |
| `C-c m w` | `tochemfig-mode-toggle-wrap-chemfig` | Toggle `\chemfig{}` wrapping          |
| `C-c m r` | `tochemfig-mode-rotate`              | Rotate this generation by an angle    |

Other useful mode commands:

| Command                     | Description                                       |
|-----------------------------|---------------------------------------------------|
| `tochemfig-convert-region`  | Replace the region with chemfig for its SMILES    |
| `tochemfig-custom`          | Alias for `tochemfig-dispatch` (options menu)     |
| `tochemfig-describe-options`| Show effective options (buffer-local vs global)   |
| `tochemfig-save-options`    | Save effective options as the global defaults     |
| `tochemfig-reload-options`  | Reload the saved global defaults                  |
| `tochemfig-reset-options`   | Reset buffer options (`C-u`: global defaults)     |

## Options menu

`tochemfig-custom` (or `C-c m o`) opens a transient menu that edits every
option live. While `tochemfig-mode` is active the options are buffer-local.

## Evil / Doom

`tochemfig-mode` is plain Emacs; its `C-c m` bindings work in Evil's normal and
visual states and there is no Evil dependency. Doom users usually map the
commands under their leader:

```elisp
(map! :leader
      (:prefix ("C" . "tochemfig")
       "g" #'tochemfig
       "o" #'tochemfig-dispatch
       "c" #'tochemfig-copy
       "f" #'tochemfig-input-file
       "d" #'tochemfig-describe-options
       "t" #'tochemfig-mode-toggle-terse
       "w" #'tochemfig-mode-toggle-wrap-chemfig
       "r" #'tochemfig-mode-rotate))
```

Generating from a visual selection replaces it and exits visual state. The
`*tochemfig options*` buffer is read-only and uses Emacs state, so `q` quits it.
Doom's menu bar is off, so use `C-c m o` / which-key / your leader mapping.

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

Caffeine with wrapper (`M-x tochemfig-wrap-chemfig RET CN1C=NC2=C1C(=O)N(C(=O)N2C)C RET`)
produces the fully annotated structure shown in the README.

## All options

| Variable                                   | Default     | Description                              |
|--------------------------------------------|-------------|------------------------------------------|
| `tochemfig-default-input`                  | `direct`    | Input mode: `direct` or `file`           |
| `tochemfig-default-terse`                  | `nil`       | Compact output                           |
| `tochemfig-default-strict`                 | `t`         | Abide by Indigo's structure validation   |
| `tochemfig-default-indent`                 | `4`         | Branch indentation                       |
| `tochemfig-default-recalculate-coordinates`| `nil`       | Discard and recalculate coordinates      |
| `tochemfig-default-angle`                  | `0.0`       | Rotation angle (degrees)                 |
| `tochemfig-default-relative-angles`        | `nil`       | Relative bond angles                     |
| `tochemfig-default-flip`                   | `nil`       | Mirror horizontally                      |
| `tochemfig-default-flop`                   | `nil`       | Mirror vertically                        |
| `tochemfig-default-show-carbons`           | `nil`       | Show C labels                            |
| `tochemfig-default-show-methyls`           | `nil`       | Show CH3 labels                          |
| `tochemfig-default-hydrogens`              | `keep`      | H handling: `keep`/`add`/`delete`        |
| `tochemfig-default-aromatic-circles`       | `nil`       | Draw circles in aromatic rings           |
| `tochemfig-default-fancy-bonds`            | `nil`       | Enhanced double/triple bond style        |
| `tochemfig-default-atom-numbers`           | `nil`       | Show atom indices                        |
| `tochemfig-default-bond-scale`             | `normalize` | Bond scaling mode: `keep`/`scale`/`normalize` |
| `tochemfig-default-bond-stretch`           | `1.0`       | Bond length factor / target average      |
| `tochemfig-default-markers`                | `nil`       | Atom/bond marker prefix                  |
| `tochemfig-default-cross-bond`             | `nil`       | Bonds to draw crossing over others       |
| `tochemfig-default-legacy-lewis`           | `nil`       | Use legacy `\lewis` (ChemFig < 1.6)      |
| `tochemfig-default-wrap-chemfig`           | `nil`       | Wrap in `\chemfig{...}`                  |
| `tochemfig-default-submol-name`            | `nil`       | Wrap in `\definesubmol{NAME}{...}`       |
| `tochemfig-default-entry-atom`             | `nil`       | First atom of a sub-molecule             |
| `tochemfig-default-exit-atom`              | `nil`       | Last atom of a sub-molecule              |

Set your preferred defaults in your config:

```elisp
(setq tochemfig-default-input 'direct)
(setq tochemfig-default-relative-angles t)
(setq tochemfig-default-fancy-bonds t)
(setq tochemfig-default-wrap-chemfig t)
(setq tochemfig-default-angle -30.0)
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
(global-set-key (kbd "C-c m g") 'tochemfig)          ; region / point / prompt
(global-set-key (kbd "C-c m o") 'tochemfig-custom)   ; options menu
(global-set-key (kbd "C-c m c") 'tochemfig-copy)
(global-set-key (kbd "C-c m f") 'tochemfig-input-file)
(global-set-key (kbd "C-c m d") 'tochemfig-describe-options)
```
