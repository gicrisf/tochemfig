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
or the SMILES string at point. In LaTeX and Org buffers the output is wrapped
in `\chemfig{...}` automatically.

```elisp
(add-hook 'latex-mode-hook #'tochemfig-mode)
(add-hook 'org-mode-hook #'tochemfig-mode)
```

The commands are bound under the `C-c m` prefix:

| Key       | Command                  | Description                           |
|-----------|--------------------------|---------------------------------------|
| `C-c m d` | `tochemfig`              | Generate for region / point / prompt  |
| `C-c m c` | `tochemfig-dispatch`     | Transient options menu                |
| `C-c m f` | `tochemfig-input-file`   | Generate from a MOL file              |
| `C-c m w` | `tochemfig-wrap-chemfig` | Wrap output in `\chemfig{}`           |
| `C-c m t` | `tochemfig-terse`        | Compact output                        |
| `C-c m r` | `tochemfig-rotate`       | Rotate / flip / flop                  |
| `C-c m p` | `tochemfig-copy`         | Copy to the kill ring                 |

Other useful mode commands:

| Command                     | Description                                       |
|-----------------------------|---------------------------------------------------|
| `tochemfig-convert-region`  | Replace the region with chemfig for its SMILES    |
| `tochemfig-custom`          | Alias for `tochemfig-dispatch` (options menu)     |

## Options menu

`tochemfig-custom` (or `C-c m c`) opens a transient menu that edits every
option live. While `tochemfig-mode` is active the options are buffer-local.

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
(global-set-key (kbd "C-c m d") 'tochemfig)          ; region / point / prompt
(global-set-key (kbd "C-c m c") 'tochemfig-custom)   ; options menu
(global-set-key (kbd "C-c m f") 'tochemfig-input-file)
(global-set-key (kbd "C-c m w") 'tochemfig-wrap-chemfig)
```
