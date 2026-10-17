# tochemfig Quickstart Guide

Generate LaTeX chemfig code from molecular structures directly in Emacs.

`tochemfig` is a native Emacs Lisp port of the `mol2chemfig` generator: it
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
   break byte-compilation of tochemfig.
3. **LaTeX** with the `chemfig`/TiKZ packages (to render the output).

## Installation

Add to your Emacs config (`~/.emacs` or `~/.emacs.d/init.el`):

```elisp
;; Add paths
(add-to-list 'load-path "/path/to/tochemfig")
(add-to-list 'load-path "/path/to/tochemfig/emacs-indigo")

;; Load the package
(require 'tochemfig)

;; Optional: context-aware minor mode (the transient is autoloaded)
(require 'tochemfig-mode)
```

## Transient menu (primary interface)

`M-x tochemfig-dispatch` (also `tochemfig-custom`) opens the options menu. It
works from any buffer and enables `tochemfig-mode` if needed. If the region or
the text at point looks like a molecule, that is used; otherwise you are
prompted with the current input mode.

| Key   | Action                                             |
|-------|----------------------------------------------------|
| `RET` | Generate and insert                                |
| `C`   | Copy generated code to the kill ring               |
| `F`   | Generate from a MOL file (one-off)                 |
| `A`   | Rotate the next generation only                    |
| `?`   | Show the `C-c m` key bindings                      |

For example, put point on `CCO` and press `RET`, or run `M-x tochemfig` to do
the same without opening the menu. Ethanol is inserted as
`-[:330]-[:30,,,1]OH`.

The old one-shot commands (`tochemfig-input-direct`, `tochemfig-input-file`,
`tochemfig-terse`, `tochemfig-rotate`, `tochemfig-fancy-bonds`,
`tochemfig-wrap-chemfig`, ...) have been removed. Use the corresponding menu
option or `M-x tochemfig` instead.

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

| Key       | Command                      | Description                           |
|-----------|------------------------------|---------------------------------------|
| `C-c m g` | `tochemfig`                  | Generate for region / point / prompt  |
| `C-c m o` | `tochemfig-dispatch`         | Transient options menu                |
| `C-c m c` | `tochemfig-copy`             | Copy to the kill ring                 |
| `C-c m d` | `tochemfig-describe-options` | Describe effective options            |

Other useful commands:

| Command                     | Description                                       |
|-----------------------------|---------------------------------------------------|
| `tochemfig-custom`          | Alias for `tochemfig-dispatch` (options menu)     |
| `tochemfig-describe-options`| Show effective options (buffer-local vs global)   |
| `tochemfig-save-options`    | Save effective options as the global defaults     |
| `tochemfig-reload-options`  | Reload the saved global defaults                  |
| `tochemfig-reset-options`   | Reset buffer options (`C-u`: global defaults)     |

## Options menu

The menu edits every option live and generates with `RET`. Options are
buffer-local, and are only localized the first time you edit them, so global
default changes still reach untouched buffers. The action keys are `RET`
(generate), `C` (copy), `F` (MOL file), `A` (rotate once) and `?` (bindings).

## Evil / Doom

`tochemfig-mode` is plain Emacs; its `C-c m` bindings work in Evil's normal and
visual states and there is no Evil dependency. Doom users usually make the
transient menu the primary entry point and map the top-level commands under
their leader:

```elisp
(map! :leader
      (:prefix ("C" . "tochemfig")
       :desc "Options menu (RET generate, C copy, F file, A rotate)"
       "o" #'tochemfig-dispatch
       :desc "Generate at point/region" "g" #'tochemfig
       :desc "Copy to kill ring" "c" #'tochemfig-copy
       :desc "Describe options" "d" #'tochemfig-describe-options
       :desc "Save options as global defaults" "s" #'tochemfig-save-options
       :desc "Reload saved options" "r" #'tochemfig-reload-options
       :desc "Reset buffer options" "R" #'tochemfig-reset-options))
```

`SPC C o` is the primary entry point. Inside the menu use `RET` to generate,
`C` to copy, `F` for one-off MOL input, `A` to rotate once and `?` for the
bindings. The `g`, `c` and `d` leader keys are shortcuts for the same top-level
commands; `s`, `r` and `R` manage the option defaults. File input and one-off
rotation intentionally have no separate leader bindings: they live in the menu.
The old `C-c m f/t/w/r` shortcuts are gone.

Generating from a visual selection replaces it and exits visual state. The
`*tochemfig options*` buffer is read-only and uses Emacs state, so `q` quits it.
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

Caffeine with wrapper (open `M-x tochemfig-dispatch`, enable `w` if needed,
then `RET` on `CN1C=NC2=C1C(=O)N(C(=O)N2C)C`) produces the fully annotated
structure shown in the README.

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
(global-set-key (kbd "C-c m o") 'tochemfig-dispatch) ; options menu
(global-set-key (kbd "C-c m c") 'tochemfig-copy)
(global-set-key (kbd "C-c m d") 'tochemfig-describe-options)
```
