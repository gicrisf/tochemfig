# Doom/straight local package notes

This repository is normally loaded into Doom Emacs as an **out-of-tree local
package** with a straight recipe like:

```elisp
(package! chem-mode
  :recipe (:local-repo "/home/ann/Projects/tochemfig"
           :branch "dsp-exp"
           :build (:not compile native-compile)))
```

## What straight actually does

For `:local-repo` recipes, straight does **not** clone the package into
`~/.config/emacs/.local/straight/repos/`. It creates a build directory:

```text
~/.config/emacs/.local/straight/build-<emacs-version>/chem-mode/
```

and symlinks the package's `.el` files to the working tree:

```text
build-31.1/chem-mode/chemlisp.el
  -> /home/ann/Projects/tochemfig/chemlisp.el
```

That means edits to existing files are picked up automatically. New files,
autoloads, and recipe metadata are not necessarily picked up until the package
is rebuilt.

The `:branch` value in the recipe is metadata. It does **not** switch the
working tree branch. The code Doom loads is whatever branch/commit
`/home/ann/Projects/tochemfig` is currently on.

## Symptom

Doom keeps loading an old version, or a new file such as `chemlisp.el` is
missing from the build directory.

## Checks

From a shell:

```sh
git -C /home/ann/Projects/tochemfig branch --show-current
ls -l ~/.config/emacs/.local/straight/build-31.1/chem-mode/chemlisp.el
```

From Doom:

```elisp
(locate-library "chemlisp")
(fboundp 'chemlisp-compile)
(fboundp 'chemlisp--drawing-to-mol)
```

`locate-library` should return the path inside the straight build directory,
and the functions should be defined.

## Fix

1. Make sure the repository is on the branch Doom should use:

   ```sh
   git -C /home/ann/Projects/tochemfig switch dsp-exp
   ```

2. Make sure the Doom recipe uses the same branch in
   `/home/ann/jellynix/doom/packages.el`.

3. Run:

   ```sh
   ~/.config/emacs/bin/doom sync
   ```

   Doom will report:

   ```text
   Skipping chem-mode because it is out-of-tree...
   ```

   That is expected. Sync still updates the rest of the config and profile.

4. If new files are still missing, force a rebuild from Doom:

   ```text
   M-x straight-rebuild-package RET chem-mode
   ```

   Rebuild `indigo` the same way if the `emacs-indigo` checkout changed.

5. Restart Emacs. A running Emacs will not reload an already-loaded feature.

## Why `doom sync` alone can be misleading

`doom sync` deliberately skips out-of-tree packages. It can say all packages
are up-to-date while a local package is still using an old build directory.
Always check the symlink in the build directory, not only the sync output.

## Verification without Doom

Load directly from the straight build directory:

```sh
emacs -Q --batch \
  -L ~/.config/emacs/.local/straight/build-31.1/chem-mode \
  -L ~/.config/emacs/.local/straight/build-31.1/indigo \
  --eval '(progn
            (require (quote chemlisp))
            (require (quote chem-exp-svg))
            (princ (format "locate=%s\n" (locate-library "chemlisp")))
            (princ (format "drawing-to-mol=%S\n"
                           (fboundp (quote chemlisp--drawing-to-mol))))
            (princ (format "svg-len=%d\n"
                           (length (chemlisp-render
                                    "(molecule (C (C (O))))" (quote svg))))))'
```

For the repository test suite:

```sh
emacs -Q --batch -L . -L emacs-indigo -L test \
  -l test/chemlisp-test.el -f ert-run-tests-batch-and-exit
```
