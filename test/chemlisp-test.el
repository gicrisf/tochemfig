;;; chemlisp-test.el --- Tests for CheMLisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Experimental CheMLisp reader, expansion, compiler, and exporter tests.
;; Compilation uses Indigo for layout and hydrogen perception.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chemlisp)
(require 'chem-exp-svg)
(require 'chem-exp-chemfig)

(defun chemlisp-test--atoms (drawing)
  "Return DRAWING's atoms ordered by index."
  (let (atoms)
    (maphash (lambda (idx atom) (push (cons idx atom) atoms))
             (chem-drawing-atoms drawing))
    (mapcar #'cdr (sort atoms (lambda (a b) (< (car a) (car b)))))))

(defun chemlisp-test--edges (drawing)
  "Return DRAWING's edges ordered by id."
  (chem-edges drawing))

(defun chemlisp-test--compile (source &optional options)
  "Compile SOURCE with OPTIONS."
  (chemlisp-compile source options))

(ert-deftest chemlisp-read-basic ()
  "The reader handles multiple forms and comments."
  (should (equal '((a . 0) (b . 1))
                 (chemlisp-read "a b")))
  (should (equal '((a . 0))
                 (chemlisp-read "a ; comment\n"))))

(ert-deftest chemlisp-read-rejects-read-eval ()
  "Read-time evaluation and circular structures are rejected."
  (should-error (chemlisp-read "#.(+ 1 2)"))
  (should-error (chemlisp-read "#1=(a . #1#)")))

(ert-deftest chemlisp-ethanol ()
  "Ethanol compiles to three atoms and two single bonds."
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C (O))))"))
         (atoms (chemlisp-test--atoms drawing))
         (edges (chemlisp-test--edges drawing)))
    (should (= 3 (length atoms)))
    (should (= 2 (length edges)))
    (should (equal '("C" "C" "O") (mapcar #'chem-atom-element atoms)))
    (should (equal '(3 2 1) (mapcar #'chem-atom-hydrogens atoms)))
    (should (cl-every (lambda (edge) (eq :single (chem-edge-order edge)))
                      edges))))

(ert-deftest chemlisp-acetic-acid ()
  "Acetic acid has a double and a single bond."
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C (=O) (O))))"))
         (edges (chemlisp-test--edges drawing)))
    (should (= 4 (hash-table-count (chem-drawing-atoms drawing))))
    (should (= 3 (length edges)))
    (should (equal '(:single :double :single)
                   (mapcar #'chem-edge-order edges)))))

(ert-deftest chemlisp-benzene-ring ()
  "Benzene compiles to an aromatic six-membered ring."
  (let* ((source "(molecule (c (c (c (c (c (c (-> ^5))))))))")
         (drawing (chemlisp-test--compile source))
         (rings (chem-drawing-rings drawing)))
    (should (= 6 (hash-table-count (chem-drawing-atoms drawing))))
    (should (= 6 (hash-table-count (chem-drawing-edges drawing))))
    (should (= 1 (length rings)))
    (should (chem-ring-aromatic-p (car rings)))
    (should (= 6 (length (chem-ring-atoms (car rings)))))
    (should (string-match-p "\\\\mcfcringle"
                            (chemlisp-render source 'chemfig
                                             (list :aromatic-circles t))))
    (should (string-match-p "<circle"
                            (chemlisp-render source 'svg
                                             (list :svg-aromatic 'circle))))))

(ert-deftest chemlisp-auto-layout-bond-lengths ()
  "Indigo layout is normalized to a unit bond length."
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C (O))))"))
         (lengths (mapcar (lambda (edge)
                            (chem-edge-length drawing edge))
                          (chem-edges drawing))))
    (should (cl-every (lambda (length)
                        (< (abs (- length 1.0)) 1e-3))
                      lengths))))

(ert-deftest chemlisp-auto-layout-ring ()
  "A ring without authored angles is still a closed regular ring."
  (let* ((source "(molecule (c (c (c (c (c (c (-> ^5))))))))")
         (drawing (chemlisp-test--compile source))
         (ring (car (chem-drawing-rings drawing)))
         (lengths (mapcar (lambda (edge-id)
                            (chem-edge-length drawing
                                              (chem-edge-at drawing edge-id)))
                          (chem-ring-bonds ring))))
    (should (= 6 (length (chem-ring-atoms ring))))
    (should (chem-ring-aromatic-p ring))
    (should (cl-every (lambda (length)
                        (< (abs (- length 1.0)) 1e-3))
                      lengths))
    (should (< (- (apply #'max (chem-ring-vertex-radii drawing ring))
                  (apply #'min (chem-ring-vertex-radii drawing ring)))
               1e-3))))

(ert-deftest chemlisp-ancestor-closures ()
  "Ancestor-relative closures parse and resolve."
  (let* ((triangle (chemlisp-test--compile
                    "(molecule (c (c (c (-> ^^)))))"))
         (benzene (chemlisp-test--compile
                   "(molecule (c (c (c (c (c (c (-> ^^^^^))))))))")))
    (should (= 1 (length (chem-drawing-rings triangle))))
    (should (= 1 (length (chem-drawing-rings benzene))))))

(ert-deftest chemlisp-naphthalene ()
  "Naphthalene can be authored as one tree without IDs."
  (let* ((source "(molecule (c (c (c (c (c (c (-> ^5))))) (c (c (c (c (-> ^5))))))))")
         (drawing (chemlisp-test--compile source))
         (rings (chem-drawing-rings drawing)))
    (should (= 10 (hash-table-count (chem-drawing-atoms drawing))))
    (should (= 11 (hash-table-count (chem-drawing-edges drawing))))
    (should (= 2 (length rings)))
    (should (cl-every #'chem-ring-aromatic-p rings))))

(ert-deftest chemlisp-naphthalene-shared-ids ()
  "Shared atoms can be named explicitly when fusing rings."
  (let* ((source "(molecule (c :id a (c :id b (c (c (c (c (-> a))))) (c (c (c (c (-> a))))))))")
         (drawing (chemlisp-test--compile source))
         (rings (chem-drawing-rings drawing)))
    (should (= 10 (hash-table-count (chem-drawing-atoms drawing))))
    (should (= 2 (length rings)))
    (should (cl-every #'chem-ring-aromatic-p rings))))

(ert-deftest chemlisp-ancestor-errors ()
  "Invalid ancestor references and fragment crossings are rejected."
  (should-error (chemlisp-test--compile "(molecule (c (c (-> ^2))))"))
  (should-error (chemlisp-test--compile "(molecule (c) (c (c (c (-> ^5)))))"))
  (should-error (chemlisp-test--compile "(molecule (c (c (-> ^0)))))"))
  (should-error (chemlisp-test--compile "(molecule (c (c (-> ^foo)))))"))
  (should-error (chemlisp-test--compile "(molecule (c (c (-> ^)))))"))
  (should-error (chemlisp-test--compile "(molecule (c :id ^foo))")))

(ert-deftest chemlisp-close-removed ()
  "The old `close` form is rejected in favor of `->`."
  (should-error (chemlisp-test--compile "(molecule (c (close 1)))")))

(ert-deftest chemlisp-aromatic-nitrogen ()
  "Indigo hydrogens distinguish pyridine from pyrrole."
  (let* ((pyridine "(molecule (n (c (c (c (c (c (-> ^5))))))))")
         (pyrrole "(molecule (n :h 1 (c (c (c (c (c (-> ^5))))))))")
         (pyridine-atom (car (chemlisp-test--atoms
                              (chemlisp-test--compile pyridine))))
         (pyrrole-atom (car (chemlisp-test--atoms
                             (chemlisp-test--compile pyrrole)))))
    (should (= 0 (chem-atom-hydrogens pyridine-atom)))
    (should (= 1 (chem-atom-hydrogens pyrrole-atom)))))

(ert-deftest chemlisp-explicit-length ()
  "Explicit :length overrides the normalized layout length."
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C :length 2)))"))
         (edge (car (chem-edges drawing))))
    (should (< (abs (- (chem-edge-length drawing edge) 2.0)) 1e-6))))

(ert-deftest chemlisp-root-at ()
  "A fragment root :at places the root at the requested coordinates."
  (let* ((drawing (chemlisp-test--compile
                   "(molecule (Na :charge 1 :at (2 3)) (Cl :charge -1))"))
         (na (gethash 0 (chem-drawing-atoms drawing))))
    (should (= 2.0 (chem-atom-x na)))
    (should (= 3.0 (chem-atom-y na)))))

(ert-deftest chemlisp-rejects-ring-geometry ()
  "Explicit geometry on a ring bond is rejected."
  (should-error
   (chemlisp-test--compile
    "(molecule (c (c :angle 0 (c (-> ^2)))))"))
  (should-error
   (chemlisp-test--compile
    "(molecule (c (c :length 1.5 (c (-> ^2)))))")))

(ert-deftest chemlisp-hydrogen-validation ()
  "Impossible :h values are rejected."
  (should-error (chemlisp-test--compile "(molecule (C :h 2))"))
  (should-error (chemlisp-test--compile "(molecule (H :h 1))")))

(ert-deftest chemlisp-value-definition ()
  "A value definition is reusable and appends use-site children."
  (let* ((source "(chem-def hydroxyl () (O)) (molecule (C (hydroxyl (C))))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 3 (length atoms)))
    (should (equal '("C" "O" "C") (mapcar #'chem-atom-element atoms)))))

(ert-deftest chemlisp-value-use-attrs-and-bond ()
  "Use-site attributes and bond markers override the definition."
  (let* ((source "(chem-def hydroxyl () (O)) (molecule (C (=hydroxyl)))")
         (drawing (chemlisp-test--compile source))
         (edges (chemlisp-test--edges drawing))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 2 (length atoms)))
    (should (= 1 (length edges)))
    (should (eq :double (chem-edge-order (car edges))))
    (should (= 0 (chem-atom-charge (nth 1 atoms)))))
  (let* ((source "(chem-def hydroxyl () (O)) (molecule (C (hydroxyl :charge -1)))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 2 (length atoms)))
    (should (= -1 (chem-atom-charge (nth 1 atoms))))))

(ert-deftest chemlisp-template ()
  "A template substitutes positional and keyword holes."
  (let* ((source "(chem-def pair (x y) (C (x) (y))) (molecule (pair (O) (N)))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 3 (length atoms)))
    (should (equal '("C" "O" "N") (mapcar #'chem-atom-element atoms))))
  (let* ((source "(chem-def pair (x y) (C (x) (y))) (molecule (pair :y (O) :x (N)))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (equal '("C" "N" "O") (mapcar #'chem-atom-element atoms))))
  (let* ((source "(chem-def pair (x y) (C (x) (y))) (molecule (pair nil (O)))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 2 (length atoms)))
    (should (equal '("C" "O") (mapcar #'chem-atom-element atoms))))
  (let* ((source "(chem-def pair (x y) (C (x) (y))) (molecule (pair :x (O) :y (N) :at (2 3)))")
         (drawing (chemlisp-test--compile source))
         (atom (car (chemlisp-test--atoms drawing))))
    (should (= 2 (chem-atom-x atom)))
    (should (= 3 (chem-atom-y atom)))))

(ert-deftest chemlisp-template-use-site-angle ()
  "A template use can set its incoming edge geometry."
  (let* ((source "(chem-def pair (x y) (C (x) (y))) (molecule (C (pair :x (O) :angle 90)))")
         (drawing (chemlisp-test--compile source)))
    (should (= 3 (hash-table-count (chem-drawing-atoms drawing))))
    (should (string-match-p "\\[:90"
                            (chemlisp-render source 'chemfig)))))

(ert-deftest chemlisp-let-parallel-and-sequential ()
  "chem-let is parallel and chem-let* is sequential."
  (let* ((source "(chem-let ((x (C)) (y (O))) (molecule (C (x) (y))))")
         (drawing (chemlisp-test--compile source)))
    (should (= 3 (hash-table-count (chem-drawing-atoms drawing)))))
  (let* ((source "(chem-let* ((x (C)) (y (C (x)))) (molecule (y)))")
         (drawing (chemlisp-test--compile source)))
    (should (= 2 (hash-table-count (chem-drawing-atoms drawing)))))
  (should-error
   (chemlisp-test--compile "(chem-let ((x (y)) (y (C))) (molecule (x)))")))

(ert-deftest chemlisp-let-as-subtree ()
  "chem-let can appear in a child position and expand to one node."
  (let* ((source "(molecule (C (chem-let ((x (O))) (x))))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 2 (length atoms)))
    (should (equal '("C" "O") (mapcar #'chem-atom-element atoms)))))

(ert-deftest chemlisp-let-shadows-definition ()
  "A local binding shadows a document definition."
  (let* ((source "(chem-def x () (O)) (chem-let ((x (C))) (molecule (x)))")
         (drawing (chemlisp-test--compile source))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 1 (length atoms)))
    (should (equal "C" (chem-atom-element (car atoms))))))

(ert-deftest chemlisp-fresh-ids-per-use ()
  "Reusing a definition with an internal closure creates independent rings."
  (let* ((source "(chem-def ringlet () (c (c (c (-> ^2))))) (molecule (C (ringlet) (ringlet)))")
         (drawing (chemlisp-test--compile source)))
    (should (= 7 (hash-table-count (chem-drawing-atoms drawing))))
    (should (= 8 (hash-table-count (chem-drawing-edges drawing))))
    (should (= 2 (length (chem-drawing-rings drawing))))))

(ert-deftest chemlisp-definitions-are-order-independent ()
  "A definition may be used before it is declared."
  (let* ((source "(molecule (C (C (hydroxyl)))) (chem-def hydroxyl () (O))")
         (drawing (chemlisp-test--compile source)))
    (should (= 3 (hash-table-count (chem-drawing-atoms drawing))))))

(ert-deftest chemlisp-numeric-closure ()
  "Numeric closures use one-based pre-order indices."
  (let* ((source "(molecule (c (c (c (-> 1)))))")
         (drawing (chemlisp-test--compile source)))
    (should (= 1 (length (chem-drawing-rings drawing))))))

(ert-deftest chemlisp-fragments ()
  "Multiple roots are connected with an invisible link edge."
  (let* ((source "(molecule (Na :charge 1) (Cl :charge -1))")
         (drawing (chemlisp-test--compile source))
         (edges (chemlisp-test--edges drawing)))
    (should (= 2 (hash-table-count (chem-drawing-atoms drawing))))
    (should (= 1 (length edges)))
    (should (chem-edge-link-p (car edges)))))

(ert-deftest chemlisp-hydrogens-and-ions ()
  "Hydrogens are inferred, overridden, and never added to H itself."
  (let* ((drawing (chemlisp-test--compile "(molecule (N :charge 1) (Cl :charge -1) (H))"))
         (atoms (chemlisp-test--atoms drawing)))
    (should (equal '(4 0 0) (mapcar #'chem-atom-hydrogens atoms))))
  (let* ((drawing (chemlisp-test--compile "(molecule (C (N :h 1 (C)) (C)))"))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 1 (chem-atom-hydrogens (nth 1 atoms))))))

(ert-deftest chemlisp-view-and-options ()
  "View and common options transform coordinates."
  (let* ((plain (chemlisp-test--compile "(molecule (C (C)))"))
         (scaled (chemlisp-test--compile "(molecule :scale 2 (C (C)))")))
    (should (= 1.0 (chem-edge-length plain (car (chem-edges plain)))))
    (should (= 2.0 (chem-edge-length scaled (car (chem-edges scaled))))))
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C)))" (list :flip t)))
         (atoms (chemlisp-test--atoms drawing)))
    (should (= 0.0 (chem-atom-x (car atoms))))
    (should (< (chem-atom-x (nth 1 atoms)) 0)))
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C :turn 90)))"))
         (atoms (chemlisp-test--atoms drawing)))
    (should (< (abs (chem-atom-x (nth 1 atoms))) 1e-9))
    (should (> (chem-atom-y (nth 1 atoms)) 0))))

(ert-deftest chemlisp-show-overrides ()
  "Per-atom :show overrides the normal carbon label."
  (let* ((drawing (chemlisp-test--compile "(molecule (C :show :always))"))
         (atom (car (chemlisp-test--atoms drawing))))
    (should (equal "C" (chem-atom-label atom))))
  (let* ((drawing (chemlisp-test--compile "(molecule (O :show :never))"))
         (atom (car (chemlisp-test--atoms drawing))))
    (should-not (chem-atom-label atom))))

(ert-deftest chemlisp-markers ()
  "The markers option populates atom and edge markers."
  (let* ((drawing (chemlisp-test--compile "(molecule (C (C)))"
                                          (list :markers "a")))
         (atoms (chemlisp-test--atoms drawing))
         (edge (car (chemlisp-test--edges drawing))))
    (should (equal "a1" (chem-atom-marker (car atoms))))
    (should (equal "a2" (chem-atom-marker (nth 1 atoms))))
    (should (equal "a1-2" (chem-edge-marker edge)))))

(ert-deftest chemlisp-chemfig-does-not-emit-definesubmol ()
  "chem-def is source reuse, not a chemfig submolecule."
  (let ((output (chemlisp-render
                 "(chem-def hydroxyl () (O)) (molecule (C (hydroxyl)))"
                 'chemfig)))
    (should-not (string-match-p "\\\\definesubmol" output)))
  (let ((output (chemlisp-render
                 "(chem-def hydroxyl () (O)) (molecule (C (hydroxyl)))"
                 'chemfig (list :submol-name "A"))))
    (should (string-match-p "\\\\definesubmol{A}" output))))

(ert-deftest chemlisp-svg-deterministic ()
  "The same source produces the same SVG bytes."
  (let* ((source "(molecule (C (C (=O) (O))))")
         (first (chemlisp-render source 'svg))
         (second (chemlisp-render source 'svg)))
    (should (equal first second))
    (should (string-prefix-p "<svg" first))))

(ert-deftest chemlisp-errors ()
  "Invalid documents signal user errors."
  (should-error (chemlisp-test--compile "(molecule (Xx))"))
  (should-error (chemlisp-test--compile "(molecule (C (-> zz)))"))
  (should-error (chemlisp-test--compile "(molecule (C (-> 5)))"))
  (should-error (chemlisp-test--compile "(molecule (C :exit t (C :exit t)))"))
  (should-error (chemlisp-test--compile "(molecule (C :id a) (C :id a))"))
  (should-error (chemlisp-test--compile
                 "(molecule (C (C) (C (-> 2))))"))
  (should-error (chemlisp-test--compile
                 "(chem-def x () (O)) (chem-def x () (C)) (molecule (x))"))
  (should-error (chemlisp-test--compile
                 "(chem-def self () (self)) (molecule (self))"))
  (should-error (chemlisp-test--compile
                 "(chem-def a () (b)) (chem-def b () (a)) (molecule (a))"))
  (should-error (chemlisp-test--compile
                 "(chem-def pair (x y) (C (x))) (molecule (pair (O) (N)))"))
  (should-error (chemlisp-test--compile
                 "(chem-def pair (x y) (C (x) (x))) (molecule (pair (O) (N)))"))
  (should-error (chemlisp-test--compile
                 "(chem-def bad () (molecule (C))) (molecule (C))")))

(provide 'chemlisp-test)
;;; chemlisp-test.el ends here
