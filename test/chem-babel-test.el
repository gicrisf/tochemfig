;;; chem-babel-test.el --- Tests for Org Babel integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for optional Org Babel support.  These tests require Org, but the
;; core package does not.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'ox-html)
(require 'chem-mode)
(require 'chem-babel)

(defconst chem-babel-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the test files.")

(defmacro chem-babel-test--with-org (content &rest body)
  "Run BODY in a temporary Org buffer containing CONTENT."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "chem-babel-" t))
          (file (expand-file-name "test.org" dir)))
     (unwind-protect
         (with-temp-buffer
           (setq buffer-file-name file
                 default-directory (file-name-as-directory dir))
           (org-mode)
           (insert ,content)
           (goto-char (point-min))
           (let ((org-confirm-babel-evaluate nil))
             ,@body))
       (delete-directory dir t))))

(defun chem-babel-test--execute-first ()
  "Execute the first source block in the current buffer."
  (goto-char (point-min))
  (search-forward "#+begin_src")
  (beginning-of-line)
  (org-babel-execute-src-block))

(defun chem-babel-test--link ()
  "Return the first file link in the current buffer, or nil."
  (when (string-match "\\[\\[file:\\([^]]+\\)\\]\\]" (buffer-string))
    (match-string 1 (buffer-string))))

(defun chem-babel-test--path (link)
  "Resolve LINK relative to the current buffer file."
  (expand-file-name link (file-name-directory buffer-file-name)))

(defun chem-babel-test--contents (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun chem-babel-test--link-for (content)
  "Execute CONTENT and return its first result link."
  (chem-babel-test--with-org content
    (chem-babel-test--execute-first)
    (chem-babel-test--link)))

(defun chem-babel-test--svg-source ()
  "Return a simple SVG block using the default SVG renderer."
  "#+begin_src chem-smiles :results file
CCO
#+end_src
")

(ert-deftest chem-babel-languages-defined ()
  "The Org Babel language entry points exist."
  (should (fboundp 'org-babel-execute:chem-smiles))
  (should (fboundp 'org-babel-execute:chem-mol))
  (should (fboundp 'org-babel-execute:chem-rxn)))

(ert-deftest chem-babel-default-header-args ()
  "The default Babel header arguments are installed."
  (should (equal "svg"
                 (cdr (assq :renderer
                            org-babel-default-header-args:chem-smiles))))
  (should (equal "results"
                 (cdr (assq :exports
                            org-babel-default-header-args:chem-smiles))))
  (should-not (assq :results org-babel-default-header-args:chem-smiles))
  (should (equal "svg"
                 (cdr (assq :renderer
                            org-babel-default-header-args:chem-rxn)))))

(ert-deftest chem-babel-smiles-file ()
  "A SMILES block writes SVG and inserts a relative link."
  (chem-babel-test--with-org (chem-babel-test--svg-source)
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link)))
      (should link)
      (should (string-match-p
               "\\`\\.chem-cache/chem-[0-9a-f]+\\.svg\\'" link))
      (should (file-exists-p path))
      (should (string-match-p "<svg " (chem-babel-test--contents path))))))

(ert-deftest chem-babel-deterministic ()
  "Executing the same block twice produces the same file and bytes."
  (chem-babel-test--with-org (chem-babel-test--svg-source)
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link))
           (contents (chem-babel-test--contents path)))
      (chem-babel-test--execute-first)
      (should (equal link (chem-babel-test--link)))
      (should (equal path (chem-babel-test--path (chem-babel-test--link))))
      (should (equal contents
                     (chem-babel-test--contents
                      (chem-babel-test--path (chem-babel-test--link))))))))

(ert-deftest chem-babel-options-change-output ()
  "Header options change the content-addressed output."
  (let ((plain (chem-babel-test--link-for
                "#+begin_src chem-smiles :results file
CCO
#+end_src
"))
        (dark (chem-babel-test--link-for
               "#+begin_src chem-smiles :results file :svg-theme dark
CCO
#+end_src
")))
    (should plain)
    (should dark)
    (should-not (equal plain dark))))

(ert-deftest chem-babel-ambient-options-ignored ()
  "Ambient option values do not affect block output."
  (let* ((content (chem-babel-test--svg-source))
         (plain (chem-babel-test--link-for content))
         (ambient (let ((chem-exp-svg-default-theme 'dark)
                        (chem-default-angle 90.0))
                    (chem-babel-test--link-for content))))
    (should plain)
    (should (equal plain ambient))))

(ert-deftest chem-babel-aromatic-mapping ()
  "The :aromatic header maps to each exporter correctly."
  (let ((svg (chem-babel--overrides '((:aromatic . "alternating")) 'svg))
        (chemfig (chem-babel--overrides '((:aromatic . "circle")) 'chemfig)))
    (should (eq 'alternating (cdr (assoc "svg-aromatic" svg))))
    (should (eq t (cdr (assoc "aromatic-circles" chemfig)))))
  (should-error
   (chem-babel--overrides '((:aromatic . "auto")) 'chemfig)
   :type 'user-error))

(ert-deftest chem-babel-unknown-header ()
  "Unknown header keys signal a user error."
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :results file :svg-bogus 1
CCO
#+end_src
"
    (should-error (chem-babel-test--execute-first) :type 'user-error)))

(ert-deftest chem-babel-invalid-aromatic ()
  "Invalid :aromatic values signal a user error."
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :results file :aromatic alternate
CCO
#+end_src
"
    (should-error (chem-babel-test--execute-first) :type 'user-error)))

(ert-deftest chem-babel-invalid-renderer ()
  "The deferred :renderer auto value is rejected."
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :results file :renderer auto
CCO
#+end_src
"
    (should-error (chem-babel-test--execute-first) :type 'user-error)))

(ert-deftest chem-babel-result-types ()
  "SVG requires file results and chemfig requires source results."
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :results value
CCO
#+end_src
"
    (should-error (chem-babel-test--execute-first) :type 'user-error))
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :renderer chemfig :results file
CCO
#+end_src
"
    (should-error (chem-babel-test--execute-first) :type 'user-error)))

(ert-deftest chem-babel-chemfig-source ()
  "The chemfig renderer returns source text."
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :renderer chemfig :results value
CCO
#+end_src
"
    (chem-babel-test--execute-first)
    (should (string-match-p "chemfig" (buffer-string)))
    (should-not (chem-babel-test--link))))

(ert-deftest chem-babel-mol-block ()
  "A chem-mol block renders the molecule."
  (let ((mol (chem-babel-test--contents
              (expand-file-name "corpus/ethanol.mol"
                                chem-babel-test--root))))
    (chem-babel-test--with-org
        (concat "#+begin_src chem-mol :results file\n"
                mol
                "\n#+end_src\n")
      (chem-babel-test--execute-first)
      (let* ((link (chem-babel-test--link))
             (path (chem-babel-test--path link)))
        (should link)
        (should (file-exists-p path))
        (should (string-match-p "<svg " (chem-babel-test--contents path)))))))

(ert-deftest chem-babel-rxn-svg-file ()
  "A chem-rxn block writes SVG and inserts a relative link."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :results file
CCO>>CC=O
#+end_src
"
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link)))
      (should link)
      (should (file-exists-p path))
      (should (string-match-p "<svg " (chem-babel-test--contents path)))
      (should (string-match-p "data-reaction-part=\"arrow\""
                              (chem-babel-test--contents path))))))

(ert-deftest chem-babel-rxn-svg-options ()
  "SVG reaction options reach the generated document."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :results file :reaction-arrow reverse :reaction-above heat
CCO>>CC=O
#+end_src
"
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link))
           (contents (chem-babel-test--contents path)))
      (should (string-match-p "data-reaction-part=\"arrow\"" contents))
      (should (string-match-p "heat" contents))
      (should (string-match-p "<polygon " contents)))))

(ert-deftest chem-babel-rxn-deterministic ()
  "Executing the same reaction block twice produces identical bytes."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :results file
CCO>>CC=O
#+end_src
"
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link))
           (contents (chem-babel-test--contents path)))
      (chem-babel-test--execute-first)
      (should (equal link (chem-babel-test--link)))
      (should (equal contents
                     (chem-babel-test--contents
                      (chem-babel-test--path (chem-babel-test--link))))))))

(ert-deftest chem-babel-rxn-clean-cache ()
  "Cache cleanup handles reaction-generated files."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :results file
CCO>>CC=O
#+end_src
"
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link))
           (extra (expand-file-name "chem-deadbeef.svg"
                                    (chem-babel--cache-directory))))
      (with-temp-file extra (insert "unreferenced"))
      (chem-org-clean-cache)
      (should (file-exists-p path))
      (should-not (file-exists-p extra)))))

(ert-deftest chem-babel-rxn-chemfig ()
  "A chem-rxn block can return chemfig source."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :renderer chemfig :results value
CCO>>CC=O
#+end_src
"
    (chem-babel-test--execute-first)
    (should (string-match-p (regexp-quote "\\schemestart") (buffer-string)))
    (should (string-match-p (regexp-quote "\\arrow") (buffer-string)))
    (should-not (chem-babel-test--link))))

(ert-deftest chem-babel-rxn-chemfig-options ()
  "Reaction header options reach chemfig output."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :renderer chemfig :results value :reaction-arrow reverse :reaction-above heat
CCO>>CC=O
#+end_src
"
    (chem-babel-test--execute-first)
    (should (string-match-p (regexp-quote "\\arrow{<-[heat]}")
                            (buffer-string)))
    (should (string-match-p "heat" (buffer-string)))))

(ert-deftest chem-babel-rxn-options ()
  "Reaction options are accepted by the correct renderer."
  (let ((svg (chem-babel--overrides
              '((:reaction-arrow . "reverse")
                (:reaction-above . "heat")
                (:reaction-spacing . 0.5))
              'svg t))
        (chemfig (chem-babel--overrides
                  '((:reaction-wrap . "yes")
                    (:reaction-arrow-chemfig . "->"))
                  'chemfig t)))
    (should (eq 'reverse
                (cdr (assoc "reaction-arrow" svg))))
    (should (equal "heat"
                   (cdr (assoc "reaction-above" svg))))
    (should (= 0.5
               (cdr (assoc "reaction-spacing" svg))))
    (should (eq t (cdr (assoc "reaction-wrap" chemfig))))
    (should (equal "->"
                   (cdr (assoc "reaction-arrow-chemfig" chemfig))))
    (should-error
     (chem-babel--overrides '((:reaction-spacing . 1)) 'chemfig t)
     :type 'user-error)
    (should-error
     (chem-babel--overrides '((:reaction-wrap . "yes")) 'svg t)
     :type 'user-error)
    (should-error
     (chem-babel--overrides '((:wrap-chemfig . "yes")) 'chemfig t)
     :type 'user-error)
    (should-error
     (chem-babel--overrides '((:reaction-above . "heat")) 'svg)
     :type 'user-error)))

(ert-deftest chem-babel-rxn-regenerate-buffer ()
  "`chem-org-regenerate-buffer' executes chem-rxn blocks."
  (chem-babel-test--with-org
      "#+begin_src chem-rxn :results file
CCO>>CC=O
#+end_src
"
    (chem-org-regenerate-buffer)
    (should (chem-babel-test--link))))

(ert-deftest chem-babel-from-mol-string ()
  "`chem-from-mol-string' builds a drawing from MOL text."
  (let* ((mol (chem-babel-test--contents
               (expand-file-name "corpus/ethanol.mol"
                                 chem-babel-test--root)))
         (options (chem--build-options nil 'svg t))
         (drawing (chem-from-mol-string mol options)))
    (should (= 3 (hash-table-count (chem-drawing-atoms drawing))))))

(ert-deftest chem-babel-regenerate-buffer ()
  "`chem-org-regenerate-buffer' executes every chem block."
  (chem-babel-test--with-org
      "#+begin_src chem-smiles :results file
CCO
#+end_src

#+begin_src chem-smiles :results file
c1ccccc1
#+end_src
"
    (chem-org-regenerate-buffer)
    (should (= 2 (let ((count 0))
                   (save-excursion
                     (goto-char (point-min))
                     (while (re-search-forward "\\[\\[file:" nil t)
                       (setq count (1+ count))))
                   count)))))

(ert-deftest chem-babel-clean-cache ()
  "Cache cleanup removes only unreferenced generated files."
  (chem-babel-test--with-org (chem-babel-test--svg-source)
    (chem-babel-test--execute-first)
    (let* ((link (chem-babel-test--link))
           (path (chem-babel-test--path link))
           (extra (expand-file-name "chem-deadbeef.svg"
                                    (chem-babel--cache-directory))))
      (with-temp-file extra (insert "unreferenced"))
      (chem-org-clean-cache)
      (should (file-exists-p path))
      (should-not (file-exists-p extra)))))

(ert-deftest chem-babel-html-export ()
  "Org HTML export includes the generated SVG image."
  (chem-babel-test--with-org (chem-babel-test--svg-source)
    (chem-babel-test--execute-first)
    (let ((org-export-babel-evaluate nil)
          (org-html-inline-images t)
          (html (org-export-as 'html nil nil t)))
      (should (string-match-p "<img" html))
      (should (string-match-p "\\.svg" html)))))

(provide 'chem-babel-test)
;;; chem-babel-test.el ends here
