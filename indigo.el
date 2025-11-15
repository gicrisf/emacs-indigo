;;; indigo.el --- Emacs interface to the Indigo cheminformatics library -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Giovanni Crisalfi
;; Version: 0.8.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: data tools extensions
;; URL: https://github.com/gicrisf/emacs-indigo

;;; Commentary:

;; This package provides Emacs Lisp bindings for the Indigo toolkit,
;; enabling molecular structure manipulation, chemical file I/O,
;; and cheminformatics operations from within Emacs.
;;
;; The package provides both low-level stateful functions that work
;; directly with Indigo object handles, and high-level abstractions
;; including:
;; - `indigo-let*` macro for automatic resource management with sequential binding
;; - Helper functions for functional-style iterator processing
;; - Mapping functions for atoms, bonds, and other molecular features

;;; Code:

(require 'cl-lib)

;;; Forward declarations for C module functions

;; Core functions
(declare-function indigo-free "indigo-module")
(declare-function indigo-clone "indigo-module")
(declare-function indigo-next "indigo-module")

;; Loading functions
(declare-function indigo-load-molecule-from-string "indigo-module")
(declare-function indigo-load-molecule-from-file "indigo-module")
(declare-function indigo-load-query-molecule-from-string "indigo-module")
(declare-function indigo-load-query-molecule-from-file "indigo-module")
(declare-function indigo-load-smarts-from-string "indigo-module")
(declare-function indigo-load-smarts-from-file "indigo-module")
(declare-function indigo-load-reaction-from-string "indigo-module")
(declare-function indigo-load-reaction-from-file "indigo-module")

;; Iterator functions
(declare-function indigo-iterate-atoms "indigo-module")
(declare-function indigo-iterate-bonds "indigo-module")
(declare-function indigo-iterate-neighbors "indigo-module")
(declare-function indigo-iterate-components "indigo-module")
(declare-function indigo-iterate-sssr "indigo-module")
(declare-function indigo-iterate-rings "indigo-module")
(declare-function indigo-iterate-subtrees "indigo-module")
(declare-function indigo-iterate-stereocenters "indigo-module")
(declare-function indigo-iterate-reactants "indigo-module")
(declare-function indigo-iterate-products "indigo-module")

;; Property functions
(declare-function indigo-fingerprint "indigo-module")
(declare-function indigo-substructure-matcher "indigo-module")
(declare-function indigo-create-array "indigo-module")
(declare-function indigo-create-file-writer "indigo-module")
(declare-function indigo-create-buffer-writer "indigo-module")

;; Molecular operations (used by helper functions)
(declare-function indigo-symbol "indigo-module")

;; Bond property functions
(declare-function indigo-source "indigo-module")
(declare-function indigo-destination "indigo-module")

;;; Module Loading

;; Load the compiled module
(defun indigo-load-module ()
  "Load the indigo dynamic module, building it first if necessary."
  (let* ((pkg-dir (file-name-directory (or load-file-name buffer-file-name)))
         (module-path (expand-file-name "build/indigo-module.so" pkg-dir))
         (indigo-dir (expand-file-name "indigo-install" pkg-dir)))
    ;; Check if Indigo library is installed
    (unless (file-directory-p indigo-dir)
      (error "Indigo library not found. Run './install.sh' in %s" pkg-dir))
    ;; Build module if it doesn't exist
    (unless (file-exists-p module-path)
      (message "Indigo module not found, building...")
      (let ((default-directory pkg-dir))
        (unless (zerop (call-process "make" nil "*indigo-build*" nil))
          (error "Failed to build indigo module. Check *indigo-build* buffer for details"))))
    ;; Load the module
    (if (file-exists-p module-path)
        (module-load module-path)
      (error "Indigo module not found at %s. Run 'make' in %s" module-path pkg-dir))))

;; Load the module when this file is loaded
(indigo-load-module)

;; Load bond enum mappings
(require 'indigo-bond)

;;; Iterator Helper Functions

(defun indigo-map (func iterator)
  "Map FUNC over ITERATOR items, handling cleanup automatically.
Each item is freed after FUNC is applied to it."
  (cl-loop for item = (indigo-next iterator)
           while (and item (/= item -1))
           collect (unwind-protect
                       (funcall func item)
                     (indigo-free item))))

;;; Lazy Stream Abstraction

(defun indigo-stream (iterator)
  "Create a lazy stream from an Indigo ITERATOR.
Returns a stream which is either:
  - nil (empty stream, iterator exhausted)
  - (element . thunk) where thunk produces the next stream

The stream does NOT take ownership of the iterator - the caller
is responsible for freeing it when done (use indigo's `let`s!).

Example (fully manual):
  (let ((iter (indigo-iterate-atoms mol)))
    (unwind-protect
        (let ((stream (indigo-stream iter)))
          (indigo-stream-collect stream #\\='indigo-symbol))
      (indigo-free iter)))
  => (\"C\" \"C\" \"O\")"
  (when iterator
    (let ((element (indigo-next iterator)))
      (when element
        ;; Create stream node: (value . thunk-to-next-node)
        (cons element
              (lambda () (indigo-stream iterator)))))))

(defun indigo-stream-next (stream)
  "Advance STREAM by forcing the thunk, returning the next stream.
Returns nil if stream is exhausted."
  (when stream
    (let ((thunk (cdr stream)))
      (when (functionp thunk)
        (funcall thunk)))))

;;; Resource Management Macros

(defmacro indigo-let* (bindings &rest body)
  "Universal Indigo resource management macro with sequential binding.

BINDINGS is a list of binding specifications.  Each binding can be:

Molecules:
  (:molecule VAR MOL-STRING)     - Load molecule from SMILES/MOL
  (:mol-file VAR FILENAME)       - Load molecule from file
  (:query VAR QUERY-STRING)      - Load query molecule from string
  (:query-file VAR FILENAME)     - Load query molecule from file
  (:smarts VAR SMARTS-STRING)    - Load SMARTS pattern from string
  (:smarts-file VAR FILENAME)    - Load SMARTS pattern from file

Reactions:
  (:reaction VAR RXN-STRING)     - Load reaction from string
  (:rxn-file VAR FILENAME)       - Load reaction from file

Iterators:
  (:atoms VAR MOL-VAR)           - Iterate over atoms in molecule
  (:bonds VAR MOL-VAR)           - Iterate over bonds in molecule
  (:neighbors VAR ATOM-VAR)      - Iterate over atom neighbors
  (:components VAR MOL-VAR)      - Iterate over components
  (:sssr VAR MOL-VAR)            - Iterate over SSSR rings
  (:rings VAR MOL-VAR MIN MAX)   - Iterate over rings (size range)
  (:subtrees VAR MOL-VAR MIN MAX) - Iterate over subtrees
  (:stereocenters VAR MOL-VAR)   - Iterate over stereocenters

Fingerprints:
  (:fingerprint VAR OBJ TYPE)    - Generate fingerprint
                                   (TYPE: \"sim\", \"sub\", etc.)

Matchers:
  (:matcher VAR TARGET)          - Create substructure matcher

Arrays:
  (:array VAR)                   - Create empty array

Writers:
  (:file-writer VAR FILENAME)    - Create file writer
  (:buffer-writer VAR)           - Create buffer writer

Regular variables:
  (VAR VALUE)                    - Regular let-style binding

BODY contains the forms to evaluate with bindings in scope.

Bindings are evaluated sequentially (like let*), so later bindings
can reference earlier ones.  All Indigo objects are automatically
freed when leaving the scope, even if an error occurs.

Example usage:
  (indigo-let* ((:molecule mol \"CCO\")
                 (:atoms atoms mol))  ; atoms can reference mol
    (indigo-map #\\='indigo-symbol atoms))
  => (\"C\" \"C\" \"O\")"
  (declare (indent 1))
  ;; Helper function to build nested let/unwind-protect structure
  (cl-labels ((build-nested (bindings-list body)
                (if (null bindings-list)
                    ;; Base case: no more bindings, just execute body
                    `(progn ,@body)
                  ;; Recursive case: process one binding at a time
                  (let* ((current-binding (car bindings-list))
                         (other-bindings (cdr bindings-list))
                         (indigo-var nil)
                         (let-binding nil))

                    (pcase current-binding
                      ;; Molecules
                      (`(:molecule ,var ,mol-string)
                       (setq let-binding `(,var (indigo-load-molecule-from-string ,mol-string)))
                       (setq indigo-var var))
                      (`(:mol-file ,var ,filename)
                       (setq let-binding `(,var (indigo-load-molecule-from-file ,filename)))
                       (setq indigo-var var))
                      (`(:query ,var ,query-string)
                       (setq let-binding `(,var (indigo-load-query-molecule-from-string ,query-string)))
                       (setq indigo-var var))
                      (`(:query-file ,var ,filename)
                       (setq let-binding `(,var (indigo-load-query-molecule-from-file ,filename)))
                       (setq indigo-var var))
                      (`(:smarts ,var ,smarts-string)
                       (setq let-binding `(,var (indigo-load-smarts-from-string ,smarts-string)))
                       (setq indigo-var var))
                      (`(:smarts-file ,var ,filename)
                       (setq let-binding `(,var (indigo-load-smarts-from-file ,filename)))
                       (setq indigo-var var))

                      ;; Reactions
                      (`(:reaction ,var ,rxn-string)
                       (setq let-binding `(,var (indigo-load-reaction-from-string ,rxn-string)))
                       (setq indigo-var var))
                      (`(:rxn-file ,var ,filename)
                       (setq let-binding `(,var (indigo-load-reaction-from-file ,filename)))
                       (setq indigo-var var))

                      ;; Iterators (basic)
                      (`(:atoms ,var ,mol-var)
                       (setq let-binding `(,var (indigo-iterate-atoms ,mol-var)))
                       (setq indigo-var var))
                      (`(:bonds ,var ,mol-var)
                       (setq let-binding `(,var (indigo-iterate-bonds ,mol-var)))
                       (setq indigo-var var))
                      (`(:neighbors ,var ,atom-var)
                       (setq let-binding `(,var (indigo-iterate-neighbors ,atom-var)))
                       (setq indigo-var var))
                      (`(:components ,var ,mol-var)
                       (setq let-binding `(,var (indigo-iterate-components ,mol-var)))
                       (setq indigo-var var))
                      (`(:sssr ,var ,mol-var)
                       (setq let-binding `(,var (indigo-iterate-sssr ,mol-var)))
                       (setq indigo-var var))
                      (`(:stereocenters ,var ,mol-var)
                       (setq let-binding `(,var (indigo-iterate-stereocenters ,mol-var)))
                       (setq indigo-var var))
                      (`(:reactants ,var ,rxn-var)
                       (setq let-binding `(,var (indigo-iterate-reactants ,rxn-var)))
                       (setq indigo-var var))
                      (`(:products ,var ,rxn-var)
                       (setq let-binding `(,var (indigo-iterate-products ,rxn-var)))
                       (setq indigo-var var))

                      ;; Iterators (with parameters)
                      (`(:rings ,var ,mol-var ,min ,max)
                       (setq let-binding `(,var (indigo-iterate-rings ,mol-var ,min ,max)))
                       (setq indigo-var var))
                      (`(:subtrees ,var ,mol-var ,min ,max)
                       (setq let-binding `(,var (indigo-iterate-subtrees ,mol-var ,min ,max)))
                       (setq indigo-var var))

                      ;; Fingerprints
                      (`(:fingerprint ,var ,obj ,type)
                       (setq let-binding `(,var (indigo-fingerprint ,obj ,type)))
                       (setq indigo-var var))

                      ;; Matchers
                      (`(:matcher ,var ,target)
                       (setq let-binding `(,var (indigo-substructure-matcher ,target)))
                       (setq indigo-var var))

                      ;; Arrays
                      (`(:array ,var)
                       (setq let-binding `(,var (indigo-create-array)))
                       (setq indigo-var var))

                      ;; Writers
                      (`(:file-writer ,var ,filename)
                       (setq let-binding `(,var (indigo-create-file-writer ,filename)))
                       (setq indigo-var var))
                      (`(:buffer-writer ,var)
                       (setq let-binding `(,var (indigo-create-buffer-writer)))
                       (setq indigo-var var))

                      ;; Regular variables - no cleanup needed
                      (_
                       (setq let-binding current-binding)))

                    ;; Build nested structure
                    (if indigo-var
                        ;; Resource needs cleanup: wrap in unwind-protect
                        `(let (,let-binding)
                           (unwind-protect
                               ,(build-nested other-bindings body)
                             (when ,indigo-var (indigo-free ,indigo-var))))
                      ;; Regular variable: just let binding
                      `(let (,let-binding)
                         ,(build-nested other-bindings body)))))))

    (build-nested bindings body)))

(provide 'indigo)

;;; indigo.el ends here
