;;; indigo-let.el --- Description -*- lexical-binding: t; -*-
;;
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
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; Resource Management Macros

(defun indigo--parse-binding (binding)
  "Parse a single BINDING specification.
Returns (LET-BINDING . CLEANUP-FORM) where:
  - LET-BINDING is the form for let/let* binding
  - CLEANUP-FORM is the cleanup code form, or nil if no cleanup needed."
  (let ((cleanup-form nil)
        (let-binding nil))

    (pcase binding
      ;; Molecules
      (`(:molecule ,var ,mol-string)
       (setq let-binding `(,var (indigo-load-molecule-from-string ,mol-string)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:mol-file ,var ,filename)
       (setq let-binding `(,var (indigo-load-molecule-from-file ,filename)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:query ,var ,query-string)
       (setq let-binding `(,var (indigo-load-query-molecule-from-string ,query-string)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:query-file ,var ,filename)
       (setq let-binding `(,var (indigo-load-query-molecule-from-file ,filename)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:smarts ,var ,smarts-string)
       (setq let-binding `(,var (indigo-load-smarts-from-string ,smarts-string)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:smarts-file ,var ,filename)
       (setq let-binding `(,var (indigo-load-smarts-from-file ,filename)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Reactions
      (`(:reaction ,var ,rxn-string)
       (setq let-binding `(,var (indigo-load-reaction-from-string ,rxn-string)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:rxn-file ,var ,filename)
       (setq let-binding `(,var (indigo-load-reaction-from-file ,filename)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Iterators (basic)
      (`(:atoms ,var ,mol-var)
       (setq let-binding `(,var (indigo-iterate-atoms ,mol-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:bonds ,var ,mol-var)
       (setq let-binding `(,var (indigo-iterate-bonds ,mol-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:neighbors ,var ,atom-var)
       (setq let-binding `(,var (indigo-iterate-neighbors ,atom-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:components ,var ,mol-var)
       (setq let-binding `(,var (indigo-iterate-components ,mol-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:sssr ,var ,mol-var)
       (setq let-binding `(,var (indigo-iterate-sssr ,mol-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:stereocenters ,var ,mol-var)
       (setq let-binding `(,var (indigo-iterate-stereocenters ,mol-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:reactants ,var ,rxn-var)
       (setq let-binding `(,var (indigo-iterate-reactants ,rxn-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:products ,var ,rxn-var)
       (setq let-binding `(,var (indigo-iterate-products ,rxn-var)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Iterators (with parameters)
      (`(:rings ,var ,mol-var ,min ,max)
       (setq let-binding `(,var (indigo-iterate-rings ,mol-var ,min ,max)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))
      (`(:subtrees ,var ,mol-var ,min ,max)
       (setq let-binding `(,var (indigo-iterate-subtrees ,mol-var ,min ,max)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Fingerprints
      (`(:fingerprint ,var ,obj ,type)
       (setq let-binding `(,var (indigo-fingerprint ,obj ,type)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Matchers
      (`(:matcher ,var ,target)
       (setq let-binding `(,var (indigo-substructure-matcher ,target)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Arrays
      (`(:array ,var)
       (setq let-binding `(,var (indigo-create-array)))
       (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Writers - NOT YET IMPLEMENTED in C module
      ;; (`(:file-writer ,var ,filename)
      ;;  (setq let-binding `(,var (indigo-create-file-writer ,filename)))
      ;;  (setq cleanup-form `(when ,var (indigo-free ,var))))
      ;; (`(:buffer-writer ,var)
      ;;  (setq let-binding `(,var (indigo-create-buffer-writer)))
      ;;  (setq cleanup-form `(when ,var (indigo-free ,var))))

      ;; Regular variables - no cleanup needed
      (_
       (setq let-binding binding)))

    (cons let-binding cleanup-form)))

(defmacro indigo-let (bindings &rest body)
  "Universal Indigo resource management macro with parallel binding.

BINDINGS is a list of binding specifications (same syntax as `indigo-let*`).

Unlike `indigo-let*`, bindings are evaluated in parallel (like `let`).
All binding expressions are evaluated before any variable is bound.
This means later bindings CANNOT reference earlier ones.

All Indigo objects are automatically freed when leaving the scope,
even if an error occurs.

Example usage:
  (indigo-let ((:molecule mol1 \"CCO\")
                (:molecule mol2 \"c1ccccc1\"))
    ;; Both molecules created in parallel
    (list (indigo-molecular-weight mol1)
          (indigo-molecular-weight mol2)))
  => (46.069 78.114)

For sequential binding where later bindings can reference earlier ones,
use `indigo-let*` instead."
  (declare (indent 1))
  ;; Parse all bindings
  (let ((let-bindings '())
        (cleanup-forms '()))

    (dolist (binding bindings)
      (let* ((parsed (indigo--parse-binding binding))
             (let-binding (car parsed))
             (cleanup-form (cdr parsed)))
        (push let-binding let-bindings)
        (when cleanup-form
          (push cleanup-form cleanup-forms))))

    ;; Reverse to maintain original order
    (setq let-bindings (nreverse let-bindings))
    (setq cleanup-forms (nreverse cleanup-forms))

    ;; Build cleanup chain: wrap body in nested unwind-protect for each resource
    (let ((body-form `(progn ,@body)))
      (dolist (cleanup-form cleanup-forms)
        (setq body-form
              `(unwind-protect
                   ,body-form
                 ,cleanup-form)))

      ;; Finally wrap in let
      `(let ,let-bindings
         ,body-form))))

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
  (:reactants VAR RXN-VAR)       - Iterate over reactants in reaction
  (:products VAR RXN-VAR)        - Iterate over products in reaction

Fingerprints:
  (:fingerprint VAR OBJ TYPE)    - Generate fingerprint
                                   (TYPE: \"sim\", \"sub\", etc.)

Matchers:
  (:matcher VAR TARGET)          - Create substructure matcher

Arrays:
  (:array VAR)                   - Create empty array

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
  ;; Helper function to build nested let*/unwind-protect structure
  (cl-labels ((build-nested (bindings-list body)
                (if (null bindings-list)
                    ;; Base case: no more bindings, just execute body
                    `(progn ,@body)
                  ;; Recursive case: process one binding at a time
                  (let* ((current-binding (car bindings-list))
                         (other-bindings (cdr bindings-list))
                         (parsed (indigo--parse-binding current-binding))
                         (let-binding (car parsed))
                         (cleanup-form (cdr parsed)))

                    ;; Build nested structure
                    (if cleanup-form
                        ;; Resource needs cleanup: wrap in unwind-protect
                        `(let (,let-binding)
                           (unwind-protect
                               ,(build-nested other-bindings body)
                             ,cleanup-form))
                      ;; Regular variable: just let binding
                      `(let (,let-binding)
                         ,(build-nested other-bindings body)))))))

    (build-nested bindings body)))

(provide 'indigo-let)
;;; indigo-let.el ends here
