;;; indigo-react.el --- Reaction functions for Indigo -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This module provides high-level, idiomatic Emacs Lisp functions for
;; chemical reaction operations:
;;
;; Loading and I/O (should probably be moved to IO):
;; - `indigo-load-reaction-from-string' - Load reaction from SMILES string
;; - `indigo-load-reaction-from-file' - Load reaction from file
;;
;; Iteration (should probably be moved to iter):
;; - `indigo-iterate-reactants' - Iterate over reactants
;; - `indigo-iterate-products' - Iterate over products
;;
;; Reaction Mapping:
;; - `indigo-automap' - Automatic atom mapping
;; - `indigo-get-atom-mapping-number' - Get atom mapping number
;; - `indigo-set-atom-mapping-number' - Set atom mapping number
;; - `indigo-clear-aam' - Clear all atom-to-atom mappings
;; - `indigo-correct-reacting-centers' - Correct reacting centers
;;
;; Reacting Centers:
;; - `indigo-get-reacting-center' - Get reacting center for bond
;; - `indigo-set-reacting-center' - Set reacting center for bond
;;
;; PKA Calculations:
;; - `indigo-build-pka-model' - Build PKA prediction model
;; - `indigo-get-acid-pka-value' - Get acidic PKA value
;; - `indigo-get-basic-pka-value' - Get basic PKA value
;;
;; All functions follow Emacs Lisp conventions with keyword returns,
;; boolean values, and automatic error signaling.

;;; Code:

(declare-function indigo-get-last-error "indigo-module")
(declare-function indigo-free "indigo-module")

;;; Forward declarations for C module functions

;; Loading functions from C module
(declare-function indigo--load-reaction-from-string "indigo-module")
(declare-function indigo--load-reaction-from-file "indigo-module")

;; Iterator functions from C module
(declare-function indigo--iterate-reactants "indigo-module")
(declare-function indigo--iterate-products "indigo-module")

;; Reaction mapping functions from C module
(declare-function indigo-automap "indigo-module")
(declare-function indigo-get-atom-mapping-number "indigo-module")
(declare-function indigo-set-atom-mapping-number "indigo-module")
(declare-function indigo-clear-aam "indigo-module")
(declare-function indigo-correct-reacting-centers "indigo-module")

;; Reacting center functions from C module
(declare-function indigo-get-reacting-center "indigo-module")
(declare-function indigo-set-reacting-center "indigo-module")

;; PKA functions from C module
(declare-function indigo-build-pka-model "indigo-module")
(declare-function indigo-get-acid-pka-value "indigo-module")
(declare-function indigo-get-basic-pka-value "indigo-module")

;;; Reaction Loading Functions

(defun indigo-load-reaction-from-string (string)
  "Load reaction from STRING (reaction SMILES format).
Signals an error if loading fails."
  (let ((handle (indigo--load-reaction-from-string string)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load reaction from string: %s" (indigo-get-last-error)))))

(defun indigo-load-reaction-from-file (filename)
  "Load reaction from FILENAME.
Signals an error if loading fails or file doesn't exist."
  (unless (file-exists-p filename)
    (error "File does not exist: %s" filename))
  (let ((handle (indigo--load-reaction-from-file filename)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load reaction from file %s: %s"
             filename (indigo-get-last-error)))))

;;; Reaction Iterator Functions

(defun indigo-iterate-reactants (reaction)
  "Create an iterator over reactants in REACTION.
Returns an iterator handle that can be used with iterator functions.
Signals an error if iterator creation fails.

Example:
  (indigo-with-reaction (rxn \"CC>>CCO\")
    (indigo-with-reactants-iterator (reactants rxn)
      (indigo-count reactants)))
  ;; => 1"
  (let ((handle (indigo--iterate-reactants reaction)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create reactants iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-products (reaction)
  "Create an iterator over products in REACTION.
Returns an iterator handle that can be used with iterator functions.
Signals an error if iterator creation fails.

Example:
  (indigo-with-reaction (rxn \"CC>>CCO\")
    (indigo-with-products-iterator (products rxn)
      (indigo-count products)))
  ;; => 1"
  (let ((handle (indigo--iterate-products reaction)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create products iterator: %s" (indigo-get-last-error)))))

;;; With-style Macros for Reactions

(defmacro indigo-with-reaction (binding &rest body)
  "Load reaction from string with automatic cleanup.

Usage: (indigo-with-reaction (VAR RXN-STRING) BODY...)

Example:
  (indigo-with-reaction (rxn \"CC>>CCO\")
    (indigo-automap rxn \"discard\"))
  ;; => 0"
  (declare (indent 1))
  (let ((var (car binding))
        (rxn-string (cadr binding)))
    `(let ((,var (indigo-load-reaction-from-string ,rxn-string)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-rxn-file (binding &rest body)
  "Load reaction from file with automatic cleanup.

Usage: (indigo-with-rxn-file (VAR FILENAME) BODY...)

Example:
  (indigo-with-rxn-file (rxn \"reaction.rxn\")
    (indigo-automap rxn \"discard\"))"
  (declare (indent 1))
  (let ((var (car binding))
        (filename (cadr binding)))
    `(let ((,var (indigo-load-reaction-from-file ,filename)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

;; Auto-generate sequential binding versions
(define-indigo-with* "reaction")
(define-indigo-with* "rxn-file")

(provide 'indigo-react)

;;; indigo-react.el ends here
