;;; indigo-with-star-defs.el --- Sequential binding macro definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi
;;
;; This file is part of emacs-indigo.

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
;;
;; Author: Giovanni Crisalfi

;;; Commentary:

;; This module defines the `define-indigo-with*' meta-macro and serves as the
;; manifest for all `indigo-with-*' sequential binding macros in the package.
;;
;; The meta-macro generates star versions (e.g., `indigo-with-molecule*') from
;; base macros (e.g., `indigo-with-molecule'), providing let*-style sequential
;; binding semantics with automatic resource cleanup.

;;; Code:

;;; Meta-Macro Definition

(defmacro define-indigo-with* (base-name)
  "Auto-generate a sequential binding version (*) of an indigo-with- macro.

Given BASE-NAME (e.g., \"molecule\"), creates `indigo-with-molecule*' that
wraps `indigo-with-molecule' with sequential binding semantics (like let*).

The generated macro accepts multiple bindings and evaluates them sequentially,
ensuring proper cleanup for each resource even if a later binding fails.

Example usage:
  (define-indigo-with* \"molecule\")
  ;; Creates indigo-with-molecule* from indigo-with-molecule

The generated macro can then be used like:
  (indigo-with-molecule* ((mol1 \"CCO\")
                          (mol2 \"c1ccccc1\"))
    (list mol1 mol2))"
  (let* ((base-macro (intern (format "indigo-with-%s" base-name)))
         (star-macro (intern (format "indigo-with-%s*" base-name))))
    `(defmacro ,star-macro (bindings &rest body)
       ,(format "Sequential binding version of `%s'.

BINDINGS is a list of bindings: ((VAR1 ARG1...) (VAR2 ARG2...) ...)
Bindings are evaluated sequentially (like let*) with automatic cleanup."
                base-macro)
       (declare (indent 1))
       (if (null bindings)
           `(progn ,@body)
         `(,',base-macro ,(car bindings)
            (,',star-macro ,(cdr bindings)
              ,@body))))))

;;; Macro Definitions Manifest
;;
;; All indigo-with-* sequential binding macros are defined here.
;; The actual base macros (indigo-with-molecule, etc.) are defined
;; in their respective modules.

;; Molecule macros (from indigo-mol.el)
(define-indigo-with* "molecule")
(define-indigo-with* "mol-file")
(define-indigo-with* "query")
(define-indigo-with* "query-file")
(define-indigo-with* "smarts")
(define-indigo-with* "smarts-file")
(define-indigo-with* "fingerprint")
(define-indigo-with* "matcher")

;; Reaction macros (from indigo-react.el)
(define-indigo-with* "reaction")
(define-indigo-with* "rxn-file")

;; Stream macros (from indigo-stream.el)
(define-indigo-with* "stream-from-iterator")

;; Stream-iterator bridge macros (from indigo-stream-iter.el)
(define-indigo-with* "atoms-stream")
(define-indigo-with* "bonds-stream")
(define-indigo-with* "neighbors-stream")
(define-indigo-with* "components-stream")
(define-indigo-with* "sssr-stream")
(define-indigo-with* "rings-stream")
(define-indigo-with* "subtrees-stream")
(define-indigo-with* "stereocenters-stream")
(define-indigo-with* "reactants-stream")
(define-indigo-with* "products-stream")

;; Render macros (from indigo-render.el)
(define-indigo-with* "array")

;; Iterator macros (from indigo-iter.el)
(define-indigo-with* "atoms-iterator")
(define-indigo-with* "bonds-iterator")
(define-indigo-with* "neighbors-iterator")
(define-indigo-with* "components-iterator")
(define-indigo-with* "sssr-iterator")
(define-indigo-with* "rings-iterator")
(define-indigo-with* "subtrees-iterator")
(define-indigo-with* "stereocenters-iterator")
(define-indigo-with* "reactants-iterator")
(define-indigo-with* "products-iterator")

(provide 'indigo-with-star-defs)

;;; indigo-with-star-defs.el ends here
