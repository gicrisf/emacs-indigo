;;; indigo-macros.el --- Core macro definitions for emacs-indigo -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi
;; Package-Requires: ((emacs "25.1"))

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

;;; Commentary:

;; This module contains ONLY macro definitions with no dependencies.
;; It must be loaded before any file that uses `define-indigo-with*'.
;;
;; The separation ensures proper byte-compilation ordering: this file
;; compiles independently, then other files can use its macros.

;;; Code:

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

(provide 'indigo-macros)

;;; indigo-macros.el ends here
