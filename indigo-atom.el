;;; indigo-atom.el --- Atom property mappings for Indigo -*- lexical-binding: t; -*-

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

;; This file provides keyword symbol mappings for Indigo atom integer values.
;;
;; Atom Radical Functions:
;;
;; - `indigo-radical' - Returns radical state as keyword (:none, :singlet, :doublet, :triplet)
;; - `indigo-radical-electrons' - Returns number of electrons in radical center (integer)
;;
;; The two functions are complementary:
;; - Radical state describes the spin configuration
;; - Radical electrons counts the electrons involved
;;
;; Example:
;;   Singlet carbene [CH2]: state = :singlet, electrons = 2
;;   Doublet radical [CH3]: state = :doublet, electrons = 1
;;   Triplet carbene: state = :triplet, electrons = 2

;;; Code:

;;; Forward declarations for C module functions
(declare-function indigo-symbol "indigo-module")
(declare-function indigo-charge "indigo-module")

(declare-function indigo--radical-raw "indigo-module")
(declare-function indigo-radical-electrons "indigo-module")

;;; Radical State Mappings

(defconst indigo-radicals
  '((0 . :none)
    (101 . :singlet)
    (102 . :doublet)
    (103 . :triplet))
  "Mapping from Indigo radical state integer codes to keyword symbols.

Integer codes from Indigo C API (from indigo.h):
  0   - No radical
  101 - Singlet radical (INDIGO_SINGLET)
  102 - Doublet radical (INDIGO_DOUBLET)
  103 - Triplet radical (INDIGO_TRIPLET)")

(defun indigo-radical (atom)
  "Get the radical state of ATOM as a keyword symbol.

Returns one of:
  :none    - No radical
  :singlet - Singlet radical
  :doublet - Doublet radical
  :triplet - Triplet radical
  nil      - Error or invalid atom

Example:
  (indigo-let* ((:molecule mol \"[O]\")  ; Oxygen radical
                (:atoms atoms-iter mol)
                (atom (indigo-next atoms-iter)))
    (indigo-radical atom))
  ;; => :doublet"
  (when-let ((code (indigo--radical-raw atom)))
    (alist-get code indigo-radicals)))

;;; Reverse Mapping

(defun indigo-radical-code (keyword)
  "Get the integer code for radical state KEYWORD.
For advanced usage when interfacing with raw C API.

Example: (indigo-radical-code :doublet) => 102"
  (car (rassoc keyword indigo-radicals)))

;;; Convenience Predicates

(defun indigo-atom-radical-p (atom)
  "Return non-nil if ATOM has a radical state."
  (not (eq (indigo-radical atom) :none)))

(defun indigo-atom-singlet-p (atom)
  "Return non-nil if ATOM is a singlet radical."
  (eq (indigo-radical atom) :singlet))

(defun indigo-atom-doublet-p (atom)
  "Return non-nil if ATOM is a doublet radical."
  (eq (indigo-radical atom) :doublet))

(defun indigo-atom-triplet-p (atom)
  "Return non-nil if ATOM is a triplet radical."
  (eq (indigo-radical atom) :triplet))

(provide 'indigo-atom)
;;; indigo-atom.el ends here
