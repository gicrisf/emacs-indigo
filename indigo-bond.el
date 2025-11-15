;;; indigo-bond.el --- Bond property mappings for Indigo -*- lexical-binding: t; -*-

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

;; This file provides keyword symbol mappings for Indigo bond integer values.
;;
;;; Code:

;;; Forward declarations for C module raw functions
(declare-function indigo--bond-order-raw "indigo-module")
(declare-function indigo--bond-stereo-raw "indigo-module")
(declare-function indigo-source "indigo-module")
(declare-function indigo-destination "indigo-module")

;;; Bond Order Mappings

(defconst indigo-bond-orders
  '((0 . :query)
    (1 . :single)
    (2 . :double)
    (3 . :triple)
    (4 . :aromatic))
  "Mapping from Indigo bond order integer codes to keyword symbols.

Integer codes from Indigo C API:
  0 - Query/ambiguous bond
  1 - Single bond
  2 - Double bond
  3 - Triple bond
  4 - Aromatic bond")

(defun indigo-bond-order (bond)
  "Get the bond order of BOND as a keyword symbol.

Returns one of:
  :query    - Query or ambiguous bond
  :single   - Single bond
  :double   - Double bond
  :triple   - Triple bond
  :aromatic - Aromatic bond
  nil       - Error or invalid bond

Example:
  (indigo-let* ((:molecule mol \"C=C\")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    (indigo-bond-order bond))
  ;; => :double"
  (when-let ((code (indigo--bond-order-raw bond)))
    (alist-get code indigo-bond-orders)))

;;; Stereochemistry Mappings

(defconst indigo-bond-stereos
  '((0 . :none)
    (4 . :either)
    (5 . :up)
    (6 . :down)
    (7 . :cis)
    (8 . :trans))
  "Mapping from Indigo stereochemistry integer codes to keyword symbols.

Integer codes from Indigo C API:
  0 - No stereochemistry
  4 - Either configuration (wavy bond)
  5 - Up/wedge bond
  6 - Down/hashed bond
  7 - Cis double bond
  8 - Trans double bond")

(defun indigo-bond-stereo (bond)
  "Get the stereochemistry of BOND as a keyword symbol.

Returns one of:
  :none   - No stereochemistry
  :either - Either configuration (wavy bond)
  :up     - Wedge bond (pointing up)
  :down   - Hashed bond (pointing down)
  :cis    - Cis double bond
  :trans  - Trans double bond
  nil     - Error or invalid bond

Example:
  (indigo-let* ((:molecule mol \"C[C@H](O)Cl\")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    (indigo-bond-stereo bond))
  ;; => :up or :none depending on which bond"
  (when-let ((code (indigo--bond-stereo-raw bond)))
    (alist-get code indigo-bond-stereos)))

;;; Reverse Mappings

(defun indigo-bond-order-code (keyword)
  "Get the integer code for bond order KEYWORD.
For advanced usage when interfacing with raw C API.

Example: (indigo-bond-order-code :double) => 2"
  (car (rassoc keyword indigo-bond-orders)))

(defun indigo-bond-stereo-code (keyword)
  "Get the integer code for stereochemistry KEYWORD.
For advanced usage when interfacing with raw C API.

Example: (indigo-bond-stereo-code :up) => 5"
  (car (rassoc keyword indigo-bond-stereos)))

;;; Convenience Predicates

(defun indigo-bond-single-p (bond)
  "Return non-nil if BOND is a single bond."
  (eq (indigo-bond-order bond) :single))

(defun indigo-bond-double-p (bond)
  "Return non-nil if BOND is a double bond."
  (eq (indigo-bond-order bond) :double))

(defun indigo-bond-triple-p (bond)
  "Return non-nil if BOND is a triple bond."
  (eq (indigo-bond-order bond) :triple))

(defun indigo-bond-aromatic-p (bond)
  "Return non-nil if BOND is aromatic."
  (eq (indigo-bond-order bond) :aromatic))

(defun indigo-bond-has-stereo-p (bond)
  "Return non-nil if BOND has stereochemistry."
  (not (eq (indigo-bond-stereo bond) :none)))

(provide 'indigo-bond)
;;; indigo-bond.el ends here
