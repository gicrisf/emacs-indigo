;;; indigo-mol.el --- Molecule functions for Indigo -*- lexical-binding: t; -*-

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
;; molecule operations:
;;
;; Structure Manipulation:
;; - `indigo-aromatize' - Detect and mark aromatic rings
;; - `indigo-layout' - Calculate 2D coordinates
;; - `indigo-fold-hydrogens' - Remove explicit hydrogen atoms
;; - `indigo-unfold-hydrogens' - Add explicit hydrogen atoms
;;
;; Normalization:
;; - `indigo-normalize' - Normalize molecule structure
;; - `indigo-standardize' - Standardize charges and stereochemistry
;; - `indigo-ionize' - Ionize molecule at specified pH
;;
;; Search and Analysis:
;; - `indigo-fingerprint' - Generate molecular fingerprints
;; - `indigo-substructure-matcher' - Create substructure matcher

;;; Code:

(declare-function indigo-get-last-error "indigo-module")

;; Search
(declare-function indigo--fingerprint "indigo-module")
(declare-function indigo--substructure-matcher "indigo-module")
(declare-function indigo--similarity "indigo-module")

(defun indigo-fingerprint (object type)
  "Generate fingerprint of TYPE for OBJECT.
TYPE can be \"sim\", \"sub\", etc.
Signals an error if fingerprint generation fails."
  (let ((handle (indigo--fingerprint object type)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to generate fingerprint: %s" (indigo-get-last-error)))))

(defun indigo-similarity (fp1 fp2 &optional metric &rest params)
  "Calculate similarity between fingerprints FP1 and FP2 using METRIC.

METRIC is an optional keyword specifying the similarity metric. If omitted,
defaults to :tanimoto. Can be followed by numeric PARAMS for metrics that
support them:

Simple metrics:
  :tanimoto        - Tanimoto coefficient (Jaccard index) [default]
  :euclid-sub      - Euclidean distance for substructure fingerprints
  :normalized-edit - Normalized edit distance

Tversky metric with optional parameters:
  :tversky              - Tversky similarity with default weights
  :tversky ALPHA BETA   - Tversky with custom weights
    ALPHA - Weight for query fingerprint bits (float, typically 0.0-1.0)
    BETA  - Weight for target fingerprint bits (float, typically 0.0-1.0)
    Example: :tversky 0.7 0.3 emphasizes query over target

Returns a float between 0.0 and 1.0 representing the similarity score.

Examples:
  (indigo-similarity fp1 fp2)                ; Uses :tanimoto (default)
  (indigo-similarity fp1 fp2 :tanimoto)
  (indigo-similarity fp1 fp2 :tversky)
  (indigo-similarity fp1 fp2 :tversky 0.7 0.3)"
  (unless metric
    (setq metric :tanimoto))
  (unless (keywordp metric)
    (error "METRIC must be a keyword, got: %S" metric))
  (let ((metrics-str (concat (substring (symbol-name metric) 1)
                             (when params
                               (concat " " (mapconcat #'number-to-string params " "))))))
    (indigo--similarity fp1 fp2 metrics-str)))

(defun indigo-substructure-matcher (target)
  "Create a substructure matcher for TARGET molecule.
Signals an error if matcher creation fails."
  (let ((handle (indigo--substructure-matcher target)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create substructure matcher: %s" (indigo-get-last-error)))))

;; Structure manipulation

(declare-function indigo--aromatize "indigo-module")
(declare-function indigo--layout "indigo-module")
(declare-function indigo--fold-hydrogens "indigo-module")
(declare-function indigo--unfold-hydrogens "indigo-module")
(declare-function indigo--normalize "indigo-module")
(declare-function indigo--standardize "indigo-module")
(declare-function indigo--ionize "indigo-module")

(defun indigo-aromatize (molecule)
  "Aromatize MOLECULE (detect and mark aromatic rings).

Returns a keyword indicating the result:
  :changed   - Molecule was aromatized (Kekulé form converted to aromatic)
  :unchanged - No changes needed (already aromatic or no aromatic rings)"
  (let ((result (indigo--aromatize molecule)))
    (pcase result
      (1 :changed)
      (0 :unchanged)
      (-1 (error "Aromatization failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--aromatize: %d" result)))))

(defun indigo-layout (molecule)
  "Calculate 2D coordinates for MOLECULE.

Generates aesthetically pleasing 2D coordinates suitable for visualization.
Returns t on success.

Signals an error if the operation fails.

Example:
  (indigo-let* ((:molecule mol \"CCO\"))
    (indigo-layout mol)
    (indigo-has-coordinates mol))
  ;; => t"
  ;; TODO: Research why indigoLayout uses POSIX convention (0=success)
  ;; while most other Indigo functions return 1 on success.
  ;; Check Indigo documentation/mailing lists/GitHub issues for rationale.
  (let ((result (indigo--layout molecule)))
    (pcase result
      (0 t)
      (-1 (error "Layout calculation failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--layout: %d" result)))))

(defun indigo-fold-hydrogens (molecule)
  "Remove explicit hydrogen atoms from MOLECULE (convert to implicit).

Returns t on success, nil on failure.
Signals an error if the operation encounters an error condition.

Example:
  (indigo-let* ((:molecule mol \"CCO\"))
    (indigo-unfold-hydrogens mol)   ; Add explicit H
    (indigo-count-atoms mol)         ; => 9 (3 heavy + 6 H)
    (indigo-fold-hydrogens mol)      ; Remove explicit H
    (indigo-count-atoms mol))        ; => 3 (only heavy atoms)"
  (let ((result (indigo--fold-hydrogens molecule)))
    (pcase result
      (1 t)
      (0 nil)
      (-1 (error "Folding hydrogens failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--fold-hydrogens: %d" result)))))

(defun indigo-unfold-hydrogens (molecule)
  "Add explicit hydrogen atoms to MOLECULE.

Makes all implicit hydrogen atoms explicit in the structure.
Returns t on success, nil on failure.
Signals an error if the operation encounters an error condition.

Example:
  (indigo-let* ((:molecule mol \"CCO\"))
    (indigo-count-atoms mol)           ; => 3 (only heavy atoms)
    (indigo-unfold-hydrogens mol)      ; Add explicit H
    (indigo-count-atoms mol))          ; => 9 (3 heavy + 6 H)"
  (let ((result (indigo--unfold-hydrogens molecule)))
    (pcase result
      (1 t)
      (0 nil)
      (-1 (error "Unfolding hydrogens failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--unfold-hydrogens: %d" result)))))

;;; Normalization Functions

(defun indigo-normalize (molecule &optional options)
  "Normalize MOLECULE structure with optional OPTIONS string.

Returns a keyword indicating the result:
  :changed   - Molecule was normalized (structure modifications made)
  :unchanged - No changes needed (already normalized)

OPTIONS is an optional string with normalization configuration.
See Indigo documentation for available normalization options."
  (let ((result (indigo--normalize molecule options)))
    (pcase result
      (1 :changed)
      (0 :unchanged)
      (-1 (error "Normalization failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--normalize: %d" result)))))

(defun indigo-standardize (molecule)
  "Standardize MOLECULE charges, stereochemistry, etc.

Returns a keyword indicating the result:
  :changed   - Molecule was standardized (changes applied)
  :unchanged - No changes needed (already standardized)

Standardization includes:
- Charge standardization
- Stereochemistry normalization
- Other structural standardizations"
  (let ((result (indigo--standardize molecule)))
    (pcase result
      (1 :changed)
      (0 :unchanged)
      (-1 (error "Standardization failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--standardize: %d" result)))))

(defun indigo-ionize (molecule ph ph-tolerance)
  "Ionize MOLECULE at specified PH with PH-TOLERANCE.

Returns a keyword indicating the result:
  :changed   - Molecule was ionized (protonation state changed)
  :unchanged - No changes needed (already at correct ionization state)

PH is the target pH value (typically 0.0-14.0).
PH-TOLERANCE is the acceptable pH range around the target."
  (let ((result (indigo--ionize molecule ph ph-tolerance)))
    (pcase result
      (1 :changed)
      (0 :unchanged)
      (-1 (error "Ionization failed: %s" (indigo-get-last-error)))
      (_ (error "Unexpected return value from indigo--ionize: %d" result)))))

;;; With-style Macros for Molecules

(defmacro indigo-with-molecule (binding &rest body)
  "Load molecule from string with automatic cleanup.

Usage: (indigo-with-molecule (VAR MOL-STRING) BODY...)

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-molecular-weight mol))
  => 46.069"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-string (cadr binding)))
    `(let ((,var (indigo-load-molecule-from-string ,mol-string)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-mol-file (binding &rest body)
  "Load molecule from file with automatic cleanup.

Usage: (indigo-with-mol-file (VAR FILENAME) BODY...)

Example:
  (indigo-with-mol-file (mol \"molecule.mol\")
    (indigo-canonical-smiles mol))"
  (declare (indent 1))
  (let ((var (car binding))
        (filename (cadr binding)))
    `(let ((,var (indigo-load-molecule-from-file ,filename)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-query (binding &rest body)
  "Load query molecule from string with automatic cleanup.

Usage: (indigo-with-query (VAR QUERY-STRING) BODY...)

Example:
  (indigo-with-query (query \"C=O\")
    (indigo-smiles query))"
  (declare (indent 1))
  (let ((var (car binding))
        (query-string (cadr binding)))
    `(let ((,var (indigo-load-query-molecule-from-string ,query-string)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-query-file (binding &rest body)
  "Load query molecule from file with automatic cleanup.

Usage: (indigo-with-query-file (VAR FILENAME) BODY...)

Example:
  (indigo-with-query-file (query \"query.mol\")
    (indigo-smiles query))"
  (declare (indent 1))
  (let ((var (car binding))
        (filename (cadr binding)))
    `(let ((,var (indigo-load-query-molecule-from-file ,filename)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-smarts (binding &rest body)
  "Load SMARTS pattern from string with automatic cleanup.

Usage: (indigo-with-smarts (VAR SMARTS-STRING) BODY...)

Example:
  (indigo-with-smarts (pattern \"[#6]=[#8]\")
    (indigo-smiles pattern))"
  (declare (indent 1))
  (let ((var (car binding))
        (smarts-string (cadr binding)))
    `(let ((,var (indigo-load-smarts-from-string ,smarts-string)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-smarts-file (binding &rest body)
  "Load SMARTS pattern from file with automatic cleanup.

Usage: (indigo-with-smarts-file (VAR FILENAME) BODY...)

Example:
  (indigo-with-smarts-file (pattern \"pattern.sma\")
    (indigo-smiles pattern))"
  (declare (indent 1))
  (let ((var (car binding))
        (filename (cadr binding)))
    `(let ((,var (indigo-load-smarts-from-file ,filename)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-fingerprint (binding &rest body)
  "Generate fingerprint with automatic cleanup.

Usage: (indigo-with-fingerprint (VAR OBJ TYPE) BODY...)

TYPE can be \"sim\" (similarity) or \"sub\" (substructure).

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-fingerprint (fp mol \"sim\")
      (indigo-fingerprint-to-string fp)))"
  (declare (indent 1))
  (let ((var (car binding))
        (obj (cadr binding))
        (type (caddr binding)))
    `(let ((,var (indigo-fingerprint ,obj ,type)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-matcher (binding &rest body)
  "Create substructure matcher with automatic cleanup.

Usage: (indigo-with-matcher (VAR TARGET) BODY...)

Example:
  (indigo-with-molecule (mol \"c1ccccc1CCO\")
    (indigo-with-matcher (matcher mol)
      (indigo-with-query (query \"c1ccccc1\")
        (indigo-match matcher query))))"
  (declare (indent 1))
  (let ((var (car binding))
        (target (cadr binding)))
    `(let ((,var (indigo-substructure-matcher ,target)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(provide 'indigo-mol)
;;; indigo-mol.el ends here
