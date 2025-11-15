;;; indigo-iter.el --- Iterator functions and abstractions for Indigo -*- lexical-binding: t; -*-

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

;; This module provides iterator wrapper functions for working with Indigo
;; molecular structures.
;;
;; For lazy stream abstractions built on top of iterators, see indigo-stream.el

;;; Code:

;;; Forward declarations for C module functions

(declare-function indigo-free "indigo-module")
(declare-function indigo-next "indigo-module")
(declare-function indigo-get-last-error "indigo-module")

;; Iterator creation functions from C module
(declare-function indigo--iterate-atoms "indigo-module")
(declare-function indigo--iterate-bonds "indigo-module")
(declare-function indigo--iterate-neighbors "indigo-module")
(declare-function indigo--iterate-components "indigo-module")
(declare-function indigo--iterate-sssr "indigo-module")
(declare-function indigo--iterate-rings "indigo-module")
(declare-function indigo--iterate-subtrees "indigo-module")
(declare-function indigo--iterate-stereocenters "indigo-module")
(declare-function indigo--iterate-reactants "indigo-module")
(declare-function indigo--iterate-products "indigo-module")

;;; Iterator Wrapper Functions

(defun indigo-iterate-atoms (molecule)
  "Create an iterator over atoms in MOLECULE.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-atoms molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create atoms iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-bonds (molecule)
  "Create an iterator over bonds in MOLECULE.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-bonds molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create bonds iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-neighbors (atom)
  "Create an iterator over neighbors of ATOM.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-neighbors atom)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create neighbors iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-components (molecule)
  "Create an iterator over connected components in MOLECULE.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-components molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create components iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-sssr (molecule)
  "Create an iterator over SSSR rings in MOLECULE.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-sssr molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create SSSR iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-rings (molecule min-atoms max-atoms)
  "Create an iterator over rings in MOLECULE with atom count in range [MIN-ATOMS, MAX-ATOMS].
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-rings molecule min-atoms max-atoms)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create rings iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-subtrees (molecule min-atoms max-atoms)
  "Create an iterator over subtrees in MOLECULE with atom count in range [MIN-ATOMS, MAX-ATOMS].
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-subtrees molecule min-atoms max-atoms)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create subtrees iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-stereocenters (molecule)
  "Create an iterator over stereocenters in MOLECULE.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-stereocenters molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create stereocenters iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-reactants (reaction)
  "Create an iterator over reactants in REACTION.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-reactants reaction)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create reactants iterator: %s" (indigo-get-last-error)))))

(defun indigo-iterate-products (reaction)
  "Create an iterator over products in REACTION.
Signals an error if iterator creation fails."
  (let ((handle (indigo--iterate-products reaction)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create products iterator: %s" (indigo-get-last-error)))))

;;; Iterator Helper Functions

(defun indigo-map (func iterator)
  "Map FUNC over ITERATOR items, handling cleanup automatically.
Each item is freed after FUNC is applied to it."
  (let ((result nil)
        (item (indigo-next iterator)))
    (while (and item (/= item -1))
      (push (unwind-protect
                (funcall func item)
              (indigo-free item))
            result)
      (setq item (indigo-next iterator)))
    (nreverse result)))

;;; With-style Macros for Iterators

(defmacro indigo-with-atoms-iterator (binding &rest body)
  "Create atom iterator with automatic cleanup.

Usage: (indigo-with-atoms (VAR MOL-VAR) BODY...)

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms (atoms mol)
      (indigo-stream atoms)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding)))
    `(let ((,var (indigo-iterate-atoms ,mol-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-bonds-iterator (binding &rest body)
  "Create bond iterator with automatic cleanup.

Usage: (indigo-with-bonds (VAR MOL-VAR) BODY...)

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-bonds (bonds mol)
      (indigo-stream bonds)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding)))
    `(let ((,var (indigo-iterate-bonds ,mol-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-neighbors-iterator (binding &rest body)
  "Create neighbor iterator with automatic cleanup.

Usage: (indigo-with-neighbors (VAR ATOM-VAR) BODY...)

Example:
  (indigo-with-molecule (mol \"CCO\")
    (let ((atom (indigo-get-atom mol 0)))
      (indigo-with-neighbors (neighbors atom)
        (indigo-stream neighbors))))"
  (declare (indent 1))
  (let ((var (car binding))
        (atom-var (cadr binding)))
    `(let ((,var (indigo-iterate-neighbors ,atom-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-components-iterator (binding &rest body)
  "Create component iterator with automatic cleanup.

Usage: (indigo-with-components (VAR MOL-VAR) BODY...)

Example:
  (indigo-with-molecule (mol \"CCO.CC\")
    (indigo-with-components (components mol)
      (indigo-stream components)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding)))
    `(let ((,var (indigo-iterate-components ,mol-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-sssr-iterator (binding &rest body)
  "Create SSSR ring iterator with automatic cleanup.

Usage: (indigo-with-sssr (VAR MOL-VAR) BODY...)

Example:
  (indigo-with-molecule (mol \"c1ccccc1\")
    (indigo-with-sssr (rings mol)
      (indigo-stream rings)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding)))
    `(let ((,var (indigo-iterate-sssr ,mol-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-rings-iterator (binding &rest body)
  "Create ring iterator with size range and automatic cleanup.

Usage: (indigo-with-rings (VAR MOL-VAR MIN MAX) BODY...)

Example:
  (indigo-with-molecule (mol \"c1ccc2ccccc2c1\")
    (indigo-with-rings (rings mol 5 7)
      (indigo-stream rings)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding))
        (min (caddr binding))
        (max (cadddr binding)))
    `(let ((,var (indigo-iterate-rings ,mol-var ,min ,max)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-subtrees-iterator (binding &rest body)
  "Create subtree iterator with size range and automatic cleanup.

Usage: (indigo-with-subtrees (VAR MOL-VAR MIN MAX) BODY...)

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-subtrees (subtrees mol 1 3)
      (indigo-stream subtrees)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding))
        (min (caddr binding))
        (max (cadddr binding)))
    `(let ((,var (indigo-iterate-subtrees ,mol-var ,min ,max)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-stereocenters-iterator (binding &rest body)
  "Create stereocenter iterator with automatic cleanup.

Usage: (indigo-with-stereocenters (VAR MOL-VAR) BODY...)

Example:
  (indigo-with-molecule (mol \"C[C@H](O)CC\")
    (indigo-with-stereocenters (stereos mol)
      (indigo-stream stereos)))"
  (declare (indent 1))
  (let ((var (car binding))
        (mol-var (cadr binding)))
    `(let ((,var (indigo-iterate-stereocenters ,mol-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-reactants-iterator (binding &rest body)
  "Create reactant iterator with automatic cleanup.

Usage: (indigo-with-reactants (VAR RXN-VAR) BODY...)

Example:
  (indigo-with-reaction (rxn \"CCO+CC>>CCOCC\")
    (indigo-with-reactants (reactants rxn)
      (indigo-stream reactants)))"
  (declare (indent 1))
  (let ((var (car binding))
        (rxn-var (cadr binding)))
    `(let ((,var (indigo-iterate-reactants ,rxn-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(defmacro indigo-with-products-iterator (binding &rest body)
  "Create product iterator with automatic cleanup.

Usage: (indigo-with-products (VAR RXN-VAR) BODY...)

Example:
  (indigo-with-reaction (rxn \"CCO+CC>>CCOCC\")
    (indigo-with-products (products rxn)
      (indigo-stream products)))"
  (declare (indent 1))
  (let ((var (car binding))
        (rxn-var (cadr binding)))
    `(let ((,var (indigo-iterate-products ,rxn-var)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

(provide 'indigo-iter)

;;; indigo-iter.el ends here
