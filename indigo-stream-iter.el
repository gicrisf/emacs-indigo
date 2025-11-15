;;; indigo-stream-iter.el --- Stream-iterator bridge macros for Indigo -*- lexical-binding: t; -*-

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
;;
;; Author: Giovanni Crisalfi
;;
;;; Commentary:

;; This module provides high-level stream wrapper macros that combine
;; iterator creation from `indigo-iter.el' with stream abstraction from
;; `indigo-stream.el'.
;;
;; Instead of nesting iterator and stream macros:
;;   (indigo-with-molecule (mol "CCO")
;;     (indigo-with-atoms-iterator (atoms mol)
;;       (indigo-with-stream-from-iterator (stream atoms)
;;         ...)))
;;
;; You can use the convenient one-liner:
;;   (indigo-with-molecule (mol "CCO")
;;     (indigo-with-atoms-stream (stream mol)
;;       ...))
;;
;; Available stream macros:
;; - Molecule-based (8 types):
;;   - `indigo-with-atoms-stream'
;;   - `indigo-with-bonds-stream'
;;   - `indigo-with-components-stream'
;;   - `indigo-with-sssr-stream'
;;   - `indigo-with-rings-stream' (requires MIN-ATOMS MAX-ATOMS)
;;   - `indigo-with-subtrees-stream' (requires MIN-ATOMS MAX-ATOMS)
;;   - `indigo-with-stereocenters-stream'
;;
;; - Atom-based (1 type):
;;   - `indigo-with-neighbors-stream'
;;
;; - Reaction-based (2 types):
;;   - `indigo-with-reactants-stream'
;;   - `indigo-with-products-stream'

;;; Code:

(require 'indigo-iter)
(require 'indigo-stream)

;;; High-level Stream Macros

(defmacro indigo-with-atoms-stream (binding &rest body)
  "Create stream from atoms iterator with automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (iter-var (gensym "atoms-iter-")))
    `(indigo-with-atoms-iterator (,iter-var ,mol-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-bonds-stream (binding &rest body)
  "Create stream from bonds iterator with automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-bonds-stream (stream mol)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (iter-var (gensym "bonds-iter-")))
    `(indigo-with-bonds-iterator (,iter-var ,mol-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-neighbors-stream (binding &rest body)
  "Create stream from neighbors iterator with automatic cleanup.
BINDING is (STREAM-VAR ATOM-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((atom (indigo-next atoms)))
        (indigo-with-neighbors-stream (stream atom)
          (indigo-stream-car stream)))))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (atom-var (cadr binding))
        (iter-var (gensym "neighbors-iter-")))
    `(indigo-with-neighbors-iterator (,iter-var ,atom-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-components-stream (binding &rest body)
  "Create stream from components iterator with automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"C.C.C\")
    (indigo-with-components-stream (stream mol)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (iter-var (gensym "components-iter-")))
    `(indigo-with-components-iterator (,iter-var ,mol-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-sssr-stream (binding &rest body)
  "Create stream from SSSR rings iterator with automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"c1ccccc1\")
    (indigo-with-sssr-stream (stream mol)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (iter-var (gensym "sssr-iter-")))
    `(indigo-with-sssr-iterator (,iter-var ,mol-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-rings-stream (binding &rest body)
  "Create stream from rings iterator with size range and automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR MIN-ATOMS MAX-ATOMS).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"c1ccccc1\")
    (indigo-with-rings-stream (stream mol 3 7)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (min (caddr binding))
        (max (cadddr binding))
        (iter-var (gensym "rings-iter-")))
    `(indigo-with-rings-iterator (,iter-var ,mol-var ,min ,max)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-subtrees-stream (binding &rest body)
  "Create stream from subtrees iterator with size range and automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR MIN-ATOMS MAX-ATOMS).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-subtrees-stream (stream mol 1 3)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (min (caddr binding))
        (max (cadddr binding))
        (iter-var (gensym "subtrees-iter-")))
    `(indigo-with-subtrees-iterator (,iter-var ,mol-var ,min ,max)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-stereocenters-stream (binding &rest body)
  "Create stream from stereocenters iterator with automatic cleanup.
BINDING is (STREAM-VAR MOL-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-molecule (mol \"C[C@H](O)N\")
    (indigo-with-stereocenters-stream (stream mol)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (mol-var (cadr binding))
        (iter-var (gensym "stereocenters-iter-")))
    `(indigo-with-stereocenters-iterator (,iter-var ,mol-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-reactants-stream (binding &rest body)
  "Create stream from reactants iterator with automatic cleanup.
BINDING is (STREAM-VAR RXN-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-reaction (rxn \"CCO>>CC=O\")
    (indigo-with-reactants-stream (stream rxn)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (rxn-var (cadr binding))
        (iter-var (gensym "reactants-iter-")))
    `(indigo-with-reactants-iterator (,iter-var ,rxn-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(defmacro indigo-with-products-stream (binding &rest body)
  "Create stream from products iterator with automatic cleanup.
BINDING is (STREAM-VAR RXN-VAR).
BODY forms are executed with STREAM-VAR bound to the stream.
All forced elements and the iterator are freed on exit.

Example:
  (indigo-with-reaction (rxn \"CCO>>CC=O\")
    (indigo-with-products-stream (stream rxn)
      (indigo-stream-car stream)))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (rxn-var (cadr binding))
        (iter-var (gensym "products-iter-")))
    `(indigo-with-products-iterator (,iter-var ,rxn-var)
       (indigo-with-stream-from-iterator (,stream-var ,iter-var)
         ,@body))))

(provide 'indigo-stream-iter)

;;; indigo-stream-iter.el ends here
