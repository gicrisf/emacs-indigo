;;; indigo-render.el --- Rendering and visualization functions for Indigo -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This module provides rendering and visualization functions for Indigo.
;; It includes functions for:
;;
;; - Creating arrays for grid rendering
;; - Creating file and buffer writers for output
;; - Rendering molecules and reactions to files and buffers
;; - Grid rendering for multiple molecules
;;
;; All functions include proper error handling and validation.

;;; Code:

;;; Forward declarations

(declare-function indigo-get-last-error "indigo-module")

;; Array functions
(declare-function indigo--create-array "indigo-module")

;; Writer functions - NOT YET IMPLEMENTED in C module
;; (declare-function indigo--create-file-writer "indigo-module")
;; (declare-function indigo--create-buffer-writer "indigo-module")

;; Exposed
(declare-function indigo-render "indigo-module")
(declare-function indigo-render-to-file "indigo-module")
(declare-function indigo-render-grid "indigo-module")
(declare-function indigo-render-grid-to-file "indigo-module")
(declare-function indigo-render-reset "indigo-module")

;;; Array Creation Functions

(defun indigo-create-array ()
  "Create an empty array object.
Arrays are used for grid rendering with multiple molecules.
Signals an error if creation fails."
  (let ((handle (indigo--create-array)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create array: %s" (indigo-get-last-error)))))

;;; Writer Creation Functions

;; NOT YET IMPLEMENTED - C module functions missing
;; (defun indigo-create-file-writer (filename)
;;   "Create a file writer for FILENAME.
;; File writers are used to direct rendering output to files.
;; Signals an error if creation fails."
;;   (let ((handle (indigo--create-file-writer filename)))
;;     (if (and handle (> handle 0))
;;         handle
;;       (error "Failed to create file writer for %s: %s"
;;              filename (indigo-get-last-error)))))

;; (defun indigo-create-buffer-writer ()
;;   "Create a buffer writer for in-memory output.
;; Buffer writers are used to capture rendering output in memory.
;; Signals an error if creation fails."
;;   (let ((handle (indigo--create-buffer-writer)))
;;     (if (and handle (> handle 0))
;;         handle
;;       (error "Failed to create buffer writer: %s" (indigo-get-last-error)))))

;;; With-style Macros for Rendering

(defmacro indigo-with-array (binding &rest body)
  "Create empty array with automatic cleanup.

Usage: (indigo-with-array (VAR) BODY...)

Example:
  (indigo-with-array (arr)
    (indigo-with-molecule (mol \"CCO\")
      (indigo-array-add arr mol))
    arr)"
  (declare (indent 1))
  (let ((var (car binding)))
    `(let ((,var (indigo-create-array)))
       (unwind-protect
           (progn ,@body)
         (when ,var (indigo-free ,var))))))

;; Auto-generate sequential binding version
(define-indigo-with* "array")

;; NOT YET IMPLEMENTED - C module functions missing
;; (defmacro indigo-with-file-writer (binding &rest body)
;;   "Create file writer with automatic cleanup.

;; Usage: (indigo-with-file-writer (VAR FILENAME) BODY...)

;; Example:
;;   (indigo-with-file-writer (writer \"output.sdf\")
;;     (indigo-with-molecule (mol \"CCO\")
;;       (indigo-sdf-append writer mol)))"
;;   (declare (indent 1))
;;   (let ((var (car binding))
;;         (filename (cadr binding)))
;;     `(let ((,var (indigo-create-file-writer ,filename)))
;;        (unwind-protect
;;            (progn ,@body)
;;          (when ,var (indigo-free ,var))))))

;; (defmacro indigo-with-buffer-writer (binding &rest body)
;;   "Create buffer writer with automatic cleanup.

;; Usage: (indigo-with-buffer-writer (VAR) BODY...)

;; Example:
;;   (indigo-with-buffer-writer (writer)
;;     (indigo-with-molecule (mol \"CCO\")
;;       (indigo-sdf-append writer mol))
;;     (indigo-buffer-writer-to-string writer))"
;;   (declare (indent 1))
;;   (let ((var (car binding)))
;;     `(let ((,var (indigo-create-buffer-writer)))
;;        (unwind-protect
;;            (progn ,@body)
;;          (when ,var (indigo-free ,var))))))

(provide 'indigo-render)

;;; indigo-render.el ends here
