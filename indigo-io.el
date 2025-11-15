;;; indigo-io.el --- I/O functions for Indigo chemical objects -*- lexical-binding: t; -*-

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

;; This module provides I/O functions for Indigo chemical objects.

;;; Code:

;;; Forward declarations for C module functions

(declare-function indigo-get-last-error "indigo-module")

;; Creation functions from C module
(declare-function indigo--create-molecule "indigo-module")
(declare-function indigo--create-query-molecule "indigo-module")

;; Loading functions from C module
(declare-function indigo--load-molecule-from-string "indigo-module")
(declare-function indigo--load-molecule-from-file "indigo-module")
(declare-function indigo--load-query-molecule-from-string "indigo-module")
(declare-function indigo--load-query-molecule-from-file "indigo-module")
(declare-function indigo--load-smarts-from-string "indigo-module")
(declare-function indigo--load-smarts-from-file "indigo-module")

;;; Molecule Creation Functions

(defun indigo-create-molecule ()
  "Create an empty molecule.
Signals an error if creation fails."
  (let ((handle (indigo--create-molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create molecule: %s" (indigo-get-last-error)))))

(defun indigo-create-query-molecule ()
  "Create an empty query molecule.
Signals an error if creation fails."
  (let ((handle (indigo--create-query-molecule)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to create query molecule: %s" (indigo-get-last-error)))))

;;; Molecule Loading Functions

(defun indigo-load-molecule-from-string (string)
  "Load molecule from STRING.
STRING can be in SMILES, MOL, or other supported formats.
Signals an error if loading fails."
  (let ((handle (indigo--load-molecule-from-string string)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load molecule from string: %s" (indigo-get-last-error)))))

(defun indigo-load-molecule-from-file (filename)
  "Load molecule from FILENAME.
Signals an error if loading fails or file doesn't exist."
  (unless (file-exists-p filename)
    (error "File does not exist: %s" filename))
  (let ((handle (indigo--load-molecule-from-file filename)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load molecule from file %s: %s"
             filename (indigo-get-last-error)))))

;;; Query Molecule Loading Functions

(defun indigo-load-query-molecule-from-string (string)
  "Load query molecule from STRING.
Signals an error if loading fails."
  (let ((handle (indigo--load-query-molecule-from-string string)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load query molecule from string: %s" (indigo-get-last-error)))))

(defun indigo-load-query-molecule-from-file (filename)
  "Load query molecule from FILENAME.
Signals an error if loading fails or file doesn't exist."
  (unless (file-exists-p filename)
    (error "File does not exist: %s" filename))
  (let ((handle (indigo--load-query-molecule-from-file filename)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load query molecule from file %s: %s"
             filename (indigo-get-last-error)))))

;;; SMARTS Loading Functions

(defun indigo-load-smarts-from-string (string)
  "Load SMARTS pattern from STRING.
Signals an error if loading fails."
  (let ((handle (indigo--load-smarts-from-string string)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load SMARTS from string: %s" (indigo-get-last-error)))))

(defun indigo-load-smarts-from-file (filename)
  "Load SMARTS pattern from FILENAME.
Signals an error if loading fails or file doesn't exist."
  (unless (file-exists-p filename)
    (error "File does not exist: %s" filename))
  (let ((handle (indigo--load-smarts-from-file filename)))
    (if (and handle (> handle 0))
        handle
      (error "Failed to load SMARTS from file %s: %s"
             filename (indigo-get-last-error)))))

(provide 'indigo-io)

;;; indigo-io.el ends here
