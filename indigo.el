;;; indigo.el --- Emacs interface to the Indigo cheminformatics library -*- lexical-binding: t; -*-

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
;; Version: 0.8.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: data tools extensions
;; URL: https://github.com/gicrisf/emacs-indigo

;;; Commentary:

;; This package provides Emacs Lisp bindings for the Indigo toolkit,
;; enabling molecular structure manipulation, chemical file I/O,
;; and cheminformatics operations from within Emacs.
;;
;; The package is organized into modular components:
;; - Core module loading and resource management (indigo.el)
;; - Atom property functions and enums (indigo-atom.el)
;; - Bond property functions and enums (indigo-bond.el)
;; - Chemical object I/O and creation (indigo-io.el)
;; - Iterator functions (indigo-iterator.el)
;; - Lazy stream abstraction (indigo-stream.el)
;; - Molecule operations and search (indigo-mol.el)
;; - Reaction operations and mapping (indigo-reaction.el)
;; - Rendering and visualization (indigo-render.el)
;;
;; The package provides both low-level stateful functions that work
;; directly with Indigo object handles, and high-level abstractions
;; including:
;; - `indigo-let` and `indigo-let*` macros for universal resource management
;; - `indigo-with-*` macros for classical with-style resource management
;;   (distributed across modules: indigo-mol, indigo-io, indigo-iterator, indigo-render)

;;; Code:

(require 'cl-lib)

;;; Forward declarations for C module functions
;;
;; Only cross-cutting core functions are declared here.
;; Domain-specific functions (atoms, bonds, iterators, molecules, etc.)
;; are declared in their respective modules.

;; Core memory and error functions used across all modules
(declare-function indigo-free "indigo-module")
(declare-function indigo-clone "indigo-module")
(declare-function indigo-get-last-error "indigo-module")

;;; Module Loading

;; Load the compiled module
(defun indigo-load-module ()
  "Load the indigo dynamic module, building it first if necessary."
  (let* ((pkg-dir (file-name-directory (or load-file-name buffer-file-name)))
         (module-path (expand-file-name "build/indigo-module.so" pkg-dir))
         (indigo-dir (expand-file-name "indigo-install" pkg-dir)))
    ;; Check if Indigo library is installed
    (unless (file-directory-p indigo-dir)
      (error "Indigo library not found. Run './install.sh' in %s" pkg-dir))
    ;; Build module if it doesn't exist
    (unless (file-exists-p module-path)
      (message "Indigo module not found, building...")
      (let ((default-directory pkg-dir))
        (unless (zerop (call-process "make" nil "*indigo-build*" nil))
          (error "Failed to build indigo module. Check *indigo-build* buffer for details"))))
    ;; Load the module
    (if (file-exists-p module-path)
        (module-load module-path)
      (error "Indigo module not found at %s. Run 'make' in %s" module-path pkg-dir))))

;; Load the module when this file is loaded
(indigo-load-module)

;; Load module components
(require 'indigo-bond)
(require 'indigo-atom)
(require 'indigo-io)
(require 'indigo-iter)
(require 'indigo-stream)
(require 'indigo-stream-iter)
(require 'indigo-render)
(require 'indigo-mol)
(require 'indigo-react)
(require 'indigo-let)

(provide 'indigo)

;;; indigo.el ends here
