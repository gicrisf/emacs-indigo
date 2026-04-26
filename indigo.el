;;; indigo.el --- Emacs interface to the Indigo cheminformatics library -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Version: 0.11.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: data tools extensions
;; URL: https://github.com/gicrisf/emacs-indigo

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
;; directly with Indigo object handles, and high-level `indigo-with-*`
;; macros for automatic resource management (distributed across modules:
;; indigo-mol, indigo-io, indigo-iterator, indigo-render).

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

;;; Installation

(defvar indigo-install-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where emacs-indigo is installed.")

(defvar indigo--available-platforms
  '("linux-x86_64")
  "List of platforms with pre-built Indigo binaries.
Each entry corresponds to a supported platform for downloading
pre-compiled Indigo libraries.")

(defun indigo-install (&optional no-confirm)
  "Install the Indigo library and build the dynamic module.

This function runs the installation scripts to:
1. Download and build dependencies (zlib, TinyXML)
2. Download and extract the Indigo cheminformatics library
3. Compile the Emacs dynamic module

Prompts for target platform from `indigo--available-platforms'.
With prefix argument NO-CONFIRM, skip the confirmation prompt.

After installation, use `indigo-doctor' to verify."
  (interactive "P")
  (let* ((pkg-dir indigo-install-directory)
         (install-script (expand-file-name "install.sh" pkg-dir))
         (buffer-name "*indigo-install*")
         (platform (completing-read "Select platform: "
                                    indigo--available-platforms nil t)))
    (unless (file-exists-p install-script)
      (error "Installation script not found at %s" install-script))
    (when (or no-confirm
              (yes-or-no-p "This will download and compile the Indigo library. Continue? "))
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "=== Installing Emacs Indigo ===\n"))
          (insert (format "Platform: %s\n" platform))
          (insert (format "Working directory: %s\n\n" pkg-dir))))
      (pop-to-buffer buffer-name)
      (let ((default-directory pkg-dir)
            (process-environment (cons "TERM=dumb" process-environment)))
        (set-process-sentinel
         (start-process "indigo-install" buffer-name "bash" install-script platform)
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (with-current-buffer (process-buffer process)
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (if (zerop (process-exit-status process))
                     (progn
                       (insert "\n\n=== Installation successful! ===\n")
                       (insert "Run M-x indigo-doctor to verify.\n")
                       (insert "Then restart Emacs or eval (require 'indigo) to load.\n")
                       (message "Indigo installation completed successfully!"))
                   (insert "\n\n=== Installation failed! ===\n")
                   (insert "Check the output above for errors.\n")
                   (message "Indigo installation failed. See *indigo-install* buffer.")))))))))))

(defun indigo-doctor ()
  "Check if Indigo is properly installed and the module can be loaded.

Returns t if everything is OK, nil otherwise."
  (interactive)
  (let* ((pkg-dir indigo-install-directory)
         (module-path (expand-file-name "build/indigo-module.so" pkg-dir))
         (indigo-dir (expand-file-name "indigo-install" pkg-dir))
         (deps-dir (expand-file-name "deps-install" pkg-dir))
         (issues nil))
    ;; Check dependencies
    (unless (file-exists-p (expand-file-name "lib/libz.a" deps-dir))
      (push "zlib not found in deps-install/lib/" issues))
    (unless (file-exists-p (expand-file-name "lib/libtinyxml.a" deps-dir))
      (push "TinyXML not found in deps-install/lib/" issues))
    ;; Check Indigo library
    (unless (file-exists-p (expand-file-name "lib/libindigo-static.a" indigo-dir))
      (push "Indigo library not found in indigo-install/lib/" issues))
    (unless (file-exists-p (expand-file-name "include/indigo.h" indigo-dir))
      (push "Indigo headers not found in indigo-install/include/" issues))
    ;; Check module
    (unless (file-exists-p module-path)
      (push "Dynamic module (indigo-module.so) not found in build/" issues))
    ;; Report results
    (if issues
        (progn
          (message "Indigo installation issues:\n  - %s\nRun M-x indigo-install to fix."
                   (mapconcat #'identity (nreverse issues) "\n  - "))
          nil)
      (message "Indigo installation OK: all components found.")
      t)))

(defun indigo-installed-p ()
  "Return non-nil if Indigo appears to be installed."
  (let* ((pkg-dir indigo-install-directory)
         (module-path (expand-file-name "build/indigo-module.so" pkg-dir)))
    (file-exists-p module-path)))

;;; Module Loading

(defvar indigo--module-loaded nil
  "Non-nil if the Indigo dynamic module has been loaded.")

(defun indigo-load-module ()
  "Load the indigo dynamic module and submodules.

If the module is not built, prints a message directing the user
to run `indigo-install'.  Returns non-nil if loading succeeded."
  (interactive)
  (if indigo--module-loaded
      t
    (let* ((pkg-dir indigo-install-directory)
           (module-path (expand-file-name "build/indigo-module.so" pkg-dir))
           (indigo-dir (expand-file-name "indigo-install" pkg-dir)))
      (cond
       ;; Module exists - load it
       ((file-exists-p module-path)
        (module-load module-path)
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
        ;; Generate star macro versions after all base macros are defined
        (require 'indigo-with-star-defs)
        (setq indigo--module-loaded t)
        (message "Indigo module loaded successfully.")
        t)
       ;; Indigo library installed but module not built - try building
       ((file-directory-p indigo-dir)
        (message "Indigo module not found, building...")
        (let ((default-directory pkg-dir))
          (if (zerop (call-process "make" nil "*indigo-build*" nil "module"))
              (indigo-load-module)  ; Retry after building
            (message "Failed to build indigo module. Check *indigo-build* buffer.")
            nil)))
       ;; Nothing installed
       (t
        (message "Indigo not installed. Run M-x indigo-install first.")
        nil)))))

;; Try to load the module, but don't error if it's not available
;; This allows the package to be loaded for running indigo-install
(indigo-load-module)

(provide 'indigo)

;;; indigo.el ends here
