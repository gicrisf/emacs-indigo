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
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: chemistry, cheminformatics, molecules
;; URL: https://github.com/gicrisf/emacs-indigo

;;; Commentary:

;; This package provides Emacs Lisp bindings for the Indigo toolkit,
;; enabling molecular structure manipulation, chemical file I/O,
;; and cheminformatics operations from within Emacs.

;;; Code:

;; Load the compiled module
(defun indigo-load-module ()
  "Load the indigo dynamic module."
  (let ((module-path (expand-file-name "build/indigo-module.so"
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))
    (when (file-exists-p module-path)
      (module-load module-path))))

;; Load the module when this file is loaded
(indigo-load-module)

(provide 'indigo)

;;; indigo.el ends here