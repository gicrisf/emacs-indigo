;;; test-indigo-system.el --- Tests for Indigo system operations -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Tests for Indigo system operations including version info, session 
;; management, error handling, memory management, and option functions.

;;; Code:

(require 'ert)
(require 'indigo)

;;; System functions tests

(ert-deftest test-indigo-version ()
  "Test getting Indigo version."
  (let ((version (indigo-version)))
    (should (stringp version))
    (should (not (string-empty-p version)))))

(ert-deftest test-indigo-session-management ()
  "Test session allocation, switching, and release."
  (let ((session-id-1 (indigo-alloc-session-id))
        (session-id-2 (indigo-alloc-session-id)))
    (should (integerp session-id-1))
    (should (integerp session-id-2))
    (should (> session-id-1 0))
    (should (> session-id-2 0))
    (should (not (= session-id-1 session-id-2)))
    
    ;; Switch to first session and verify by loading a molecule
    (should (eq (indigo-set-session-id session-id-1) t))
    (let ((handle-1 (indigo-load-molecule-from-string "CCO")))
      (should (integerp handle-1))
      
      ;; Switch to second session and verify it's different
      (should (eq (indigo-set-session-id session-id-2) t))
      (let ((handle-2 (indigo-load-molecule-from-string "c1ccccc1")))
        (should (integerp handle-2))
        
        ;; Switch back to first session and verify handle still works
        (should (eq (indigo-set-session-id session-id-1) t))
        (let ((smiles-1 (indigo-canonical-smiles handle-1)))
          (should (stringp smiles-1)))
        
        ;; Switch back to second session and verify its handle works
        (should (eq (indigo-set-session-id session-id-2) t))
        (let ((smiles-2 (indigo-canonical-smiles handle-2)))
          (should (stringp smiles-2)))
        
        ;; Clean up
        (indigo-free handle-2))
      
      ;; Switch back to first session to clean up
      (should (eq (indigo-set-session-id session-id-1) t))
      (indigo-free handle-1))
    
    ;; Release both sessions
    (should (eq (indigo-release-session-id session-id-1) t))
    (should (eq (indigo-release-session-id session-id-2) t))))

(ert-deftest test-indigo-error-handling ()
  "Test error handling functions."
  ;; First clear any existing errors by getting the last error
  (indigo-get-last-error)
  
  ;; Induce an error by trying to use an invalid handle
  (condition-case nil
      (indigo-canonical-smiles -1) ; Invalid handle should cause an error
    (error nil)) ; Ignore the error, we just want it logged
  
  ;; Now check that we can retrieve the error message
  (let ((error (indigo-get-last-error)))
    (should (stringp error))
    (should (not (string-empty-p error)))
    (should (string-match-p "can not access object" error))))

(ert-deftest test-indigo-reference-counting ()
  "Test reference counting functions."
  (let ((count-before (indigo-count-references)))
    (should (integerp count-before))
    (should (>= count-before 0))
    
    ;; Load a molecule and check reference count increased
    (let ((handle (indigo-load-molecule-from-string "CCO")))
      (let ((count-after (indigo-count-references)))
        (should (> count-after count-before))
        (indigo-free handle)))))

(ert-deftest test-indigo-free-all-objects ()
  "Test freeing all objects in current session."
  ;; Load some molecules
  (let ((handle1 (indigo-load-molecule-from-string "CCO"))
        (handle2 (indigo-load-molecule-from-string "c1ccccc1")))

    ;; Counting
    (let ((count-before (indigo-count-references)))
      ;; (message "before: %s" count-before)
      (should (> count-before 0))

      ;; Free all objects
      (let ((result (indigo-free-all-objects)))
        (should (integerp result))

        ;; Should be zero now
        (let ((count-after (indigo-count-references)))
          ;; (message "after: %s" count-after)
          (should (= count-after 0)))))))

;;; Option function tests

(ert-deftest test-indigo-set-option ()
  "Test setting Indigo option with string value."
  (let ((result (indigo-set-option "render-output-format" "png")))
    (should (integerp result))))

(ert-deftest test-indigo-set-option-int ()
  "Test setting Indigo option with integer value."
  (let ((result (indigo-set-option-int "render-image-width" 400)))
    (should (integerp result))))

(ert-deftest test-indigo-set-option-bool ()
  "Test setting Indigo option with boolean value."
  (let ((result (indigo-set-option-bool "render-implicit-hydrogens-visible" 1)))
    (should (integerp result))))

(ert-deftest test-indigo-set-option-float ()
  "Test setting Indigo option with float value."
  (let ((result (indigo-set-option-float "render-bond-length" 20.0)))
    (should (integerp result))))

(ert-deftest test-indigo-set-option-color ()
  "Test setting Indigo option with RGB color values."
  (let ((result (indigo-set-option-color "render-background-color" 1.0 1.0 1.0)))
    (should (integerp result))))

(ert-deftest test-indigo-set-option-xy ()
  "Test setting Indigo option with X,Y coordinate values."
  (let ((result (indigo-set-option-xy "render-image-size" 400 300)))
    (should (integerp result)))
  
  ;; Test error handling with invalid option names
  (let ((result1 (indigo-set-option "invalid-option-name" "value"))
        (result2 (indigo-set-option-int "another-invalid-option" 42)))
    (should (integerp result1))
    (should (integerp result2))))

(ert-deftest test-indigo-option-error-handling ()
  "Test option functions with invalid option names."
  (let ((result1 (indigo-set-option "invalid-option-name" "value"))
        (result2 (indigo-set-option-int "another-invalid-option" 42)))
    (should (integerp result1))
    (should (integerp result2))))

(provide 'test-indigo-system)

;;; test-indigo-system.el ends here