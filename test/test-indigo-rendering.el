;;; test-indigo-rendering.el --- Tests for Indigo rendering operations -*- lexical-binding: t; -*-

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

;; Tests for Indigo rendering operations including writer creation, array
;; management, molecule rendering, grid rendering, and configuration functions.
;; These tests cover the complete rendering workflow from output creation to
;; final image generation.

;;; Code:

(require 'ert)
(require 'indigo)

;;; Writer and Output Functions Tests

(ert-deftest test-indigo-write-buffer ()
  "Test creating buffer writer objects."
  (let ((writer (indigo-write-buffer)))
    (should (integerp writer))
    (should (> writer 0))
    ;; Clean up
    (indigo-free writer)))

(ert-deftest test-indigo-write-file ()
  "Test creating file writer objects."
  (let* ((temp-file (make-temp-file "indigo-test-" nil ".png"))
         (writer (indigo-write-file temp-file)))
    (unwind-protect
        (progn
          (should (integerp writer))
          (should (> writer 0))
          ;; Clean up
          (indigo-free writer))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-indigo-to-buffer ()
  "Test converting buffer writer to string content."
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (let ((writer (indigo-write-buffer)))
          (unwind-protect
              (progn
                ;; Set rendering options for SVG output
                (should (eq (indigo-set-option "render-output-format" "svg") 1))
                ;; Render molecule to buffer
                (should (eq (indigo-render mol writer) 1))
                ;; Get buffer contents
                (let ((content (indigo-to-buffer writer)))
                  (should (stringp content))
                  (should (not (string-empty-p content)))
                  ;; Should contain SVG content
                  (should (string-match-p "<svg" content))))
            (indigo-free writer)))
      (indigo-free mol))))

;; ;;; Array Functions Tests

(ert-deftest test-indigo-create-array ()
  "Test creating empty arrays for grid rendering."
  (let ((array (indigo-create-array)))
    (should (integerp array))
    (should (> array 0))
    ;; Clean up
    (indigo-free array)))

(ert-deftest test-indigo-array-add ()
  "Test adding molecules to arrays."
  (let ((array (indigo-create-array))
        (mol1 (indigo-load-molecule-from-string "CCO"))
        (mol2 (indigo-load-molecule-from-string "c1ccccc1")))
    (unwind-protect
        (progn
          ;; Add molecules to array
          (should (eq (indigo-array-add array mol1) 1))
          (should (eq (indigo-array-add array mol2) 1))
          ;; Verify array can be used for iteration
          (let ((iter (indigo-iterate-array array))
                (count 0))
            (unwind-protect
                (progn
                  (let ((item (indigo-next iter)))
                    (while item
                      (setq count (1+ count))
                      (setq item (indigo-next iter))))
                  (should (eq count 2)))
              (indigo-free iter))))
      (indigo-free mol1)
      (indigo-free mol2)
      (indigo-free array))))

;;; Basic Rendering Tests

(ert-deftest test-indigo-render ()
  "Test basic molecule rendering to buffer."
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (let ((writer (indigo-write-buffer)))
          (unwind-protect
              (progn
                ;; Set rendering options
                (should (eq (indigo-set-option "render-output-format" "svg") 1))
                (should (eq (indigo-set-option-int "render-image-width" 300) 1))
                (should (eq (indigo-set-option-int "render-image-height" 200) 1))
                ;; Render molecule
                (let ((result (indigo-render mol writer)))
                  (should (eq result 1))
                  ;; Verify output contains content
                  (let ((content (indigo-to-buffer writer)))
                    (should (stringp content))
                    (should (not (string-empty-p content))))))
            (indigo-free writer)))
      (indigo-free mol))))

(ert-deftest test-indigo-render-to-file ()
  "Test direct file rendering."
  (let* ((mol (indigo-load-molecule-from-string "c1ccccc1"))
         (temp-file (make-temp-file "indigo-molecule-" nil ".png")))
    (unwind-protect
        (progn
          ;; Set rendering options for PNG
          (should (eq (indigo-set-option "render-output-format" "png") 1))
          (should (eq (indigo-set-option-int "render-image-width" 400) 1))
          (should (eq (indigo-set-option-int "render-image-height" 300) 1))
          ;; Render directly to file
          (let ((result (indigo-render-to-file mol temp-file)))
            (should (eq result 1))
            ;; File should exist and have content
            (should (file-exists-p temp-file))
            (should (> (file-attribute-size (file-attributes temp-file)) 0))))
      (indigo-free mol)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Grid Rendering Tests

(ert-deftest test-indigo-render-grid ()
  "Test grid rendering to buffer."
  (let ((array (indigo-create-array))
        (mol1 (indigo-load-molecule-from-string "CCO"))
        (mol2 (indigo-load-molecule-from-string "c1ccccc1"))
        (mol3 (indigo-load-molecule-from-string "CC(C)O")))
    (unwind-protect
        (progn
          ;; Add molecules to array
          (should (eq (indigo-array-add array mol1) 1))
          (should (eq (indigo-array-add array mol2) 1))
          (should (eq (indigo-array-add array mol3) 1))

          (let ((writer (indigo-write-buffer)))
            (unwind-protect
                (progn
                  ;; Set rendering options
                  (should (eq (indigo-set-option "render-output-format" "svg") 1))
                  (should (eq (indigo-set-option-int "render-image-width" 600) 1))
                  (should (eq (indigo-set-option-int "render-image-height" 400) 1))
                  ;; Render grid with 2 columns, no ref atoms specified (nil)
                  (let ((result (indigo-render-grid array nil 2 writer)))
                    (should (eq result 1))
                    ;; Verify output
                    (let ((content (indigo-to-buffer writer)))
                      (should (stringp content))
                      (should (not (string-empty-p content)))
                      (should (string-match-p "<svg" content)))))
              (indigo-free writer))))
      (indigo-free mol1)
      (indigo-free mol2)
      (indigo-free mol3)
      (indigo-free array))))

(ert-deftest test-indigo-render-grid-to-file ()
  "Test grid rendering directly to file."
  (let ((array (indigo-create-array))
        (mol1 (indigo-load-molecule-from-string "CCO"))
        (mol2 (indigo-load-molecule-from-string "CCC"))
        (temp-file (make-temp-file "indigo-grid-" nil ".svg")))
    (unwind-protect
        (progn
          ;; Add molecules to array
          (should (eq (indigo-array-add array mol1) 1))
          (should (eq (indigo-array-add array mol2) 1))

          ;; Set rendering options
          (should (eq (indigo-set-option "render-output-format" "svg") 1))
          (should (eq (indigo-set-option-int "render-image-width" 500) 1))

          ;; Render grid directly to file with 1 column
          (let ((result (indigo-render-grid-to-file array nil 1 temp-file)))
            (should (eq result 1))
            ;; File should exist and contain SVG content
            (should (file-exists-p temp-file))
            (should (> (file-attribute-size (file-attributes temp-file)) 0))
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "<svg" (buffer-string))))))
      (indigo-free mol1)
      (indigo-free mol2)
      (indigo-free array)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Rendering Configuration Tests

(ert-deftest test-indigo-render-reset ()
  "Test resetting rendering options to defaults."
  (progn
    ;; Set some custom options
    (should (eq (indigo-set-option "render-output-format" "png") 1))
    (should (eq (indigo-set-option-int "render-image-width" 1000) 1))
    (should (eq (indigo-set-option-color "render-background-color" 1.0 0.5 0.0) 1))

    ;; Reset rendering options
    (let ((result (indigo-render-reset)))
      (should (eq result 1)))

    ;; Verify reset worked by rendering a simple molecule
    (let ((mol (indigo-load-molecule-from-string "CCO")))
      (unwind-protect
          (let ((writer (indigo-write-buffer)))
            (unwind-protect
                (progn
                  (should (eq (indigo-render mol writer) 1))
                  (let ((content (indigo-to-buffer writer)))
                    (should (stringp content))
                    (should (not (string-empty-p content)))))
              (indigo-free writer)))
        (indigo-free mol)))))

;;; Integration Tests with Real Chemical Data

(ert-deftest test-indigo-render-complex-molecule ()
  "Test rendering complex molecules with various features."
  (let ((test-molecules '("CC(=O)OC1=CC=CC=C1C(=O)O"  ; aspirin
                          "CC1=CC=C(C=C1)C(C)C(=O)O"   ; ibuprofen
                          "c1ccc2c(c1)ccc3c2ccc4c3cccc4" ; anthracene
                          "C[C@H](C(=O)N[C@@H](CC1=CC=CC=C1)C(=O)O)N"))) ; phenylalanine
    (dolist (smiles test-molecules)
      (let ((mol (indigo-load-molecule-from-string smiles)))
        (when mol  ; Some molecules might fail to load in certain Indigo versions
          (unwind-protect
              (let ((temp-file (make-temp-file "indigo-complex-" nil ".svg")))
                (unwind-protect
                    (progn
                      ;; Set high-quality rendering options
                      (should (eq (indigo-set-option "render-output-format" "svg") 1))
                      (should (eq (indigo-set-option-int "render-image-width" 400) 1))
                      (should (eq (indigo-set-option-int "render-image-height" 300) 1))
                      (should (eq (indigo-set-option-float "render-bond-length" 30.0) 1))

                      ;; Render molecule
                      (let ((result (indigo-render-to-file mol temp-file)))
                        (should (eq result 1))
                        (should (file-exists-p temp-file))
                        (should (> (file-attribute-size (file-attributes temp-file)) 100))))
                  (when (file-exists-p temp-file)
                    (delete-file temp-file))))
            (indigo-free mol)))))))

;;; Error Handling Tests

(ert-deftest test-indigo-render-error-handling ()
  "Test rendering error handling with invalid inputs."
  ;; Test rendering with invalid molecule handle
  (should-error (indigo-render -1 (indigo-write-buffer)))

  ;; Test rendering with invalid writer handle
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (should-error (indigo-render mol -1))
      (indigo-free mol)))

  ;; Test array operations with invalid handles
  (should-error (indigo-array-add -1 (indigo-load-molecule-from-string "CCO")))

  ;; Test file rendering with invalid path
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (let ((result (indigo-render-to-file mol "/invalid/path/molecule.png")))
          ;; Should fail gracefully (return error code, not crash)
          (should (or (eq result 0) (eq result -1))))
      (indigo-free mol))))

;;; Performance and Memory Tests

(ert-deftest test-indigo-render-memory-management ()
  "Test proper memory management during rendering operations."
  (let ((initial-refs (indigo-count-references)))
    ;; Perform multiple render operations
    (dotimes (i 5)
      (let ((mol (indigo-load-molecule-from-string "c1ccccc1"))
            (writer (indigo-write-buffer)))
        (unwind-protect
            (progn
              (should (eq (indigo-render mol writer) 1))
              (let ((content (indigo-to-buffer writer)))
                (should (stringp content))))
          (indigo-free mol)
          (indigo-free writer))))

    ;; Memory should be properly cleaned up
    (let ((final-refs (indigo-count-references)))
      (should (eq initial-refs final-refs)))))

;;; HDC Rendering Tests (Platform-specific placeholder)

(ert-deftest test-indigo-render-write-hdc ()
  "Test HDC writer creation (placeholder for Windows-specific functionality)."
  ;; HDC rendering is Windows-specific and currently returns nil
  ;; This test verifies the function exists and handles the call gracefully
  (let ((result (indigo-render-write-hdc nil 0)))
    (should (or (null result) (integerp result)))))

;;; Option Configuration Tests for Rendering

(ert-deftest test-indigo-rendering-options ()
  "Test various rendering options and their effects."
  (let ((mol (indigo-load-molecule-from-string "c1ccccc1")))
    (unwind-protect
        (progn
          ;; Test different output formats
          (dolist (format '("svg" "png"))
            (should (eq (indigo-set-option "render-output-format" format) 1))
            (let ((writer (indigo-write-buffer)))
              (unwind-protect
                  (progn
                    (should (eq (indigo-render mol writer) 1))
                    (let ((content (indigo-to-buffer writer)))
                      (should (stringp content))
                      (should (not (string-empty-p content)))))
                (indigo-free writer))))

          ;; Test dimension options
          (should (eq (indigo-set-option-int "render-image-width" 200) 1))
          (should (eq (indigo-set-option-int "render-image-height" 150) 1))

          ;; Test color options
          (should (eq (indigo-set-option-color "render-background-color" 1.0 1.0 1.0) 1))

          ;; Test coordinate options
          (should (eq (indigo-set-option-xy "render-grid-margins" 10 15) 1))

          ;; Test float options
          (should (eq (indigo-set-option-float "render-bond-length" 25.5) 1)))
      (indigo-free mol))))

;;; Grid Rendering with Reference Atoms

(ert-deftest test-indigo-render-grid-with-ref-atoms ()
  "Test grid rendering with reference atom highlighting."
  (let ((array (indigo-create-array))
        (mol1 (indigo-load-molecule-from-string "CCO"))
        (mol2 (indigo-load-molecule-from-string "CCC")))
    (unwind-protect
        (progn
          ;; Add molecules to array
          (should (eq (indigo-array-add array mol1) 1))
          (should (eq (indigo-array-add array mol2) 1))

          ;; Create reference atoms list (first atom of each molecule)
          (let ((ref-atoms '(0 0))  ; First atom index for both molecules
                (temp-file (make-temp-file "indigo-ref-atoms-" nil ".svg")))
            (unwind-protect
                (progn
                  ;; Set rendering options
                  (should (eq (indigo-set-option "render-output-format" "svg") 1))

                  ;; Render grid with reference atoms
                  (let ((result (indigo-render-grid-to-file array ref-atoms 2 temp-file)))
                    (should (eq result 1))
                    (should (file-exists-p temp-file))
                    (should (> (file-attribute-size (file-attributes temp-file)) 0))))
              (when (file-exists-p temp-file)
                (delete-file temp-file)))))
      (indigo-free mol1)
      (indigo-free mol2)
      (indigo-free array))))

;;; With-style Rendering Macro Tests

(ert-deftest test-indigo-with-array ()
  "Test indigo-with-array macro."
  (indigo-with-array (arr)
    (should (integerp arr))
    (should (> arr 0))
    (indigo-with-molecule (mol "CCO")
      (indigo-array-add arr mol))))

(provide 'test-indigo-rendering)
;;; test-indigo-rendering.el ends here
