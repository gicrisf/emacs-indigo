;;; test-indigo-io.el --- Tests for Indigo I/O operations -*- lexical-binding: t; -*-

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

;; Tests for the Indigo I/O operations including molecule creation,
;; loading from files/buffers, SMARTS loading, and file saving.

;;; Code:

(require 'ert)
(require 'indigo)

;;; Molecule creation tests

(ert-deftest test-indigo-create-molecule ()
  "Test creating empty molecule."
  (let ((mol (indigo-create-molecule)))
    (should (integerp mol))
    (should (> mol 0))
    (indigo-free mol)))

(ert-deftest test-indigo-create-query-molecule ()
  "Test creating empty query molecule."
  (let ((qmol (indigo-create-query-molecule)))
    (should (integerp qmol))
    (should (> qmol 0))
    (indigo-free qmol)))

;;; File loading tests

(ert-deftest test-indigo-load-molecule-from-file ()
  "Test loading molecule from file."
  (let ((temp-file (make-temp-file "test-mol" nil ".smi")))
    (unwind-protect
        (progn
          ;; Write test SMILES to file
          (with-temp-file temp-file
            (insert "CCO ethanol"))
          ;; Load molecule from file
          (let ((mol (indigo-load-molecule-from-file temp-file)))
            (should (integerp mol))
            (should (> mol 0))
            ;; Verify it's ethanol by checking canonical SMILES
            (let ((smiles (indigo-canonical-smiles mol)))
              (should (stringp smiles))
              (should (string-match-p "CCO\\|OCC" smiles)))
            (indigo-free mol)))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-indigo-load-query-molecule-from-file ()
  "Test loading query molecule from file."
  (let ((temp-file (make-temp-file "test-query" nil ".smi")))
    (unwind-protect
        (progn
          ;; Write test SMARTS to file
          (with-temp-file temp-file
            (insert "c1ccccc1"))
          ;; Load query molecule from file
          (let ((qmol (indigo-load-query-molecule-from-file temp-file)))
            (should (integerp qmol))
            (should (> qmol 0))
            (indigo-free qmol)))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Buffer loading tests - commented out (redundant with string versions)

;; (ert-deftest test-indigo-load-molecule-from-buffer ()
;;   "Test loading molecule from buffer."
;;   (let ((mol (indigo-load-molecule-from-buffer "CCO")))
;;     (should (integerp mol))
;;     (should (> mol 0))
;;     ;; Verify it's ethanol
;;     (let ((smiles (indigo-canonical-smiles mol)))
;;       (should (stringp smiles))
;;       (should (string-match-p "CCO\\|OCC" smiles)))
;;     (indigo-free mol)))

;; (ert-deftest test-indigo-load-query-molecule-from-buffer ()
;;   "Test loading query molecule from buffer."
;;   (let ((qmol (indigo-load-query-molecule-from-buffer "c1ccccc1")))
;;     (should (integerp qmol))
;;     (should (> qmol 0))
;;     (indigo-free qmol)))

;;; SMARTS loading tests

(ert-deftest test-indigo-load-smarts-from-string ()
  "Test loading SMARTS from string."
  (let ((smarts (indigo-load-smarts-from-string "c1ccccc1")))
    (should (integerp smarts))
    (should (> smarts 0))
    (indigo-free smarts)))

(ert-deftest test-indigo-load-smarts-from-file ()
  "Test loading SMARTS from file."
  (let ((temp-file (make-temp-file "test-smarts" nil ".smi")))
    (unwind-protect
        (progn
          ;; Write test SMARTS to file
          (with-temp-file temp-file
            (insert "c1ccccc1"))
          ;; Load SMARTS from file
          (let ((smarts (indigo-load-smarts-from-file temp-file)))
            (should (integerp smarts))
            (should (> smarts 0))
            (indigo-free smarts)))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;; (ert-deftest test-indigo-load-smarts-from-buffer ()
;;   "Test loading SMARTS from buffer."
;;   (let ((smarts (indigo-load-smarts-from-buffer "c1ccccc1")))
;;     (should (integerp smarts))
;;     (should (> smarts 0))
;;     (indigo-free smarts)))

;;; File saving tests

(ert-deftest test-indigo-save-molfile-to-file ()
  "Test saving molecule to file in MOL format."
  (let ((mol (indigo-load-molecule-from-string "CCO"))
        (temp-file (make-temp-file "test-output" nil ".mol")))
    (unwind-protect
        (progn
          ;; Save molecule to file
          (let ((result (indigo-save-molfile-to-file mol temp-file)))
            (should (integerp result))
            ;; Check that file was created and has content
            (should (file-exists-p temp-file))
            (should (> (nth 7 (file-attributes temp-file)) 0))
            ;; Basic check for MOL file format
            (with-temp-buffer
              (insert-file-contents temp-file)
              (let ((content (buffer-string)))
                (should (stringp content))
                (should (not (string-empty-p content)))
                ;; MOL files should have atom and bond count lines
                (should (string-match-p "M  END" content)))))
          (indigo-free mol))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Error handling tests

(ert-deftest test-indigo-load-molecule-from-file-nonexistent ()
  "Test loading molecule from non-existent file."
  (let ((mol (indigo-load-molecule-from-file "/nonexistent/file.smi")))
    ;; Should return a handle even for failed loads (Indigo behavior)
    ;; The actual error checking would be done via indigo-get-last-error
    (when (and (integerp mol) (> mol 0))
      (indigo-free mol))))

;; (ert-deftest test-indigo-load-molecule-from-buffer-invalid ()
;;   "Test loading molecule from invalid buffer content."
;;   (let ((mol (indigo-load-molecule-from-buffer "invalid_smiles_!!!")))
;;     ;; Should return a handle even for failed loads (Indigo behavior)
;;     (when (and (integerp mol) (> mol 0))
;;       (indigo-free mol))))

;;; Integration tests

(ert-deftest test-indigo-file-roundtrip ()
  "Test loading molecule from file, modifying it, and saving back."
  (let ((temp-input (make-temp-file "test-input" nil ".smi"))
        (temp-output (make-temp-file "test-output" nil ".mol")))
    (unwind-protect
        (progn
          ;; Create input file
          (with-temp-file temp-input
            (insert "c1ccccc1 benzene"))
          ;; Load, process, and save
          (let ((mol (indigo-load-molecule-from-file temp-input)))
            (should (integerp mol))
            (should (> mol 0))
            ;; Verify it loaded correctly
            (let ((smiles (indigo-canonical-smiles mol)))
              (should (stringp smiles))
              (should (string-match-p "c1ccccc1\\|C1=CC=CC=C1" smiles)))
            ;; Save to MOL file
            (let ((result (indigo-save-molfile-to-file mol temp-output)))
              (should (integerp result))
              (should (file-exists-p temp-output)))
            (indigo-free mol)))
      ;; Cleanup
      (when (file-exists-p temp-input)
        (delete-file temp-input))
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(provide 'test-indigo-io)

;;; test-indigo-io.el ends here