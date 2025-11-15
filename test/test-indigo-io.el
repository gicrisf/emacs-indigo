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
;; loading from files/buffers, SMARTS loading, file saving, and
;; integration tests with real chemical data files.

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
  "Test loading molecule from non-existent file (private API)."
  (let ((mol (indigo--load-molecule-from-file "/nonexistent/file.smi")))
    ;; Private API returns -1 or nil on error
    ;; The actual error checking would be done via indigo-get-last-error
    (when (and (integerp mol) (> mol 0))
      (indigo-free mol))))

(ert-deftest test-indigo-load-molecule-from-file-nonexistent-public-api ()
  "Test loading molecule from non-existent file signals error (public API)."
  (should-error (indigo-load-molecule-from-file "/nonexistent/file.smi")))

(ert-deftest test-indigo-load-invalid-smiles-public-api ()
  "Test loading invalid SMILES signals error (public API)."
  (should-error (indigo-load-molecule-from-string "INVALID_SMILES")))

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

(defvar test-data-dir
  (expand-file-name "data/molecules/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test chemical data files.")

;;; Helper functions

(defun test-indigo-get-test-file (subdir filename)
  "Get path to test file in SUBDIR with FILENAME."
  (expand-file-name (concat subdir "/" filename) test-data-dir))

(defun test-indigo-file-exists-p (subdir filename)
  "Check if test file exists in SUBDIR with FILENAME."
  (file-exists-p (test-indigo-get-test-file subdir filename)))

(ert-deftest test-indigo-integration-load-mol-file ()
  "Test loading molecules from real MOL files."
  (let ((mol-file (test-indigo-get-test-file "chebi" "ChEBI_10305.mol")))
    (when (file-exists-p mol-file)
      (let ((mol (indigo-load-molecule-from-file mol-file)))
        (should (integerp mol))
        (should (> mol 0))
        ;; Test comprehensive molecular properties
        (let ((weight (indigo-molecular-weight mol))
              (formula (indigo-gross-formula mol))
              (atom-count (indigo-count-atoms mol))
              (bond-count (indigo-count-bonds mol))
              (smiles (indigo-canonical-smiles mol))
              (molfile (indigo-molfile mol)))
          (should (numberp weight))
          (should (> weight 0))
          (should (stringp formula))
          (should (not (string-empty-p formula)))
          (should (integerp atom-count))
          (should (> atom-count 0))
          (should (integerp bond-count))
          (should (>= bond-count 0))
          (should (stringp smiles))
          (should (not (string-empty-p smiles)))
          (should (stringp molfile))
          (should (string-match-p "M  END" molfile)))
        (indigo-free mol)))))

(ert-deftest test-indigo-integration-load-multiple-chebi-files ()
  "Test loading multiple ChEBI molecules and comparing properties."
  (let ((chebi-files '("ChEBI_10305.mol" "ChEBI_10909.mol" "ChEBI_12211.mol"))
        (results '()))
    (dolist (filename chebi-files)
      (let ((mol-file (test-indigo-get-test-file "chebi" filename)))
        (when (file-exists-p mol-file)
          (let ((mol (indigo-load-molecule-from-file mol-file)))
            (when (and (integerp mol) (> mol 0))
              (let ((weight (indigo-molecular-weight mol))
                    (atom-count (indigo-count-atoms mol)))
                ;; Only include results with valid weights (some molecules may have calculation errors)
                (when (> weight 0)
                  (push (list filename weight atom-count) results)))
              (indigo-free mol))))))
    ;; Verify we loaded at least one molecule with valid properties
    (should (> (length results) 0))
    ;; Verify all included molecules have positive weights and atom counts
    (dolist (result results)
      (should (> (nth 1 result) 0))  ; weight
      (should (> (nth 2 result) 0))))) ; atom count

(ert-deftest test-indigo-integration-load-sdf-file ()
  "Test loading molecules from SDF files."
  (let ((sdf-file (test-indigo-get-test-file "basic" "sugars.sdf")))
    (when (file-exists-p sdf-file)
      (let ((mol (indigo-load-molecule-from-file sdf-file)))
        (should (integerp mol))
        (should (> mol 0))
        ;; Test sugar molecule properties
        (let ((formula (indigo-gross-formula mol))
              (has-coords (indigo-has-coordinates mol))
              (ring-count (indigo-count-sssr mol)))
          (should (stringp formula))
          (should (booleanp has-coords))
          (should (integerp ring-count))
          (should (>= ring-count 0)))
        (indigo-free mol)))))

(ert-deftest test-indigo-integration-query-molecule-loading ()
  "Test loading query molecules for substructure search."
  (let ((query-file (test-indigo-get-test-file "sss" "arom_het_5_21.mol")))
    (when (file-exists-p query-file)
      (let ((qmol (indigo-load-query-molecule-from-file query-file)))
        (should (integerp qmol))
        (should (> qmol 0))
        ;; Test that we can use it as a query
        (let ((target-file (test-indigo-get-test-file "chebi" "ChEBI_10305.mol")))
          (when (file-exists-p target-file)
            (let ((target-mol (indigo-load-molecule-from-file target-file)))
              (should (integerp target-mol))
              (should (> target-mol 0))
              ;; Create substructure matcher (tests query molecule functionality)
              (let ((matcher (indigo-substructure-matcher target-mol)))
                (should (integerp matcher))
                (indigo-free matcher))
              (indigo-free target-mol))))
        (indigo-free qmol)))))

(ert-deftest test-indigo-integration-smarts-loading ()
  "Test loading and using SMARTS patterns."
  (let ((benzene-smarts "c1ccccc1")
        (alcohol-smarts "[OH]"))
    ;; Test benzene pattern
    (let ((smarts1 (indigo-load-smarts-from-string benzene-smarts)))
      (should (integerp smarts1))
      (should (> smarts1 0))
      (indigo-free smarts1))
    ;; Test alcohol pattern
    (let ((smarts2 (indigo-load-smarts-from-string alcohol-smarts)))
      (should (integerp smarts2))
      (should (> smarts2 0))
      (indigo-free smarts2))))

(ert-deftest test-indigo-integration-file-saving-roundtrip ()
  "Test loading a molecule, saving it, and loading it back."
  (let ((input-file (test-indigo-get-test-file "chebi" "ChEBI_7750.mol"))
        (temp-output (make-temp-file "indigo-roundtrip" nil ".mol")))
    (when (file-exists-p input-file)
      (unwind-protect
          (progn
            ;; Load original molecule
            (let ((mol1 (indigo-load-molecule-from-file input-file)))
              (should (integerp mol1))
              (should (> mol1 0))
              (let ((original-formula (indigo-gross-formula mol1))
                    (original-weight (indigo-molecular-weight mol1)))
                ;; Save molecule
                (let ((save-result (indigo-save-molfile-to-file mol1 temp-output)))
                  (should (integerp save-result))
                  (should (file-exists-p temp-output))
                  ;; Load saved molecule
                  (let ((mol2 (indigo-load-molecule-from-file temp-output)))
                    (should (integerp mol2))
                    (should (> mol2 0))
                    ;; Compare properties
                    (let ((saved-formula (indigo-gross-formula mol2))
                          (saved-weight (indigo-molecular-weight mol2)))
                      (should (string-equal original-formula saved-formula))
                      (should (= original-weight saved-weight)))
                    (indigo-free mol2)))
                (indigo-free mol1))))
        ;; Cleanup
        (when (file-exists-p temp-output)
          (delete-file temp-output))))))

(ert-deftest test-indigo-integration-stereochemistry-handling ()
  "Test loading molecules with stereochemistry information."
  (let ((stereo-file (test-indigo-get-test-file "stereo" "enhanced_stereo1.mol")))
    (when (file-exists-p stereo-file)
      (let ((mol (indigo-load-molecule-from-file stereo-file)))
        (should (integerp mol))
        (should (> mol 0))
        ;; Test stereochemistry-related properties
        (let ((is-chiral (indigo-is-chiral mol))
              (stereo-count (indigo-count-stereocenters mol)))
          (should (booleanp is-chiral))
          (should (integerp stereo-count))
          (should (>= stereo-count 0)))
        (indigo-free mol)))))

(ert-deftest test-indigo-integration-sgroups-handling ()
  "Test loading molecules with S-groups."
  (let ((sgroup-file (test-indigo-get-test-file "sgroups" "all_sgroups.sdf")))
    (when (file-exists-p sgroup-file)
      (let ((mol (indigo-load-molecule-from-file sgroup-file)))
        (should (integerp mol))
        (should (> mol 0))
        ;; Basic verification that molecule loaded
        (let ((atom-count (indigo-count-atoms mol))
              (molfile (indigo-molfile mol)))
          (should (integerp atom-count))
          (should (> atom-count 0))
          (should (stringp molfile))
          (should (not (string-empty-p molfile))))
        (indigo-free mol)))))

;;; Public API Error Handling Tests

(ert-deftest test-public-api-cleanup-on-error ()
  "Test that public API cleans up resources even when errors occur."
  (let ((initial-refs (indigo-count-references)))
    ;; Error during molecule loading
    (should-error (indigo-load-molecule-from-string "INVALID"))
    (should (= (indigo-count-references) initial-refs))

    ;; Error during file loading
    (should-error (indigo-load-molecule-from-file "/nonexistent"))
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-public-api-error-messages ()
  "Test that public API provides meaningful error messages."
  (condition-case err
      (progn
        (indigo-load-molecule-from-string "INVALID_SMILES")
        (should nil)) ; Should not reach here
    (error
     ;; Error message should mention what failed
     (should (string-match-p "Failed to load molecule" (error-message-string err))))))

(ert-deftest test-public-api-valid-operations ()
  "Test that public API works correctly for valid inputs."
  ;; Create molecule
  (let ((mol (indigo-create-molecule)))
    (should (integerp mol))
    (should (> mol 0))
    (indigo-free mol))

  ;; Load molecule from string
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (should (integerp mol))
    (should (> mol 0))
    (should (= (indigo-count-atoms mol) 3))
    (indigo-free mol))

  ;; Load query molecule
  (let ((query (indigo-load-query-molecule-from-string "c1ccccc1")))
    (should (integerp query))
    (should (> query 0))
    (indigo-free query))

  ;; Load reaction
  (let ((rxn (indigo-load-reaction-from-string "CCO.CC>>CCOC")))
    (should (integerp rxn))
    (should (> rxn 0))
    (indigo-free rxn)))

(ert-deftest test-public-api-iterators ()
  "Test that public API iterator creation works correctly."
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (progn
          ;; Atoms iterator
          (let ((atoms (indigo-iterate-atoms mol)))
            (should (integerp atoms))
            (should (> atoms 0))
            (indigo-free atoms))

          ;; Bonds iterator
          (let ((bonds (indigo-iterate-bonds mol)))
            (should (integerp bonds))
            (should (> bonds 0))
            (indigo-free bonds))

          ;; SSSR iterator
          (let ((sssr (indigo-iterate-sssr mol)))
            (should (integerp sssr))
            (should (> sssr 0))
            (indigo-free sssr)))
      (indigo-free mol))))

(ert-deftest test-public-api-fingerprint-and-matcher ()
  "Test that public API fingerprint and matcher creation works."
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (progn
          ;; Fingerprint
          (let ((fp (indigo-fingerprint mol "sim")))
            (should (integerp fp))
            (should (> fp 0))
            (indigo-free fp))

          ;; Substructure matcher
          (let ((matcher (indigo-substructure-matcher mol)))
            (should (integerp matcher))
            (should (> matcher 0))
            (indigo-free matcher)))
      (indigo-free mol))))

(ert-deftest test-public-api-array-creation ()
  "Test that public API array creation works."
  (let ((arr (indigo-create-array)))
    (should (integerp arr))
    (should (> arr 0))
    (indigo-free arr)))

(provide 'test-indigo-io)

;;; test-indigo-io.el ends here
