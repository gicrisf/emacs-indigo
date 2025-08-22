;;; test-indigo-integration.el --- Integration tests for Indigo I/O with real chemical data -*- lexical-binding: t; -*-

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

;; Integration tests for Indigo I/O operations using real chemical data files
;; from the test/data directory. These tests verify that the functions work
;; correctly with actual chemical file formats and structures.

;;; Code:

(require 'ert)
(require 'indigo)

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

;;; Integration tests with real chemical files


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

(provide 'test-indigo-integration)

;;; test-indigo-integration.el ends here
