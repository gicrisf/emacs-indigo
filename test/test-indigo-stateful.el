;;; test-indigo-stateful.el --- Tests for stateful Indigo operations -*- lexical-binding: t; -*-

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

;; Tests for the stateful Indigo operations that work with molecule handles.
;; These functions allow for more efficient repeated operations by avoiding
;; repeated parsing and cleanup.

;;; Code:

(require 'ert)
(require 'indigo)

;; Test molecules
(defvar test-molecules-stateful
  '(("ethanol" . "CCO")
    ("benzene" . "c1ccccc1")
    ("water" . "O")
    ("methane" . "C")
    ("caffeine" . "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))
  "Test molecules for stateful operations.")

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

;;; Molecule loading and memory management tests

(ert-deftest test-indigo-load-molecule-from-string ()
  "Test loading molecules from strings."
  (dolist (mol test-molecules-stateful)
    (let ((handle (indigo-load-molecule-from-string (cdr mol))))
      (should (integerp handle))
      (should (> handle 0))
      (indigo-free handle))))

(ert-deftest test-indigo-load-query-molecule-from-string ()
  "Test loading query molecules from strings."
  (let ((handle (indigo-load-query-molecule-from-string "c1ccccc1")))
    (should (integerp handle))
    (should (> handle 0))
    (indigo-free handle)))

(ert-deftest test-indigo-free ()
  "Test freeing molecule handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (should (eq (indigo-free handle) t))))

(ert-deftest test-indigo-clone ()
  "Test cloning molecule handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((cloned (indigo-clone handle)))
      (should (integerp cloned))
      (should (> cloned 0))
      (should (not (= handle cloned)))
      (indigo-free handle)
      (indigo-free cloned))))

;;; Format conversion tests

(ert-deftest test-indigo-canonical-smiles ()
  "Test canonical SMILES generation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((smiles (indigo-canonical-smiles handle)))
      (should (stringp smiles))
      (should (string-match-p "CCO\\|OCC" smiles)))
    (indigo-free handle)))

(ert-deftest test-indigo-smiles ()
  "Test SMILES generation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((smiles (indigo-smiles handle)))
      (should (stringp smiles)))
    (indigo-free handle)))

(ert-deftest test-indigo-molfile ()
  "Test MOL file generation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((molfile (indigo-molfile handle)))
      (should (stringp molfile))
      (should (string-match-p "V2000" molfile)))
    (indigo-free handle)))

(ert-deftest test-indigo-cml ()
  "Test CML generation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((cml (indigo-cml handle)))
      (should (stringp cml))
      (should (string-match-p "<molecule" cml)))
    (indigo-free handle)))

;;; Molecular property tests

(ert-deftest test-indigo-molecular-weight ()
  "Test molecular weight calculation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((weight (indigo-molecular-weight handle)))
      (should (floatp weight))
      (should (> weight 40))
      (should (< weight 50)))
    (indigo-free handle)))

(ert-deftest test-indigo-gross-formula ()
  "Test gross formula generation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((formula (indigo-gross-formula handle)))
      (should (stringp formula))
      (should (string-match-p "C.*H.*O" formula)))
    (indigo-free handle)))

;;; Counting function tests

(ert-deftest test-indigo-count-atoms ()
  "Test atom counting from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((count (indigo-count-atoms handle)))
      (should (integerp count))
      (should (= count 3)))
    (indigo-free handle)))

(ert-deftest test-indigo-count-bonds ()
  "Test bond counting from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((count (indigo-count-bonds handle)))
      (should (integerp count))
      (should (= count 2)))
    (indigo-free handle)))

(ert-deftest test-indigo-count-implicit-hydrogens ()
  "Test implicit hydrogen counting from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((count (indigo-count-implicit-hydrogens handle)))
      (should (integerp count))
      (should (>= count 0)))
    (indigo-free handle)))

(ert-deftest test-indigo-count-sssr ()
  "Test SSSR ring counting from handles."
  (let ((handle (indigo-load-molecule-from-string "c1ccccc1")))
    (let ((count (indigo-count-sssr handle)))
      (should (integerp count))
      (should (= count 1)))
    (indigo-free handle)))

(ert-deftest test-indigo-count-stereocenters ()
  "Test stereocenter counting from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((count (indigo-count-stereocenters handle)))
      (should (integerp count))
      (should (>= count 0)))
    (indigo-free handle)))

;;; Boolean property tests

(ert-deftest test-indigo-is-chiral ()
  "Test chirality detection from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((chiral (indigo-is-chiral handle)))
      (should (or (eq chiral t) (eq chiral nil))))
    (indigo-free handle)))

(ert-deftest test-indigo-has-coordinates ()
  "Test coordinate detection from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((has-coords (indigo-has-coordinates handle)))
      (should (or (eq has-coords t) (eq has-coords nil))))
    (indigo-free handle)))

;;; Matching function tests

(ert-deftest test-indigo-exact-match ()
  "Test exact matching between molecule handles."
  (let ((handle1 (indigo-load-molecule-from-string "CCO"))
        (handle2 (indigo-load-molecule-from-string "CCO"))
        (handle3 (indigo-load-molecule-from-string "CCC")))
    (let ((match1 (indigo-exact-match handle1 handle2 ""))
          (match2 (indigo-exact-match handle1 handle3 "")))
      (should (integerp match1))
      (should (integerp match2)))
    (indigo-free handle1)
    (indigo-free handle2)
    (indigo-free handle3)))

(ert-deftest test-indigo-substructure-matcher ()
  "Test substructure matcher creation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((matcher (indigo-substructure-matcher handle)))
      (should (integerp matcher))
      (indigo-free matcher))
    (indigo-free handle)))

;;; Fingerprint and similarity tests

(ert-deftest test-indigo-fingerprint ()
  "Test fingerprint generation from handles."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((fp (indigo-fingerprint handle "sim")))
      (should (integerp fp))
      (indigo-free fp))
    (indigo-free handle)))

(ert-deftest test-indigo-similarity ()
  "Test similarity calculation between fingerprints."
  (let ((handle1 (indigo-load-molecule-from-string "CCO"))
        (handle2 (indigo-load-molecule-from-string "CCO")))
    (let ((fp1 (indigo-fingerprint handle1 "sim"))
          (fp2 (indigo-fingerprint handle2 "sim")))
      (let ((similarity (indigo-similarity fp1 fp2 "tanimoto")))
        (should (floatp similarity))
        (should (>= similarity 0.0))
        (should (<= similarity 1.0)))
      (indigo-free fp1)
      (indigo-free fp2))
    (indigo-free handle1)
    (indigo-free handle2)))

;;; Utility function tests

(ert-deftest test-indigo-to-string ()
  "Test converting Indigo objects to strings."
  (let ((handle (indigo-load-molecule-from-string "CCO")))
    (let ((formula-handle (indigo-gross-formula handle)))
      (should (stringp formula-handle)))
    (indigo-free handle)))

;;; Performance comparison tests

(ert-deftest test-stateful-vs-stateless-performance ()
  "Compare performance of stateful vs stateless operations."
  (let ((smiles "CCO")
        (iterations 10))
    
    ;; Test stateless operations
    (let ((start-time (current-time)))
      (dotimes (_ iterations)
        (indigo-do-molecular-weight smiles))
      (let ((stateless-time (float-time (time-subtract (current-time) start-time))))
        
        ;; Test stateful operations
        (let ((handle (indigo-load-molecule-from-string smiles))
              (start-time (current-time)))
          (dotimes (_ iterations)
            (indigo-molecular-weight handle))
          (let ((stateful-time (float-time (time-subtract (current-time) start-time))))
            (indigo-free handle)
            
            ;; Stateful should be faster for repeated operations
            (should (< stateful-time stateless-time))
            ;; (message "Stateless: %.4fs, Stateful: %.4fs (%.1fx faster)"
            ;;          stateless-time stateful-time (/ stateless-time stateful-time))
            ))))))

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
    (should (integerp result))))

(ert-deftest test-indigo-option-error-handling ()
  "Test option functions with invalid option names."
  (let ((result1 (indigo-set-option "invalid-option-name" "value"))
        (result2 (indigo-set-option-int "another-invalid-option" 42)))
    (should (integerp result1))
    (should (integerp result2))))

;;; Normalization functions tests

(ert-deftest test-indigo-normalize-basic ()
  "Test basic molecule normalization."
  (let ((mol (indigo-load-molecule-from-string "[H]C([H])([H])C([H])([H])O[H]")))  ; ethanol with explicit hydrogens
    ;; (message "Loaded molecule handle: %s" mol)
    (unwind-protect
        (let ((original-smiles (indigo-smiles mol)))
          ;; (message "Original SMILES: %s" original-smiles)
          (let ((result (indigo-normalize mol)))
            ;; (message "Normalize result: %s" result)
            (when (= result 0)
              (message "Normalize error: %s" (indigo-get-last-error)))
            (should (integerp result))
            (should (> result 0))  ; Expect success
            ;; Check that normalization worked by converting back to SMILES
            (let ((normalized-smiles (indigo-smiles mol)))
              ;; (message "Normalized SMILES: %s" normalized-smiles)
              (should (stringp normalized-smiles)))))
      (indigo-free mol))))

(ert-deftest test-indigo-normalize-with-options ()
  "Test molecule normalization with options."
  (let ((mol (indigo-load-molecule-from-string "[H]C([H])([H])C([H])([H])O[H]")))
    (unwind-protect
        (let ((original-smiles (indigo-smiles mol)))
          ;; (message "Original SMILES (options test): %s" original-smiles)
          (let ((result (indigo-normalize mol "")))
            ;; (message "Normalize with empty options result: %s" result)
            (when (= result 0)
              (message "Normalize with options error: %s" (indigo-get-last-error)))
            (should (integerp result))
            (should (> result 0))  ; Expect success
            (let ((normalized-smiles (indigo-smiles mol)))
              ;; (message "Normalized SMILES (options test): %s" normalized-smiles)
              (should (stringp normalized-smiles))))
          )
      (indigo-free mol))))

(ert-deftest test-indigo-standardize-basic ()
  "Test basic molecule standardization."
  (let ((mol (indigo-load-molecule-from-string "[H]N([H])C([H])([H])C(=O)O[H]")))  ; glycine with explicit hydrogens and ionizable groups
    (unwind-protect
        (let ((original-smiles (indigo-smiles mol)))
          ;; (message "Original SMILES (standardize test): %s" original-smiles)
          (let ((result (indigo-standardize mol)))
            ;; (message "Standardize result: %s" result)
            (when (= result 0)
              (message "Standardize error: %s" (indigo-get-last-error)))
            (should (integerp result))
            (should (> result 0))  ; Expect success
            ;; Check that molecule is still valid after standardization
            (let ((smiles (indigo-smiles mol)))
              ;; (message "Standardized SMILES: %s" smiles)
              (should (stringp smiles)))))
      (indigo-free mol))))

(ert-deftest test-indigo-ionize-basic ()
  "Test basic molecule ionization."
  (let ((mol (indigo-load-molecule-from-string "CC(=O)O")))  ; acetic acid
    (unwind-protect
        (let ((original-smiles (indigo-smiles mol)))
          ;; (message "Original SMILES (ionize test): %s" original-smiles)
          (let ((result (indigo-ionize mol 7.0 0.1)))  ; pH 7.0 with tolerance 0.1
            ;; (message "Ionize result (pH 7.0): %s" result)
            (when (= result 0)
              (message "Ionize error: %s" (indigo-get-last-error)))
            (should (integerp result))
            (should (> result 0))  ; Expect success
            ;; Check that molecule is still valid after ionization
            (let ((smiles (indigo-smiles mol)))
              ;; (message "Ionized SMILES (pH 7.0): %s" smiles)
              (should (stringp smiles)))))
      (indigo-free mol))))

(ert-deftest test-indigo-ionize-different-ph ()
  "Test molecule ionization at different pH values."
  (let ((mol1 (indigo-load-molecule-from-string "CC(=O)O"))   ; acetic acid
        (mol2 (indigo-load-molecule-from-string "CC(=O)O")))
    (unwind-protect
        (progn
          ;; Test at acidic pH
          (let ((result1 (indigo-ionize mol1 3.0 0.1)))
            (should (integerp result1)))
          ;; Test at basic pH
          (let ((result2 (indigo-ionize mol2 10.0 0.1)))
            (should (integerp result2))))
      (indigo-free mol1)
      (indigo-free mol2))))

(ert-deftest test-normalization-error-handling ()
  "Test error handling with invalid molecule handles."
  ;; Test with invalid handle (should not crash)
  (let ((result1 (indigo-normalize -1))
        (result2 (indigo-standardize -1))
        (result3 (indigo-ionize -1 7.0 0.1)))
    (should (integerp result1))
    (should (integerp result2))  
    (should (integerp result3))))

(provide 'test-indigo-stateful)

;;; test-indigo-stateful.el ends here
