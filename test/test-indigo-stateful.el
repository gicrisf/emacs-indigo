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

;;; PKA Function Tests

(ert-deftest test-indigo-build-pka-model ()
  "Test PKA model building function."
  (should (fboundp 'indigo-build-pka-model))
  
  ;; Test with a non-existent file (should return error code)
  (let ((result (indigo-build-pka-model 3 0.5 "/nonexistent/file.txt")))
    (should (integerp result))))

(ert-deftest test-indigo-get-acid-pka-value ()
  "Test acid PKA value retrieval."
  (should (fboundp 'indigo-get-acid-pka-value))
  
  ;; Test with a simple molecule (ethanoic acid)
  (let ((mol (indigo-load-molecule-from-string "CC(=O)O")))
    (unwind-protect
        (when (and mol (> mol 0))
          ;; Test function exists and returns appropriate value
          (let ((pka-result (indigo-get-acid-pka-value mol 0 1 0)))
            (should (or (floatp pka-result) (null pka-result)))))
      (when (and mol (> mol 0))
        (indigo-free mol)))))

(ert-deftest test-indigo-get-basic-pka-value ()
  "Test basic PKA value retrieval."
  (should (fboundp 'indigo-get-basic-pka-value))
  
  ;; Test with a simple molecule (ethylamine)
  (let ((mol (indigo-load-molecule-from-string "CCN")))
    (unwind-protect
        (when (and mol (> mol 0))
          ;; Test function exists and returns appropriate value
          (let ((pka-result (indigo-get-basic-pka-value mol 0 1 0)))
            (should (or (floatp pka-result) (null pka-result)))))
      (when (and mol (> mol 0))
        (indigo-free mol)))))

;;; Reaction Mapping Function Tests

(ert-deftest test-indigo-automap ()
  "Test automatic reaction mapping."
  (should (fboundp 'indigo-automap))
  
  ;; Test with a simple reaction (esterification)
  (let ((rxn-smiles "CC(=O)O.CCO>>CC(=O)OCC.O"))
    ;; (message "DEBUG: Loading reaction: %s" rxn-smiles)
    (let ((rxn (indigo-load-reaction-from-string rxn-smiles)))
      ;; (message "DEBUG: Reaction handle: %s" rxn)
      (unwind-protect
          (when (and rxn (> rxn 0))
            ;; (message "DEBUG: Starting automap tests")
            ;; Test different mapping modes
            (let ((result1 (indigo-automap rxn "discard")))
              ;; (message "DEBUG: automap discard result: %s" result1)
              (should (integerp result1)))
            (let ((result2 (indigo-automap rxn "keep")))
              ;; (message "DEBUG: automap keep result: %s" result2)
              (should (integerp result2)))
            (let ((result3 (indigo-automap rxn "alter")))
              ;; (message "DEBUG: automap alter result: %s" result3)
              (should (integerp result3)))
            (let ((result4 (indigo-automap rxn "clear")))
              ;; (message "DEBUG: automap clear result: %s" result4)
              (should (integerp result4)))
              
            ;; Test with additional options
            (let ((result5 (indigo-automap rxn "discard ignore_charges")))
              ;; (message "DEBUG: automap with options result: %s" result5)
              (should (integerp result5))))
        (when (and rxn (> rxn 0))
          ;; (message "DEBUG: Freeing reaction handle")
          (indigo-free rxn))))))

(ert-deftest test-indigo-atom-mapping-numbers ()
  "Test atom mapping number getters and setters."
  (should (fboundp 'indigo-get-atom-mapping-number))
  (should (fboundp 'indigo-set-atom-mapping-number))
  
  ;; Test with a simple reaction
  (let ((rxn-smiles "C.C>>CC"))
    ;; (message "DEBUG: Loading reaction for mapping test: %s" rxn-smiles)
    (let ((rxn (indigo-load-reaction-from-string rxn-smiles)))
      ;; (message "DEBUG: Reaction handle for mapping: %s" rxn)
      (unwind-protect
          (when (and rxn (> rxn 0))
            ;; Get reaction atoms iterator
            ;; (message "DEBUG: Getting reactants iterator")
            (let ((reactants-iter (indigo-iterate-reactants rxn)))
              ;; (message "DEBUG: Reactants iterator: %s" reactants-iter)
              (unwind-protect
                  (let ((reactant1 (indigo-next reactants-iter)))
                    ;; (message "DEBUG: First reactant: %s" reactant1)
                    (when reactant1
                      (let ((atoms-iter (indigo-iterate-atoms reactant1)))
                        ;; (message "DEBUG: Atoms iterator: %s" atoms-iter)
                        (unwind-protect
                            (let ((atom1 (indigo-next atoms-iter)))
                              ;; (message "DEBUG: First atom: %s" atom1)
                              (when atom1
                                ;; Test setting and getting mapping number
                                ;; (message "DEBUG: Setting atom mapping number")
                                (let ((set-result (indigo-set-atom-mapping-number rxn atom1 1)))
                                  ;; (message "DEBUG: Set mapping result: %s" set-result)
                                  (should (integerp set-result))
                                  
                                  ;; (message "DEBUG: Getting atom mapping number")
                                  (let ((mapping-num (indigo-get-atom-mapping-number rxn atom1)))
                                    ;; (message "DEBUG: Mapping number: %s" mapping-num)
                                    (should (integerp mapping-num))))))
                          (indigo-free atoms-iter)))))
                (indigo-free reactants-iter))))
        (when (and rxn (> rxn 0))
          (indigo-free rxn))))))

(ert-deftest test-indigo-clear-aam ()
  "Test clearing atom-to-atom mapping."
  (should (fboundp 'indigo-clear-aam))
  
  (let ((rxn-smiles "C.C>>CC"))
    ;; (message "DEBUG: Loading reaction for clear AAM test: %s" rxn-smiles)
    (let ((rxn (indigo-load-reaction-from-string rxn-smiles)))
      ;; (message "DEBUG: Reaction handle for clear AAM: %s" rxn)
      (unwind-protect
          (when (and rxn (> rxn 0))
            ;; First map the reaction
            ;; (message "DEBUG: Auto-mapping reaction")
            (let ((map-result (indigo-automap rxn "discard")))
              ;; (message "DEBUG: Automap result: %s" map-result)
              )
            
            ;; Then clear the mapping
            ;; (message "DEBUG: Clearing AAM")
            (let ((result (indigo-clear-aam rxn)))
              ;; (message "DEBUG: Clear AAM result: %s" result)
              (should (integerp result))))
        (when (and rxn (> rxn 0))
          (indigo-free rxn))))))

(ert-deftest test-indigo-correct-reacting-centers ()
  "Test correcting reacting centers based on mapping."
  (should (fboundp 'indigo-correct-reacting-centers))
  
  (let ((rxn-smiles "C.C>>CC"))
    ;; (message "DEBUG: Loading reaction for correct reacting centers test: %s" rxn-smiles)
    (let ((rxn (indigo-load-reaction-from-string rxn-smiles)))
      ;; (message "DEBUG: Reaction handle for correct reacting centers: %s" rxn)
      (unwind-protect
          (when (and rxn (> rxn 0))
            ;; First map the reaction
            ;; (message "DEBUG: Auto-mapping reaction before correcting centers")
            (let ((map-result (indigo-automap rxn "discard")))
              ;; (message "DEBUG: Automap result before correct: %s" map-result)
              )
            
            ;; Then correct reacting centers
            ;; (message "DEBUG: Correcting reacting centers")
            (let ((result (indigo-correct-reacting-centers rxn)))
              ;; (message "DEBUG: Correct reacting centers result: %s" result)
              (should (integerp result))))
        (when (and rxn (> rxn 0))
          (indigo-free rxn))))))

;;; Reacting Center Function Tests

(ert-deftest test-indigo-reacting-center-operations ()
  "Test reacting center getters and setters."
  (should (fboundp 'indigo-get-reacting-center))
  (should (fboundp 'indigo-set-reacting-center))
  
  ;; Test with a simple reaction that has bonds
  (let ((rxn-smiles "CC.CC>>CCCC"))
    ;; (message "DEBUG: Loading reaction for reacting center test: %s" rxn-smiles)
    (let ((rxn (indigo-load-reaction-from-string rxn-smiles)))
      ;; (message "DEBUG: Reaction handle for reacting center: %s" rxn)
      (unwind-protect
          (when (and rxn (> rxn 0))
            ;; Get reaction molecules iterator
            ;; (message "DEBUG: Getting molecules iterator")
            (let ((molecules-iter (indigo-iterate-molecules rxn)))
              ;; (message "DEBUG: Molecules iterator: %s" molecules-iter)
              (unwind-protect
                  (let ((mol1 (indigo-next molecules-iter)))
                    ;; (message "DEBUG: First molecule: %s" mol1)
                    (when mol1
                      (let ((bonds-iter (indigo-iterate-bonds mol1)))
                        ;; (message "DEBUG: Bonds iterator: %s" bonds-iter)
                        (unwind-protect
                            (let ((bond1 (indigo-next bonds-iter)))
                              ;; (message "DEBUG: First bond: %s" bond1)
                              (when bond1
                                ;; Test setting reacting center
                                ;; (message "DEBUG: Setting reacting center")
                                (let ((set-result (indigo-set-reacting-center rxn bond1 1)))
                                  ;; (message "DEBUG: Set reacting center result: %s" set-result)
                                  (should (integerp set-result))
                                  
                                  ;; Test getting reacting center
                                  ;; (message "DEBUG: Getting reacting center")
                                  (let ((rc-value (indigo-get-reacting-center rxn bond1)))
                                    ;; (message "DEBUG: Reacting center value: %s" rc-value)
                                    (should (or (integerp rc-value) (null rc-value)))))))
                          (indigo-free bonds-iter)))))
                (indigo-free molecules-iter))))
        (when (and rxn (> rxn 0))
          (indigo-free rxn))))))

;;; Advanced Error Handling Tests

(ert-deftest test-indigo-advanced-error-handling ()
  "Test error handling for advanced functions."
  ;; Test PKA functions with invalid handles
  (should (null (indigo-get-acid-pka-value -1 0 1 0)))
  (should (null (indigo-get-basic-pka-value -1 0 1 0)))
  
  ;; Test mapping functions with invalid handles
  (should (integerp (indigo-automap -1 "discard")))  ; Should return error code
  (should (integerp (indigo-get-atom-mapping-number -1 -1)))
  (should (integerp (indigo-set-atom-mapping-number -1 -1 1)))
  (should (integerp (indigo-clear-aam -1)))
  (should (integerp (indigo-correct-reacting-centers -1)))
  
  ;; Test reacting center functions with invalid handles
  (should (null (indigo-get-reacting-center -1 -1)))
  (should (integerp (indigo-set-reacting-center -1 -1 1))))

(provide 'test-indigo-stateful)

;;; test-indigo-stateful.el ends here
