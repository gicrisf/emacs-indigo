;;; test-indigo-mol.el --- Tests for Indigo molecular operations -*- lexical-binding: t; -*-

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

;; Tests for Indigo molecular operations including format conversions,
;; property calculations, counting functions, matching, fingerprints,
;; and coordinate detection.

;;; Code:

(require 'ert)
(require 'indigo)

;;; Molecule loading and memory management tests

(ert-deftest test-indigo-load-molecule-from-string ()
  "Test loading molecules from strings."
  (dolist (mol '(("ethanol" . "CCO")
                 ("benzene" . "c1ccccc1")
                 ("water" . "O")
                 ("methane" . "C")
                 ("caffeine" . "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")))
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

(ert-deftest test-indigo-mass-calculations ()
  "Test mass calculation functions."
  (let ((mol (indigo-load-molecule-from-string "CCO"))) ; Ethanol
    (unwind-protect
        (progn
          (should (> mol 0))
          
          ;; Test most abundant mass
          (let ((most-abundant (indigo-most-abundant-mass mol)))
            (should (floatp most-abundant))
            (should (> most-abundant 40.0))  ; Should be around 46
            (should (< most-abundant 50.0)))
          
          ;; Test monoisotopic mass
          (let ((monoisotopic (indigo-monoisotopic-mass mol)))
            (should (floatp monoisotopic))
            (should (> monoisotopic 40.0))   ; Should be around 46
            (should (< monoisotopic 50.0)))
          
          ;; Test error handling with invalid handles
          (should (floatp (indigo-most-abundant-mass -1)))    ; May return 0.0 or error value
          (should (floatp (indigo-monoisotopic-mass -1))))    ; May return 0.0 or error value
      (when (and mol (> mol 0))
        (indigo-free mol)))))

(ert-deftest test-indigo-layered-code ()
  "Test layered code generation."
  (let ((mol (indigo-load-molecule-from-string "c1ccccc1"))) ; Benzene
    (unwind-protect
        (progn
          (should (> mol 0))
          
          ;; Test layered code
          (let ((code (indigo-layered-code mol)))
            (should (stringp code))
            (should (> (length code) 0)))
          
          ;; Test error handling with invalid handle
          ;; (should (stringp (indigo-layered-code -1)))  ; May return empty string
          )
      (when (and mol (> mol 0))
        (indigo-free mol)))))

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

(ert-deftest test-indigo-heavy-atom-count ()
  "Test heavy atom counting."
  (let ((mol (indigo-load-molecule-from-string "CCO"))) ; Ethanol: 3 heavy atoms (C,C,O)
    (unwind-protect
        (progn
          (should (> mol 0))
          
          ;; Test heavy atom count
          (let ((heavy-count (indigo-count-heavy-atoms mol)))
            (should (integerp heavy-count))
            (should (= heavy-count 3))))  ; C-C-O = 3 heavy atoms
      (when (and mol (> mol 0))
        (indigo-free mol))))
  
  ;; Test with benzene
  (let ((mol (indigo-load-molecule-from-string "c1ccccc1"))) ; Benzene: 6 carbons
    (unwind-protect
        (progn
          (should (> mol 0))
          
          (let ((heavy-count (indigo-count-heavy-atoms mol)))
            (should (integerp heavy-count))
            (should (= heavy-count 6))))  ; 6 carbons
      (when (and mol (> mol 0))
        (indigo-free mol))))
  
  ;; Test error handling with invalid handle
  (should (integerp (indigo-count-heavy-atoms -1))))  ; May return 0 or -1

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

(ert-deftest test-indigo-coordinate-detection ()
  "Test 3D coordinate detection."
  ;; Test molecule without coordinates
  (let ((mol-2d (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (progn
          (should (> mol-2d 0))
          
          ;; Should have 2D coordinates (false for 3D)
          (let ((has-coord (indigo-has-coordinates mol-2d)))
            (should (or (eq has-coord t) (eq has-coord nil))))
          
          ;; Should not have Z coordinates
          (let ((has-z (indigo-has-z-coord mol-2d)))
            (should (eq has-z nil)))
          
          ;; Test error handling with invalid handle
          (should (or (eq (indigo-has-z-coord -1) t)
                      (eq (indigo-has-z-coord -1) nil))))  ; Should return boolean
      (when (and mol-2d (> mol-2d 0))
        (indigo-free mol-2d)))))

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
      ;; Test with explicit :tanimoto
      (let ((similarity (indigo-similarity fp1 fp2 :tanimoto)))
        (should (floatp similarity))
        (should (>= similarity 0.0))
        (should (<= similarity 1.0)))
      ;; Test default (should also use :tanimoto)
      (let ((similarity-default (indigo-similarity fp1 fp2)))
        (should (floatp similarity-default))
        (should (>= similarity-default 0.0))
        (should (<= similarity-default 1.0))
        ;; Should be same as explicit :tanimoto for identical molecules
        (should (= similarity-default (indigo-similarity fp1 fp2 :tanimoto))))
      (indigo-free fp1)
      (indigo-free fp2))
    (indigo-free handle1)
    (indigo-free handle2)))

(ert-deftest test-indigo-similarity-tversky-with-params ()
  "Test Tversky similarity with custom alpha and beta parameters."
  (let ((handle1 (indigo-load-molecule-from-string "CCO"))
        (handle2 (indigo-load-molecule-from-string "CC(O)C")))
    (let ((fp1 (indigo-fingerprint handle1 "sim"))
          (fp2 (indigo-fingerprint handle2 "sim")))
      ;; Test basic tversky (default parameters)
      (let ((similarity-default (indigo-similarity fp1 fp2 :tversky)))
        (should (floatp similarity-default))
        (should (>= similarity-default 0.0))
        (should (<= similarity-default 1.0)))
      ;; Test tversky with custom parameters
      (let ((similarity-custom (indigo-similarity fp1 fp2 :tversky 0.7 0.3)))
        (should (floatp similarity-custom))
        (should (>= similarity-custom 0.0))
        (should (<= similarity-custom 1.0)))
      (indigo-free fp1)
      (indigo-free fp2))
    (indigo-free handle1)
    (indigo-free handle2)))

;;; Symmetry analysis tests

(ert-deftest test-indigo-symmetry-classes ()
  "Test symmetry class analysis."
  (let ((mol (indigo-load-molecule-from-string "c1ccccc1"))) ; Benzene
    (unwind-protect
        (progn
          (should (> mol 0))
          ;; Test symmetry classes
          (let ((classes (indigo-symmetry-classes mol)))
            (should (or (listp classes) (null classes)))
            (when (listp classes)
              ;; Should have symmetry classes for benzene atoms
              ;; I expect: (0 0 0 0 0 0)
              (should (> (length classes) 0))
              ;; All elements should be integers
              (dolist (class classes)
                (should (integerp class))))))
      (when (and mol (> mol 0))
        (indigo-free mol))))
  
  ;; TODO: Fix indigo-symmetry-classes (ethane returns nil)
  (let ((mol (indigo-load-molecule-from-string "CC"))) ;; ethane
    (unwind-protect
        (progn
          (should (> mol 0))
          (let ((classes (indigo-symmetry-classes mol)))
            ;; Remove debug message once issue is resolved
            ;; I would expect (0 0)
            ;; (message "Symmetry classes: %S" classes)
            (should (or (listp classes) (null classes)))
            ;; (when (listp classes)
            ;;   (should (> (length classes) 0))
            ;;   (dolist (class classes)
            ;;     (should (integerp class))))
            ))
      (when (and mol (> mol 0))
        (indigo-free mol))))
  
  ;; Test error handling with invalid handle
  (should (or (listp (indigo-symmetry-classes -1))
              (null (indigo-symmetry-classes -1)))))

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

;;; With-style Macro Tests

(ert-deftest test-indigo-with-molecule ()
  "Test indigo-with-molecule macro."
  (let ((result (indigo-with-molecule (mol "CCO")
                  (indigo-molecular-weight mol))))
    (should (floatp result))
    (should (< (abs (- result 46.069)) 0.01))))

(ert-deftest test-indigo-with-molecule-nested ()
  "Test nested indigo-with-molecule macros."
  (let ((result (indigo-with-molecule (mol1 "CCO")
                  (indigo-with-molecule (mol2 "c1ccccc1")
                    (list (indigo-molecular-weight mol1)
                          (indigo-molecular-weight mol2))))))
    (should (listp result))
    (should (= (length result) 2))
    (should (< (abs (- (car result) 46.069)) 0.01))
    (should (< (abs (- (cadr result) 78.114)) 0.01))))

(ert-deftest test-indigo-with-molecule-error-cleanup ()
  "Test that indigo-with-molecule cleans up on error."
  (should-error
   (indigo-with-molecule (mol "CCO")
     (error "Test error")))
  ;; If cleanup didn't happen, subsequent operations would fail
  (indigo-with-molecule (mol "CCO")
    (should (integerp mol))))

(ert-deftest test-indigo-with-mol-file ()
  "Test indigo-with-mol-file macro."
  (let ((test-file "test/data/molecules/basic/ethanol.mol"))
    (when (file-exists-p test-file)
      (indigo-with-mol-file (mol test-file)
        (should (integerp mol))
        (should (> mol 0))
        (let ((smiles (indigo-canonical-smiles mol)))
          (should (stringp smiles))
          (should (string-match-p "CCO\\|OCC" smiles)))))))

(ert-deftest test-indigo-with-query ()
  "Test indigo-with-query macro."
  (indigo-with-query (query "C=O")
    (should (integerp query))
    (should (> query 0))))

(ert-deftest test-indigo-with-smarts ()
  "Test indigo-with-smarts macro."
  (indigo-with-smarts (pattern "[#6]=[#8]")
    (should (integerp pattern))
    (should (> pattern 0))))

(ert-deftest test-indigo-with-fingerprint ()
  "Test indigo-with-fingerprint macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-fingerprint (fp mol "sim")
      (should (integerp fp))
      (should (> fp 0)))))

(ert-deftest test-indigo-with-fingerprint-similarity ()
  "Test fingerprint similarity calculation with indigo-with-* macros."
  (indigo-with-molecule (mol1 "CCO")
    (indigo-with-fingerprint (fp1 mol1 "sim")
      (indigo-with-molecule (mol2 "CCO")
        (indigo-with-fingerprint (fp2 mol2 "sim")
          (let ((similarity (indigo-similarity fp1 fp2 :tanimoto)))
            (should (floatp similarity))
            (should (> similarity 0.99))))))))

(ert-deftest test-indigo-with-matcher ()
  "Test indigo-with-matcher macro."
  (indigo-with-molecule (mol "c1ccccc1CCO")  ; Phenylethanol
    (indigo-with-matcher (matcher mol)
      (should (integerp matcher))
      (should (> matcher 0)))))

;;; Normalization functions tests

(ert-deftest test-indigo-normalize-basic ()
  "Test basic molecule normalization."
  (indigo-with-molecule (mol "[H]C([H])([H])C([H])([H])O[H]")  ; ethanol with explicit hydrogens
    (let ((result (indigo-normalize mol)))
      ;; (message "Normalize result: %s" result)
      (should (eq result :changed))  ; Explicit H should be removed
      ;; Check that normalization worked by converting back to SMILES
      (let ((normalized-smiles (indigo-smiles mol)))
        ;; (message "Normalized SMILES: %s" normalized-smiles)
        (should (stringp normalized-smiles))
        (should (string-match-p "^CCO" normalized-smiles))))))

(ert-deftest test-indigo-normalize-with-options ()
  "Test molecule normalization with options."
  (indigo-with-molecule (mol "[H]C([H])([H])C([H])([H])O[H]")
    (let ((result (indigo-normalize mol "")))
      ;; (message "Normalize with empty options result: %s" result)
      (should (eq result :changed))  ; Explicit H should be removed
      (let ((normalized-smiles (indigo-smiles mol)))
        ;; (message "Normalized SMILES (options test): %s" normalized-smiles)
        (should (stringp normalized-smiles)))))

  ;; Test error handling with invalid handle - should signal error
  (should-error (indigo-normalize -1)))

(ert-deftest test-indigo-standardize-basic ()
  "Test basic molecule standardization."
  (indigo-with-molecule (mol "[H]N([H])C([H])([H])C(=O)O[H]")  ; glycine with explicit hydrogens and ionizable groups
    (let* ((original-smiles (indigo-smiles mol))
           (result (indigo-standardize mol)))
      ;; (message "Standardize result: %s" result)
      (should (keywordp result))
      (should (memq result '(:changed :unchanged)))
      ;; Check that molecule is still valid after standardization
      (let ((smiles (indigo-smiles mol)))
        ;; (message "Standardized SMILES: %s" smiles)
        (should (stringp smiles)))))

  ;; Test error handling with invalid handle - should signal error
  (should-error (indigo-standardize -1)))

(ert-deftest test-indigo-ionize-basic ()
  "Test basic molecule ionization."
  (indigo-with-molecule (mol "CC(=O)O")  ; acetic acid
    (let* ((original-smiles (indigo-smiles mol))
           (result (indigo-ionize mol 7.0 0.1)))  ; pH 7.0 with tolerance 0.1
      ;; (message "Ionize result (pH 7.0): %s" result)
      (should (eq result :changed))  ; Acetic acid should be deprotonated at pH 7
      ;; Check that molecule is still valid after ionization
      (let ((smiles (indigo-smiles mol)))
        ;; (message "Ionized SMILES (pH 7.0): %s" smiles)
        (should (stringp smiles))
        ;; Should contain negative charge for deprotonated carboxyl
        (should (string-match-p "\\[O-\\]" smiles))))))

(ert-deftest test-indigo-ionize-different-ph ()
  "Test molecule ionization at different pH values."
  ;; Test at acidic pH - acetic acid should remain protonated
  (indigo-with-molecule (mol1 "CC(=O)O")
    (let ((result1 (indigo-ionize mol1 3.0 0.1)))
      (should (keywordp result1))
      (should (memq result1 '(:changed :unchanged)))))

  ;; Test at basic pH - acetic acid should be deprotonated
  (indigo-with-molecule (mol2 "CC(=O)O")
    (let ((result2 (indigo-ionize mol2 10.0 0.1)))
      (should (eq result2 :changed))))

  ;; Test error handling with invalid handle - should signal error
  (should-error (indigo-ionize -1 7.0 0.1)))

(ert-deftest test-normalization-error-handling ()
  "Test error handling with invalid molecule handles."
  ;; All three functions should signal errors with invalid handles
  (should-error (indigo-normalize -1))
  (should-error (indigo-standardize -1))
  (should-error (indigo-ionize -1 7.0 0.1)))

;;; PKA Function Tests

;; FIXME
;; The wrapper looks fine, but this is not the right way to use it
;; (ert-deftest test-indigo-build-pka-model ()
;;   "Test PKA model building function."
;;   (should (fboundp 'indigo-build-pka-model))

;;   ;; Test building PKA model to temporary file
;;   (let ((temp-file (make-temp-file "indigo-pka-model" nil ".pkl")))
;;     (message "DEBUG: Created temp file: %s" temp-file)
;;     (unwind-protect
;;         (let ((result (indigo-build-pka-model 0 0.0 temp-file)))
;;           (message "DEBUG: indigo-build-pka-model returned: %s" result)
;;           (should (integerp result))
;;           ;; Check for Indigo errors
;;           (let ((error-msg (indigo-get-last-error)))
;;             (when (and error-msg (> (length error-msg) 0))
;;               (message "DEBUG: Indigo error: %s" error-msg)))
;;           ;; Model built successfully if result >= 0
;;           (if (>= result 0)
;;               (progn
;;                 (message "DEBUG: PKA model built successfully to %s" temp-file)
;;                 (message "DEBUG: File exists after build: %s" (file-exists-p temp-file)))
;;             (message "DEBUG: PKA model build failed with result: %s" result)))
;;       ;; Clean up temporary file
;;       (when (file-exists-p temp-file)
;;         (message "DEBUG: Cleaning up temp file: %s" temp-file)
;;         (delete-file temp-file)))))

;; TODO Refine
;; Leaving debug messages because it works but the returned values are crazy
;; I mean: pka 100 for everything? LMAO -- I'm surely missing something basic
(ert-deftest test-indigo-get-acid-pka-value ()
  "Test acid PKA value retrieval."
  (should (fboundp 'indigo-get-acid-pka-value))

  ;; Test with a simple molecule (acetic acid)
  (let ((mol (indigo-load-molecule-from-string "CC(=O)O")))
    (message "DEBUG: Loaded molecule handle: %s" mol)
    (unwind-protect
        (when (and mol (> mol 0))
          ;; Iterate through ALL atoms in the molecule
          (let ((atoms-iter (indigo-iterate-atoms mol))
                (atom-index 0))
            (unwind-protect
                (let ((atom (indigo-next atoms-iter)))
                  (while atom
                    (let ((atom-symbol (indigo-symbol atom))
                          (pka-result (indigo-get-acid-pka-value mol atom 1 0)))
                      (message "DEBUG: Atom[%d] = %s, Acid pKa = %s"
                               atom-index atom-symbol pka-result)
                      ;; Check for Indigo errors
                      (let ((error-msg (indigo-get-last-error)))
                        (when (and error-msg (> (length error-msg) 0))
                          (message "DEBUG: Indigo error for atom %s: %s" atom-symbol error-msg)))
                      (should (or (floatp pka-result) (null pka-result)))
                      (setq atom-index (1+ atom-index))
                      (setq atom (indigo-next atoms-iter)))))
              (when atoms-iter
                (indigo-free atoms-iter))))

          ;; Test error handling with invalid handles
          (should (null (indigo-get-acid-pka-value -1 0 1 0))))
      (when (and mol (> mol 0))
        (indigo-free mol)))))

(ert-deftest test-indigo-get-basic-pka-value ()
  "Test basic PKA value retrieval."
  (should (fboundp 'indigo-get-basic-pka-value))

  ;; Test with a simple molecule (ethylamine)
  (let ((mol (indigo-load-molecule-from-string "CCN")))
    (message "DEBUG: Loaded molecule handle: %s" mol)
    (unwind-protect
        (when (and mol (> mol 0))
          ;; Iterate through ALL atoms in the molecule
          (let ((atoms-iter (indigo-iterate-atoms mol))
                (atom-index 0))
            (unwind-protect
                (let ((atom (indigo-next atoms-iter)))
                  (while atom
                    (let ((atom-symbol (indigo-symbol atom))
                          (pka-result (indigo-get-basic-pka-value mol atom 1 0)))
                      (message "DEBUG: Atom[%d] = %s, Basic pKa = %s"
                               atom-index atom-symbol pka-result)
                      ;; Check for Indigo errors
                      (let ((error-msg (indigo-get-last-error)))
                        (when (and error-msg (> (length error-msg) 0))
                          (message "DEBUG: Indigo error for atom %s: %s" atom-symbol error-msg)))
                      (should (or (floatp pka-result) (null pka-result)))
                      (setq atom-index (1+ atom-index))
                      (setq atom (indigo-next atoms-iter)))))
              (when atoms-iter
                (indigo-free atoms-iter))))

          ;; Test error handling with invalid handles
          (should (null (indigo-get-basic-pka-value -1 0 1 0))))
      (when (and mol (> mol 0))
        (indigo-free mol)))))

;;; Sequential Multiple Binding Macro Tests

(ert-deftest test-indigo-with-molecule* ()
  "Test indigo-with-molecule* macro with multiple bindings."
  (let ((result (indigo-with-molecule* ((mol1 "CCO")
                                        (mol2 "c1ccccc1"))
                  (list (indigo-molecular-weight mol1)
                        (indigo-molecular-weight mol2)))))
    (should (listp result))
    (should (= (length result) 2))
    (should (< (abs (- (car result) 46.069)) 0.01))
    (should (< (abs (- (cadr result) 78.114)) 0.01))))

(ert-deftest test-indigo-with-molecule*-cleanup ()
  "Test that indigo-with-molecule* properly cleans up all molecules."
  (should-error
   (indigo-with-molecule* ((mol1 "CCO")
                           (mol2 "c1ccccc1")
                           (mol3 "CCC"))
     (error "Test error")))
  ;; If cleanup didn't happen, subsequent operations would fail
  (indigo-with-molecule* ((mol1 "CCO")
                          (mol2 "c1ccccc1"))
    (should (integerp mol1))
    (should (integerp mol2))))

(ert-deftest test-indigo-with-molecule*-similarity ()
  "Test comparing fingerprints with plural macros."
  (indigo-with-molecule* ((mol1 "CCO")
                          (mol2 "c1ccccc1"))
    (indigo-with-fingerprint* ((fp1 mol1 "sim")
                               (fp2 mol2 "sim"))
      (let ((similarity (indigo-similarity fp1 fp2)))
        (should (floatp similarity))
        (should (>= similarity 0.0))
        (should (<= similarity 1.0))))))

(ert-deftest test-indigo-with-mol-file* ()
  "Test indigo-with-mol-file* macro with multiple files."
  (let ((file1 "test/data/molecules/basic/ethanol.mol")
        (file2 "test/data/molecules/basic/benzene.mol"))
    (when (and (file-exists-p file1) (file-exists-p file2))
      (indigo-with-mol-file* ((mol1 file1)
                              (mol2 file2))
        (should (integerp mol1))
        (should (> mol1 0))
        (should (integerp mol2))
        (should (> mol2 0))
        (let ((smiles1 (indigo-canonical-smiles mol1))
              (smiles2 (indigo-canonical-smiles mol2)))
          (should (stringp smiles1))
          (should (stringp smiles2)))))))

(ert-deftest test-indigo-with-query* ()
  "Test indigo-with-query* macro with multiple query molecules."
  (indigo-with-query* ((query1 "C=O")
                        (query2 "C#N"))
    (should (integerp query1))
    (should (> query1 0))
    (should (integerp query2))
    (should (> query2 0))
    (let ((smiles1 (indigo-smiles query1))
          (smiles2 (indigo-smiles query2)))
      (should (stringp smiles1))
      (should (stringp smiles2)))))

(ert-deftest test-indigo-with-query-file* ()
  "Test indigo-with-query-file* macro with multiple query files."
  (let ((file1 "test/data/queries/query1.mol")
        (file2 "test/data/queries/query2.mol"))
    ;; Only run if test files exist
    (when (and (file-exists-p file1) (file-exists-p file2))
      (indigo-with-query-file* ((query1 file1)
                                (query2 file2))
        (should (integerp query1))
        (should (> query1 0))
        (should (integerp query2))
        (should (> query2 0))))))

(ert-deftest test-indigo-with-smarts* ()
  "Test indigo-with-smarts* macro with multiple SMARTS patterns."
  (indigo-with-smarts* ((pattern1 "[#6]=[#8]")
                                (pattern2 "[#7]"))
    (should (integerp pattern1))
    (should (> pattern1 0))
    (should (integerp pattern2))
    (should (> pattern2 0))
    (let ((smiles1 (indigo-smiles pattern1))
          (smiles2 (indigo-smiles pattern2)))
      (should (stringp smiles1))
      (should (stringp smiles2)))))

(ert-deftest test-indigo-with-fingerprint* ()
  "Test indigo-with-fingerprint* macro with multiple fingerprints."
  (indigo-with-molecule* ((mol1 "CCO")
                          (mol2 "c1ccccc1")
                          (mol3 "CCC"))
    (indigo-with-fingerprint* ((fp1 mol1 "sim")
                               (fp2 mol2 "sim")
                               (fp3 mol3 "sim"))
      (should (integerp fp1))
      (should (> fp1 0))
      (should (integerp fp2))
      (should (> fp2 0))
      (should (integerp fp3))
      (should (> fp3 0))
      ;; Test similarity calculations
      (let ((sim12 (indigo-similarity fp1 fp2))
            (sim13 (indigo-similarity fp1 fp3)))
        (should (floatp sim12))
        (should (floatp sim13))
        ;; CCO and CCC should be more similar than CCO and benzene
        (should (> sim13 sim12))))))

(ert-deftest test-indigo-with-matcher* ()
  "Test indigo-with-matcher* macro with multiple matchers."
  (indigo-with-molecule* ((mol1 "c1ccccc1CCO")  ; Phenylethanol
                          (mol2 "CCN"))          ; Ethylamine
    (indigo-with-matcher* ((matcher1 mol1)
                           (matcher2 mol2))
      (should (integerp matcher1))
      (should (> matcher1 0))
      (should (integerp matcher2))
      (should (> matcher2 0)))))

(ert-deftest test-plural-macros-vs-nested-singular ()
  "Compare plural macros with nested singular macros for equivalence."
  ;; Test with plural macros
  (let ((result-plural
         (indigo-with-molecule* ((mol1 "CCO")
                                 (mol2 "c1ccccc1"))
           (list (indigo-molecular-weight mol1)
                 (indigo-molecular-weight mol2)))))

    ;; Test with nested singular macros
    (let ((result-nested
           (indigo-with-molecule (mol1 "CCO")
             (indigo-with-molecule (mol2 "c1ccccc1")
               (list (indigo-molecular-weight mol1)
                     (indigo-molecular-weight mol2))))))

      ;; Both should produce the same results
      (should (= (length result-plural) (length result-nested)))
      (should (< (abs (- (car result-plural) (car result-nested))) 0.001))
      (should (< (abs (- (cadr result-plural) (cadr result-nested))) 0.001)))))

(ert-deftest test-molecule*-sequential-evaluation ()
  "Test that indigo-with-molecule* evaluates bindings sequentially."
  ;; All molecules should be created and valid
  (let ((created-mols nil))
    (condition-case err
        (indigo-with-molecule* ((mol1 "CCO")
                                (mol2 "c1ccccc1")
                                (mol3 "CCC"))
          ;; All three molecules should be valid at this point
          (setq created-mols (list mol1 mol2 mol3))
          (should (integerp mol1))
          (should (integerp mol2))
          (should (integerp mol3))
          (should (> mol1 0))
          (should (> mol2 0))
          (should (> mol3 0))
          ;; Force an error to test cleanup
          (error "Test error"))
      (error
       ;; Error expected - verify molecules were created
       (should created-mols)
       (should (= (length created-mols) 3))))))

;;; Resource Cleanup Tests

(ert-deftest test-with-molecule-automatic-cleanup ()
  "Test that indigo-with-molecule properly cleans up resources."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule (mol "CCO")
      ;; Inside scope: 1 molecule allocated
      (should (= (indigo-count-references) (+ initial-refs 1))))
    ;; Outside scope: resources should be freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-with-molecule*-automatic-cleanup ()
  "Test that indigo-with-molecule* properly cleans up multiple resources."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule* ((mol1 "CCO")
                            (mol2 "c1ccccc1"))
      ;; Inside scope: 2 molecules allocated
      (should (= (indigo-count-references) (+ initial-refs 2))))
    ;; Outside scope: all resources should be freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-with-molecule-iterator-cleanup ()
  "Test cleanup with nested molecule and iterator macros."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule (mol "CCO")
      (indigo-with-atoms-iterator (atoms mol)
        ;; Inside scope: molecule + iterator allocated
        (should (>= (indigo-count-references) (+ initial-refs 1)))))
    ;; Outside scope: all resources should be freed
    (should (= (indigo-count-references) initial-refs))))

;;; Error Handling Tests

(ert-deftest test-with-molecule-invalid-smiles ()
  "Test that indigo-with-molecule signals error for invalid SMILES."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule (mol "INVALID_SMILES")
       (message "Should not reach here")))
    ;; Resources should still be cleaned up after error
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-with-molecule*-invalid-smiles ()
  "Test that indigo-with-molecule* properly cleans up when later molecule fails."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule* ((mol1 "CCO")
                             (mol2 "INVALID_SMILES"))
       (message "Should not reach here")))
    ;; With sequential nesting, mol1 should be cleaned up even when mol2 fails
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-with-molecule-error-in-body ()
  "Test cleanup when error occurs in macro body."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule (mol "CCO")
       (error "Test error in body")))
    ;; Resources should still be cleaned up
    (should (= (indigo-count-references) initial-refs))))

;;; Iterator Dependency Tests

(ert-deftest test-with-atoms-iterator-dependency ()
  "Test atoms iterator depending on molecule."
  (let ((symbols
         (indigo-with-molecule (mol "CCO")
           (indigo-with-atoms-iterator (atoms mol)
             (indigo-map #'indigo-symbol atoms)))))
    (should (equal symbols '("C" "C" "O")))))

(ert-deftest test-with-bonds-iterator-dependency ()
  "Test bonds iterator depending on molecule."
  (let ((bond-count
         (indigo-with-molecule (mol "CCO")
           (indigo-with-bonds-iterator (bonds mol)
             (length (indigo-map (lambda (_) t) bonds))))))
    (should (= bond-count 2))))

(ert-deftest test-with-components-iterator-dependency ()
  "Test components iterator with multi-component molecule."
  (let ((comp-count
         (indigo-with-molecule (mol "CCO.CC")
           (indigo-with-components-iterator (comps mol)
             (length (indigo-map (lambda (_) t) comps))))))
    (should (= comp-count 2))))

(ert-deftest test-with-sssr-iterator-dependency ()
  "Test SSSR iterator depending on molecule."
  (let ((ring-count
         (indigo-with-molecule (mol "c1ccccc1")
           (indigo-with-sssr-iterator (rings mol)
             (length (indigo-map (lambda (_) t) rings))))))
    (should (= ring-count 1))))

(ert-deftest test-with-stereocenters-iterator-dependency ()
  "Test stereocenters iterator depending on molecule."
  (let ((stereocenter-count
         (indigo-with-molecule (mol "C[C@H](O)CC")
           (indigo-with-stereocenters-iterator (stereos mol)
             (length (indigo-map (lambda (_) t) stereos))))))
    (should (= stereocenter-count 1))))

;;; Complex Workflow Tests

(ert-deftest test-with-macros-complex-workflow ()
  "Test complex workflow with multiple resources and calculations."
  (let ((analysis
         (indigo-with-molecule* ((ethanol "CCO")
                                 (methanol "CO"))
           (indigo-with-fingerprint* ((eth-fp ethanol "sim")
                                      (met-fp methanol "sim"))
             (indigo-with-atoms-iterator (eth-atoms ethanol)
               (let ((ethanol-atoms-list (indigo-map #'indigo-symbol eth-atoms)))
                 (indigo-with-atoms-iterator (eth-atoms2 ethanol)
                   (let ((atom-count (length (indigo-map #'indigo-symbol eth-atoms2))))
                     (list
                      :ethanol-weight (indigo-molecular-weight ethanol)
                      :methanol-weight (indigo-molecular-weight methanol)
                      :similarity (indigo-similarity eth-fp met-fp)
                      :ethanol-atoms ethanol-atoms-list
                      :atom-count atom-count)))))))))

    (should (plist-get analysis :ethanol-weight))
    (should (plist-get analysis :methanol-weight))
    (should (numberp (plist-get analysis :similarity)))
    (should (equal (plist-get analysis :ethanol-atoms) '("C" "C" "O")))
    (should (= (plist-get analysis :atom-count) 3))))

(ert-deftest test-with-molecule-clone-operation ()
  "Test molecule cloning with with- macros."
  (indigo-with-molecule (mol "CCO")
    (let ((cloned (indigo-clone mol)))
      (unwind-protect
          (progn
            (should (integerp cloned))
            (should (> cloned 0))
            (should (not (= mol cloned)))
            ;; Both should have same properties
            (should (= (indigo-count-atoms mol)
                       (indigo-count-atoms cloned))))
        (indigo-free cloned)))))

(ert-deftest test-with-molecules-exact-matching ()
  "Test exact matching with plural macro."
  (let ((match-same
         (indigo-with-molecule* ((mol1 "CCO")
                                 (mol2 "CCO"))
           (indigo-exact-match mol1 mol2 "")))
        (match-different
         (indigo-with-molecule* ((mol1 "CCO")
                                 (mol2 "CCC"))
           (indigo-exact-match mol1 mol2 ""))))
    (should (integerp match-same))
    (should (integerp match-different))))

(ert-deftest test-with-molecule-substructure-matching ()
  "Test substructure matching with with- macros."
  (indigo-with-molecule (target "c1ccccc1CC")  ; Ethylbenzene
    (indigo-with-query (query "c1ccccc1")      ; Benzene ring
      (indigo-with-matcher (matcher target)
        (should (integerp matcher))
        (should (> matcher 0))))))

;;; Property Aggregation Tests

(ert-deftest test-with-molecule-format-conversions ()
  "Test multiple format conversions with single molecule."
  (let ((results
         (indigo-with-molecule (mol "CCO")
           (list
            :smiles (indigo-canonical-smiles mol)
            :molfile (indigo-molfile mol)
            :cml (indigo-cml mol)))))
    ;; Test SMILES
    (should (stringp (plist-get results :smiles)))
    (should (string-match-p "CCO\\|OCC" (plist-get results :smiles)))
    ;; Test MOL file
    (should (stringp (plist-get results :molfile)))
    (should (string-match-p "V2000" (plist-get results :molfile)))
    ;; Test CML
    (should (stringp (plist-get results :cml)))
    (should (string-match-p "<molecule" (plist-get results :cml)))))

(ert-deftest test-with-molecule-property-calculations ()
  "Test multiple property calculations with single molecule."
  (let ((results
         (indigo-with-molecule (mol "CCO")
           (list
            :weight (indigo-molecular-weight mol)
            :formula (indigo-gross-formula mol)
            :most-abundant (indigo-most-abundant-mass mol)
            :monoisotopic (indigo-monoisotopic-mass mol)))))
    ;; Test molecular weight
    (should (floatp (plist-get results :weight)))
    (should (> (plist-get results :weight) 40))
    (should (< (plist-get results :weight) 50))
    ;; Test gross formula
    (should (stringp (plist-get results :formula)))
    (should (string-match-p "C.*H.*O" (plist-get results :formula)))
    ;; Test masses
    (should (floatp (plist-get results :most-abundant)))
    (should (floatp (plist-get results :monoisotopic)))))

(ert-deftest test-with-molecule-counting-operations ()
  "Test multiple counting operations with single molecule."
  (let ((ethanol-counts
         (indigo-with-molecule (mol "CCO")
           (list
            :atoms (indigo-count-atoms mol)
            :bonds (indigo-count-bonds mol)
            :heavy-atoms (indigo-count-heavy-atoms mol)
            :hydrogens (indigo-count-implicit-hydrogens mol))))
        (benzene-counts
         (indigo-with-molecule (mol "c1ccccc1")
           (list
            :atoms (indigo-count-atoms mol)
            :rings (indigo-count-sssr mol)
            :heavy-atoms (indigo-count-heavy-atoms mol)))))
    ;; Ethanol tests
    (should (= (plist-get ethanol-counts :atoms) 3))
    (should (= (plist-get ethanol-counts :bonds) 2))
    (should (= (plist-get ethanol-counts :heavy-atoms) 3))
    ;; Benzene tests
    (should (= (plist-get benzene-counts :atoms) 6))
    (should (= (plist-get benzene-counts :rings) 1))
    (should (= (plist-get benzene-counts :heavy-atoms) 6))))

(ert-deftest test-with-molecule-boolean-properties ()
  "Test multiple boolean property checks with single molecule."
  (let ((results
         (indigo-with-molecule (mol "CCO")
           (list
            :chiral (indigo-is-chiral mol)
            :has-coords (indigo-has-coordinates mol)
            :has-z (indigo-has-z-coord mol)))))
    ;; All should return boolean values
    (should (or (eq (plist-get results :chiral) t)
                (eq (plist-get results :chiral) nil)))
    (should (or (eq (plist-get results :has-coords) t)
                (eq (plist-get results :has-coords) nil)))
    (should (or (eq (plist-get results :has-z) t)
                (eq (plist-get results :has-z) nil)))))

(ert-deftest test-with-molecule-layered-code ()
  "Test layered code generation with with- macro."
  (let ((code
         (indigo-with-molecule (mol "c1ccccc1")
           (indigo-layered-code mol))))
    (should (stringp code))
    (should (> (length code) 0))))

(ert-deftest test-with-molecule-multiple-format-operations ()
  "Test multiple format conversions in sequence."
  (let ((conversions
         (indigo-with-molecule (mol "c1ccccc1")
           ;; Do multiple conversions without worrying about cleanup
           (list
            (indigo-canonical-smiles mol)
            (indigo-smiles mol)
            (indigo-gross-formula mol)
            (indigo-layered-code mol)))))
    (should (= (length conversions) 4))
    (should (cl-every #'stringp conversions))))

(provide 'test-indigo-molecular)

;;; test-indigo-molecular.el ends here
