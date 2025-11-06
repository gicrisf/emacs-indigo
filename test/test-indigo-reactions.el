;;; test-indigo-reactions.el --- Tests for Indigo reaction operations -*- lexical-binding: t; -*-

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

;; Tests for Indigo reaction operations including normalization,
;; PKA functions, atom mapping, and reacting center manipulation.

;;; Code:

(require 'ert)
(require 'indigo)

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
              (should (stringp normalized-smiles)))
            
            ;; Test error handling with invalid handle
            (let ((error-result (indigo-normalize -1)))
              (should (integerp error-result)))))
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
              (should (stringp smiles)))
            
            ;; Test error handling with invalid handle
            (let ((error-result (indigo-standardize -1)))
              (should (integerp error-result)))))
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
            (should (integerp result2)))
          
          ;; Test error handling with invalid handle (should not crash)
          (let ((result3 (indigo-ionize -1 7.0 0.1)))
            (should (integerp result3))))
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
              (should (integerp result5)))
            
            ;; Test error handling with invalid handles
            (should (integerp (indigo-automap -1 "discard"))))  ; Should return error code
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
                (indigo-free reactants-iter)))
            
            ;; Test error handling with invalid handles
            (should (integerp (indigo-get-atom-mapping-number -1 -1)))
            (should (integerp (indigo-set-atom-mapping-number -1 -1 1))))
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
              (should (integerp result)))
            
            ;; Test error handling with invalid handles
            (should (integerp (indigo-clear-aam -1))))
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
              (should (integerp result)))
            
            ;; Test error handling with invalid handles
            (should (integerp (indigo-correct-reacting-centers -1))))
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
                (indigo-free molecules-iter)))
            
            ;; Test error handling with invalid handles
            (should (null (indigo-get-reacting-center -1 -1)))
            (should (integerp (indigo-set-reacting-center -1 -1 1))))
        (when (and rxn (> rxn 0))
          (indigo-free rxn))))))

(provide 'test-indigo-reactions)

;;; test-indigo-reactions.el ends here
