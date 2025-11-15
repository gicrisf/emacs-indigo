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
(require 'indigo-mol)

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

;;; With-style Reaction Macro Tests

(ert-deftest test-indigo-with-reaction ()
  "Test indigo-with-reaction macro."
  (indigo-with-reaction (rxn "CC>>CCO")
    (should (integerp rxn))
    (should (> rxn 0))
    ;; Verify it's a valid reaction by iterating reactants
    (indigo-with-reactants-iterator (reactants rxn)
      (should (integerp reactants))
      (should (> reactants 0)))))

(ert-deftest test-indigo-with-reaction-complex ()
  "Test indigo-with-reaction with complex reaction."
  (indigo-with-reaction (rxn "CCO.CC>>CCOC")
    (should (integerp rxn))
    (should (> rxn 0))
    ;; Count reactants manually
    (let ((reactant-count 0))
      (indigo-with-reactants-iterator (reactants rxn)
        (while (indigo-next reactants)
          (setq reactant-count (1+ reactant-count))))
      (should (= reactant-count 2)))
    ;; Count products manually
    (let ((product-count 0))
      (indigo-with-products-iterator (products rxn)
        (while (indigo-next products)
          (setq product-count (1+ product-count))))
      (should (= product-count 1)))))

(ert-deftest test-indigo-with-reaction-workflow ()
  "Test reaction workflow with indigo-with-* macros."
  (indigo-with-reaction (rxn "CCO.CC>>CCOC")
    (let ((reactant-count 0)
          (product-count 0))
      (indigo-with-reactants-iterator (reactants rxn)
        (while (indigo-next reactants)
          (setq reactant-count (1+ reactant-count))))
      (indigo-with-products-iterator (products rxn)
        (while (indigo-next products)
          (setq product-count (1+ product-count))))
      (should (= reactant-count 2))
      (should (= product-count 1)))))

(provide 'test-indigo-reactions)

;;; test-indigo-reactions.el ends here
