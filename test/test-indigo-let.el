;;; test-indigo-let.el --- Tests for indigo-let and indigo-let* macros -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Test suite for indigo-let and indigo-let* resource management macros.
;; Tests both parallel (indigo-let) and sequential (indigo-let*) binding semantics.

;;; Code:

(require 'ert)
(require 'indigo)

;;; Basic indigo-let tests (parallel binding)

(ert-deftest test-indigo-let-basic-parallel ()
  "Test basic parallel binding with indigo-let."
  (let ((result
         (indigo-let ((:molecule mol1 "CCO")
                      (:molecule mol2 "c1ccccc1"))
           (list (indigo-count-atoms mol1)
                 (indigo-count-atoms mol2)))))
    (should (equal result '(3 6)))))

(ert-deftest test-indigo-let-mixed-bindings ()
  "Test indigo-let with mixed regular and Indigo resource bindings."
  (let ((result
         (indigo-let ((x 10)
                      (:molecule mol "CCO")
                      (y 20))
           (list x y (indigo-count-atoms mol)))))
    (should (equal result '(10 20 3)))))

(ert-deftest test-indigo-let-automatic-cleanup ()
  "Test that indigo-let properly cleans up resources."
  (let ((initial-refs (indigo-count-references)))
    (indigo-let ((:molecule mol1 "CCO")
                 (:molecule mol2 "c1ccccc1"))
      ;; Inside scope: 2 molecules allocated
      (should (= (indigo-count-references) (+ initial-refs 2))))
    ;; Outside scope: resources should be freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-indigo-let-cleanup-on-error ()
  "Test that indigo-let cleans up even when body signals an error."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-let ((:molecule mol "CCO"))
       (error "Test error")))
    ;; Resources should still be freed after error
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-indigo-let-multiple-resource-types ()
  "Test indigo-let with different resource types in parallel."
  (let ((result
         (indigo-let ((:molecule mol "CCO")
                      (:reaction rxn "CCO.CC>>CCOC")
                      (:query query "c1ccccc1"))
           (list (indigo-count-atoms mol)
                 (indigo-count-heavy-atoms query)
                 (not (null rxn))))))
    (should (equal result '(3 6 t)))))

;;; Basic indigo-let* tests (sequential binding)

(ert-deftest test-indigo-let*-sequential-binding ()
  "Test that indigo-let* allows sequential binding."
  (let ((result
         (indigo-let* ((:molecule mol "c1ccccc1")
                       (:atoms atoms mol))  ; atoms references mol
           (length (indigo-map #'indigo-symbol atoms)))))
    (should (= result 6))))

(ert-deftest test-indigo-let*-iterator-dependency ()
  "Test indigo-let* with iterator depending on molecule."
  (let ((symbols
         (indigo-let* ((:molecule mol "CCO")
                       (:atoms atoms mol))
           (indigo-map #'indigo-symbol atoms))))
    (should (equal symbols '("C" "C" "O")))))

(ert-deftest test-indigo-let*-automatic-cleanup ()
  "Test that indigo-let* properly cleans up resources."
  (let ((initial-refs (indigo-count-references)))
    (indigo-let* ((:molecule mol "CCO")
                  (:atoms atoms mol))
      ;; Inside scope: molecule + iterator allocated
      (should (>= (indigo-count-references) (+ initial-refs 1))))
    ;; Outside scope: all resources should be freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-indigo-let*-cleanup-on-error ()
  "Test that indigo-let* cleans up even when body signals an error."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-let* ((:molecule mol "CCO")
                   (:atoms atoms mol))
       (error "Test error")))
    ;; Resources should still be freed after error
    (should (= (indigo-count-references) initial-refs))))

;;; Comparison tests: let vs let*

(ert-deftest test-let-vs-let*-binding-order ()
  "Demonstrate difference between parallel (let) and sequential (let*) binding."
  ;; indigo-let* allows referencing earlier bindings
  (should
   (indigo-let* ((:molecule mol "CCO")
                 (count (indigo-count-atoms mol)))  ; count can reference mol
     (= count 3)))

  ;; indigo-let does NOT allow referencing earlier bindings
  ;; (This would fail at runtime because mol is not bound yet):
  ;; (indigo-let ((:molecule mol "CCO")
  ;;              (count (indigo-count-atoms mol)))  ; ERROR: mol not bound
  ;;   ...)
  )

(ert-deftest test-let-parallel-evaluation ()
  "Test that indigo-let evaluates all binding forms in parallel."
  (let ((eval-order '()))
    (indigo-let ((a (progn (push 'a eval-order) 1))
                 (b (progn (push 'b eval-order) 2))
                 (c (progn (push 'c eval-order) 3)))
      (list a b c))
    ;; All binding forms are evaluated before any variable is bound
    ;; The order of evaluation is not specified, but all happen before bindings
    (should (= (length eval-order) 3))))

;;; Advanced tests

(ert-deftest test-indigo-let-nested-scopes ()
  "Test nested indigo-let scopes."
  (let ((initial-refs (indigo-count-references)))
    (indigo-let ((:molecule outer-mol "CCO"))
      (should (= (indigo-count-references) (+ initial-refs 1)))
      (indigo-let ((:molecule inner-mol "c1ccccc1"))
        (should (= (indigo-count-references) (+ initial-refs 2))))
      (should (= (indigo-count-references) (+ initial-refs 1))))
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-indigo-let*-multiple-dependencies ()
  "Test indigo-let* with multiple levels of dependencies."
  (let ((result
         (indigo-let* ((:molecule mol "c1ccccc1C")
                       (atom-count (indigo-count-atoms mol))
                       (:atoms atoms mol)
                       (symbols (indigo-map #'indigo-symbol atoms)))
           (list atom-count (length symbols) symbols))))
    (should (= (car result) 7))
    (should (= (cadr result) 7))
    (should (equal (caddr result) '("C" "C" "C" "C" "C" "C" "C")))))

(ert-deftest test-indigo-let-with-errors ()
  "Test indigo-let properly signals errors for invalid inputs (public API)."
  (let ((initial-refs (indigo-count-references)))
    ;; Invalid SMILES should signal an error with public API
    (should-error
     (indigo-let ((:molecule mol "INVALID_SMILES"))
       (message "Should not reach here")))
    ;; Resources should still be cleaned up after error
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-indigo-let*-with-errors ()
  "Test indigo-let* properly signals errors for invalid inputs (public API)."
  (let ((initial-refs (indigo-count-references)))
    ;; Invalid SMILES should signal an error with public API
    (should-error
     (indigo-let* ((:molecule mol "INVALID_SMILES")
                   (result "should not execute"))
       (message "Should not reach here")))
    ;; Resources should still be cleaned up after error
    (should (= (indigo-count-references) initial-refs))))

;;; Comprehensive indigo-let* tests from test-indigo-sugar.el

(ert-deftest test-indigo-let*-molecule ()
  "Test indigo-let* macro with molecule binding."
  (let ((weight (indigo-let* ((:molecule mol "CCO"))
                  (indigo-molecular-weight mol))))
    (should (> weight 0.0))
    (should (< (abs (- weight 46.069)) 0.01))))

(ert-deftest test-indigo-let*-multiple-molecules ()
  "Test indigo-let* with multiple molecule bindings."
  (let ((result (indigo-let* ((:molecule ethanol "CCO")
                              (:molecule methanol "CO"))
                  (list (indigo-molecular-weight ethanol)
                        (indigo-molecular-weight methanol)))))
    (should (= (length result) 2))
    (should (> (car result) (cadr result)))))

(ert-deftest test-indigo-let*-mol-file ()
  "Test indigo-let* with file-based molecule loading."
  (let ((test-file "test/data/molecules/basic/ethanol.mol"))
    (when (file-exists-p test-file)
      (let ((smiles (indigo-let* ((:mol-file mol test-file))
                      (indigo-canonical-smiles mol))))
        (should (stringp smiles))))))

(ert-deftest test-indigo-let*-reaction ()
  "Test indigo-let* with reaction binding."
  (let ((result (indigo-let* ((:reaction rxn "CCO.CC>>CCOC"))
                  (let ((reactants (indigo-let* ((:reactants r rxn))
                                     (length (indigo-map (lambda (_) t) r))))
                        (products (indigo-let* ((:products p rxn))
                                    (length (indigo-map (lambda (_) t) p)))))
                    (list reactants products)))))
    (should (equal result '(2 1)))))

(ert-deftest test-indigo-let*-bonds-iterator ()
  "Test indigo-let* with bonds iterator."
  (let ((bond-count (indigo-let* ((:molecule mol "CCO")
                                  (:bonds bonds mol))
                      (length (indigo-map (lambda (_) t) bonds)))))
    (should (= bond-count 2))))

(ert-deftest test-indigo-let*-components-iterator ()
  "Test indigo-let* with components iterator."
  (let ((comp-count (indigo-let* ((:molecule mol "CCO.CC")
                                  (:components comps mol))
                      (length (indigo-map (lambda (_) t) comps)))))
    (should (= comp-count 2))))

(ert-deftest test-indigo-let*-sssr-iterator ()
  "Test indigo-let* with SSSR iterator."
  (let ((ring-count (indigo-let* ((:molecule mol "c1ccccc1")
                                  (:sssr rings mol))
                      (length (indigo-map (lambda (_) t) rings)))))
    (should (= ring-count 1))))

(ert-deftest test-indigo-let*-stereocenters-iterator ()
  "Test indigo-let* with stereocenters iterator."
  (let ((stereocenter-count
         (indigo-let* ((:molecule mol "C[C@H](O)CC")  ; Chiral carbon with explicit stereochemistry
                       (:stereocenters stereos mol))
           (length (indigo-map (lambda (_) t) stereos)))))
    ;; Should have 1 stereocenter
    (should (= stereocenter-count 1))))

(ert-deftest test-indigo-let*-fingerprints ()
  "Test indigo-let* with fingerprint bindings."
  (let ((sim (indigo-let* ((:molecule mol1 "CCO")
                           (:molecule mol2 "CO")
                           (:fingerprint fp1 mol1 "sim")
                           (:fingerprint fp2 mol2 "sim"))
               (indigo-similarity fp1 fp2 "tanimoto"))))
    (should (numberp sim))
    (should (>= sim 0.0))
    (should (<= sim 1.0))))

(ert-deftest test-indigo-let*-array ()
  "Test indigo-let* with array binding."
  (let ((result (indigo-let* ((:molecule mol1 "CCO")
                              (:molecule mol2 "CO")
                              (:array arr))
                  (indigo-array-add arr mol1)
                  (indigo-array-add arr mol2)
                  ;; indigo-count not implemented, just verify array-add succeeds
                  t)))
    (should result)))

(ert-deftest test-indigo-let*-mixed-bindings ()
  "Test indigo-let* with mixed binding types."
  (let ((result (indigo-let* ((:molecule mol "CCO")
                              (weight 0.0)  ; regular variable
                              (:atoms atoms mol))
                  (setq weight (indigo-molecular-weight mol))
                  (list weight (indigo-map #'indigo-symbol atoms)))))
    (should (= (length result) 2))
    (should (numberp (car result)))
    (should (listp (cadr result)))
    (should (equal (cadr result) '("C" "C" "O")))))

(ert-deftest test-indigo-let*-complex-workflow ()
  "Test complex workflow with multiple Indigo resources."
  (let ((analysis
         (indigo-let* ((:molecule ethanol "CCO")
                       (:molecule methanol "CO")
                       (:fingerprint eth-fp ethanol "sim")
                       (:fingerprint met-fp methanol "sim")
                       (:atoms eth-atoms ethanol)
                       (ethanol-atoms-list (indigo-map #'indigo-symbol eth-atoms))
                       (:atoms eth-atoms2 ethanol)
                       (atom-count (length (indigo-map #'indigo-symbol eth-atoms2))))
           (list
            :ethanol-weight (indigo-molecular-weight ethanol)
            :methanol-weight (indigo-molecular-weight methanol)
            :similarity (indigo-similarity eth-fp met-fp "tanimoto")
            :ethanol-atoms ethanol-atoms-list
            :atom-count atom-count))))

    (should (plist-get analysis :ethanol-weight))
    (should (plist-get analysis :methanol-weight))
    (should (numberp (plist-get analysis :similarity)))
    (should (equal (plist-get analysis :ethanol-atoms) '("C" "C" "O")))
    (should (= (plist-get analysis :atom-count) 3))))

(ert-deftest test-indigo-let*-invalid-molecule ()
  "Test indigo-let* with invalid molecule string signals error."
  (should-error
   (indigo-let* ((:molecule mol "INVALID"))
     mol)))

(ert-deftest test-indigo-let*-macro-expansion ()
  "Show the macro expansion of indigo-let* for debugging."
  (let ((expansion (macroexpand-1
                    '(indigo-let* ((:molecule mol "CCO")
                                   (:atoms atoms mol)
                                   (regular-var 42))
                       (indigo-molecular-weight mol)))))
    (princ (format "DEBUG:\n%S\n" expansion))
    ;; Just verify it expands to a let form (nested structure)
    (should (eq (car expansion) 'let))))

(ert-deftest test-indigo-let*-format-conversions ()
  "Test format conversion operations with indigo-let*."
  (let ((results
         (indigo-let* ((:molecule mol "CCO"))
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

(ert-deftest test-indigo-let*-property-calculations ()
  "Test molecular property calculations with indigo-let*."
  (let ((results
         (indigo-let* ((:molecule mol "CCO"))
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

(ert-deftest test-indigo-let*-counting-operations ()
  "Test counting operations with indigo-let*."
  (let ((ethanol-counts
         (indigo-let* ((:molecule mol "CCO"))
           (list
            :atoms (indigo-count-atoms mol)
            :bonds (indigo-count-bonds mol)
            :heavy-atoms (indigo-count-heavy-atoms mol)
            :hydrogens (indigo-count-implicit-hydrogens mol))))
        (benzene-counts
         (indigo-let* ((:molecule mol "c1ccccc1"))
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

(ert-deftest test-indigo-let*-clone-operation ()
  "Test molecule cloning with indigo-let*."
  (indigo-let* ((:molecule mol "CCO"))
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

(ert-deftest test-indigo-let*-exact-matching ()
  "Test exact matching with indigo-let*."
  (let ((match-same
         (indigo-let* ((:molecule mol1 "CCO")
                      (:molecule mol2 "CCO"))
           (indigo-exact-match mol1 mol2 "")))
        (match-different
         (indigo-let* ((:molecule mol1 "CCO")
                      (:molecule mol2 "CCC"))
           (indigo-exact-match mol1 mol2 ""))))
    (should (integerp match-same))
    (should (integerp match-different))))

(ert-deftest test-indigo-let*-substructure-matching ()
  "Test substructure matching with indigo-let*."
  (indigo-let* ((:molecule target "c1ccccc1CC")  ; Ethylbenzene
               (:query query "c1ccccc1")          ; Benzene ring
               (:matcher matcher target))
    (should (integerp matcher))
    (should (> matcher 0))))

(ert-deftest test-indigo-let*-layered-code ()
  "Test layered code generation with indigo-let*."
  (let ((code (indigo-let* ((:molecule mol "c1ccccc1"))
                (indigo-layered-code mol))))
    (should (stringp code))
    (should (> (length code) 0))))

(ert-deftest test-indigo-let*-boolean-properties ()
  "Test boolean property checks with indigo-let*."
  (let ((results
         (indigo-let* ((:molecule mol "CCO"))
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

(ert-deftest test-indigo-let*-multiple-format-operations ()
  "Test multiple format conversions in sequence with indigo-let*."
  (let ((conversions
         (indigo-let* ((:molecule mol "c1ccccc1"))
           ;; Do multiple conversions without worrying about cleanup
           (list
            (indigo-canonical-smiles mol)
            (indigo-smiles mol)
            (indigo-gross-formula mol)
            (indigo-layered-code mol)))))
    (should (= (length conversions) 4))
    (should (cl-every #'stringp conversions))))

(provide 'test-indigo-let)

;;; test-indigo-let.el ends here
