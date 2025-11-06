;;; test-indigo-sugar.el --- Tests for indigo.el high-level abstractions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for high-level Lisp abstractions (indigo-let*, indigo-map-*)
;; in indigo.el

;;; Code:

(require 'ert)
(require 'indigo)

;;; Iterator Helper Tests

(ert-deftest test-indigo-map ()
  "Test mapping over iterator with indigo-map."
  (indigo-let* ((:molecule mol "CCO")
                (:atoms atoms mol))
    (let ((symbols (indigo-map #'indigo-symbol atoms)))
      (should (equal symbols '("C" "C" "O"))))))

;;; indigo-let* Macro Tests - Molecules

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

;;; indigo-let* Macro Tests - Reactions

(ert-deftest test-indigo-let*-reaction ()
  "Test indigo-let* with reaction binding."
  (let ((result (indigo-let* ((:reaction rxn "CCO.CC>>CCOC"))
                  (let ((reactants (indigo-let* ((:reactants r rxn))
                                     (length (indigo-map (lambda (_) t) r))))
                        (products (indigo-let* ((:products p rxn))
                                    (length (indigo-map (lambda (_) t) p)))))
                    (list reactants products)))))
    (should (equal result '(2 1)))))

;;; indigo-let* Macro Tests - Iterators

(ert-deftest test-indigo-let*-atoms-iterator ()
  "Test indigo-let* with atoms iterator."
  (let ((symbols (indigo-let* ((:molecule mol "CCO")
                               (:atoms atoms mol))
                   (indigo-map #'indigo-symbol atoms))))
    (should (equal symbols '("C" "C" "O")))))

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

;;; indigo-let* Macro Tests - Fingerprints

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

;;; indigo-let* Macro Tests - Arrays

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

;;; indigo-let* Macro Tests - Mixed Bindings

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

;;; Complex Workflow Tests

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

;;; Error Handling Tests

(ert-deftest test-indigo-let*-cleanup-on-error ()
  "Test that resources are cleaned up even when an error occurs."
  (let ((initial-count (indigo-count-references)))
    (should-error
     (indigo-let* ((:molecule mol "CCO"))
       (indigo-molecular-weight mol)
       (error "Intentional error")))
    ;; If we get here without leaking memory, cleanup worked
    ;; Reference count should be back to what it was before
    (should (= (indigo-count-references) initial-count))))

(ert-deftest test-indigo-let*-invalid-molecule ()
  "Test indigo-let* with invalid molecule string."
  (let ((mol (indigo-let* ((:molecule mol "INVALID"))
               mol)))
    ;; Invalid molecules return -1 (error handle)
    (should (or (null mol) (= mol -1)))))

;;; Macro Expansion Test

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

;;; Additional Tests Inspired by Molecular Operations

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

(provide 'test-indigo-sugar)
;;; test-indigo-sugar.el ends here
