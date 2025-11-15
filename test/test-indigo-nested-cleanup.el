;;; test-indigo-nested-cleanup.el --- Comprehensive nested scope cleanup tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; These tests verify that the sequential nesting approach (using indigo-with-*
;; macros) properly manages resources at each nesting level and cleans up
;; correctly even when errors occur.
;;

;;; Code:

(require 'ert)
(require 'indigo)
(require 'indigo-mol)

;;; Single-level nesting tests

(ert-deftest test-nested-cleanup-single-molecule ()
  "Test reference counting with a single molecule."
  (let ((initial-refs (indigo-count-references)))
    ;; Outside scope: no resources allocated
    (should (= (indigo-count-references) initial-refs))

    (indigo-with-molecule (mol "CCO")
      ;; Inside scope: 1 molecule allocated
      (should (= (indigo-count-references) (+ initial-refs 1)))
      (should (integerp mol))
      (should (> mol 0)))

    ;; Outside scope: resource freed
    (should (= (indigo-count-references) initial-refs))))

;;; Two-level nesting tests

(ert-deftest test-nested-cleanup-two-molecules-nested ()
  "Test reference counting with two nested molecules."
  (let ((initial-refs (indigo-count-references)))
    (should (= (indigo-count-references) initial-refs))

    (indigo-with-molecule (mol1 "CCO")
      ;; Level 1: 1 molecule allocated
      (should (= (indigo-count-references) (+ initial-refs 1)))

      (indigo-with-molecule (mol2 "c1ccccc1")
        ;; Level 2: 2 molecules allocated
        (should (= (indigo-count-references) (+ initial-refs 2)))
        (should (integerp mol1))
        (should (integerp mol2))
        (should (> mol1 0))
        (should (> mol2 0)))

      ;; Back to level 1: mol2 freed, mol1 still alive
      (should (= (indigo-count-references) (+ initial-refs 1))))

    ;; Outside scope: all resources freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-two-molecules-plural ()
  "Test reference counting with plural macro (sequential under the hood)."
  (let ((initial-refs (indigo-count-references)))
    (should (= (indigo-count-references) initial-refs))

    (indigo-with-molecule* ((mol1 "CCO")
                            (mol2 "c1ccccc1"))
      ;; Inside scope: 2 molecules allocated (created sequentially)
      (should (= (indigo-count-references) (+ initial-refs 2)))
      (should (integerp mol1))
      (should (integerp mol2))
      (should (> mol1 0))
      (should (> mol2 0)))

    ;; Outside scope: all resources freed
    (should (= (indigo-count-references) initial-refs))))

;;; Three-level nesting tests

(ert-deftest test-nested-cleanup-three-molecules-nested ()
  "Test reference counting with three nested molecules."
  (let ((initial-refs (indigo-count-references)))
    (should (= (indigo-count-references) initial-refs))

    (indigo-with-molecule (mol1 "CCO")
      ;; Level 1: 1 molecule
      (should (= (indigo-count-references) (+ initial-refs 1)))

      (indigo-with-molecule (mol2 "c1ccccc1")
        ;; Level 2: 2 molecules
        (should (= (indigo-count-references) (+ initial-refs 2)))

        (indigo-with-molecule (mol3 "CCC")
          ;; Level 3: 3 molecules
          (should (= (indigo-count-references) (+ initial-refs 3)))
          (should (integerp mol1))
          (should (integerp mol2))
          (should (integerp mol3))
          (should (> mol1 0))
          (should (> mol2 0))
          (should (> mol3 0)))

        ;; Back to level 2: mol3 freed
        (should (= (indigo-count-references) (+ initial-refs 2))))

      ;; Back to level 1: mol2 and mol3 freed
      (should (= (indigo-count-references) (+ initial-refs 1))))

    ;; Outside scope: all freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-three-molecules-plural ()
  "Test reference counting with three molecules using plural macro."
  (let ((initial-refs (indigo-count-references)))
    (should (= (indigo-count-references) initial-refs))

    (indigo-with-molecule* ((mol1 "CCO")
                            (mol2 "c1ccccc1")
                            (mol3 "CCC"))
      ;; Inside scope: 3 molecules allocated
      (should (= (indigo-count-references) (+ initial-refs 3)))
      (should (integerp mol1))
      (should (integerp mol2))
      (should (integerp mol3)))

    ;; Outside scope: all freed
    (should (= (indigo-count-references) initial-refs))))

;;; Error handling with partial cleanup

(ert-deftest test-nested-cleanup-error-in-first-molecule ()
  "Test cleanup when first molecule creation fails."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule (mol1 "INVALID_SMILES")
       (message "Should not reach here")))

    ;; No resources leaked (mol1 never created)
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-error-in-second-molecule ()
  "Test cleanup when second molecule creation fails (first should still cleanup)."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule (mol1 "CCO")
       (indigo-with-molecule (mol2 "INVALID_SMILES")
         (message "Should not reach here"))))

    ;; All resources cleaned up (mol1 was created then freed via unwind-protect)
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-error-in-third-molecule ()
  "Test cleanup when third molecule creation fails (first two should cleanup)."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule (mol1 "CCO")
       (indigo-with-molecule (mol2 "c1ccccc1")
         (indigo-with-molecule (mol3 "INVALID_SMILES")
           (message "Should not reach here")))))

    ;; All resources cleaned up
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-error-in-plural-second ()
  "Test cleanup with plural macro when second molecule fails."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule* ((mol1 "CCO")
                             (mol2 "INVALID_SMILES")
                             (mol3 "CCC"))
       (message "Should not reach here")))

    ;; All resources cleaned up (mol1 was created then freed)
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-error-in-plural-third ()
  "Test cleanup with plural macro when third molecule fails."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule* ((mol1 "CCO")
                             (mol2 "c1ccccc1")
                             (mol3 "INVALID_SMILES"))
       (message "Should not reach here")))

    ;; All resources cleaned up (mol1 and mol2 were created then freed)
    (should (= (indigo-count-references) initial-refs))))

;;; Error in body (after all molecules created)

(ert-deftest test-nested-cleanup-error-in-body-nested ()
  "Test cleanup when error occurs in body (after molecules created)."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule (mol1 "CCO")
       (indigo-with-molecule (mol2 "c1ccccc1")
         (error "Test error in body"))))

    ;; All resources cleaned up
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-error-in-body-plural ()
  "Test cleanup when error occurs in body with plural macro."
  (let ((initial-refs (indigo-count-references)))
    (should-error
     (indigo-with-molecule* ((mol1 "CCO")
                             (mol2 "c1ccccc1")
                             (mol3 "CCC"))
       (error "Test error in body")))

    ;; All resources cleaned up
    (should (= (indigo-count-references) initial-refs))))

;;; Mixed molecule and iterator nesting

(ert-deftest test-nested-cleanup-molecule-and-iterator ()
  "Test reference counting with molecule and iterator nesting."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule (mol "CCO")
      ;; Level 1: 1 molecule
      (should (= (indigo-count-references) (+ initial-refs 1)))

      (indigo-with-atoms-iterator (atoms mol)
        ;; Level 2: molecule + iterator
        ;; Note: iterators may or may not increment reference count
        ;; depending on Indigo implementation
        (should (>= (indigo-count-references) (+ initial-refs 1)))
        (let ((atom (indigo-next atoms)))
          (should atom)
          (indigo-free atom)))

      ;; Back to level 1: iterator freed
      (should (= (indigo-count-references) (+ initial-refs 1))))

    ;; Outside scope: all freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-multiple-molecules-and-iterators ()
  "Test complex nesting with multiple molecules and iterators."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule* ((mol1 "CCO")
                            (mol2 "c1ccccc1"))
      ;; Both molecules allocated
      (should (= (indigo-count-references) (+ initial-refs 2)))

      (indigo-with-atoms-iterator (atoms1 mol1)
        ;; Molecule + iterator
        (should (>= (indigo-count-references) (+ initial-refs 2)))

        (indigo-with-atoms-iterator (atoms2 mol2)
          ;; 2 molecules + 2 iterators
          (should (>= (indigo-count-references) (+ initial-refs 2)))
          (let ((atom1 (indigo-next atoms1))
                (atom2 (indigo-next atoms2)))
            (should atom1)
            (should atom2)
            (indigo-free atom1)
            (indigo-free atom2)))

        ;; Second iterator freed
        (should (>= (indigo-count-references) (+ initial-refs 2))))

      ;; First iterator freed
      (should (= (indigo-count-references) (+ initial-refs 2))))

    ;; All freed
    (should (= (indigo-count-references) initial-refs))))

;;; Mixed resource types

(ert-deftest test-nested-cleanup-molecules-and-fingerprints ()
  "Test reference counting with molecules and fingerprints."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule* ((mol1 "CCO")
                            (mol2 "c1ccccc1"))
      ;; 2 molecules
      (should (= (indigo-count-references) (+ initial-refs 2)))

      (indigo-with-fingerprint* ((fp1 mol1 "sim")
                                 (fp2 mol2 "sim"))
        ;; 2 molecules + 2 fingerprints = 4
        (should (= (indigo-count-references) (+ initial-refs 4)))
        (let ((sim (indigo-similarity fp1 fp2)))
          (should (floatp sim))))

      ;; Fingerprints freed, molecules still alive
      (should (= (indigo-count-references) (+ initial-refs 2))))

    ;; All freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-reaction-and-iterators ()
  "Test reference counting with reactions and iterators."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-reaction (rxn "CCO.CC>>CCOC")
      ;; 1 reaction
      (should (= (indigo-count-references) (+ initial-refs 1)))

      (indigo-with-reactants-iterator (reactants rxn)
        ;; Reaction + iterator
        (should (>= (indigo-count-references) (+ initial-refs 1)))

        (indigo-with-products-iterator (products rxn)
          ;; Reaction + 2 iterators
          (should (>= (indigo-count-references) (+ initial-refs 1)))
          (should reactants)
          (should products))

        ;; Products iterator freed
        (should (>= (indigo-count-references) (+ initial-refs 1))))

      ;; Reactants iterator freed
      (should (= (indigo-count-references) (+ initial-refs 1))))

    ;; All freed
    (should (= (indigo-count-references) initial-refs))))

;;; Deep nesting (stress test)

(ert-deftest test-nested-cleanup-five-levels ()
  "Test reference counting with five levels of nesting (stress test)."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule (mol1 "C")
      (should (= (indigo-count-references) (+ initial-refs 1)))

      (indigo-with-molecule (mol2 "CC")
        (should (= (indigo-count-references) (+ initial-refs 2)))

        (indigo-with-molecule (mol3 "CCC")
          (should (= (indigo-count-references) (+ initial-refs 3)))

          (indigo-with-molecule (mol4 "CCCC")
            (should (= (indigo-count-references) (+ initial-refs 4)))

            (indigo-with-molecule (mol5 "CCCCC")
              (should (= (indigo-count-references) (+ initial-refs 5)))
              ;; All 5 molecules should be alive
              (should (integerp mol1))
              (should (integerp mol2))
              (should (integerp mol3))
              (should (integerp mol4))
              (should (integerp mol5)))

            ;; mol5 freed
            (should (= (indigo-count-references) (+ initial-refs 4))))

          ;; mol4 and mol5 freed
          (should (= (indigo-count-references) (+ initial-refs 3))))

        ;; mol3, mol4, mol5 freed
        (should (= (indigo-count-references) (+ initial-refs 2))))

      ;; mol2-mol5 freed
      (should (= (indigo-count-references) (+ initial-refs 1))))

    ;; All freed
    (should (= (indigo-count-references) initial-refs))))

(ert-deftest test-nested-cleanup-five-molecules-plural ()
  "Test reference counting with five molecules using plural macro."
  (let ((initial-refs (indigo-count-references)))
    (indigo-with-molecule* ((mol1 "C")
                            (mol2 "CC")
                            (mol3 "CCC")
                            (mol4 "CCCC")
                            (mol5 "CCCCC"))
      ;; All 5 allocated
      (should (= (indigo-count-references) (+ initial-refs 5)))
      (should (integerp mol1))
      (should (integerp mol2))
      (should (integerp mol3))
      (should (integerp mol4))
      (should (integerp mol5)))

    ;; All freed
    (should (= (indigo-count-references) initial-refs))))

(provide 'test-indigo-nested-cleanup)

;;; test-indigo-nested-cleanup.el ends here
