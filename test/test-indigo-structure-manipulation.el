;;; test-indigo-structure-manipulation.el --- Tests for structure manipulation functions -*- lexical-binding: t; -*-

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

;; Tests for Indigo structure manipulation functions including:
;; - indigo-aromatize (detect and mark aromatic systems)
;; - indigo-layout (calculate 2D coordinates)
;; - indigo-fold-hydrogens (remove explicit H atoms)
;; - indigo-unfold-hydrogens (add explicit H atoms)

;;; Code:

(require 'ert)
(require 'indigo)
(require 'indigo-mol)

;;; Aromatization Tests

(ert-deftest test-indigo-aromatize-api ()
  "Test aromatize function with new idiomatic API."
  ;; Test 1: Kekulé benzene - should return :changed
  (indigo-with-molecule (mol1 "C1=CC=CC=C1")
    (should (eq (indigo-aromatize mol1) :changed)))

  ;; Test 2: Already aromatic benzene - should return :unchanged
  (indigo-with-molecule (mol2 "c1ccccc1")
    (should (eq (indigo-aromatize mol2) :unchanged)))

  ;; Test 3: Pyridine (already aromatic) - should return :unchanged
  (indigo-with-molecule (mol3 "c1cccnc1")
    (should (eq (indigo-aromatize mol3) :unchanged)))

  ;; Test 4: Propane (aliphatic, no aromatic rings) - should return :unchanged
  (indigo-with-molecule (mol4 "CCC")
    (should (eq (indigo-aromatize mol4) :unchanged))))

(ert-deftest test-indigo-aromatize-benzene ()
  "Test aromatizing benzene from Kekulé form."
  (indigo-with-molecule (mol "C1=CC=CC=C1") ; Kekulé benzene
    ;; Should return :changed (Kekulé converted to aromatic)
    (let ((status (indigo-aromatize mol)))
      (should (eq status :changed)))))

(ert-deftest test-indigo-aromatize-aliphatic ()
  "Test aromatizing aliphatic molecule (no aromatic rings)."
  (indigo-with-molecule (mol "CCC") ; Propane
    ;; Should return :unchanged (no aromatic rings)
    (let ((status (indigo-aromatize mol)))
      (should (eq status :unchanged)))))

(ert-deftest test-indigo-aromatize-heterocycle ()
  "Test aromatizing heterocyclic aromatic compound."
  (indigo-with-molecule (mol "c1cccnc1") ; Pyridine (already aromatic)
    ;; Should return :unchanged (already aromatic)
    (let ((status (indigo-aromatize mol)))
      (should (eq status :unchanged)))))

;;; Layout (2D Coordinate Calculation) Tests

(ert-deftest test-indigo-layout-api ()
  "Test layout function with new idiomatic API."
  ;; Test 1: Molecule without coordinates
  (indigo-with-molecule (mol1 "CCO")
    (should-not (indigo-has-coordinates mol1))
    (should (indigo-layout mol1))
    (should (indigo-has-coordinates mol1)))

  ;; Test 2: Call layout again (should succeed - recalculates coords)
  (indigo-with-molecule (mol2 "CCO")
    (should (indigo-layout mol2)) ; First layout
    (should (indigo-has-coordinates mol2))
    (should (indigo-layout mol2)) ; Second layout should also succeed
    (should (indigo-has-coordinates mol2)))

  ;; Test 3: Benzene
  (indigo-with-molecule (mol3 "c1ccccc1")
    (should (indigo-layout mol3)))

  ;; Test 4: Complex molecule
  (indigo-with-molecule (mol4 "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")
    (should (indigo-layout mol4))))

(ert-deftest test-indigo-layout-simple ()
  "Test calculating 2D coordinates for simple molecule."
  (indigo-with-molecule (mol "CCO") ; Ethanol
    ;; Calculate layout (returns t on success)
    (should (indigo-layout mol))
    ;; After layout, should have coordinates
    (should (indigo-has-coordinates mol))))

(ert-deftest test-indigo-layout-benzene ()
  "Test calculating 2D coordinates for benzene."
  (indigo-with-molecule (mol "c1ccccc1") ; Benzene
    ;; Calculate layout (returns t on success)
    (should (indigo-layout mol))
    ;; After layout, should have coordinates
    (should (indigo-has-coordinates mol))
    ;; Should not have Z coordinates (2D only)
    (should-not (indigo-has-z-coord mol))))

(ert-deftest test-indigo-layout-complex ()
  "Test calculating 2D coordinates for complex molecule."
  (indigo-with-molecule (mol "CN1C=NC2=C1C(=O)N(C(=O)N2C)C") ; Caffeine
    ;; Calculate layout (returns t on success)
    (should (indigo-layout mol))
    ;; After layout, should have coordinates
    (should (indigo-has-coordinates mol))))

;;; Hydrogen Folding Tests

(ert-deftest test-indigo-fold-unfold-hydrogens-api ()
  "Test fold/unfold hydrogens with new idiomatic API."
  (indigo-with-molecule (mol "CCO") ; Ethanol
    (let ((initial-count (indigo-count-atoms mol)))
      (should (= initial-count 3)))

    ;; Unfold should succeed and return t
    (should (indigo-unfold-hydrogens mol))
    (let ((unfolded-count (indigo-count-atoms mol)))
      (should (> unfolded-count 3)))

    ;; Fold should succeed and return t
    (should (indigo-fold-hydrogens mol))
    (let ((folded-count (indigo-count-atoms mol)))
      (should (= folded-count 3)))))

(ert-deftest test-indigo-fold-hydrogens-ethanol ()
  "Test folding (removing) explicit hydrogen atoms from ethanol."
  (indigo-with-molecule (mol "CCO") ; Ethanol
    ;; First unfold to add explicit hydrogens
    (should (indigo-unfold-hydrogens mol))
    ;; Count atoms with explicit H
    (let ((atom-count-with-h (indigo-count-atoms mol)))
      (should (> atom-count-with-h 3))) ; More than 3 heavy atoms
    ;; Fold hydrogens (remove explicit H)
    (should (indigo-fold-hydrogens mol))
    ;; Count atoms after folding
    (let ((atom-count-folded (indigo-count-atoms mol)))
      (should (= atom-count-folded 3))))) ; Only C, C, O

(ert-deftest test-indigo-fold-hydrogens-benzene ()
  "Test folding hydrogen atoms from benzene."
  (indigo-with-molecule (mol "c1ccccc1") ; Benzene
    ;; Unfold first
    (should (indigo-unfold-hydrogens mol))
    (let ((atom-count-unfolded (indigo-count-atoms mol)))
      (should (= atom-count-unfolded 12))) ; 6 C + 6 H
    ;; Fold hydrogens
    (should (indigo-fold-hydrogens mol))
    (let ((atom-count-folded (indigo-count-atoms mol)))
      (should (= atom-count-folded 6))))) ; Only 6 C

;;; Hydrogen Unfolding Tests

(ert-deftest test-indigo-unfold-hydrogens-ethanol ()
  "Test unfolding (adding) explicit hydrogen atoms to ethanol."
  (indigo-with-molecule (mol "CCO") ; Ethanol
    ;; Count atoms before unfolding
    (let ((atom-count-before (indigo-count-atoms mol)))
      (should (= atom-count-before 3))) ; C, C, O
    ;; Unfold hydrogens
    (should (indigo-unfold-hydrogens mol))
    ;; Count atoms after unfolding
    (let ((atom-count-after (indigo-count-atoms mol)))
      (should (> atom-count-after 3))))) ; C, C, O + H atoms

(ert-deftest test-indigo-unfold-hydrogens-benzene ()
  "Test unfolding hydrogen atoms to benzene."
  (indigo-with-molecule (mol "c1ccccc1") ; Benzene
    ;; Count atoms before unfolding
    (let ((atom-count-before (indigo-count-atoms mol)))
      (should (= atom-count-before 6))) ; 6 C atoms
    ;; Unfold hydrogens
    (should (indigo-unfold-hydrogens mol))
    ;; Count atoms after unfolding
    (let ((atom-count-after (indigo-count-atoms mol)))
      (should (= atom-count-after 12))))) ; 6 C + 6 H

(ert-deftest test-indigo-unfold-hydrogens-water ()
  "Test unfolding hydrogen atoms to water molecule."
  (indigo-with-molecule (mol "O") ; Water
    ;; Count atoms before unfolding
    (let ((atom-count-before (indigo-count-atoms mol)))
      (should (= atom-count-before 1))) ; Just O
    ;; Unfold hydrogens
    (should (indigo-unfold-hydrogens mol))
    ;; Count atoms after unfolding
    (let ((atom-count-after (indigo-count-atoms mol)))
      (should (= atom-count-after 3))))) ; O + 2 H

;;; Combined Operations Tests

(ert-deftest test-indigo-aromatize-then-layout ()
  "Test combining aromatize and layout operations."
  (indigo-with-molecule (mol "C1=CC=CC=C1") ; Kekulé benzene
    ;; Aromatize first (returns :changed)
    (should (eq (indigo-aromatize mol) :changed))
    ;; Then calculate layout (returns t)
    (should (indigo-layout mol))
    ;; Should have coordinates
    (should (indigo-has-coordinates mol))))

(ert-deftest test-indigo-unfold-layout-fold ()
  "Test unfold, layout, then fold sequence."
  (indigo-with-molecule (mol "CCO") ; Ethanol
    ;; Unfold hydrogens
    (should (indigo-unfold-hydrogens mol))
    (let ((count-unfolded (indigo-count-atoms mol)))
      (should (> count-unfolded 3)))
    ;; Calculate layout
    (should (indigo-layout mol))
    (should (indigo-has-coordinates mol))
    ;; Fold hydrogens back
    (should (indigo-fold-hydrogens mol))
    (let ((count-folded (indigo-count-atoms mol)))
      (should (= count-folded 3)))
    ;; Should still have coordinates
    (should (indigo-has-coordinates mol))))

(ert-deftest test-indigo-fold-unfold-roundtrip ()
  "Test fold/unfold roundtrip preserves structure."
  (indigo-with-molecule (mol "c1ccccc1") ; Benzene
    ;; Get initial SMILES
    (let ((smiles-initial (indigo-canonical-smiles mol)))
      ;; Unfold then fold
      (should (indigo-unfold-hydrogens mol))
      (should (indigo-fold-hydrogens mol))
      ;; SMILES should be the same
      (let ((smiles-after (indigo-canonical-smiles mol)))
        (should (string= smiles-initial smiles-after))))))

;;; Iterator Tests with Structure Manipulation

(ert-deftest test-indigo-iterate-after-unfold ()
  "Test iterating atoms after unfolding hydrogens."
  (indigo-with-molecule (mol "CC") ; Ethane
    ;; Count atoms before unfold
    (let ((count-before (indigo-count-atoms mol)))
      (should (= count-before 2)))
    ;; Unfold hydrogens
    (should (indigo-unfold-hydrogens mol))
    ;; Iterate and count atoms manually
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((count 0))
        (while (indigo-next atoms-iter)
          (setq count (1+ count)))
        ;; Should have 2 C + 6 H = 8 atoms
        (should (= count 8))))))

(provide 'test-indigo-structure-manipulation)
;;; test-indigo-structure-manipulation.el ends here
