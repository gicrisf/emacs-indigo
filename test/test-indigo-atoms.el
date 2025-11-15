;;; test-indigo-atoms.el --- Tests for atom property functions -*- lexical-binding: t; -*-

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

;; Test suite for atom property functions:
;; - indigo-radical (returns keywords)
;; - Atom predicates and enum mappings

;;; Code:

(require 'ert)
(require 'indigo)
(require 'indigo-atom)

;;; Basic Radical State Tests

(ert-deftest test-atom-radical-none ()
  "Test that non-radical atoms return :none."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let* ((atom (indigo-next atoms-iter))
             (radical (indigo-radical atom)))
        (should (keywordp radical))
        (should (eq radical :none))))))

(ert-deftest test-atom-radical-raw-function ()
  "Test that raw integer function works."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        ;; Raw function should return integer
        (should (integerp (indigo--radical-raw atom)))
        (should (= (indigo--radical-raw atom) 0))
        ;; Wrapper should return keyword
        (should (eq (indigo-radical atom) :none))))))

;;; Reverse Mapping Tests

(ert-deftest test-radical-reverse-mapping ()
  "Test converting keywords back to codes."
  (should (= (indigo-radical-code :none) 0))
  (should (= (indigo-radical-code :singlet) 101))
  (should (= (indigo-radical-code :doublet) 102))
  (should (= (indigo-radical-code :triplet) 103)))

;;; Predicate Tests

(ert-deftest test-radical-predicates ()
  "Test convenience predicate functions."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        ;; Normal atoms should not be radicals
        (should-not (indigo-atom-radical-p atom))
        (should-not (indigo-atom-singlet-p atom))
        (should-not (indigo-atom-doublet-p atom))
        (should-not (indigo-atom-triplet-p atom))))))

(ert-deftest test-radical-predicates-with-radicals ()
  "Test predicates with actual radical molecules."
  ;; Test singlet carbene
  (indigo-with-molecule (mol "[CH2]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (should (indigo-atom-radical-p atom))
        (should (indigo-atom-singlet-p atom))
        (should-not (indigo-atom-doublet-p atom))
        (should-not (indigo-atom-triplet-p atom)))))

  ;; Test doublet methyl radical
  (indigo-with-molecule (mol "[CH3]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (should (indigo-atom-radical-p atom))
        (should (indigo-atom-doublet-p atom))
        (should-not (indigo-atom-singlet-p atom))
        (should-not (indigo-atom-triplet-p atom))))))

;;; Integration Tests

(ert-deftest test-all-atoms-return-radical-keywords ()
  "Test that all atoms return valid radical keywords."
  (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((radicals '())
            (atom (indigo-next atoms-iter)))
        (while atom
          (push (indigo-radical atom) radicals)
          (setq atom (indigo-next atoms-iter)))

        ;; All values should be keyword symbols
        (should (cl-every #'keywordp radicals))
        ;; For normal molecules, all should be :none
        (should (cl-every (lambda (r) (eq r :none)) radicals))))))

;;; Radical Molecule Tests

(ert-deftest test-radical-oxygen-singlet ()
  "Test oxygen radical [O] - Indigo treats it as singlet."
  (indigo-with-molecule (mol "[O]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (when atom
          (let ((raw-code (indigo--radical-raw atom))
                (keyword (indigo-radical atom)))
            (should (= raw-code 101))
            (should (eq keyword :singlet))))))))

(ert-deftest test-radical-methyl-doublet ()
  "Test methyl radical [CH3] - should be doublet."
  (indigo-with-molecule (mol "[CH3]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (when atom
          (let ((raw-code (indigo--radical-raw atom))
                (keyword (indigo-radical atom)))
            (should (= raw-code 102))
            (should (eq keyword :doublet))))))))

(ert-deftest test-radical-carbene-singlet ()
  "Test carbene [CH2] - Indigo treats it as singlet."
  (indigo-with-molecule (mol "[CH2]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (when atom
          (let ((raw-code (indigo--radical-raw atom))
                (keyword (indigo-radical atom)))
            (should (= raw-code 101))
            (should (eq keyword :singlet))))))))

(ert-deftest test-radical-molecular-oxygen ()
  "Test molecular oxygen O=O - normal molecule, no radical."
  (indigo-with-molecule (mol "O=O")  ; Molecular oxygen
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let* ((atom (indigo-next atoms-iter))
             (raw-code (indigo--radical-raw atom))
             (keyword (indigo-radical atom)))
        (should (= raw-code 0))
        (should (eq keyword :none))))))

;;; Radical Electrons Tests

(ert-deftest test-radical-electrons-none ()
  "Test that normal atoms have 0 radical electrons."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let* ((atom (indigo-next atoms-iter))
             (electrons (indigo-radical-electrons atom)))
        (should (integerp electrons))
        (should (= electrons 0))))))

(ert-deftest test-radical-electrons-doublet ()
  "Test that doublet radicals have 1 electron."
  (indigo-with-molecule (mol "[CH3]")  ; Methyl radical
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let* ((atom (indigo-next atoms-iter))
             (electrons (indigo-radical-electrons atom)))
        (should (integerp electrons))
        (should (= electrons 1))))))

(ert-deftest test-radical-electrons-singlet ()
  "Test that singlet radicals have 2 electrons (paired in carbene orbital)."
  (indigo-with-molecule (mol "[CH2]")  ; Carbene
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let* ((atom (indigo-next atoms-iter))
             (electrons (indigo-radical-electrons atom)))
        (should (integerp electrons))
        (should (= electrons 2))))))

(ert-deftest test-radical-electrons-consistency ()
  "Test that radical electrons are consistent with radical state."
  ;; Doublet should have 1 electron
  (indigo-with-molecule (mol "[CH3]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (should (eq (indigo-radical atom) :doublet))
        (should (= (indigo-radical-electrons atom) 1)))))

  ;; Singlet should have 2 electrons (paired)
  (indigo-with-molecule (mol "[CH2]")
    (indigo-with-atoms-iterator (atoms-iter mol)
      (let ((atom (indigo-next atoms-iter)))
        (should (eq (indigo-radical atom) :singlet))
        (should (= (indigo-radical-electrons atom) 2))))))

(provide 'test-indigo-atoms)
;;; test-indigo-atoms.el ends here
