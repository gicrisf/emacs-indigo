;;; test-indigo-bonds.el --- Tests for bond property functions -*- lexical-binding: t; -*-

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

;; Test suite for bond property functions.

;;; Code:

(require 'ert)
(require 'indigo)
(require 'indigo-bond)

;;; Basic Bond Property Tests

(ert-deftest test-bond-source ()
  "Test indigo-source returns atom handle."
  (indigo-let* ((:molecule mol "CCO")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (source-atom (indigo-source bond)))
    (should (integerp source-atom))
    (should (> source-atom 0))  ; Should be a valid handle
    ;; Get the index from the handle
    (let ((idx (indigo-index source-atom)))
      (should (integerp idx))
      (should (= idx 0)))))  ; First bond C-C should start at atom 0

(ert-deftest test-bond-destination ()
  "Test indigo-destination returns atom handle."
  (indigo-let* ((:molecule mol "CCO")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (dest-atom (indigo-destination bond)))
    (should (integerp dest-atom))
    (should (> dest-atom 0))  ; Should be a valid handle
    ;; Get the index from the handle
    (let ((idx (indigo-index dest-atom)))
      (should (integerp idx))
      (should (= idx 1)))))  ; First bond C-C should end at atom 1

(ert-deftest test-bond-order-single ()
  "Test indigo-bond-order returns :single for single bonds."
  (indigo-let* ((:molecule mol "CCO")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (order (indigo-bond-order bond)))
    (should (keywordp order))
    (should (eq order :single))))

(ert-deftest test-bond-order-double ()
  "Test indigo-bond-order returns :double for double bonds."
  (indigo-let* ((:molecule mol "C=C")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (order (indigo-bond-order bond)))
    (should (keywordp order))
    (should (eq order :double))))

(ert-deftest test-bond-order-triple ()
  "Test indigo-bond-order returns :triple for triple bonds."
  (indigo-let* ((:molecule mol "C#C")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (order (indigo-bond-order bond)))
    (should (keywordp order))
    (should (eq order :triple))))

(ert-deftest test-bond-order-aromatic ()
  "Test indigo-bond-order returns :aromatic for aromatic bonds."
  (indigo-let* ((:molecule mol "c1ccccc1")  ; Benzene
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (order (indigo-bond-order bond)))
    (should (keywordp order))
    (should (eq order :aromatic))))

(ert-deftest test-bond-stereo ()
  "Test indigo-bond-stereo returns keyword value."
  (indigo-let* ((:molecule mol "CCO")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter))
                (stereo (indigo-bond-stereo bond)))
    (should (keywordp stereo))
    ;; Simple molecules typically have no stereochemistry
    (should (eq stereo :none))))

;;; Complete Bond Data Collection Tests

(ert-deftest test-collect-all-bonds ()
  "Test collecting all bond properties from ethanol."
  (indigo-let* ((:molecule mol "CCO")
                (:bonds bonds-iter mol)
                (bond-data '()))
    (let ((bond (indigo-next bonds-iter)))
      (while bond
        (let* ((source-atom (indigo-source bond))
               (dest-atom (indigo-destination bond))
               (source-idx (indigo-index source-atom))
               (dest-idx (indigo-index dest-atom))
               (order (indigo-bond-order bond))
               (stereo (indigo-bond-stereo bond)))
          (push (list source-idx dest-idx order stereo) bond-data))
        (setq bond (indigo-next bonds-iter))))
    (setq bond-data (nreverse bond-data))

    ;; Should have 2 bonds (C-C and C-O)
    (should (= (length bond-data) 2))

    ;; Verify first bond (C-C)
    (let ((bond1 (nth 0 bond-data)))
      (should (= (nth 0 bond1) 0))  ; source index
      (should (= (nth 1 bond1) 1))  ; destination index
      (should (eq (nth 2 bond1) :single))  ; order (single)
      (should (eq (nth 3 bond1) :none))) ; stereo (none)

    ;; Verify second bond (C-O)
    (let ((bond2 (nth 1 bond-data)))
      (should (= (nth 0 bond2) 1))  ; source index
      (should (= (nth 1 bond2) 2))  ; destination index
      (should (eq (nth 2 bond2) :single))  ; order (single)
      (should (eq (nth 3 bond2) :none))))) ; stereo (none)

(ert-deftest test-benzene-bonds ()
  "Test all bonds in benzene are aromatic."
  (indigo-let* ((:molecule mol "c1ccccc1")
                (:bonds bonds-iter mol)
                (bond-orders '()))
    (let ((bond (indigo-next bonds-iter)))
      (while bond
        (push (indigo-bond-order bond) bond-orders)
        (setq bond (indigo-next bonds-iter))))

    ;; Should have 6 bonds
    (should (= (length bond-orders) 6))

    ;; All should be aromatic keywords
    (should (cl-every (lambda (order) (eq order :aromatic)) bond-orders))))

(ert-deftest test-bond-connectivity ()
  "Test that bonds connect valid atom indices."
  (indigo-let* ((:molecule mol "CCCO")
                ;; First count atoms
                (:atoms atoms-iter mol)
                (atom-count 0))
    (let ((atom (indigo-next atoms-iter)))
      (while atom
        (setq atom-count (1+ atom-count))
        (setq atom (indigo-next atoms-iter))))

    ;; Now check bonds
    (indigo-let* ((:bonds bonds-iter mol))
      (let ((bond (indigo-next bonds-iter)))
        (while bond
          (let* ((src-atom (indigo-source bond))
                 (dst-atom (indigo-destination bond))
                 (src-idx (indigo-index src-atom))
                 (dst-idx (indigo-index dst-atom)))
            ;; All indices should be within valid range
            (should (>= src-idx 0))
            (should (< src-idx atom-count))
            (should (>= dst-idx 0))
            (should (< dst-idx atom-count))
            ;; Source and destination should be different
            (should-not (= src-idx dst-idx)))
          (setq bond (indigo-next bonds-iter)))))))

;;; Multiple Bond Types Test

(ert-deftest test-mixed-bond-orders ()
  "Test molecule with different bond orders."
  (indigo-let* ((:molecule mol "C=CC#N")  ; ethylene + nitrile
                (:bonds bonds-iter mol)
                (orders '()))
    (let ((bond (indigo-next bonds-iter)))
      (while bond
        (push (indigo-bond-order bond) orders)
        (setq bond (indigo-next bonds-iter))))
    (setq orders (nreverse orders))

    ;; Should have 3 bonds
    (should (= (length orders) 3))
    ;; Should contain: single, double, triple keywords
    (should (member :single orders))
    (should (member :double orders))
    (should (member :triple orders))))

;;; Bond Count Tests

(ert-deftest test-bond-count-simple ()
  "Test bond count for simple molecules."
  (indigo-let* ((:molecule mol "CCO")
                (:bonds bonds-iter mol)
                (count 0))
    (let ((bond (indigo-next bonds-iter)))
      (while bond
        (setq count (1+ count))
        (setq bond (indigo-next bonds-iter))))
    (should (= count 2))))

(ert-deftest test-bond-count-cyclic ()
  "Test bond count for cyclic molecule."
  (indigo-let* ((:molecule mol "C1CCC1")  ; Cyclobutane
                (:bonds bonds-iter mol)
                (count 0))
    (let ((bond (indigo-next bonds-iter)))
      (while bond
        (setq count (1+ count))
        (setq bond (indigo-next bonds-iter))))
    (should (= count 4))))

;;; Error Handling Tests

(ert-deftest test-bond-properties-all-valid ()
  "Test that all bond property functions return valid values."
  (indigo-let* ((:molecule mol "c1ccccc1")
                (:bonds bonds-iter mol))
    (let ((bond (indigo-next bonds-iter)))
      (while bond
        (let ((src-atom (indigo-source bond))
              (dst-atom (indigo-destination bond))
              (order (indigo-bond-order bond))
              (stereo (indigo-bond-stereo bond)))
          ;; Atom handles should return integers
          (should (integerp src-atom))
          (should (integerp dst-atom))
          ;; Order and stereo should return keywords
          (should (keywordp order))
          (should (keywordp stereo))
          ;; Atom handles should be positive
          (should (> src-atom 0))
          (should (> dst-atom 0))
          ;; Order should be valid keyword
          (should (member order '(:query :single :double :triple :aromatic)))
          ;; Stereo should be valid keyword
          (should (member stereo '(:none :either :up :down :cis :trans))))
        (setq bond (indigo-next bonds-iter))))))

;;; Enum Mapping Tests

(ert-deftest test-bond-order-raw-functions ()
  "Test that raw integer functions still work."
  (indigo-let* ((:molecule mol "C=C")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    ;; Raw function should return integer
    (should (integerp (indigo--bond-order-raw bond)))
    (should (= (indigo--bond-order-raw bond) 2))
    ;; Wrapper should return keyword
    (should (eq (indigo-bond-order bond) :double))))

(ert-deftest test-bond-stereo-raw-functions ()
  "Test that raw stereo integer functions still work."
  (indigo-let* ((:molecule mol "CC")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    ;; Raw function should return integer
    (should (integerp (indigo--bond-stereo-raw bond)))
    (should (= (indigo--bond-stereo-raw bond) 0))
    ;; Wrapper should return keyword
    (should (eq (indigo-bond-stereo bond) :none))))

(ert-deftest test-bond-order-reverse-mapping ()
  "Test converting keywords back to codes."
  (should (= (indigo-bond-order-code :single) 1))
  (should (= (indigo-bond-order-code :double) 2))
  (should (= (indigo-bond-order-code :triple) 3))
  (should (= (indigo-bond-order-code :aromatic) 4))
  (should (= (indigo-bond-order-code :query) 0)))

(ert-deftest test-bond-stereo-reverse-mapping ()
  "Test converting stereo keywords back to codes."
  (should (= (indigo-bond-stereo-code :none) 0))
  (should (= (indigo-bond-stereo-code :up) 5))
  (should (= (indigo-bond-stereo-code :down) 6))
  (should (= (indigo-bond-stereo-code :either) 4))
  (should (= (indigo-bond-stereo-code :cis) 7))
  (should (= (indigo-bond-stereo-code :trans) 8)))

(ert-deftest test-bond-predicates ()
  "Test convenience predicate functions."
  ;; Single bond
  (indigo-let* ((:molecule mol "CC")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    (should (indigo-bond-single-p bond))
    (should-not (indigo-bond-double-p bond))
    (should-not (indigo-bond-triple-p bond))
    (should-not (indigo-bond-aromatic-p bond)))

  ;; Double bond
  (indigo-let* ((:molecule mol "C=C")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    (should (indigo-bond-double-p bond))
    (should-not (indigo-bond-single-p bond)))

  ;; Triple bond
  (indigo-let* ((:molecule mol "C#C")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    (should (indigo-bond-triple-p bond))
    (should-not (indigo-bond-single-p bond)))

  ;; Aromatic bond
  (indigo-let* ((:molecule mol "c1ccccc1")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    (should (indigo-bond-aromatic-p bond))
    (should-not (indigo-bond-single-p bond))))

(ert-deftest test-bond-has-stereo-predicate ()
  "Test stereochemistry predicate."
  (indigo-let* ((:molecule mol "CC")
                (:bonds bonds-iter mol)
                (bond (indigo-next bonds-iter)))
    ;; Simple molecules typically have no stereochemistry
    (should-not (indigo-bond-has-stereo-p bond))))

(provide 'test-indigo-bonds)
;;; test-indigo-bonds.el ends here
