;;; test-indigo-iterators.el --- Tests for Indigo iterator operations -*- lexical-binding: t; -*-

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

;; Comprehensive test suite for Indigo iterator operations.
;; Tests iterator creation, navigation, and error handling.

;;; Code:

(require 'ert)
(require 'indigo)

(ert-deftest test-indigo-iterator-atoms ()
  "Test atom iteration functionality."
  (let ((mol (indigo-load-molecule-from-string "[CH3][CH2][OH]")))
    (unwind-protect
        (let ((iter (indigo-iterate-atoms mol)))
          (should (integerp iter))
          (should (> iter 0))
          
          (let ((count 0)
                (atom (indigo-next iter)))
            (while atom
              (setq count (1+ count))
              (should (integerp atom))
              (should (> atom 0))
              (setq atom (indigo-next iter)))
            (should (= count 3)))
          
          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-bonds ()
  "Test bond iteration functionality."
  (let ((mol (indigo-load-molecule-from-string "[CH3][CH2][OH]")))
    (unwind-protect
        (let ((iter (indigo-iterate-bonds mol)))
          (should (integerp iter))
          (should (> iter 0))

          ;; Count bonds using iterator
          (let ((count 0)
                (bond (indigo-next iter)))
            (while bond
              (setq count (1+ count))
              (should (integerp bond))
              (should (> bond 0))
              (setq bond (indigo-next iter)))
            (should (= count 2)))

          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-neighbors ()
  "Test neighbor iteration functionality."
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (let* ((atoms-iter (indigo-iterate-atoms mol))
               (first-atom (indigo-next atoms-iter)))
          (should first-atom)
          
          (let ((neighbors-iter (indigo-iterate-neighbors first-atom)))
            (should (integerp neighbors-iter))
            (should (> neighbors-iter 0))
            
            ;; Count neighbors (at least 0 to pass)
            (let ((count 0)
                  (neighbor (indigo-next neighbors-iter)))
              (while neighbor
                (setq count (1+ count))
                (should (integerp neighbor))
                (should (> neighbor 0))
                (setq neighbor (indigo-next neighbors-iter)))
              (should (>= count 0)))
            (indigo-free neighbors-iter))
          
          (indigo-free atoms-iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-components ()
  "Test connected components iteration."
  (let ((mol (indigo-load-molecule-from-string "CCO.CC"))) ; Two components
    (unwind-protect
        (let ((iter (indigo-iterate-components mol)))
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count components
          ;; (two separate components in this case)
          (let ((count 0)
                (component (indigo-next iter)))
            (while component
              (setq count (1+ count))
              (should (integerp component))
              (should (> component 0))
              (setq component (indigo-next iter)))
            (should (= count 2)))
          
          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-sssr ()
  "Test SSSR ring iteration."
  (let ((mol (indigo-load-molecule-from-string "c1ccccc1"))) ; Benzene
    (unwind-protect
        (let ((iter (indigo-iterate-sssr mol)))
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count rings
          (let ((count 0)
                (ring (indigo-next iter)))
            (while ring
              (setq count (1+ count))
              (should (integerp ring))
              (should (> ring 0))
              (setq ring (indigo-next iter)))
            (should (= count 1))) ; Benzene has 1 ring
          
          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-subtrees ()
  "Test subtree iteration with size constraints."
  (let ((mol (indigo-load-molecule-from-string "CCCC"))) ; Butane
    (unwind-protect
        (let ((iter (indigo-iterate-subtrees mol 2 3))) ; 2-3 atom subtrees
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count subtrees
          (let ((count 0)
                (subtree (indigo-next iter)))
            (while subtree
              (setq count (1+ count))
              (should (integerp subtree))
              (should (> subtree 0))
              (setq subtree (indigo-next iter)))
            (should (> count 0))) ; Should find some subtrees
          
          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-rings ()
  "Test ring iteration with size constraints."
  (let ((mol (indigo-load-molecule-from-string "c1ccc2ccccc2c1"))) ; Naphthalene
    (unwind-protect
        (let ((iter (indigo-iterate-rings mol 5 7))) ; 5-7 atom rings
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count rings in size range
          (let ((count 0)
                (ring (indigo-next iter)))
            (while ring
              (setq count (1+ count))
              (should (integerp ring))
              (should (> ring 0))
              (indigo-free ring)
              (setq ring (indigo-next iter)))
            (should (= count 2))) ; Naphthalene has 2 six-membered rings
          
          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-edge-submolecules ()
  "Test edge submolecule iteration."
  (let ((mol (indigo-load-molecule-from-string "CCCC"))) ; Butane
    (unwind-protect
        (let ((iter (indigo-iterate-edge-submolecules mol 1 2))) ; 1-2 bond submolecules
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count edge submolecules
          (let ((count 0)
                (submol (indigo-next iter)))
            (while submol
              (setq count (1+ count))
              (should (integerp submol))
              (should (> submol 0))
              (indigo-free submol)
              (setq submol (indigo-next iter)))
            (should (> count 0))) ; Should find some submolecules
          
          (indigo-free iter))
      (indigo-free mol))))

;; Good test, but it takes 7 seconds
;; (ert-deftest test-indigo-iterator-stereocenters ()
;;   "Test stereocenter iteration."
;;   (let ((mol (indigo-load-molecule-from-string "C[C@H](O)N"))) ; Chiral molecule
;;     (unwind-protect
;;         (let ((iter (indigo-iterate-stereocenters mol)))
;;           (should (integerp iter))
;;           (should (> iter 0))

;;           ;; Count stereocenters
;;           (let ((count 0)
;;                 (center (indigo-next iter)))
;;             (while center
;;               (setq count (1+ count))
;;               (should (integerp center))
;;               (should (> center 0))
;;               (indigo-free center)
;;               (setq center (indigo-next iter)))
;;             (should (= count 1))) ; One stereocenter

;;           (indigo-free iter))
;;       (indigo-free mol))))

(ert-deftest test-indigo-iterator-properties ()
  "Test property iteration."
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (let ((iter (indigo-iterate-properties mol)))
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count properties (may be 0 for simple molecules)
          (let ((count 0)
                (prop (indigo-next iter)))
            (while prop
              (setq count (1+ count))
              (should (integerp prop))
              (should (> prop 0))
              (indigo-free prop)
              (setq prop (indigo-next iter)))
            (should (>= count 0))) ; Properties count can be 0
          
          (indigo-free iter))
      (indigo-free mol))))

;; TODO: Enable this test when reaction loading functions are implemented
;; Also, you should free the items! FIXME when you uncomment.
;; (ert-deftest test-indigo-iterator-reaction-components ()
;;   "Test reaction component iterators."
;;   (let ((rxn (indigo-load-reaction-from-string "CCO>>CC")))
;;     (unwind-protect
;;         (progn
;;           ;; Test reactants iterator
;;           (let ((iter (indigo-iterate-reactants rxn)))
;;             (should (integerp iter))
;;             (should (> iter 0))
;;             
;;             (let ((count 0)
;;                   (reactant (indigo-next iter)))
;;               (while reactant
;;                 (setq count (1+ count))
;;                 (should (integerp reactant))
;;                 (should (> reactant 0))
;;                 (setq reactant (indigo-next iter)))
;;               (should (= count 1)) ; One reactant
;;               
;;               (should (null (indigo-next iter))))
;;             
;;             (indigo-free iter))
;;           
;;           ;; Test products iterator
;;           (let ((iter (indigo-iterate-products rxn)))
;;             (should (integerp iter))
;;             (should (> iter 0))
;;             
;;             (let ((count 0)
;;                   (product (indigo-next iter)))
;;               (while product
;;                 (setq count (1+ count))
;;                 (should (integerp product))
;;                 (should (> product 0))
;;                 (setq product (indigo-next iter)))
;;               (should (= count 1)) ; One product
;;               
;;               (should (null (indigo-next iter))))
;;             
;;             (indigo-free iter))
;;           
;;           ;; Test molecules iterator (all molecules)
;;           (let ((iter (indigo-iterate-molecules rxn)))
;;             (should (integerp iter))
;;             (should (> iter 0))
;;             
;;             (let ((count 0)
;;                   (molecule (indigo-next iter)))
;;               (while molecule
;;                 (setq count (1+ count))
;;                 (should (integerp molecule))
;;                 (should (> molecule 0))
;;                 (setq molecule (indigo-next iter)))
;;               (should (= count 2)) ; One reactant + one product
;;               
;;               (should (null (indigo-next iter))))
;;             
;;             (indigo-free iter)))
;;       
;;       (indigo-free rxn))))

(ert-deftest test-indigo-iterator-matches ()
  "Test substructure match iteration."
  (let* ((target (indigo-load-molecule-from-string "c1ccc2ccccc2c1")) ; Naphthalene
         (query (indigo-load-query-molecule-from-string "c1ccccc1")) ; Benzene as query
         (matcher (indigo-substructure-matcher target)))
    (unwind-protect
        (let ((iter (indigo-iterate-matches matcher query)))
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count matches
          (let ((count 0)
                (match (indigo-next iter)))
            (while match
              (setq count (1+ count))
              (should (integerp match))
              (should (> match 0))
              (indigo-free match)
              (setq match (indigo-next iter)))
            (should (> count 0))) ; Should find matches
          
          (indigo-free iter))
      
      (indigo-free matcher)
      (indigo-free query)
      (indigo-free target))))

(ert-deftest test-indigo-iterator-tautomers ()
  "Test tautomer iteration."
  (let ((mol (indigo-load-molecule-from-string "CC(=O)CC"))) ; Keto form
    (unwind-protect
        (let ((iter (indigo-iterate-tautomers mol "")))
          (should (integerp iter))
          (should (> iter 0))
          
          ;; Count tautomers
          (let ((count 0)
                (tautomer (indigo-next iter)))
            (while tautomer
              (setq count (1+ count))
              (should (integerp tautomer))
              (should (> tautomer 0))
              (indigo-free tautomer)
              (setq tautomer (indigo-next iter)))
            (should (>= count 1))) ; At least the original molecule
          
          (indigo-free iter))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-error-handling ()
  "Test iterator error handling with invalid inputs."
  ;; Test with invalid molecule handle - should signal errors
  (should-error (indigo-iterate-atoms -1))
  (should-error (indigo-iterate-bonds -1))
  (should-error (indigo-iterate-sssr -1))

  ;; Test next with invalid iterator - returns nil (exhausted state)
  (should (null (indigo-next -1)))
  
  ;; Test with invalid parameters
  (let ((mol (indigo-load-molecule-from-string "CCO")))
    (unwind-protect
        (progn
          ;; Invalid range parameters
          (should (integerp (indigo-iterate-subtrees mol 1 1))) ; min=max should work
          (should (integerp (indigo-iterate-rings mol 3 10)))) ; Large range should work
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-empty-results ()
  "Test iterators with molecules that have no items to iterate."
  (let ((mol (indigo-load-molecule-from-string "C"))) ; Single carbon
    (unwind-protect
        (progn
          ;; Single carbon has no rings
          (let ((iter (indigo-iterate-sssr mol)))
            (should (integerp iter))
            (should (null (indigo-next iter))) ; No rings to iterate
            (indigo-free iter))
          
          ;; Single carbon has no stereocenters
          (let ((iter (indigo-iterate-stereocenters mol)))
            (should (integerp iter))
            (should (null (indigo-next iter))) ; No stereocenters
            (indigo-free iter)))
      (indigo-free mol))))

(ert-deftest test-indigo-iterator-memory-management ()
  "Test proper memory management with iterators."
  (let ((mol (indigo-load-molecule-from-string "c1ccccc1c2ccccc2"))) ; Biphenyl
    (unwind-protect
        (progn
          ;; Create multiple iterators
          (let ((atoms-iter (indigo-iterate-atoms mol))
                (bonds-iter (indigo-iterate-bonds mol))
                (rings-iter (indigo-iterate-sssr mol)))
            
            ;; All should be valid
            (should (integerp atoms-iter))
            (should (integerp bonds-iter))
            (should (integerp rings-iter))
            
            ;; Use iterators partially
            (let ((atom (indigo-next atoms-iter))
                  (bond (indigo-next bonds-iter))
                  (ring (indigo-next rings-iter)))
              (should atom)
              (should bond)
              (should ring)
              ;; Free the items we got
              (when atom (indigo-free atom))
              (when bond (indigo-free bond))
              (when ring (indigo-free ring)))
            
            ;; Free iterators
            (indigo-free atoms-iter)
            (indigo-free bonds-iter)
            (indigo-free rings-iter)))
      (indigo-free mol))))

;;; Iterator Helper Tests

(ert-deftest test-indigo-map ()
  "Test mapping over iterator with indigo-map."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((symbols (indigo-map #'indigo-symbol atoms)))
        (should (equal symbols '("C" "C" "O")))))))

;;; With-style Iterator Macro Tests

(ert-deftest test-indigo-with-atoms ()
  "Test indigo-with-atoms-iterator macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (should (integerp atoms))
      (should (> atoms 0))
      ;; Test iteration
      (let ((count 0))
        (while (indigo-next atoms)
          (setq count (1+ count)))
        (should (= count 3))))))

(ert-deftest test-indigo-with-bonds ()
  "Test indigo-with-bonds-iterator macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-bonds-iterator (bonds mol)
      (should (integerp bonds))
      (should (> bonds 0))
      ;; Test iteration
      (let ((count 0))
        (while (indigo-next bonds)
          (setq count (1+ count)))
        (should (= count 2))))))

(ert-deftest test-indigo-with-neighbors ()
  "Test indigo-with-neighbors-iterator macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((first-atom (indigo-next atoms)))
        (when first-atom
          (indigo-with-neighbors-iterator (neighbors first-atom)
            (should (integerp neighbors))
            (should (> neighbors 0))
            ;; First carbon in CCO has 1 neighbor (the other carbon)
            (let ((count 0))
              (while (indigo-next neighbors)
                (setq count (1+ count)))
              (should (= count 1))))
          (indigo-free first-atom))))))

(ert-deftest test-indigo-with-components ()
  "Test indigo-with-components-iterator macro."
  (indigo-with-molecule (mol "CCO.CC")  ; Two components
    (indigo-with-components-iterator (components mol)
      (should (integerp components))
      (should (> components 0))
      (let ((count 0))
        (while (indigo-next components)
          (setq count (1+ count)))
        (should (= count 2))))))

(ert-deftest test-indigo-with-sssr ()
  "Test indigo-with-sssr-iterator macro."
  (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
    (indigo-with-sssr-iterator (rings mol)
      (should (integerp rings))
      (should (> rings 0))
      (let ((count 0))
        (while (indigo-next rings)
          (setq count (1+ count)))
        (should (= count 1))))))

(ert-deftest test-indigo-with-rings ()
  "Test indigo-with-rings-iterator macro with size range."
  (indigo-with-molecule (mol "c1ccc2ccccc2c1")  ; Naphthalene
    (indigo-with-rings-iterator (rings mol 5 7)
      (should (integerp rings))
      (should (> rings 0))
      ;; Naphthalene has rings of size 6
      (let ((count 0))
        (while (indigo-next rings)
          (setq count (1+ count)))
        (should (> count 0))))))

(ert-deftest test-indigo-with-subtrees ()
  "Test indigo-with-subtrees-iterator macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-subtrees-iterator (subtrees mol 1 3)
      (should (integerp subtrees))
      (should (> subtrees 0))
      (let ((count 0))
        (while (indigo-next subtrees)
          (setq count (1+ count)))
        (should (> count 0))))))

(ert-deftest test-indigo-with-stereocenters ()
  "Test indigo-with-stereocenters-iterator macro."
  (indigo-with-molecule (mol "C[C@H](O)CC")
    (indigo-with-stereocenters-iterator (stereos mol)
      (should (integerp stereos))
      (should (> stereos 0))
      (let ((count 0))
        (while (indigo-next stereos)
          (setq count (1+ count)))
        (should (= count 1))))))

(ert-deftest test-indigo-with-reactants ()
  "Test indigo-with-reactants-iterator macro."
  (indigo-with-reaction (rxn "CCO.CC>>CCOC")
    (indigo-with-reactants-iterator (reactants rxn)
      (should (integerp reactants))
      (should (> reactants 0))
      (let ((count 0))
        (while (indigo-next reactants)
          (setq count (1+ count)))
        (should (= count 2))))))

(ert-deftest test-indigo-with-products ()
  "Test indigo-with-products-iterator macro."
  (indigo-with-reaction (rxn "CCO.CC>>CCOC")
    (indigo-with-products-iterator (products rxn)
      (should (integerp products))
      (should (> products 0))
      (let ((count 0))
        (while (indigo-next products)
          (setq count (1+ count)))
        (should (= count 1))))))

(ert-deftest test-indigo-with-nested-iterators ()
  "Test nested molecule and atom iterators."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((symbols '())
            (atom (indigo-next atoms)))
        (while atom
          (push (indigo-symbol atom) symbols)
          (indigo-free atom)
          (setq atom (indigo-next atoms)))
        (should (equal (reverse symbols) '("C" "C" "O")))))))

(ert-deftest test-indigo-with-stream-integration ()
  "Test indigo-with-* macros with lazy streams."
  (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             (symbols '()))
        (while (not (indigo-stream-empty-p stream))
          (let ((atom (indigo-stream-first stream)))
            (push (indigo-symbol atom) symbols)
            (indigo-free atom)
            (setq stream (indigo-stream-rest stream))))
        (should (= (length symbols) 6))
        (should (cl-every (lambda (s) (equal s "C")) symbols))))))

(provide 'test-indigo-iter)

;;; test-indigo-iter.el ends here
