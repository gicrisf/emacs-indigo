;;; test-readme-examples.el --- Tests for all README.md examples -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This file contains tests for every example shown in README.md
;; to ensure all documented examples work correctly.
;; Examples are tested verbatim as they appear in the README.

;;; Code:

(require 'ert)
(require 'indigo)

;;; Basic Examples - Stateless Functions

(ert-deftest test-readme-molecular-weight ()
  (let ((result (indigo-do-molecular-weight "CCO")))
    (should (floatp result))
    (should (< (abs (- result 46.069)) 0.01))))

(ert-deftest test-readme-molecular-formula ()
  (let ((result (indigo-do-molecular-formula "CCO")))
    (should (stringp result))
    (should (string-match-p "C2.*H6.*O" result))))

(ert-deftest test-readme-canonical-smiles ()
  (let ((result (indigo-do-canonical-smiles "CCO")))
    (should (stringp result))
    (should (string-match-p "CCO\\|OCC" result))))

(ert-deftest test-readme-atom-count ()
  (let ((result (indigo-do-atom-count "c1ccccc1")))
    (should (integerp result))
    (should (= result 6))))

(ert-deftest test-readme-ring-count ()
  (let ((result (indigo-do-ring-count "c1ccccc1")))
    (should (integerp result))
    (should (= result 1))))

(ert-deftest test-readme-substructure-match ()
  (let ((result (indigo-do-substructure-match "CCO" "CO")))
    (should (eq result t))))

(ert-deftest test-readme-exact-match ()
  (let ((result (indigo-do-exact-match "CCO" "OCC")))
    (should (eq result t))))

(ert-deftest test-readme-reaction-reactants-count ()
  (let ((result (indigo-do-reaction-reactants-count "CCO.CC>>CCOC")))
    (should (integerp result))
    (should (= result 2))))

;;; Advanced Examples - indigo-let* and indigo-map

(ert-deftest test-readme-advanced-example ()
  (let ((result
         (indigo-let* ((:molecule mol "c1ccccc1")  ; Benzene
                       (:atoms atoms mol))
           ;; Get all atom symbols
           (indigo-map #'indigo-symbol atoms))))
    (should (listp result))
    (should (= (length result) 6))
    (should (equal result '("C" "C" "C" "C" "C" "C")))))

;;; Reaction Examples

(ert-deftest test-readme-reaction-example ()
  (let ((result
         (indigo-let* ((:reaction rxn "CCO.CC(=O)O>>CCOC(=O)C")  ; Esterification
                       (:reactants reactants rxn)
                       (:products products rxn))
           (list :reactant-count (length (indigo-map #'indigo-canonical-smiles reactants))
                 :product-count (length (indigo-map #'indigo-canonical-smiles products))))))
    (should (listp result))
    (should (= (plist-get result :reactant-count) 2))
    (should (= (plist-get result :product-count) 1))))

;;; Rendering Examples

(ert-deftest test-readme-rendering-example ()
  (let ((temp-file (make-temp-file "benzene" nil ".svg")))
    (unwind-protect
        (progn
          (indigo-let* ((:molecule mol "c1ccccc1"))
            (indigo-set-option "render-output-format" "svg")
            (indigo-set-option-int "render-image-width" 300)
            (indigo-set-option-int "render-image-height" 300)
            (indigo-render-to-file mol temp-file))
          ;; Verify the file was created
          (should (file-exists-p temp-file))
          ;; Verify it contains SVG content
          (with-temp-buffer
            (insert-file-contents temp-file)
            (should (string-match-p "<svg" (buffer-string)))))
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; All Examples in One Test (Integration Test)

(ert-deftest test-readme-all-basic-examples-together ()
  (let ((results nil))
    ;; Collect all results
    (push (cons 'molecular-weight (indigo-do-molecular-weight "CCO")) results)
    (push (cons 'molecular-formula (indigo-do-molecular-formula "CCO")) results)
    (push (cons 'canonical-smiles (indigo-do-canonical-smiles "CCO")) results)
    (push (cons 'atom-count (indigo-do-atom-count "c1ccccc1")) results)
    (push (cons 'ring-count (indigo-do-ring-count "c1ccccc1")) results)
    (push (cons 'substructure-match (indigo-do-substructure-match "CCO" "[OH]")) results)
    (push (cons 'exact-match (indigo-do-exact-match "CCO" "OCC")) results)
    (push (cons 'reaction-reactants-count (indigo-do-reaction-reactants-count "CCO.CC>>CCOC")) results)

    ;; Verify all succeeded
    (should (= (length results) 8))
    (should (floatp (alist-get 'molecular-weight results)))
    (should (stringp (alist-get 'molecular-formula results)))
    (should (stringp (alist-get 'canonical-smiles results)))
    (should (integerp (alist-get 'atom-count results)))
    (should (integerp (alist-get 'ring-count results)))
    (should (eq (alist-get 'substructure-match results) t))
    (should (eq (alist-get 'exact-match results) t))
    (should (integerp (alist-get 'reaction-reactants-count results)))))

(provide 'test-readme-examples)
;;; test-readme-examples.el ends here
