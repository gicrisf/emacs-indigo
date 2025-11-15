;;; test-indigo-stream-iter.el --- Tests for stream-iterator bridge macros -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for the stream-iterator bridge macros in indigo-stream-iter.el
;; These macros provide convenient one-liner syntax for creating streams.

;;; Code:

(require 'ert)
(require 'indigo)

;;; Molecule-based Stream Macros

(ert-deftest test-indigo-with-atoms-stream ()
  "Test atoms stream macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-stream (stream mol)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 3 atoms
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 3 count))))))

(ert-deftest test-indigo-with-bonds-stream ()
  "Test bonds stream macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-bonds-stream (stream mol)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 2 bonds
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 2 count))))))

(ert-deftest test-indigo-with-components-stream ()
  "Test components stream macro."
  (indigo-with-molecule (mol "C.C.C")  ; Three separate components
    (indigo-with-components-stream (stream mol)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 3 components
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 3 count))))))

(ert-deftest test-indigo-with-sssr-stream ()
  "Test SSSR rings stream macro."
  (indigo-with-molecule (mol "c1ccc2ccccc2c1")  ; Naphthalene (2 rings)
    (indigo-with-sssr-stream (stream mol)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 2 SSSR rings
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 2 count))))))

(ert-deftest test-indigo-with-sssr-stream-empty ()
  "Test SSSR rings stream with acyclic molecule."
  (indigo-with-molecule (mol "CCC")  ; Propane (no rings)
    (indigo-with-sssr-stream (stream mol)
      ;; Should be empty
      (should (indigo-stream-empty-p stream)))))

(ert-deftest test-indigo-with-rings-stream ()
  "Test rings stream macro with size range."
  (indigo-with-molecule (mol "c1ccccc1")  ; Benzene (one 6-membered ring)
    (indigo-with-rings-stream (stream mol 3 7)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have at least 1 ring
      (should-not (indigo-stream-empty-p stream))
      (let ((first (indigo-stream-car stream)))
        (should (integerp first))))))

(ert-deftest test-indigo-with-subtrees-stream ()
  "Test subtrees stream macro with size range."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-subtrees-stream (stream mol 1 3)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have subtrees
      (should-not (indigo-stream-empty-p stream))
      (let ((first (indigo-stream-car stream)))
        (should (integerp first))))))

(ert-deftest test-indigo-with-stereocenters-stream ()
  "Test stereocenters stream macro."
  (indigo-with-molecule (mol "C[C@H](O)N")  ; Molecule with stereocenter
    (indigo-with-stereocenters-stream (stream mol)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 1 stereocenter
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 1 count))))))

(ert-deftest test-indigo-with-stereocenters-stream-empty ()
  "Test stereocenters stream with molecule without stereocenters."
  (indigo-with-molecule (mol "CCO")  ; Ethanol (no stereocenters)
    (indigo-with-stereocenters-stream (stream mol)
      ;; Should be empty
      (should (indigo-stream-empty-p stream)))))

;;; Atom-based Stream Macros

(ert-deftest test-indigo-with-neighbors-stream ()
  "Test neighbors stream macro."
  (indigo-with-molecule (mol "CCC")  ; Propane
    (indigo-with-atoms-iterator (atoms mol)
      ;; Get middle carbon
      (indigo-next atoms)
      (let ((middle-carbon (indigo-next atoms)))
        (indigo-with-neighbors-stream (stream middle-carbon)
          ;; Should be a function (stream thunk)
          (should (functionp stream))
          ;; Middle carbon should have 2 neighbors
          (let ((count 0))
            (while (not (indigo-stream-empty-p stream))
              (should (integerp (indigo-stream-car stream)))
              (setq stream (indigo-stream-next stream))
              (setq count (1+ count)))
            (should (= 2 count))))
        (indigo-free middle-carbon)))))

;;; Reaction-based Stream Macros

(ert-deftest test-indigo-with-reactants-stream ()
  "Test reactants stream macro."
  (indigo-with-reaction (rxn "CCO.CC>>CCOCC")
    (indigo-with-reactants-stream (stream rxn)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 2 reactants
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 2 count))))))

(ert-deftest test-indigo-with-products-stream ()
  "Test products stream macro."
  (indigo-with-reaction (rxn "CCO.CC>>CCOCC")
    (indigo-with-products-stream (stream rxn)
      ;; Should be a function (stream thunk)
      (should (functionp stream))
      ;; Should have 1 product
      (let ((count 0))
        (while (not (indigo-stream-empty-p stream))
          (should (integerp (indigo-stream-car stream)))
          (setq stream (indigo-stream-next stream))
          (setq count (1+ count)))
        (should (= 1 count))))))

;;; Integration Tests

(ert-deftest test-stream-iter-with-map ()
  "Test combining stream macros with stream-map."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-stream (stream mol)
      ;; Map to extract symbols
      (let* ((symbol-stream (indigo-stream-map #'indigo-symbol stream))
             (symbols nil))
        ;; Collect all symbols
        (while (not (indigo-stream-empty-p symbol-stream))
          (push (indigo-stream-car symbol-stream) symbols)
          (setq symbol-stream (indigo-stream-next symbol-stream)))
        ;; Should have C, C, O
        (should (equal '("O" "C" "C") symbols))))))

(ert-deftest test-stream-iter-nested ()
  "Test nesting stream macros (atoms + neighbors)."
  (let ((total-neighbors 0))
    (indigo-with-molecule (mol "CCC")  ; Propane
      (indigo-with-atoms-stream (atom-stream mol)
        ;; For each atom
        (while (not (indigo-stream-empty-p atom-stream))
          (let ((atom (indigo-stream-car atom-stream)))
            ;; Count its neighbors
            (indigo-with-neighbors-stream (neighbor-stream atom)
              (while (not (indigo-stream-empty-p neighbor-stream))
                (setq total-neighbors (1+ total-neighbors))
                (setq neighbor-stream (indigo-stream-next neighbor-stream)))))
          (setq atom-stream (indigo-stream-next atom-stream)))))
    ;; Propane: C-C-C has 1+2+1 = 4 neighbor connections
    (should (= 4 total-neighbors))))

(ert-deftest test-stream-iter-automatic-cleanup ()
  "Test that stream macros automatically clean up forced elements."
  (let ((symbols nil))
    (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
      (indigo-with-atoms-stream (stream mol)
        ;; Force first 3 elements and extract symbols
        (dotimes (_ 3)
          (push (indigo-symbol (indigo-stream-car stream)) symbols)
          (setq stream (indigo-stream-next stream)))))
    ;; Should have collected 3 carbon symbols
    (should (= 3 (length symbols)))
    (should (equal '("C" "C" "C") symbols))
    ;; All forced elements should have been freed automatically
    ))

(ert-deftest test-stream-iter-partial-consumption ()
  "Test that only forced elements are tracked and freed."
  (let ((first-symbol nil))
    (indigo-with-molecule (mol "CCCCCCCCCC")  ; 10 carbons
      (indigo-with-atoms-stream (stream mol)
        ;; Only force first element
        (let ((atom (indigo-stream-car stream)))
          (setq first-symbol (indigo-symbol atom)))))
    ;; Should have successfully gotten first symbol
    (should (equal "C" first-symbol))
    ;; Only the first atom should have been forced and freed
    ;; The rest remain unforced in the stream
    ))

(ert-deftest test-stream-iter-error-handling ()
  "Test that elements are freed even when an error occurs."
  (let ((symbols nil)
        (error-caught nil))
    (condition-case err
        (indigo-with-molecule (mol "CCO")
          (indigo-with-atoms-stream (stream mol)
            ;; Force first two elements
            (push (indigo-symbol (indigo-stream-car stream)) symbols)
            (setq stream (indigo-stream-next stream))
            (push (indigo-symbol (indigo-stream-car stream)) symbols)
            ;; Trigger an error
            (error "Test error")))
      (error (setq error-caught t)))
    ;; Error should have been caught
    (should error-caught)
    ;; But we should have collected 2 symbols before the error
    (should (= 2 (length symbols)))
    (should (equal '("C" "C") symbols))
    ;; The forced elements should have been freed by unwind-protect
    ))

(provide 'test-indigo-stream-iter)
;;; test-indigo-stream-iter.el ends here
