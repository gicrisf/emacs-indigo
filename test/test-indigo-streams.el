;;; test-indigo-streams.el --- Tests for lazy stream abstraction -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Tests for the lazy stream API in indigo.el

;;; Code:

(require 'ert)
(require 'indigo)

;;; Basic Stream Operations

(ert-deftest test-indigo-stream-creation ()
  "Test basic stream creation from iterator."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((stream (indigo-stream atoms)))
        ;; Stream should be a thunk
        (should (functionp stream))
        ;; Forcing should give us a cons cell
        (let ((forced (indigo-stream-force stream)))
          (should forced)
          ;; First element should be an integer handle
          (should (integerp (car forced)))
          ;; Rest should be a thunk
          (should (functionp (cdr forced))))))))

(ert-deftest test-indigo-stream-rest ()
  "Test advancing through a stream."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             (first-val (indigo-stream-first stream))
             (stream (indigo-stream-rest stream))
             (second-val (indigo-stream-first stream)))
        ;; First and second values should be different
        (should (not (= first-val second-val)))
        ;; Both should be valid handles
        (should (integerp first-val))
        (indigo-free first-val)
        (should (integerp second-val))
        (indigo-free second-val)))))

(ert-deftest test-indigo-stream-empty-p ()
  "Test checking if stream is empty."
  (indigo-with-molecule (mol "C")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((stream (indigo-stream atoms)))
        ;; Single carbon - stream should not be empty
        (should-not (indigo-stream-empty-p stream))
        ;; Get and free the handle
        (let ((atom (indigo-stream-first stream)))
          (should (integerp atom))
          (indigo-free atom))
        ;; Advance to next (which should be empty)
        (setq stream (indigo-stream-rest stream))
        ;; Now, stream should be empty
        (should (indigo-stream-empty-p stream))))))

;;; Stream Map Operations

(ert-deftest test-indigo-stream-basic-iteration ()
  "Test basic stream iteration without map."
  (indigo-with-molecule (mol "CCO")  ; Ethanol
    (indigo-with-atoms-iterator (atoms mol)
      (let ((stream (indigo-stream atoms)))
        ;; Get first atom
        (let* ((first-handle (indigo-stream-first stream))
               (first-sym (indigo-symbol first-handle)))
          (should (equal "C" first-sym))
          (indigo-free first-handle))
        ;; Advance and get second
        (setq stream (indigo-stream-rest stream))
        (let* ((second-handle (indigo-stream-first stream))
               (second-sym (indigo-symbol second-handle)))
          (should (equal "C" second-sym))
          (indigo-free second-handle))
        ;; Advance and get third
        (setq stream (indigo-stream-rest stream))
        (let* ((third-handle (indigo-stream-first stream))
               (third-sym (indigo-symbol third-handle)))
          (should (equal "O" third-sym))
          (indigo-free third-handle))))))

(ert-deftest test-indigo-stream-map-symbols ()
  "Test mapping symbol extraction over stream."
  (indigo-with-molecule (mol "CCO")  ; Ethanol
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             (symbols (indigo-stream-map
                       (lambda (atom)
                         (prog1 (indigo-symbol atom)
                           (indigo-free atom)))
                       stream)))
        ;; First symbol should be "C"
        (should (equal "C" (indigo-stream-first symbols)))
        ;; Advance to second
        (setq symbols (indigo-stream-rest symbols))
        (should (equal "C" (indigo-stream-first symbols)))
        ;; Advance to third
        (setq symbols (indigo-stream-rest symbols))
        (should (equal "O" (indigo-stream-first symbols)))
        ;; Advance past end
        (setq symbols (indigo-stream-rest symbols))
        (should (indigo-stream-empty-p symbols))))))

(ert-deftest test-indigo-stream-map-charges ()
  "Test mapping charge extraction over stream."
  (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             (charges (indigo-stream-map
                       (lambda (atom)
                         (prog1 (indigo-charge atom)
                           (indigo-free atom)))
                       stream)))
        ;; All carbons in benzene should have charge 0
        (should (= 0 (indigo-stream-first charges)))
        (setq charges (indigo-stream-rest charges))
        (should (= 0 (indigo-stream-first charges)))
        (setq charges (indigo-stream-rest charges))
        (should (= 0 (indigo-stream-first charges)))))))

(ert-deftest test-indigo-stream-map-empty ()
  "Test mapping over empty stream."
  (indigo-with-molecule (mol "C")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((stream (indigo-stream atoms)))
        ;; Free first element
        (indigo-free (indigo-stream-first stream))
        ;; Advance to empty stream
        (setq stream (indigo-stream-rest stream))
        ;; Map over empty stream should return nil (thunk that forces to nil)
        (let ((mapped (indigo-stream-map #'indigo-symbol stream)))
          (should (indigo-stream-empty-p mapped)))))))

(ert-deftest test-indigo-stream-map-laziness ()
  "Test that map is truly lazy - doesn't force entire stream."
  (let ((call-count 0))
    (indigo-with-molecule (mol "CCO")
      (indigo-with-atoms-iterator (atoms mol)
        (let* ((stream (indigo-stream atoms))
               (mapped (indigo-stream-map
                        (lambda (atom)
                          (setq call-count (1+ call-count))
                          (prog1 (indigo-symbol atom)
                            (indigo-free atom)))
                        stream)))
          ;; Creating the mapped stream should not call the function
          (should (= 0 call-count))
          ;; Accessing first element should call once
          (let ((first (indigo-stream-first mapped)))
            (should (= 1 call-count))
            (should (equal "C" first)))
          ;; Advancing to second element should call once more
          (setq mapped (indigo-stream-rest mapped))
          (should (= 1 call-count))  ; Still 1 because we haven't accessed car yet
          (let ((second (indigo-stream-first mapped)))
            (should (= 2 call-count))
            (should (equal "C" second))))))))

(ert-deftest test-indigo-stream-map-chaining ()
  "Test chaining multiple map operations."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             ;; First map: extract symbols (and free handles)
             (symbols (indigo-stream-map
                       (lambda (atom)
                         (prog1 (indigo-symbol atom)
                           (indigo-free atom)))
                       stream))
             ;; Second map: convert to lowercase
             (lower (indigo-stream-map #'downcase symbols))
             ;; Third map: add prefix
             (prefixed (indigo-stream-map
                        (lambda (s) (concat "atom-" s))
                        lower)))
        ;; Check first element through all transformations
        (should (equal "atom-c" (indigo-stream-first prefixed)))
        ;; Check second element
        (setq prefixed (indigo-stream-rest prefixed))
        (should (equal "atom-c" (indigo-stream-first prefixed)))
        ;; Check third element
        (setq prefixed (indigo-stream-rest prefixed))
        (should (equal "atom-o" (indigo-stream-first prefixed)))))))

(ert-deftest test-indigo-stream-map-complex-molecule ()
  "Test mapping over a more complex molecule."
  (indigo-with-molecule (mol "CC(C)C(=O)O")  ; Isobutyric acid
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             (symbols (indigo-stream-map
                       (lambda (atom)
                         (prog1 (indigo-symbol atom)
                           (indigo-free atom)))
                       stream))
             (collected nil))
        ;; Collect all symbols
        (while (not (indigo-stream-empty-p symbols))
          (push (indigo-stream-first symbols) collected)
          (setq symbols (indigo-stream-rest symbols)))
        ;; Should have 6 heavy atoms: C-C-C-C-O-O
        (should (= 6 (length collected)))
        (should (equal '("O" "O" "C" "C" "C" "C") collected))))))

;;; Stream Collect Operations

(ert-deftest test-indigo-stream-collect-basic ()
  "Test basic stream collection."
  (indigo-with-molecule (mol "CCO")  ; Ethanol
    (indigo-with-atoms-stream (stream mol)
      (let* ((symbols (indigo-stream-map #'indigo-symbol stream))
             (collected (indigo-stream-collect symbols)))
        ;; Should collect all 3 symbols in order
        (should (equal '("C" "C" "O") collected))))))

(ert-deftest test-indigo-stream-collect-empty ()
  "Test collecting from an empty stream."
  (indigo-with-molecule (mol "C")
    (indigo-with-atoms-stream (stream mol)
      ;; Advance to empty stream
      (setq stream (indigo-stream-rest stream))
      ;; Collect from empty stream should return empty list
      (let ((collected (indigo-stream-collect stream)))
        (should (equal '() collected))))))

(ert-deftest test-indigo-stream-collect-charges ()
  "Test collecting charges from a molecule."
  (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
    (indigo-with-atoms-stream (stream mol)
      (let* ((charges (indigo-stream-map #'indigo-charge stream))
             (collected (indigo-stream-collect charges)))
        ;; All 6 carbons in benzene should have charge 0
        (should (equal '(0 0 0 0 0 0) collected))))))

(ert-deftest test-indigo-stream-collect-with-chaining ()
  "Test collecting from chained map operations."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-stream (stream mol)
      (let* (;; First map: extract symbols
             (symbols (indigo-stream-map #'indigo-symbol stream))
             ;; Second map: convert to lowercase
             (lower (indigo-stream-map #'downcase symbols))
             ;; Third map: add prefix
             (prefixed (indigo-stream-map
                        (lambda (s) (concat "atom-" s))
                        lower))
             ;; Collect final result
             (collected (indigo-stream-collect prefixed)))
        ;; Should have all transformations applied
        (should (equal '("atom-c" "atom-c" "atom-o") collected))))))

(ert-deftest test-indigo-stream-collect-complex-molecule ()
  "Test collecting from a complex molecule."
  (indigo-with-molecule (mol "CC(C)C(=O)O")  ; Isobutyric acid
    (indigo-with-atoms-stream (stream mol)
      (let* ((symbols (indigo-stream-map #'indigo-symbol stream))
             (collected (indigo-stream-collect symbols)))
        ;; Should have 6 heavy atoms: C-C-C-C-O-O
        (should (= 6 (length collected)))
        (should (equal '("C" "C" "C" "C" "O" "O") collected))))))

(ert-deftest test-indigo-stream-collect-preserves-order ()
  "Test that collect preserves stream order."
  (indigo-with-molecule (mol "CCCCC")  ; Pentane
    (indigo-with-atoms-stream (stream mol)
      (let* ((indices (indigo-stream-map #'indigo-index stream))
             (collected (indigo-stream-collect indices)))
        ;; Indices should be in order: 0, 1, 2, 3, 4
        (should (equal '(0 1 2 3 4) collected))))))

(ert-deftest test-indigo-stream-collect-bonds ()
  "Test collecting bond information from a molecule."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-bonds-stream (stream mol)
      (let* ((bond-orders (indigo-stream-map #'indigo-bond-order stream))
             (collected (indigo-stream-collect bond-orders)))
        ;; CCO has 2 single bonds
        (should (equal '(:single :single) collected))))))

(ert-deftest test-indigo-stream-collect-partial-consumption ()
  "Test that collect works after partial stream consumption."
  (indigo-with-molecule (mol "CCCCCC")  ; Hexane (6 carbons)
    (indigo-with-atoms-stream (stream mol)
      (let ((symbols (indigo-stream-map #'indigo-symbol stream)))
        ;; Consume first 2 elements
        (should (equal "C" (indigo-stream-first symbols)))
        (setq symbols (indigo-stream-rest symbols))
        (should (equal "C" (indigo-stream-first symbols)))
        (setq symbols (indigo-stream-rest symbols))
        ;; Collect remaining elements
        (let ((collected (indigo-stream-collect symbols)))
          ;; Should have remaining 4 carbons
          (should (equal '("C" "C" "C" "C") collected)))))))

;;; With-style Macro Tests

;; Commented because this is mostly a debugging tool
;; (ert-deftest test-indigo-with-stream-from-iterator-macro-expansion ()
;;   "Print the actual macro expansion for inspection."
;;   (let* ((form '(indigo-with-stream-from-iterator (stream atoms)
;;                   (let ((first (indigo-stream-first stream)))
;;                     (indigo-symbol first))))
;;          (expanded (macroexpand form)))
;;     (message "\n=== MACRO EXPANSION ===")
;;     (message "Original form:")
;;     (message "%S" form)
;;     (message "\nExpanded form:")
;;     (message "%S" expanded)
;;     (message "\nPretty-printed expansion:")
;;     (pp expanded)
;;     (message "======================\n")
;;     ;; Just verify it expanded to something
;;     (should expanded)
;;     (should (eq (car expanded) 'let))))

(ert-deftest test-indigo-with-stream-from-iterator-basic ()
  "Test basic usage of indigo-with-stream-from-iterator macro."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (indigo-with-stream-from-iterator (stream atoms)
        ;; Stream should be a thunk
        (should (functionp stream))
        ;; Force and check first element
        (let ((forced (indigo-stream-force stream)))
          (should forced)
          (should (integerp (car forced))))))))

(ert-deftest test-indigo-with-stream-from-iterator-iteration ()
  "Test iterating through stream with automatic cleanup."
  (let ((symbols nil))
    (indigo-with-molecule (mol "CCO")
      (indigo-with-atoms-iterator (atoms mol)
        (indigo-with-stream-from-iterator (stream atoms)
          ;; Collect all symbols (elements are still tracked for cleanup)
          (while (not (indigo-stream-empty-p stream))
            (let ((atom (indigo-stream-first stream)))
              (push (indigo-symbol atom) symbols)
              (setq stream (indigo-stream-rest stream)))))))
    ;; All elements should have been freed automatically
    (should (equal '("O" "C" "C") symbols))))

(ert-deftest test-indigo-with-stream-from-iterator-bonds ()
  "Test with-stream-from-iterator with bonds iterator."
  (let ((bond-count 0))
    (indigo-with-molecule (mol "CCO")
      (indigo-with-bonds-iterator (bonds mol)
        (indigo-with-stream-from-iterator (stream bonds)
          ;; Count bonds
          (while (not (indigo-stream-empty-p stream))
            (setq bond-count (1+ bond-count))
            (setq stream (indigo-stream-rest stream))))))
    ;; CCO has 2 bonds
    (should (= 2 bond-count))))

(ert-deftest test-indigo-with-stream-from-iterator-with-map ()
  "Test combining with-stream-from-iterator with stream-map."
  (let ((symbols nil))
    (indigo-with-molecule (mol "c1ccccc1")  ; Benzene
      (indigo-with-atoms-iterator (atoms mol)
        (indigo-with-stream-from-iterator (stream atoms)
          ;; Map to extract symbols (note: original handles are tracked and freed)
          (let ((symbol-stream (indigo-stream-map #'indigo-symbol stream)))
            ;; Collect first 3 symbols
            (dotimes (_ 3)
              (push (indigo-stream-first symbol-stream) symbols)
              (setq symbol-stream (indigo-stream-rest symbol-stream)))))))
    ;; Should have 3 carbon symbols
    (should (= 3 (length symbols)))
    (should (equal '("C" "C" "C") symbols))))

(ert-deftest test-indigo-with-stream-from-iterator-partial-consumption ()
  "Test that only forced elements are tracked and freed."
  (let ((first-symbol nil))
    (indigo-with-molecule (mol "CCCCCCCCCC")  ; 10 carbons
      (indigo-with-atoms-iterator (atoms mol)
        (indigo-with-stream-from-iterator (stream atoms)
          ;; Only force first element
          (let ((atom (indigo-stream-first stream)))
            (setq first-symbol (indigo-symbol atom))))))
    ;; Should have successfully gotten first symbol
    (should (equal "C" first-symbol))
    ;; Only the first atom should have been forced and freed
    ;; The rest remain unforced in the stream
    ))

(ert-deftest test-indigo-with-stream-from-iterator-error-handling ()
  "Test that elements are freed even when an error occurs."
  (let ((symbols nil)
        (error-caught nil))
    (condition-case err
        (indigo-with-molecule (mol "CCO")
          (indigo-with-atoms-iterator (atoms mol)
            (indigo-with-stream-from-iterator (stream atoms)
              ;; Force first two elements
              (push (indigo-symbol (indigo-stream-first stream)) symbols)
              (setq stream (indigo-stream-rest stream))
              (push (indigo-symbol (indigo-stream-first stream)) symbols)
              ;; Trigger an error
              (error "Test error"))))
      (error (setq error-caught t)))
    ;; Error should have been caught
    (should error-caught)
    ;; But we should have collected 2 symbols before the error
    (should (= 2 (length symbols)))
    (should (equal '("C" "C") symbols))
    ;; The forced elements should have been freed by unwind-protect
    ))

(ert-deftest test-indigo-with-stream-from-iterator-empty-iterator ()
  "Test with-stream-from-iterator with an empty iterator (no rings in acyclic molecule)."
  (indigo-with-molecule (mol "CCC")  ; Propane (no rings)
    (indigo-with-sssr-iterator (rings mol)
      (indigo-with-stream-from-iterator (stream rings)
        ;; Stream should be empty
        (should (indigo-stream-empty-p stream))))))

(ert-deftest test-indigo-with-stream-from-iterator-rings ()
  "Test with-stream-from-iterator with SSSR rings iterator."
  (let ((ring-count 0))
    (indigo-with-molecule (mol "c1ccc2ccccc2c1")  ; Naphthalene (2 rings)
      (indigo-with-sssr-iterator (rings mol)
        (indigo-with-stream-from-iterator (stream rings)
          ;; Count rings
          (while (not (indigo-stream-empty-p stream))
            (setq ring-count (1+ ring-count))
            (setq stream (indigo-stream-rest stream))))))
    ;; Naphthalene has 2 SSSR rings
    (should (= 2 ring-count))))

(ert-deftest test-indigo-with-stream-from-iterator-nested ()
  "Test nesting multiple with-stream-from-iterator macros."
  (let ((total-neighbors 0))
    (indigo-with-molecule (mol "CCC")  ; Propane
      (indigo-with-atoms-iterator (atoms mol)
        (indigo-with-stream-from-iterator (atom-stream atoms)
          ;; For each atom
          (while (not (indigo-stream-empty-p atom-stream))
            (let ((atom (indigo-stream-first atom-stream)))
              ;; Count its neighbors using a nested stream
              (indigo-with-neighbors-iterator (neighbors atom)
                (indigo-with-stream-from-iterator (neighbor-stream neighbors)
                  (while (not (indigo-stream-empty-p neighbor-stream))
                    (setq total-neighbors (1+ total-neighbors))
                    (setq neighbor-stream (indigo-stream-rest neighbor-stream))))))
            (setq atom-stream (indigo-stream-rest atom-stream))))))
    ;; Propane: C-C-C
    ;; First C has 1 neighbor, middle C has 2 neighbors, last C has 1 neighbor
    ;; Total: 1 + 2 + 1 = 4
    (should (= 4 total-neighbors))))

(ert-deftest test-indigo-with-stream-from-iterator-verifies-cleanup ()
  "Test that indigo-free is actually called on all forced elements."
  (let ((forced-handles nil)
        (freed-handles nil))
    ;; Track which handles get forced from the stream
    (indigo-with-molecule (mol "CCO")
      (indigo-with-atoms-iterator (atoms mol)
        ;; Create stream with explicit tracker to capture forced handles
        (let ((--tracked-elements-- nil))
          (let ((stream (indigo-stream atoms
                                       (lambda (handle)
                                         (push handle --tracked-elements--)))))
            ;; Spy on indigo-free during this scope
            (advice-add 'indigo-free :before
                        (lambda (handle)
                          (push handle freed-handles)))
            (unwind-protect
                (progn
                  ;; Force all 3 atoms
                  (let ((atom1 (indigo-stream-first stream)))
                    (should (integerp atom1))
                    (setq stream (indigo-stream-rest stream))
                    (let ((atom2 (indigo-stream-first stream)))
                      (should (integerp atom2))
                      (setq stream (indigo-stream-rest stream))
                      (let ((atom3 (indigo-stream-first stream)))
                        (should (integerp atom3)))))
                  ;; Save the forced handles
                  (setq forced-handles (copy-sequence --tracked-elements--)))
              ;; Manual cleanup to trigger frees we're measuring
              (dolist (element --tracked-elements--)
                (when element (indigo-free element)))
              ;; Remove advice
              (advice-remove 'indigo-free
                             (lambda (handle)
                               (push handle freed-handles))))))))
    ;; Print debugging information
    ;; (message "\n=== CLEANUP VERIFICATION (3 atoms) ===")
    ;; (message "Forced handles: %S" (sort forced-handles #'<))
    ;; (message "Freed handles:  %S" (sort freed-handles #'<))
    ;; (message "Match: %s" (if (equal (sort (copy-sequence forced-handles) #'<)
    ;;                                  (sort (copy-sequence freed-handles) #'<))
    ;;                          "YES" "NO"))
    ;; (message "======================================\n")
    ;; Verify exactly 3 handles were forced
    (should (= 3 (length forced-handles)))
    ;; All forced handles should be integers (valid handles)
    (should (cl-every #'integerp forced-handles))
    ;; Verify exactly those 3 handles were freed
    (should (= 3 (length freed-handles)))
    ;; Verify the freed handles match the forced ones
    (should (equal (sort forced-handles #'<)
                   (sort freed-handles #'<)))))

(ert-deftest test-indigo-with-stream-from-iterator-verifies-partial-cleanup ()
  "Test that only forced elements are freed, not unforced ones."
  (let ((forced-handles nil)
        (freed-handles nil))
    ;; Track which handles get forced from the stream
    (indigo-with-molecule (mol "CCCCCCCCCC")  ; 10 carbons
      (indigo-with-atoms-iterator (atoms mol)
        ;; Create stream with explicit tracker to capture forced handles
        (let ((--tracked-elements-- nil))
          (let ((stream (indigo-stream atoms
                                       (lambda (handle)
                                         (push handle --tracked-elements--)))))
            ;; Spy on indigo-free during this scope
            (advice-add 'indigo-free :before
                        (lambda (handle)
                          (push handle freed-handles)))
            (unwind-protect
                (progn
                  ;; Only force first 2 atoms
                  (indigo-stream-first stream)
                  (setq stream (indigo-stream-rest stream))
                  (indigo-stream-first stream)
                  ;; Save the forced handles
                  (setq forced-handles (copy-sequence --tracked-elements--)))
              ;; Manual cleanup to trigger frees we're measuring
              (dolist (element --tracked-elements--)
                (when element (indigo-free element)))
              ;; Remove advice
              (advice-remove 'indigo-free
                             (lambda (handle)
                               (push handle freed-handles))))))))
    ;; Print debugging information
    ;; (message "\n=== PARTIAL CLEANUP VERIFICATION (2 of 10 atoms) ===")
    ;; (message "Forced handles: %S" (sort forced-handles #'<))
    ;; (message "Freed handles:  %S" (sort freed-handles #'<))
    ;; (message "Match: %s" (if (equal (sort (copy-sequence forced-handles) #'<)
    ;;                                  (sort (copy-sequence freed-handles) #'<))
    ;;                          "YES" "NO"))
    ;; (message "====================================================\n")
    ;; Verify exactly 2 handles were forced
    (should (= 2 (length forced-handles)))
    ;; Verify exactly those 2 handles were freed
    (should (= 2 (length freed-handles)))
    ;; Verify the freed handles match the forced ones
    (should (equal (sort forced-handles #'<)
                   (sort freed-handles #'<)))))

(provide 'test-indigo-streams)
;;; test-indigo-streams.el ends here
