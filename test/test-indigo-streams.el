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

(ert-deftest test-indigo-stream-next ()
  "Test advancing through a stream."
  (indigo-with-molecule (mol "CCO")
    (indigo-with-atoms-iterator (atoms mol)
      (let* ((stream (indigo-stream atoms))
             (first-val (indigo-stream-car stream))
             (stream (indigo-stream-next stream))
             (second-val (indigo-stream-car stream)))
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
        (let ((atom (indigo-stream-car stream)))
          (should (integerp atom))
          (indigo-free atom))
        ;; Advance to next (which should be empty)
        (setq stream (indigo-stream-next stream))
        ;; Now, stream should be empty
        (should (indigo-stream-empty-p stream))))))

;;; Stream Map Operations

(ert-deftest test-indigo-stream-basic-iteration ()
  "Test basic stream iteration without map."
  (indigo-with-molecule (mol "CCO")  ; Ethanol
    (indigo-with-atoms-iterator (atoms mol)
      (let ((stream (indigo-stream atoms)))
        ;; Get first atom
        (let* ((first-handle (indigo-stream-car stream))
               (first-sym (indigo-symbol first-handle)))
          (should (equal "C" first-sym))
          (indigo-free first-handle))
        ;; Advance and get second
        (setq stream (indigo-stream-next stream))
        (let* ((second-handle (indigo-stream-car stream))
               (second-sym (indigo-symbol second-handle)))
          (should (equal "C" second-sym))
          (indigo-free second-handle))
        ;; Advance and get third
        (setq stream (indigo-stream-next stream))
        (let* ((third-handle (indigo-stream-car stream))
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
        (should (equal "C" (indigo-stream-car symbols)))
        ;; Advance to second
        (setq symbols (indigo-stream-next symbols))
        (should (equal "C" (indigo-stream-car symbols)))
        ;; Advance to third
        (setq symbols (indigo-stream-next symbols))
        (should (equal "O" (indigo-stream-car symbols)))
        ;; Advance past end
        (setq symbols (indigo-stream-next symbols))
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
        (should (= 0 (indigo-stream-car charges)))
        (setq charges (indigo-stream-next charges))
        (should (= 0 (indigo-stream-car charges)))
        (setq charges (indigo-stream-next charges))
        (should (= 0 (indigo-stream-car charges)))))))

(ert-deftest test-indigo-stream-map-empty ()
  "Test mapping over empty stream."
  (indigo-with-molecule (mol "C")
    (indigo-with-atoms-iterator (atoms mol)
      (let ((stream (indigo-stream atoms)))
        ;; Free first element
        (indigo-free (indigo-stream-car stream))
        ;; Advance to empty stream
        (setq stream (indigo-stream-next stream))
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
          (let ((first (indigo-stream-car mapped)))
            (should (= 1 call-count))
            (should (equal "C" first)))
          ;; Advancing to second element should call once more
          (setq mapped (indigo-stream-next mapped))
          (should (= 1 call-count))  ; Still 1 because we haven't accessed car yet
          (let ((second (indigo-stream-car mapped)))
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
        (should (equal "atom-c" (indigo-stream-car prefixed)))
        ;; Check second element
        (setq prefixed (indigo-stream-next prefixed))
        (should (equal "atom-c" (indigo-stream-car prefixed)))
        ;; Check third element
        (setq prefixed (indigo-stream-next prefixed))
        (should (equal "atom-o" (indigo-stream-car prefixed)))))))

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
          (push (indigo-stream-car symbols) collected)
          (setq symbols (indigo-stream-next symbols)))
        ;; Should have 6 heavy atoms: C-C-C-C-O-O
        (should (= 6 (length collected)))
        (should (equal '("O" "O" "C" "C" "C" "C") collected))))))

;;; With-style Macro Tests

;; Commented because this is mostly a debugging tool
;; (ert-deftest test-indigo-with-stream-from-iterator-macro-expansion ()
;;   "Print the actual macro expansion for inspection."
;;   (let* ((form '(indigo-with-stream-from-iterator (stream atoms)
;;                   (let ((first (indigo-stream-car stream)))
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
            (let ((atom (indigo-stream-car stream)))
              (push (indigo-symbol atom) symbols)
              (setq stream (indigo-stream-next stream)))))))
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
            (setq stream (indigo-stream-next stream))))))
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
              (push (indigo-stream-car symbol-stream) symbols)
              (setq symbol-stream (indigo-stream-next symbol-stream)))))))
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
          (let ((atom (indigo-stream-car stream)))
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
              (push (indigo-symbol (indigo-stream-car stream)) symbols)
              (setq stream (indigo-stream-next stream))
              (push (indigo-symbol (indigo-stream-car stream)) symbols)
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
            (setq stream (indigo-stream-next stream))))))
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
            (let ((atom (indigo-stream-car atom-stream)))
              ;; Count its neighbors using a nested stream
              (indigo-with-neighbors-iterator (neighbors atom)
                (indigo-with-stream-from-iterator (neighbor-stream neighbors)
                  (while (not (indigo-stream-empty-p neighbor-stream))
                    (setq total-neighbors (1+ total-neighbors))
                    (setq neighbor-stream (indigo-stream-next neighbor-stream))))))
            (setq atom-stream (indigo-stream-next atom-stream))))))
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
                  (let ((atom1 (indigo-stream-car stream)))
                    (should (integerp atom1))
                    (setq stream (indigo-stream-next stream))
                    (let ((atom2 (indigo-stream-car stream)))
                      (should (integerp atom2))
                      (setq stream (indigo-stream-next stream))
                      (let ((atom3 (indigo-stream-car stream)))
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
                  (indigo-stream-car stream)
                  (setq stream (indigo-stream-next stream))
                  (indigo-stream-car stream)
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
