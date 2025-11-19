;;; indigo-stream.el --- Lazy stream abstraction for Indigo iterators -*- lexical-binding: t; -*-

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
;;
;; Author: Giovanni Crisalfi
;;
;;; Commentary:

;; This module provides a lazy stream abstraction built on top of the
;; Indigo iterator API.  Streams provide a functional, composable way to
;; work with sequences of molecular data.
;;
;; Design Principles:
;; - Fully lazy: No computation happens until explicitly forced
;; - Clear ownership: Stream does NOT own the iterator - caller is
;;   responsible for freeing it
;; - Lazy evaluation: Both head and tail are lazy - nothing computed until forced
;; - Composable: Map, filter, and other combinators preserve laziness
;;
;; Current API:
;; - `indigo-stream' - Create a lazy stream from an iterator (with optional tracking)
;; - `indigo-stream-force' - Force a stream thunk
;; - `indigo-stream-first' - Force and get the first element
;; - `indigo-stream-rest' - Force and get the rest of the stream
;; - `indigo-stream-empty-p' - Check if stream is empty
;; - `indigo-stream-map' - Map over stream elements
;; - `indigo-stream-collect' - Collect all stream elements into a list
;; - `indigo-stream-take' - Take first N elements (lazy)
;; - `indigo-stream-filter' - Filter stream by predicate (lazy)
;; - `indigo-stream-fold' - Fold/reduce operation (consuming)
;; - `indigo-with-stream-from-iterator' - Macro for automatic element cleanup

;;; Code:

;;; Forward declarations for iterator functions
(declare-function indigo-next "indigo-module")
(declare-function indigo-free "indigo-module")

;;; Lazy Stream Abstraction

(defun indigo-stream (iterator &optional tracker-fn)
  "Create a lazy stream from an Indigo ITERATOR.

If TRACKER-FN is provided, it will be called with each element when
it's first forced from the iterator. This is useful for tracking
elements for later cleanup.

Returns a memoized stream thunk that, when forced, produces
  -> (element . next-stream-thunk)

or just nil if the iterator is exhausted.

The stream does NOT take ownership of the iterator, so the caller
is responsible for freeing it when done."
  (when iterator
    ;; lol approach
    (let ((forced nil)
          (cached-value nil))
      (lambda ()
        (unless forced
          (let ((element (indigo-next iterator)))
            (when element
              ;; Track element if tracker provided
              (when tracker-fn
                (funcall tracker-fn element))
              ;; caching (value . next-stream-thunk)
              (setq cached-value (cons element (indigo-stream iterator tracker-fn))
                    forced t))))
        cached-value))))

(defun indigo-stream-force (stream)
  "Force STREAM thunk, returning (value . next-thunk) or nil."
  (when (functionp stream)
    (funcall stream)))

(defun indigo-stream-first (stream)
  "Force STREAM and return the first element, or nil if empty.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (indigo-stream-first stream)))  ; => atom handle"
  (let ((forced (indigo-stream-force stream)))
    (when forced
      (car forced))))

(defun indigo-stream-rest (stream)
  "Force STREAM and return the rest of the stream, or nil if empty.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (setq stream (indigo-stream-rest stream))
      (indigo-stream-first stream)))  ; => second atom handle"
  (let ((forced (indigo-stream-force stream)))
    (when forced
      (cdr forced))))

(defun indigo-stream-empty-p (stream)
  "Check if STREAM is empty."
  (or (null stream)
      (null (indigo-stream-force stream))))

;;; Stream Combinators

(defun indigo-stream-map (fn stream)
  "Map FN over elements in STREAM, returning a new lazy stream.

This is a lazy implementation: FN is only called when
accessing elements via `indigo-stream-first'
or advancing via `indigo-stream-rest'.

FN receives Indigo object handles and is responsible
for freeing them if they are no longer needed.

Returns a new stream thunk that when forced produces:
  - nil if STREAM is empty
  - (mapped-value . next-mapped-stream-thunk) cons cell

Example (map symbol extraction over atoms):
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (let ((symbols (indigo-stream-map #\\='indigo-symbol stream)))
        ;; symbols is now a stream of strings
        (let ((first-sym (indigo-stream-first symbols)))
          (message \"First symbol: %s\" first-sym)  ; => \"C\"
          ;; Advance to rest
          (setq symbols (indigo-stream-rest symbols))
          (message \"Second symbol: %s\" (indigo-stream-first symbols))))))  ; => \"C\"

Example (collecting charges from atoms):
  (indigo-with-molecule (mol \"c1ccccc1\")  ; Benzene
    (indigo-with-atoms-stream (stream mol)
      (let ((charges (indigo-stream-map #\\='indigo-charge stream)))
        ;; Stream of charge values
        (indigo-stream-first charges))))  ; => 0"
  (when stream
    ;; memoized thunk
    (let ((forced nil)
          (cached-value nil))
      (lambda ()
        (unless forced
          (let ((source-forced (indigo-stream-force stream)))
            (when source-forced
              (let ((value (car source-forced))
                    (next (cdr source-forced)))
                ;; Apply function to current element and cache the result
                (setq cached-value
                      (cons (funcall fn value)
                            ;; Recursively map over the rest
                            (indigo-stream-map fn next))
                      forced t)))))
        cached-value))))

(defun indigo-stream-take (n stream)
  "Take first N elements from STREAM, returning a new lazy stream.

This is a lazy implementation: elements are only forced when accessed
via `indigo-stream-first' or `indigo-stream-rest'. The resulting stream
will produce at most N elements.

N must be a non-negative integer. If N is 0 or STREAM is empty,
returns nil (empty stream).

Returns a new stream thunk that when forced produces:
  - nil if N is 0 or STREAM is empty
  - (element . next-stream-thunk) cons cell otherwise

Example (take first 3 atoms):
  (indigo-with-molecule (mol \"CCCCCC\")  ; Hexane (6 carbons)
    (indigo-with-atoms-stream (stream mol)
      (let ((first-three (indigo-stream-take 3 stream)))
        (indigo-stream-collect first-three))))  ; => (atom1 atom2 atom3)

Example (composing with map):
  (indigo-with-molecule (mol \"CCCCCC\")
    (indigo-with-atoms-stream (stream mol)
      (let* ((first-three (indigo-stream-take 3 stream))
             (symbols (indigo-stream-map #\\='indigo-symbol first-three)))
        (indigo-stream-collect symbols))))  ; => (\"C\" \"C\" \"C\")"
  (when (and stream (> n 0))
    ;; Memoized thunk following the same pattern as indigo-stream-map
    (let ((forced nil)
          (cached-value nil))
      (lambda ()
        (unless forced
          (let ((source-forced (indigo-stream-force stream)))
            (when source-forced
              (let ((value (car source-forced))
                    (next (cdr source-forced)))
                ;; Cache current element and recursively take (n-1) from rest
                (setq cached-value
                      (cons value
                            (indigo-stream-take (1- n) next))
                      forced t)))))
        cached-value))))

(defun indigo-stream-filter (predicate stream)
  "Filter STREAM by PREDICATE, returning a new lazy stream.

This is a lazy implementation: PREDICATE is only called when advancing
through the stream via `indigo-stream-first' or `indigo-stream-rest'.
The resulting stream contains only elements for which PREDICATE returns
non-nil.

PREDICATE receives Indigo object handles and is responsible for freeing
them if they are no longer needed.

Returns a new stream thunk that when forced produces:
  - nil if STREAM is empty or no elements match
  - (element . next-stream-thunk) cons cell for first matching element

Example (filter carbons only):
  (indigo-with-molecule (mol \"CCO\")  ; Ethanol
    (indigo-with-atoms-stream (stream mol)
      (let* ((carbons (indigo-stream-filter
                       (lambda (atom)
                         (equal (indigo-symbol atom) \"C\"))
                       stream))
             (symbols (indigo-stream-map #\\='indigo-symbol carbons)))
        (indigo-stream-collect symbols))))  ; => (\"C\" \"C\")

Example (filter charged atoms):
  (indigo-with-molecule (mol \"[O-]CCO\")  ; Ethoxide ion
    (indigo-with-atoms-stream (stream mol)
      (let* ((charged (indigo-stream-filter
                       (lambda (atom)
                         (not (= 0 (indigo-charge atom))))
                       stream))
             (charges (indigo-stream-map #\\='indigo-charge charged)))
        (indigo-stream-collect charges))))  ; => (-1)

Example (composition with map and take):
  (indigo-with-molecule (mol \"CCCCCCCO\")  ; Heptanol
    (indigo-with-atoms-stream (stream mol)
      (let* ((carbons (indigo-stream-filter
                       (lambda (atom)
                         (equal (indigo-symbol atom) \"C\"))
                       stream))
             (first-three (indigo-stream-take 3 carbons))
             (indices (indigo-stream-map #\\='indigo-index first-three)))
        (indigo-stream-collect indices))))  ; => (0 1 2)"
  (when stream
    ;; Memoized thunk - must search for first matching element
    (let ((forced nil)
          (cached-value nil))
      (lambda ()
        (unless forced
          ;; Search through stream until we find a matching element
          (let ((current-stream stream)
                (found nil))
            (while (and (not found)
                        (not (indigo-stream-empty-p current-stream)))
              (let* ((source-forced (indigo-stream-force current-stream))
                     (value (car source-forced))
                     (next (cdr source-forced)))
                (if (funcall predicate value)
                    ;; Found matching element - cache it and recursively filter rest
                    (progn
                      (setq cached-value
                            (cons value
                                  (indigo-stream-filter predicate next))
                            found t))
                  ;; Doesn't match - continue searching
                  (setq current-stream next))))
            (setq forced t)))
        cached-value))))

(defun indigo-stream-fold (fn init stream)
  "Fold STREAM from left to right using FN with initial value INIT.

This is a consuming operation: forces the entire stream and returns
a single accumulated result. FN is called with two arguments:
  (FN accumulator element)

and should return the new accumulator value.

FN receives Indigo object handles as elements and is responsible for
freeing them if they are no longer needed.

Example (sum of atom indices):
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (indigo-stream-fold
       (lambda (acc atom)
         (+ acc (indigo-index atom)))
       0
       stream)))  ; => 3  (0 + 1 + 2)

Example (count carbons):
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (indigo-stream-fold
       (lambda (acc atom)
         (if (equal (indigo-symbol atom) \"C\")
             (1+ acc)
           acc))
       0
       stream)))  ; => 2

Example (build string from symbols):
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (indigo-stream-fold
       (lambda (acc atom)
         (concat acc (indigo-symbol atom)))
       \"\"
       stream)))  ; => \"CCO\"

Example (find max charge):
  (indigo-with-molecule (mol \"[O-]C[NH3+]\")
    (indigo-with-atoms-stream (stream mol)
      (indigo-stream-fold
       (lambda (acc atom)
         (max acc (indigo-charge atom)))
       most-negative-fixnum
       stream)))  ; => 1

Example (compose with filter and map):
  (indigo-with-molecule (mol \"CCCCCO\")
    (indigo-with-atoms-stream (stream mol)
      (let* ((carbons (indigo-stream-filter
                       (lambda (atom)
                         (equal (indigo-symbol atom) \"C\"))
                       stream))
             (indices (indigo-stream-map #\\='indigo-index carbons)))
        (indigo-stream-fold #\\='+ 0 indices))))  ; => 10  (0+1+2+3+4)"
  (let ((acc init))
    (while (not (indigo-stream-empty-p stream))
      (setq acc (funcall fn acc (indigo-stream-first stream)))
      (setq stream (indigo-stream-rest stream)))
    acc))

(defun indigo-stream-collect (stream)
  "Collect all elements from STREAM into a list.

Forces the entire stream and returns a list of all elements.
STREAM is consumed in the process.

This is equivalent to:
  (nreverse (indigo-stream-fold (lambda (acc x) (cons x acc)) nil stream))

Example (collecting atom symbols):
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-stream (stream mol)
      (let ((symbols (indigo-stream-map #\\='indigo-symbol stream)))
        (indigo-stream-collect symbols))))  ; => (\"C\" \"C\" \"O\")

Example (collecting charges):
  (indigo-with-molecule (mol \"c1ccccc1\")  ; Benzene
    (indigo-with-atoms-stream (stream mol)
      (let ((charges (indigo-stream-map #\\='indigo-charge stream)))
        (indigo-stream-collect charges))))  ; => (0 0 0 0 0 0)"
  (let ((result nil))
    (while (not (indigo-stream-empty-p stream))
      (push (indigo-stream-first stream) result)
      (setq stream (indigo-stream-rest stream)))
    (nreverse result)))

;;; With-style Macro for Streams

(defmacro indigo-with-stream-from-iterator (binding &rest body)
  "Create a stream from an iterator with automatic element cleanup.

Usage: (indigo-with-stream-from-iterator (STREAM-VAR ITERATOR-VAR) BODY...)

The stream is created from ITERATOR-VAR, and all elements forced from
the stream are tracked and automatically freed when exiting the scope.
The iterator itself should be managed by an outer `indigo-with-*-iterator'
macro.

Example:
  (indigo-with-molecule (mol \"CCO\")
    (indigo-with-atoms-iterator (atoms mol)
      (indigo-with-stream-from-iterator (stream atoms)
        ;; Use stream here - all forced elements are freed on exit
        (let ((first (indigo-stream-first stream)))
          (indigo-symbol first)))))"
  (declare (indent 1))
  (let ((stream-var (car binding))
        (iterator-var (cadr binding)))
    `(let ((--tracked-elements-- nil))
       (let ((,stream-var (indigo-stream
                           ,iterator-var
                           (lambda (element) (push element --tracked-elements--)))))
         (unwind-protect
             (progn ,@body)
           ;; Free all tracked elements
           (dolist (element --tracked-elements--)
             (when element (indigo-free element))))))))

;; Auto-generate sequential binding version
(define-indigo-with* "stream-from-iterator")

(provide 'indigo-stream)

;;; indigo-stream.el ends here
