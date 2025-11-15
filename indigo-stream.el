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
;; - `indigo-with-stream-from-iterator' - Macro for automatic element cleanup
;;
;; Planned Extensions:
;; - indigo-stream-filter - Filter stream by predicate
;; - indigo-stream-take - Take first N elements
;; - indigo-stream-fold - Fold/reduce operation

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

(defun indigo-stream-collect (stream)
  "Collect all elements from STREAM into a list.

Forces the entire stream and returns a list of all elements.
STREAM is consumed in the process.

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

(provide 'indigo-stream)

;;; indigo-stream.el ends here
