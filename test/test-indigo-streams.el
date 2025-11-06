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
  (indigo-let* ((:molecule mol "CCO")
                (:atoms atoms mol)
                (stream (indigo-stream atoms)))
    ;; Stream should be non-nil
    (should stream)
    ;; First element should be an integer handle
    (should (integerp (car stream)))
    ;; Stream should have a thunk
    (should (functionp (cdr stream)))))

(ert-deftest test-indigo-stream-next ()
  "Test advancing through a stream."
  (indigo-let* ((:molecule mol "CCO")
                (:atoms atoms mol)
                (stream (indigo-stream atoms))
                (first-val (car stream))
                (stream (indigo-stream-next stream))
                (second-val (car stream)))
    ;; First and second values should be different
    (should (not (= first-val second-val)))
    ;; Both should be valid handles
    (should (integerp first-val))
    (indigo-free first-val)
    (should (integerp second-val))
    (indigo-free second-val)))

(ert-deftest test-indigo-stream-empty-p ()
  "Test checking if stream is empty."
  (indigo-let* ((:molecule mol "C")
                (:atoms atoms mol)
                (stream (indigo-stream atoms)))
    ;; Single carbon - stream should not be empty
    (should stream)
    ;; Now, stream is (ATOM-HANDLE . THUNK)
    ;; Free the handle
    (indigo-free (car stream))
    ;; Consume the stream
    (setq stream (indigo-stream-next stream))
    ;; Now, stream should be nil
    (should-not stream)))

(provide 'test-indigo-streams)
;;; test-indigo-streams.el ends here
