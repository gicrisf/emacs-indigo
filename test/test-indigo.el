;;; test-indigo.el --- Tests for indigo -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for the indigo module.

;;; Code:

(require 'ert)
(require 'indigo)

(ert-deftest test-molecular-formula-ethanol ()
  "Test molecular formula calculation for ethanol (CCO)."
  (should (string= (indigo-molecular-formula "CCO") "C2 H6 O")))

(ert-deftest test-molecular-formula-methane ()
  "Test molecular formula calculation for methane (C)."
  (should (string= (indigo-molecular-formula "C") "C H4")))

(ert-deftest test-molecular-formula-benzene ()
  "Test molecular formula calculation for benzene (c1ccccc1)."
  (should (string= (indigo-molecular-formula "c1ccccc1") "C6 H6")))

(ert-deftest test-molecular-formula-invalid-smiles ()
  "Test molecular formula calculation with invalid SMILES."
  (should (null (indigo-molecular-formula "invalid"))))

(ert-deftest test-molecular-formula-empty-string ()
  "Test molecular formula calculation with empty string."
  (should (string= (indigo-molecular-formula "") "")))

(ert-deftest test-indigo-version ()
  (should (stringp (indigo-version)))
  (should-not (string-empty-p (indigo-version))))

(ert-deftest test-molecular-weight-ethanol ()
  "Test molecular weight calculation for ethanol (CCO)."
  (should (floatp (indigo-molecular-weight "CCO")))
  (should (< (abs (- (indigo-molecular-weight "CCO") 46.07)) 0.1)))

(ert-deftest test-molecular-weight-methane ()
  "Test molecular weight calculation for methane (C)."
  (should (floatp (indigo-molecular-weight "C")))
  (should (< (abs (- (indigo-molecular-weight "C") 16.04)) 0.1)))

(ert-deftest test-molecular-weight-benzene ()
  "Test molecular weight calculation for benzene (c1ccccc1)."
  (should (floatp (indigo-molecular-weight "c1ccccc1")))
  (should (< (abs (- (indigo-molecular-weight "c1ccccc1") 78.11)) 0.1)))

(ert-deftest test-molecular-weight-invalid-smiles ()
  "Test molecular weight calculation with invalid SMILES."
  (should (null (indigo-molecular-weight "invalid"))))

(provide 'test-indigo)

;;; test-indigo.el ends here
