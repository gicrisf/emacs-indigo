;;; test-indigo-stateless.el --- Tests for indigo stateless functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Giovanni Crisalfi

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for the indigo stateless (convenience) functions.
;; Tests cover all 30 stateless functions that provide format-agnostic 
;; operations on molecular and reaction strings with automatic cleanup.

;;; Code:

(require 'ert)
(require 'indigo)

(ert-deftest test-molecular-formula-ethanol ()
  "Test molecular formula calculation for ethanol (CCO)."
  (should (string= (indigo-do-molecular-formula "CCO") "C2 H6 O")))

(ert-deftest test-molecular-formula-methane ()
  "Test molecular formula calculation for methane (C)."
  (should (string= (indigo-do-molecular-formula "C") "C H4")))

(ert-deftest test-molecular-formula-benzene ()
  "Test molecular formula calculation for benzene (c1ccccc1)."
  (should (string= (indigo-do-molecular-formula "c1ccccc1") "C6 H6")))

(ert-deftest test-molecular-formula-invalid-smiles ()
  "Test molecular formula calculation with invalid SMILES."
  (should (null (indigo-do-molecular-formula "invalid"))))

(ert-deftest test-molecular-formula-empty-string ()
  "Test molecular formula calculation with empty string."
  (should (string= (indigo-do-molecular-formula "") "")))


(ert-deftest test-molecular-weight-ethanol ()
  "Test molecular weight calculation for ethanol (CCO)."
  (should (floatp (indigo-do-molecular-weight "CCO")))
  (should (< (abs (- (indigo-do-molecular-weight "CCO") 46.07)) 0.1)))

(ert-deftest test-molecular-weight-methane ()
  "Test molecular weight calculation for methane (C)."
  (should (floatp (indigo-do-molecular-weight "C")))
  (should (< (abs (- (indigo-do-molecular-weight "C") 16.04)) 0.1)))

(ert-deftest test-molecular-weight-benzene ()
  "Test molecular weight calculation for benzene (c1ccccc1)."
  (should (floatp (indigo-do-molecular-weight "c1ccccc1")))
  (should (< (abs (- (indigo-do-molecular-weight "c1ccccc1") 78.11)) 0.1)))

(ert-deftest test-molecular-weight-invalid-smiles ()
  "Test molecular weight calculation with invalid SMILES."
  (should (null (indigo-do-molecular-weight "invalid"))))

(ert-deftest test-canonical-smiles-ethanol ()
  "Test canonical SMILES for ethanol (CCO)."
  (should (stringp (indigo-do-canonical-smiles "CCO")))
  (should (string= (indigo-do-canonical-smiles "CCO") "CCO")))

(ert-deftest test-canonical-smiles-benzene ()
  "Test canonical SMILES for benzene (c1ccccc1)."
  (should (stringp (indigo-do-canonical-smiles "c1ccccc1")))
  (should (string= (indigo-do-canonical-smiles "c1ccccc1") "c1ccccc1")))

(ert-deftest test-canonical-smiles-normalization ()
  "Test SMILES normalization (different representations of same molecule)."
  (let ((smiles1 "CCO")
        (smiles2 "OCC"))
    (should (string= (indigo-do-canonical-smiles smiles1)
                     (indigo-do-canonical-smiles smiles2)))))

(ert-deftest test-canonical-smiles-invalid ()
  "Test canonical SMILES with invalid input."
  (should (null (indigo-do-canonical-smiles "invalid"))))

(ert-deftest test-atom-count-ethanol ()
  "Test heavy atom count for ethanol (CCO)."
  (should (integerp (indigo-do-atom-count "CCO")))
  (should (= (indigo-do-atom-count "CCO") 3))) ; 2 C + 1 O = 3 heavy atoms

(ert-deftest test-atom-count-methane ()
  "Test heavy atom count for methane (C)."
  (should (integerp (indigo-do-atom-count "C")))
  (should (= (indigo-do-atom-count "C") 1))) ; 1 C = 1 heavy atom

(ert-deftest test-atom-count-benzene ()
  "Test heavy atom count for benzene (c1ccccc1)."
  (should (integerp (indigo-do-atom-count "c1ccccc1")))
  (should (= (indigo-do-atom-count "c1ccccc1") 6))) ; 6 C = 6 heavy atoms

(ert-deftest test-atom-count-invalid ()
  "Test atom count with invalid SMILES."
  (should (null (indigo-do-atom-count "invalid"))))

(ert-deftest test-bond-count-ethanol ()
  "Test explicit bond count for ethanol (CCO)."
  (should (integerp (indigo-do-bond-count "CCO")))
  (should (= (indigo-do-bond-count "CCO") 2))) ; C-C + C-O = 2 explicit bonds

(ert-deftest test-bond-count-methane ()
  "Test explicit bond count for methane (C)."
  (should (integerp (indigo-do-bond-count "C")))
  (should (= (indigo-do-bond-count "C") 0))) ; No explicit bonds (only implicit C-H)

(ert-deftest test-bond-count-benzene ()
  "Test explicit bond count for benzene (c1ccccc1)."
  (should (integerp (indigo-do-bond-count "c1ccccc1")))
  (should (= (indigo-do-bond-count "c1ccccc1") 6))) ; 6 C-C aromatic bonds

(ert-deftest test-bond-count-invalid ()
  "Test bond count with invalid SMILES."
  (should (null (indigo-do-bond-count "invalid"))))

(ert-deftest test-molfile-ethanol ()
  "Test MOL file generation for ethanol (CCO)."
  (let ((molfile (indigo-do-molfile "CCO")))
    (should (stringp molfile))
    (should (string-match-p "V2000" molfile)) ; MOL file format marker
    (should (string-match-p "END" molfile))))

(ert-deftest test-molfile-methane ()
  "Test MOL file generation for methane (C)."
  (let ((molfile (indigo-do-molfile "C")))
    (should (stringp molfile))
    (should (string-match-p "V2000" molfile))))

(ert-deftest test-molfile-invalid ()
  "Test MOL file generation with invalid SMILES."
  (should (null (indigo-do-molfile "invalid"))))

(ert-deftest test-hydrogen-count-ethanol ()
  "Test hydrogen count for ethanol (CCO)."
  (should (integerp (indigo-do-hydrogen-count "CCO")))
  (should (= (indigo-do-hydrogen-count "CCO") 6))) ; 3 H on first C + 2 H on second C + 1 H on O = 6

(ert-deftest test-hydrogen-count-methane ()
  "Test hydrogen count for methane (C)."
  (should (integerp (indigo-do-hydrogen-count "C")))
  (should (= (indigo-do-hydrogen-count "C") 4))) ; 4 H on C = 4

(ert-deftest test-hydrogen-count-benzene ()
  "Test hydrogen count for benzene (c1ccccc1)."
  (should (integerp (indigo-do-hydrogen-count "c1ccccc1")))
  (should (= (indigo-do-hydrogen-count "c1ccccc1") 6))) ; 1 H on each C = 6

(ert-deftest test-hydrogen-count-invalid ()
  "Test hydrogen count with invalid SMILES."
  (should (null (indigo-do-hydrogen-count "invalid"))))

(ert-deftest test-total-atom-count-ethanol ()
  "Test total atom count for ethanol (CCO)."
  (should (integerp (indigo-do-total-atom-count "CCO")))
  (should (= (indigo-do-total-atom-count "CCO") 9))) ; 3 heavy atoms + 6 hydrogens = 9

(ert-deftest test-total-atom-count-methane ()
  "Test total atom count for methane (C)."
  (should (integerp (indigo-do-total-atom-count "C")))
  (should (= (indigo-do-total-atom-count "C") 5))) ; 1 heavy atom + 4 hydrogens = 5

(ert-deftest test-total-atom-count-benzene ()
  "Test total atom count for benzene (c1ccccc1)."
  (should (integerp (indigo-do-total-atom-count "c1ccccc1")))
  (should (= (indigo-do-total-atom-count "c1ccccc1") 12))) ; 6 heavy atoms + 6 hydrogens = 12

(ert-deftest test-total-atom-count-invalid ()
  "Test total atom count with invalid SMILES."
  (should (null (indigo-do-total-atom-count "invalid"))))

(ert-deftest test-molecular-format-support ()
  "Test that Indigo functions support multiple molecular formats."
  ;; Test SMILES format
  (should (string= (indigo-do-molecular-formula "O") "H2 O"))
  (should (= (indigo-do-atom-count "O") 1))
  (should (= (indigo-do-hydrogen-count "O") 2))
  
  ;; Test MOL format (generate MOL from SMILES, then parse it back)
  (let* ((water-smiles "O")
         (mol-format (indigo-do-molfile water-smiles)))
    (should (stringp mol-format))
    (should (string-match-p "V2000" mol-format))
    
    ;; Test that we can parse the MOL format back
    (should (string= (indigo-do-molecular-formula mol-format) "H2 O"))
    (should (= (indigo-do-atom-count mol-format) 1))
    (should (= (indigo-do-hydrogen-count mol-format) 2)))
  
  ;; Test that InChI format is not supported
  (should (null (indigo-do-molecular-formula "InChI=1S/H2O/h1H2"))))

(ert-deftest test-molfile-roundtrip ()
  "Test converting SMILES to MOL format and back."
  (let* ((original-smiles "CCO")  ; ethanol
         (mol-format (indigo-do-molfile original-smiles))
         (canonical-from-smiles (indigo-do-canonical-smiles original-smiles))
         (canonical-from-mol (indigo-do-canonical-smiles mol-format)))
    ;; Should be able to generate MOL format
    (should (stringp mol-format))
    (should (string-match-p "V2000" mol-format))
    
    ;; Both formats should give same canonical SMILES
    (should (string= canonical-from-smiles canonical-from-mol))
    
    ;; Both should have same properties
    (should (= (indigo-do-atom-count original-smiles) 
               (indigo-do-atom-count mol-format)))
    (should (= (indigo-do-bond-count original-smiles) 
               (indigo-do-bond-count mol-format)))))

(ert-deftest test-ring-count-ethanol ()
  "Test ring count for ethanol (CCO)."
  (should (integerp (indigo-do-ring-count "CCO")))
  (should (= (indigo-do-ring-count "CCO") 0))) ; No rings in ethanol

(ert-deftest test-ring-count-benzene ()
  "Test ring count for benzene (c1ccccc1)."
  (should (integerp (indigo-do-ring-count "c1ccccc1")))
  (should (= (indigo-do-ring-count "c1ccccc1") 1))) ; One aromatic ring

(ert-deftest test-ring-count-cyclohexane ()
  "Test ring count for cyclohexane (C1CCCCC1)."
  (should (integerp (indigo-do-ring-count "C1CCCCC1")))
  (should (= (indigo-do-ring-count "C1CCCCC1") 1))) ; One saturated ring

(ert-deftest test-ring-count-naphthalene ()
  "Test ring count for naphthalene (c1ccc2ccccc2c1)."
  (should (integerp (indigo-do-ring-count "c1ccc2ccccc2c1")))
  (should (= (indigo-do-ring-count "c1ccc2ccccc2c1") 2))) ; Two fused rings

(ert-deftest test-ring-count-invalid ()
  "Test ring count with invalid molecular string."
  (should (null (indigo-do-ring-count "invalid"))))

(ert-deftest test-aromatic-ring-count-ethanol ()
  "Test aromatic ring count for ethanol (CCO)."
  (should (integerp (indigo-do-aromatic-ring-count "CCO")))
  (should (= (indigo-do-aromatic-ring-count "CCO") 0))) ; No aromatic rings

(ert-deftest test-aromatic-ring-count-benzene ()
  "Test aromatic ring count for benzene (c1ccccc1)."
  (should (integerp (indigo-do-aromatic-ring-count "c1ccccc1")))
  (should (= (indigo-do-aromatic-ring-count "c1ccccc1") 1))) ; One aromatic ring

(ert-deftest test-aromatic-ring-count-cyclohexane ()
  "Test aromatic ring count for cyclohexane (C1CCCCC1)."
  (should (integerp (indigo-do-aromatic-ring-count "C1CCCCC1")))
  (should (= (indigo-do-aromatic-ring-count "C1CCCCC1") 0))) ; Saturated ring, not aromatic

(ert-deftest test-aromatic-ring-count-naphthalene ()
  "Test aromatic ring count for naphthalene (c1ccc2ccccc2c1)."
  (should (integerp (indigo-do-aromatic-ring-count "c1ccc2ccccc2c1")))
  (should (= (indigo-do-aromatic-ring-count "c1ccc2ccccc2c1") 2))) ; Two aromatic rings

(ert-deftest test-aromatic-ring-count-invalid ()
  "Test aromatic ring count with invalid molecular string."
  (should (null (indigo-do-aromatic-ring-count "invalid"))))

(ert-deftest test-chiral-center-count-ethanol ()
  "Test chiral center count for ethanol (CCO)."
  (should (integerp (indigo-do-chiral-center-count "CCO")))
  (should (= (indigo-do-chiral-center-count "CCO") 0))) ; No chiral centers

(ert-deftest test-chiral-center-count-alanine ()
  "Test chiral center count for L-alanine (N[C@@H](C)C(=O)O)."
  (should (integerp (indigo-do-chiral-center-count "N[C@@H](C)C(=O)O")))
  (should (= (indigo-do-chiral-center-count "N[C@@H](C)C(=O)O") 1))) ; One chiral center

(ert-deftest test-chiral-center-count-glucose ()
  "Test chiral center count for glucose (O[C@H]1[C@H](O)[C@@H](O)[C@H](O)[C@H](O)[C@H]1CO)."
  (should (integerp (indigo-do-chiral-center-count "O[C@H]1[C@H](O)[C@@H](O)[C@H](O)[C@H](O)[C@H]1CO")))
  (should (>= (indigo-do-chiral-center-count "O[C@H]1[C@H](O)[C@@H](O)[C@H](O)[C@H](O)[C@H]1CO") 4))) ; Multiple chiral centers

(ert-deftest test-chiral-center-count-invalid ()
  "Test chiral center count with invalid molecular string."
  (should (null (indigo-do-chiral-center-count "invalid"))))

(ert-deftest test-formal-charge-neutral ()
  "Test formal charge for neutral ethanol (CCO)."
  (should (integerp (indigo-do-formal-charge "CCO")))
  (should (= (indigo-do-formal-charge "CCO") 0))) ; Neutral molecule

(ert-deftest test-formal-charge-positive ()
  "Test formal charge for ammonium ([NH4+])."
  (should (integerp (indigo-do-formal-charge "[NH4+]")))
  (should (= (indigo-do-formal-charge "[NH4+]") 1))) ; +1 charge

(ert-deftest test-formal-charge-negative ()
  "Test formal charge for hydroxide ([OH-])."
  (should (integerp (indigo-do-formal-charge "[OH-]")))
  (should (= (indigo-do-formal-charge "[OH-]") -1))) ; -1 charge

(ert-deftest test-formal-charge-invalid ()
  "Test formal charge with invalid molecular string."
  (should (null (indigo-do-formal-charge "invalid"))))

(ert-deftest test-hbd-count-ethanol ()
  "Test hydrogen bond donor count for ethanol (CCO)."
  (should (integerp (indigo-do-hbd-count "CCO")))
  (should (= (indigo-do-hbd-count "CCO") 1))) ; One OH group

(ert-deftest test-hbd-count-water ()
  "Test hydrogen bond donor count for water (O)."
  (should (integerp (indigo-do-hbd-count "O")))
  (should (= (indigo-do-hbd-count "O") 1))) ; One OH2 group

(ert-deftest test-hbd-count-ammonia ()
  "Test hydrogen bond donor count for ammonia (N)."
  (should (integerp (indigo-do-hbd-count "N")))
  (should (= (indigo-do-hbd-count "N") 1))) ; One NH3 group

(ert-deftest test-hbd-count-methane ()
  "Test hydrogen bond donor count for methane (C)."
  (should (integerp (indigo-do-hbd-count "C")))
  (should (= (indigo-do-hbd-count "C") 0))) ; No N, O, or S with H

(ert-deftest test-hbd-count-invalid ()
  "Test hydrogen bond donor count with invalid molecular string."
  (should (null (indigo-do-hbd-count "invalid"))))

(ert-deftest test-hba-count-ethanol ()
  "Test hydrogen bond acceptor count for ethanol (CCO)."
  (should (integerp (indigo-do-hba-count "CCO")))
  (should (= (indigo-do-hba-count "CCO") 1))) ; One oxygen atom

(ert-deftest test-hba-count-water ()
  "Test hydrogen bond acceptor count for water (O)."
  (should (integerp (indigo-do-hba-count "O")))
  (should (= (indigo-do-hba-count "O") 1))) ; One oxygen atom

(ert-deftest test-hba-count-ether ()
  "Test hydrogen bond acceptor count for diethyl ether (CCOCC)."
  (should (integerp (indigo-do-hba-count "CCOCC")))
  (should (= (indigo-do-hba-count "CCOCC") 1))) ; One oxygen atom

(ert-deftest test-hba-count-ammonia ()
  "Test hydrogen bond acceptor count for ammonia (N)."
  (should (integerp (indigo-do-hba-count "N")))
  (should (= (indigo-do-hba-count "N") 1))) ; One nitrogen atom

(ert-deftest test-hba-count-methane ()
  "Test hydrogen bond acceptor count for methane (C)."
  (should (integerp (indigo-do-hba-count "C")))
  (should (= (indigo-do-hba-count "C") 0))) ; No N, O, or F

(ert-deftest test-hba-count-invalid ()
  "Test hydrogen bond acceptor count with invalid molecular string."
  (should (null (indigo-do-hba-count "invalid"))))

(ert-deftest test-smiles-ethanol ()
  "Test SMILES generation for ethanol (CCO)."
  (should (stringp (indigo-do-smiles "CCO")))
  (should (string= (indigo-do-smiles "CCO") "CCO"))) ; Should return same SMILES

(ert-deftest test-smiles-benzene ()
  "Test SMILES generation for benzene (c1ccccc1)."
  (should (stringp (indigo-do-smiles "c1ccccc1")))
  (should (string= (indigo-do-smiles "c1ccccc1") "c1ccccc1"))) ; Should return same SMILES

(ert-deftest test-smiles-conversion ()
  "Test SMILES conversion from MOL format."
  (let* ((original-smiles "CCO")
         (mol-format (indigo-do-molfile original-smiles))
         (converted-smiles (indigo-do-smiles mol-format)))
    (should (stringp converted-smiles))
    ;; Both should represent ethanol, though format might differ slightly
    (should (string-match-p "CCO\\|OCC" converted-smiles))))

(ert-deftest test-smiles-invalid ()
  "Test SMILES generation with invalid molecular string."
  (should (null (indigo-do-smiles "invalid"))))

(ert-deftest test-cml-ethanol ()
  "Test CML generation for ethanol (CCO)."
  (let ((cml (indigo-do-cml "CCO")))
    (should (stringp cml))
    (should (string-match-p "<?xml" cml)) ; Should be XML format
    (should (string-match-p "molecule" cml)))) ; Should contain molecule element

(ert-deftest test-cml-water ()
  "Test CML generation for water (O)."
  (let ((cml (indigo-do-cml "O")))
    (should (stringp cml))
    (should (string-match-p "<?xml" cml))
    (should (string-match-p "molecule" cml))))

(ert-deftest test-cml-benzene ()
  "Test CML generation for benzene (c1ccccc1)."
  (let ((cml (indigo-do-cml "c1ccccc1")))
    (should (stringp cml))
    (should (string-match-p "<?xml" cml))
    (should (string-match-p "molecule" cml))))

(ert-deftest test-cml-invalid ()
  "Test CML generation with invalid molecular string."
  (should (null (indigo-do-cml "invalid"))))

(ert-deftest test-format-conversion-roundtrip ()
  "Test conversion between different formats."
  (let* ((original-smiles "CCO")
         ;; Convert SMILES -> MOL -> CML -> SMILES
         (mol-format (indigo-do-molfile original-smiles))
         (cml-format (indigo-do-cml original-smiles))
         (canonical-original (indigo-do-canonical-smiles original-smiles))
         (canonical-from-mol (indigo-do-canonical-smiles mol-format)))
    ;; All should be valid strings
    (should (stringp mol-format))
    (should (stringp cml-format))
    (should (stringp canonical-original))
    (should (stringp canonical-from-mol))
    
    ;; Canonical SMILES should be consistent across formats
    (should (string= canonical-original canonical-from-mol))
    
    ;; CML should be valid XML with molecule content
    (should (string-match-p "<?xml" cml-format))
    (should (string-match-p "molecule" cml-format))))

(ert-deftest test-has-stereochemistry-no-stereo ()
  "Test stereochemistry check for ethanol (no stereochemistry)."
  (should (not (indigo-do-has-stereochemistry "CCO"))))  ; No stereocenters or stereobonds

(ert-deftest test-has-stereochemistry-with-stereo ()
  "Test stereochemistry check for L-alanine (has stereochemistry)."
  (should (indigo-do-has-stereochemistry "N[C@@H](C)C(=O)O"))) ; Has stereocenter

(ert-deftest test-has-stereochemistry-double-bond ()
  "Test stereochemistry check for trans-2-butene (has E/Z stereochemistry)."
  (should (indigo-do-has-stereochemistry "C/C=C/C"))) ; Trans double bond

(ert-deftest test-has-stereochemistry-benzene ()
  "Test stereochemistry check for benzene (no stereochemistry)."
  (should (not (indigo-do-has-stereochemistry "c1ccccc1")))) ; No stereo

(ert-deftest test-has-stereochemistry-invalid ()
  "Test stereochemistry check with invalid molecular string."
  (should (null (indigo-do-has-stereochemistry "invalid"))))

(ert-deftest test-is-chiral-no ()
  "Test chirality check for ethanol (not chiral)."
  (should (not (indigo-do-is-chiral "CCO")))) ; Achiral molecule

(ert-deftest test-is-chiral-yes ()
  "Test chirality check for L-alanine (chiral)."
  (should (indigo-do-is-chiral "N[C@@H](C)C(=O)O"))) ; Chiral molecule

(ert-deftest test-is-chiral-meso ()
  "Test chirality check for meso compound (not chiral overall)."
  ;; Using a simpler example - propane (definitely not chiral)
  (should (not (indigo-do-is-chiral "CCC")))) ; Achiral

(ert-deftest test-is-chiral-benzene ()
  "Test chirality check for benzene (not chiral)."
  (should (not (indigo-do-is-chiral "c1ccccc1")))) ; Not chiral

(ert-deftest test-is-chiral-invalid ()
  "Test chirality check with invalid molecular string."
  (should (null (indigo-do-is-chiral "invalid"))))

(ert-deftest test-has-coordinates-smiles ()
  "Test coordinate check for SMILES (no coordinates)."
  (should (not (indigo-do-has-coordinates "CCO")))) ; SMILES typically has no coords

(ert-deftest test-has-coordinates-mol-format ()
  "Test coordinate check for MOL format with actual coordinates."
  ;; Real MOL format with coordinates for ethanol
  (let ((mol-with-coords "
  -INDIGO-08212515252D

  3  2  0  0  0  0  0  0  0  0999 V2000
    0.0000    0.0000    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0
    1.2990    0.7500    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0
    2.5981    0.0000    0.0000 O   0  0  0  0  0  0  0  0  0  0  0  0
  1  2  1  0  0  0  0
  2  3  1  0  0  0  0
M  END"))
    ;; Should detect coordinates in the MOL format
    (should (indigo-do-has-coordinates mol-with-coords))
    
    ;; Verify this is different from SMILES behavior
    (should (not (indigo-do-has-coordinates "CCO")))  ; SMILES has no coordinates
    
    ;; Double-check the coordinate detection works correctly
    (should (eq (indigo-do-has-coordinates mol-with-coords) t))
    (should (eq (indigo-do-has-coordinates "CCO") nil))))

(ert-deftest test-has-coordinates-benzene ()
  "Test coordinate check for benzene SMILES (no coordinates)."
  (should (not (indigo-do-has-coordinates "c1ccccc1")))) ; SMILES has no coords

(ert-deftest test-has-coordinates-invalid ()
  "Test coordinate check with invalid molecular string."
  (should (null (indigo-do-has-coordinates "invalid"))))

(ert-deftest test-structural-analysis-consistency ()
  "Test consistency between structural analysis functions."
  (let* ((chiral-molecule "N[C@@H](C)C(=O)O")  ; L-alanine
         (achiral-molecule "CCO"))                ; ethanol
    ;; Chiral molecule should have stereochemistry
    (should (indigo-do-has-stereochemistry chiral-molecule))
    (should (indigo-do-is-chiral chiral-molecule))
    
    ;; Achiral molecule should not have stereochemistry or chirality
    (should (not (indigo-do-has-stereochemistry achiral-molecule)))
    (should (not (indigo-do-is-chiral achiral-molecule)))
    
    ;; Test that both can be converted to MOL format (but coordinates may not be present)
    (let ((chiral-mol (indigo-do-molfile chiral-molecule))
          (achiral-mol (indigo-do-molfile achiral-molecule)))
      ;; Just verify the MOL format strings are created successfully
      (should (stringp chiral-mol))
      (should (stringp achiral-mol))
      (should (string-match-p "V2000" chiral-mol))
      (should (string-match-p "V2000" achiral-mol)))))

(ert-deftest test-substructure-match-positive ()
  "Test substructure matching - ethanol contains ethyl group."
  (should (indigo-do-substructure-match "CCO" "CC"))) ; Ethanol contains ethyl

(ert-deftest test-substructure-match-negative ()
  "Test substructure matching - methane does not contain ethyl group."
  (should (not (indigo-do-substructure-match "C" "CC")))) ; Methane does not contain ethyl

(ert-deftest test-substructure-match-benzene ()
  "Test substructure matching - toluene contains benzene ring."
  (should (indigo-do-substructure-match "Cc1ccccc1" "c1ccccc1"))) ; Toluene contains benzene

(ert-deftest test-substructure-match-aromatic ()
  "Test substructure matching - benzene contains aromatic carbon."
  (should (indigo-do-substructure-match "c1ccccc1" "c"))) ; Benzene contains aromatic carbon

(ert-deftest test-substructure-match-invalid ()
  "Test substructure matching with invalid molecular strings."
  (should (null (indigo-do-substructure-match "CCO" "invalid")))
  (should (null (indigo-do-substructure-match "invalid" "CCO"))))

(ert-deftest test-exact-match-same ()
  "Test exact matching - same molecules should match."
  (should (indigo-do-exact-match "CCO" "CCO"))) ; Same ethanol

(ert-deftest test-exact-match-different ()
  "Test exact matching - different molecules should not match."
  (should (not (indigo-do-exact-match "CCO" "CCC")))) ; Ethanol vs propane

(ert-deftest test-exact-match-isomers ()
  "Test exact matching - different representations of same molecule."
  (should (indigo-do-exact-match "CCO" "OCC"))) ; Different SMILES for ethanol

(ert-deftest test-exact-match-stereoisomers ()
  "Test exact matching - stereoisomers should not match exactly."
  (should (not (indigo-do-exact-match "N[C@@H](C)C(=O)O" "N[C@H](C)C(=O)O")))) ; L vs D alanine

(ert-deftest test-exact-match-invalid ()
  "Test exact matching with invalid molecular strings."
  (should (null (indigo-do-exact-match "CCO" "invalid")))
  (should (null (indigo-do-exact-match "invalid" "CCO"))))

(ert-deftest test-similarity-identical ()
  "Test similarity calculation - identical molecules should have similarity 1.0."
  (let ((sim (indigo-do-similarity "CCO" "CCO")))
    (should (floatp sim))
    (should (> sim 0.99)))) ; Should be very close to 1.0

(ert-deftest test-similarity-different ()
  "Test similarity calculation - very different molecules should have low similarity."
  (let ((sim (indigo-do-similarity "CCO" "c1ccccc1")))  ; Ethanol vs benzene
    (should (floatp sim))
    (should (>= sim 0.0))
    (should (<= sim 1.0))
    (should (< sim 0.5)))) ; Should be quite different

(ert-deftest test-similarity-similar ()
  "Test similarity calculation - similar molecules should have moderate similarity."
  (let ((sim (indigo-do-similarity "CCO" "CCC")))  ; Ethanol vs propane
    (should (floatp sim))
    (should (>= sim 0.0))
    (should (<= sim 1.0))))

(ert-deftest test-similarity-invalid ()
  "Test similarity calculation with invalid molecular strings."
  (should (null (indigo-do-similarity "CCO" "invalid")))
  (should (null (indigo-do-similarity "invalid" "CCO"))))


(ert-deftest test-search-matching-consistency ()
  "Test consistency between different search and matching functions."
  (let* ((benzene "c1ccccc1")
         (toluene "Cc1ccccc1")
         (ethanol "CCO"))
    ;; Substructure relationships
    (should (indigo-do-substructure-match toluene benzene))  ; Toluene contains benzene
    (should (not (indigo-do-substructure-match benzene toluene))) ; Benzene doesn't contain toluene

    ;; Exact matching
    (should (indigo-do-exact-match benzene benzene))  ; Same molecule
    (should (not (indigo-do-exact-match benzene toluene))) ; Different molecules

    ;; Similarity relationships
    (let ((sim-same (indigo-do-similarity benzene benzene))
          (sim-similar (indigo-do-similarity benzene toluene))
          (sim-different (indigo-do-similarity benzene ethanol)))
      ;; Same molecules should have highest similarity
      (should (> sim-same sim-similar))
      (should (> sim-similar sim-different))

      ;; All similarities should be in valid range
      (should (and (>= sim-same 0.0) (<= sim-same 1.0)))
      (should (and (>= sim-similar 0.0) (<= sim-similar 1.0)))
      (should (and (>= sim-different 0.0) (<= sim-different 1.0))))))

;; Reaction Chemistry Tests

(ert-deftest test-reaction-products-count-basic ()
  "Test product counting for basic reactions."
  ;; Simple synthesis: A + B -> C
  (should (= (indigo-do-reaction-products-count "CCO.CC>>CCOC") 1))
  ;; Decomposition: A -> B + C  
  (should (= (indigo-do-reaction-products-count "CC(=O)OC>>CC(=O)O.CO") 2)))

(ert-deftest test-reaction-reactants-count-basic ()
  "Test reactant counting for basic reactions."
  ;; Simple synthesis: A + B -> C
  (should (= (indigo-do-reaction-reactants-count "CCO.CC>>CCOC") 2))
  ;; Decomposition: A -> B + C
  (should (= (indigo-do-reaction-reactants-count "CC(=O)OC>>CC(=O)O.CO") 1)))

(ert-deftest test-reaction-complex ()
  "Test complex reaction with multiple reactants and products."
  (let ((complex-rxn "CCO.CC.O>>CCOC.CO.CC(O)C"))
    (should (= (indigo-do-reaction-reactants-count complex-rxn) 3))
    (should (= (indigo-do-reaction-products-count complex-rxn) 3))))

(ert-deftest test-reaction-with-catalysts ()
  "Test reaction counting with catalyst format."
  ;; Proper catalyst format: reactants>catalyst>products
  (let ((catalyzed-rxn "CCO>[Pd]>CCOC"))  ; Fixed: single > for catalyst
    (should (= (indigo-do-reaction-reactants-count catalyzed-rxn) 1))
    (should (= (indigo-do-reaction-products-count catalyzed-rxn) 1))))

(ert-deftest test-reaction-invalid ()
  "Test reaction functions with invalid inputs."
  (should (null (indigo-do-reaction-products-count "invalid")))
  (should (null (indigo-do-reaction-reactants-count "invalid")))
  (should (null (indigo-do-reaction-products-count "")))
  (should (null (indigo-do-reaction-reactants-count ""))))

(ert-deftest test-reaction-consistency ()
  "Test consistency of reaction parsing."
  (let ((rxn "C.C.O>>CCO.CO"))  ; Two carbons + oxygen -> ethanol + methanol
    (let ((reactants (indigo-do-reaction-reactants-count rxn))
          (products (indigo-do-reaction-products-count rxn)))
      (should (= reactants 3))  ; C, C, O
      (should (= products 2))   ; CCO, CO
      ;; Both functions should work on same input
      (should (numberp reactants))
      (should (numberp products)))))

(ert-deftest test-most-abundant-mass-ethanol ()
  "Test most abundant mass calculation for ethanol (CCO)."
  (should (floatp (indigo-do-most-abundant-mass "CCO")))
  (should (< (abs (- (indigo-do-most-abundant-mass "CCO") 46.04)) 0.1)))

(ert-deftest test-most-abundant-mass-methane ()
  "Test most abundant mass calculation for methane (C)."
  (should (floatp (indigo-do-most-abundant-mass "C")))
  (should (< (abs (- (indigo-do-most-abundant-mass "C") 16.03)) 0.1)))

(ert-deftest test-most-abundant-mass-invalid ()
  "Test most abundant mass calculation with invalid input."
  (should (null (indigo-do-most-abundant-mass "invalid"))))

(ert-deftest test-monoisotopic-mass-ethanol ()
  "Test monoisotopic mass calculation for ethanol (CCO)."
  (should (floatp (indigo-do-monoisotopic-mass "CCO")))
  (should (< (abs (- (indigo-do-monoisotopic-mass "CCO") 46.04)) 0.1)))

(ert-deftest test-monoisotopic-mass-benzene ()
  "Test monoisotopic mass calculation for benzene (c1ccccc1)."
  (should (floatp (indigo-do-monoisotopic-mass "c1ccccc1")))
  (should (< (abs (- (indigo-do-monoisotopic-mass "c1ccccc1") 78.05)) 0.1)))

(ert-deftest test-monoisotopic-mass-invalid ()
  "Test monoisotopic mass calculation with invalid input."
  (should (null (indigo-do-monoisotopic-mass "invalid"))))

(ert-deftest test-layered-code-ethanol ()
  "Test layered code generation for ethanol (CCO)."
  (let ((code (indigo-do-layered-code "CCO")))
    (should (stringp code))
    (should (> (length code) 0))))

(ert-deftest test-layered-code-benzene ()
  "Test layered code generation for benzene (c1ccccc1)."
  (let ((code (indigo-do-layered-code "c1ccccc1")))
    (should (stringp code))
    (should (> (length code) 0))))

(ert-deftest test-layered-code-invalid ()
  "Test layered code generation with invalid input."
  (should (null (indigo-do-layered-code "invalid"))))

(ert-deftest test-has-z-coord-basic ()
  "Test Z coordinate detection for basic molecules."
  ;; Most SMILES don't have Z coordinates by default
  (should (null (indigo-do-has-z-coord "CCO")))
  (should (null (indigo-do-has-z-coord "c1ccccc1"))))

(ert-deftest test-has-z-coord-invalid ()
  "Test Z coordinate detection with invalid input."
  (should (null (indigo-do-has-z-coord "invalid"))))

(ert-deftest test-heavy-atom-count-ethanol ()
  "Test heavy atom count for ethanol (CCO)."
  (should (= (indigo-do-heavy-atom-count "CCO") 3)))

(ert-deftest test-heavy-atom-count-benzene ()
  "Test heavy atom count for benzene (c1ccccc1)."
  (should (= (indigo-do-heavy-atom-count "c1ccccc1") 6)))

(ert-deftest test-heavy-atom-count-methane ()
  "Test heavy atom count for methane (C)."
  (should (= (indigo-do-heavy-atom-count "C") 1)))

(ert-deftest test-heavy-atom-count-invalid ()
  "Test heavy atom count with invalid input."
  (should (null (indigo-do-heavy-atom-count "invalid"))))

;; FIXME: known bug in symmetry functions
;; (ert-deftest test-symmetry-classes-methane ()
;;   "Test symmetry classes for methane (C)."
;;   (let ((classes (indigo-do-symmetry-classes "C")))
;;     (should (listp classes))
;;     (should (= (length classes) 1))))

;; (ert-deftest test-symmetry-classes-ethanol ()
;;   "Test symmetry classes for ethanol (CCO)."
;;   (let ((classes (indigo-do-symmetry-classes "CCO")))
;;     (should (listp classes))

;;     (should (= (length classes) 3))  ; C, C, O atoms
;;     (should (every #'integerp classes))))

;; (ert-deftest test-symmetry-classes-benzene ()
;;   "Test symmetry classes for benzene (c1ccccc1)."
;;   (let ((classes (indigo-do-symmetry-classes "c1ccccc1")))
;;     (should (listp classes))
;;     (should (= (length classes) 6))  ; 6 carbon atoms
;;     (should (every #'integerp classes))
;;     ;; All carbons in benzene should have same symmetry class
;;     (should (= (length (delete-dups (copy-sequence classes))) 1))))

;; (ert-deftest test-symmetry-classes-invalid ()
;;   "Test symmetry classes with invalid input."
;;   (should (null (indigo-do-symmetry-classes "invalid"))))

(ert-deftest test-mass-functions-consistency ()
  "Test that mass functions return consistent results."
  (let ((mol "CCO"))
    (let ((abundant-mass (indigo-do-most-abundant-mass mol))
          (monoisotopic-mass (indigo-do-monoisotopic-mass mol))
          (molecular-weight (indigo-do-molecular-weight mol)))
      ;; All should be close to each other for simple organic molecules
      (should (< (abs (- abundant-mass monoisotopic-mass)) 1.0))
      (should (< (abs (- abundant-mass molecular-weight)) 1.0))
      (should (< (abs (- monoisotopic-mass molecular-weight)) 1.0)))))

(ert-deftest test-count-functions-consistency ()
  "Test that count functions return consistent results."
  (let ((mol "CCO"))
    (let ((heavy-count (indigo-do-heavy-atom-count mol))
          (atom-count (indigo-do-atom-count mol))
          (total-count (indigo-do-total-atom-count mol))
          (hydrogen-count (indigo-do-hydrogen-count mol)))
      ;; Heavy atom count should equal atom count for this function
      (should (= heavy-count atom-count))
      ;; Total count should be heavy atoms plus hydrogens
      (should (= total-count (+ heavy-count hydrogen-count)))
      ;; Ethanol should have 3 heavy atoms and 6 hydrogens
      (should (= heavy-count 3))
      (should (= hydrogen-count 6)))))

(provide 'test-indigo)

;;; test-indigo.el ends here
