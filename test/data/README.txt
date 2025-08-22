Test Data Sources
=================

The chemical data files in this directory are sourced from the official Indigo project:

Repository: https://github.com/epam/Indigo/
License: Apache License 2.0
Source Path: /api/tests/integration/tests/formats/

Description:
This test data collection contains various chemical structure files in multiple formats
(MOL, SDF, SMILES, KET, CML, RXN, etc.) used for testing chemical informatics software.
The files include:

- ChEBI molecules (molecules/chebi/): Chemical entities from ChEBI database
- Basic molecules (molecules/basic/): Common chemical structures and test cases
- Stereochemistry examples (molecules/stereo/): Molecules with stereochemical features
- S-groups examples (molecules/sgroups/): Molecules with structural groups
- Substructure search queries (molecules/sss/): Query molecules for pattern matching
- Reaction data (reactions/): Chemical reaction files
- Other specialized test cases

These files are used to ensure our Emacs Indigo bindings work correctly with
real-world chemical data and cover edge cases found in actual chemical databases.

Attribution:
Files are from the Indigo toolkit test suite, developed by EPAM Systems.
Used under Apache License 2.0 for testing purposes.