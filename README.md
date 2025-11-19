# Emacs Indigo

Emacs Lisp bindings for the [Indigo](https://lifescience.opensource.epam.com/indigo/) cheminformatics library.

## Key Features

**Molecular Operations**:
- Format conversions (SMILES, MOL, CML)
- Property calculations (molecular weight, formula, mass)
- Structure analysis (rings, stereocenters, bonds, atoms)
- Substructure matching and similarity searching
- Reaction processing and manipulation

**Advanced**:
- Iterator system for traversing molecular structures
- Fingerprint generation and similarity metrics
- Chemical rendering to SVG and PNG formats
- Atom-to-atom mapping for reactions
- Automatic resource management

## Implementation Status

**Most features are implemented and tested** (400+ tests), with core operations complete. Particularly, we have complete molecular operations, a full iterator system for structure traversal, lazy stream abstraction with functional combinators, rendering and visualization support, and automatic resource management with intuitive `indigo-with-*` macros.

Areas still in development include advanced analysis features (scaffold detection, R-group deconvolution), SDF/RDF file iteration, and additional array/writer operations.

## Build

Requirements:
- Emacs 25.1+ with dynamic module support
- GCC
- Make

```bash
git clone https://github.com/gicrisf/emacs-indigo.git && emacs-indigo/install.sh
```

The install script automatically:
1. Builds and installs dependencies (zlib, TinyXML)
2. Downloads and installs the Indigo library
3. Compiles the Emacs dynamic module

Tested on Linux and WSL, should work on any Unix-like system.

## Install

Add to your config:
```elisp
(add-to-list 'load-path "/path/to/emacs-indigo")
(require 'indigo)
```

## Quick Start

### Basic Examples

Convenience functions for quick calculations:

```elisp
;; Get molecular properties from SMILES
(indigo-do-molecular-weight "CCO")        ; => 46.069
(indigo-do-molecular-formula "CCO")       ; => "C2 H6 O"
(indigo-do-canonical-smiles "CCO")        ; => "CCO"

;; Structure analysis
(indigo-do-atom-count "c1ccccc1")         ; => 6 (benzene)
(indigo-do-ring-count "c1ccccc1")         ; => 1

;; Substructure matching
(indigo-do-substructure-match "CCO" "[OH]") ; => t (ethanol contains OH)
(indigo-do-exact-match "CCO" "OCC")       ; => t (same molecule)

;; Reaction processing
(indigo-do-reaction-reactants-count "CCO.CC>>CCOC") ; => 2
```

All `indigo-do-*` functions handle resource management automatically.

### Advanced Examples

The `indigo-with-*` macros provide automatic resource management for all Indigo objects. The `indigo-map` function lets you easily work with iterators:

```elisp
;; Analyze a molecule's structure
(indigo-with-molecule (mol "c1ccccc1")  ; Benzene
  (indigo-with-atoms-iterator (atoms mol)
    ;; Get all atom symbols
    (indigo-map #'indigo-symbol atoms)))
;; => ("C" "C" "C" "C" "C" "C")

;; Compare molecular weights of multiple molecules
(indigo-with-molecule* ((ethanol "CCO")
                        (benzene "c1ccccc1")
                        (propane "CCC"))
  (list (indigo-molecular-weight ethanol)
        (indigo-molecular-weight benzene)
        (indigo-molecular-weight propane)))
;; => (46.069 78.114 44.097)
```

Available `indigo-with-*` macros:
- Molecules: `indigo-with-molecule`, `indigo-with-mol-file`, `indigo-with-query`, `indigo-with-query-file`, `indigo-with-smarts`, `indigo-with-smarts-file`
- Reactions: `indigo-with-reaction`, `indigo-with-rxn-file`
- Iterators: `indigo-with-atoms-iterator`, `indigo-with-bonds-iterator`, `indigo-with-neighbors-iterator`, `indigo-with-components-iterator`, `indigo-with-sssr-iterator`, `indigo-with-rings-iterator`, `indigo-with-subtrees-iterator`, `indigo-with-stereocenters-iterator`, `indigo-with-reactants-iterator`, `indigo-with-products-iterator`
- Fingerprints: `indigo-with-fingerprint`
- Matchers: `indigo-with-matcher`
- Arrays: `indigo-with-array`

**Sequential multiple binding versions**: Each macro has a `*` suffix version (e.g., `indigo-with-molecule*`) that works like `let*`, allowing multiple resources to be bound sequentially with proper cleanup even if later bindings fail:

```elisp
;; Compare two molecules
(indigo-with-molecule* ((mol1 "CCO")       ; Ethanol
                        (mol2 "CC(O)C"))   ; Isopropanol
  (indigo-with-fingerprint* ((fp1 mol1 "sim")
                             (fp2 mol2 "sim"))
    (indigo-similarity fp1 fp2 :tanimoto)))
;; => 0.714 (similarity coefficient)
```

### Reactions

Process reaction components:

```elisp
(indigo-with-reaction (rxn "CCO.CC(=O)O>>CCOC(=O)C")  ; Esterification
  (indigo-with-reactants-iterator (reactants rxn)
    (indigo-with-products-iterator (products rxn)
      (list :reactant-count (length (indigo-map #'indigo-canonical-smiles reactants))
            :product-count (length (indigo-map #'indigo-canonical-smiles products))))))
;; => (:reactant-count 2 :product-count 1)
```

### Rendering

Render molecule to SVG:

```elisp
(indigo-with-molecule (mol "c1ccccc1")
  (indigo-set-option "render-output-format" "svg")
  (indigo-set-option-int "render-image-width" 300)
  (indigo-set-option-int "render-image-height" 300)
  (indigo-render-to-file mol "benzene.svg"))
```

## License

This project is licensed under the GNU General Public License v3.0 or later. See the LICENSE file for details.
