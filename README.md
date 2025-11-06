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

**Version 0.8.0**

**Most features are implemented and tested** (150+ tests), with core operations complete. Particularly, we have features complete molecular operations, a full iterator system for structure traversal, rendering and visualization support, and automatic resource management. 

Areas still in development include advanced analysis features (scaffold detection, R-group deconvolution), SDF/RDF file iteration, and additional array/writer operations. I'm also working on lazy streams.

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
(indigo-do-substructure-match "CCO" "CO") ; => t (ethanol contains OH)
(indigo-do-exact-match "CCO" "OCC")       ; => t (same molecule)

;; Reaction processing
(indigo-do-reaction-reactants-count "CCO.CC>>CCOC") ; => 2
```

All `indigo-do-*` functions handle resource management automatically.

### Advanced Examples

The `indigo-let*` macro provides automatic resource management for all Indigo objects and the `indigo-map` lets you easily handle an iterator:

```elisp
;; Analyze a molecule's structure
(indigo-let* ((:molecule mol "c1ccccc1")  ; Benzene
              (:atoms atoms mol))
  ;; Get all atom symbols
  (indigo-map #'indigo-symbol atoms))
;; => ("C" "C" "C" "C" "C" "C")
```

Supported binding types:
- Molecules: `:molecule`, `:mol-file`, `:query`, `:query-file`, `:smarts`, `:smarts-file`
- Reactions: `:reaction`, `:rxn-file`
- Iterators: `:atoms`, `:bonds`, `:neighbors`, `:components`, `:sssr`, `:rings`, `:subtrees`, `:stereocenters`, `:reactants`, `:products`
- Fingerprints: `:fingerprint`
- Matchers: `:matcher`
- Arrays: `:array`
- Writers: `:file-writer`, `:buffer-writer`

### Reactions

Process reaction components:

```elisp
(indigo-let* ((:reaction rxn "CCO.CC(=O)O>>CCOC(=O)C")  ; Esterification
              (:reactants reactants rxn)
              (:products products rxn))
  (list :reactant-count (length (indigo-map #'indigo-canonical-smiles reactants))
        :product-count (length (indigo-map #'indigo-canonical-smiles products))))
;; => (:reactant-count 2 :product-count 1)
```

### Rendering

Render molecule to SVG:

```elisp
(indigo-let* ((:molecule mol "c1ccccc1"))
  (indigo-set-option "render-output-format" "svg")
  (indigo-set-option-int "render-image-width" 300)
  (indigo-set-option-int "render-image-height" 300)
  (indigo-render-to-file mol "benzene.svg"))
```

## License

This project is licensed under the GNU General Public License v3.0 or later. See the LICENSE file for details.
