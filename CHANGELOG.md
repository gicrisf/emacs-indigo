# CHANGELOG

## Version 0.10.1 (2025-11-19)

### Summary

Minor refactoring release that improves code organization by consolidating all `define-indigo-with*` macro definitions into a dedicated manifest file.

### Changes

- **New `indigo-with-star-defs.el` module**: Centralizes the `define-indigo-with*` meta-macro and all star macro definitions in one place, serving as a manifest for all sequential binding macros
- **Cleaner module structure**: Removed scattered `define-indigo-with*` calls from individual modules (`indigo-mol.el`, `indigo-react.el`, `indigo-stream.el`, `indigo-stream-iter.el`, `indigo-render.el`, `indigo-iter.el`)
- **Better dependency ordering**: Star macros are now generated after all base macros are loaded

---

## Version 0.10.0 (2025-11-17)

### Summary

Version 0.10.0 completes the functional programming infrastructure with a full stream combinator library, enabling powerful lazy composition of molecular data transformations.

### New Features

#### Complete Stream Functional API

A comprehensive set of stream combinators providing both lazy transformations and consuming operations:

**Lazy combinators (return new streams):**
- **`indigo-stream-map`** - Transform elements (already in v0.9.0)
- **`indigo-stream-filter`** - Select elements by predicate
- **`indigo-stream-take`** - Limit stream length

**Consuming operations (force and reduce streams):**
- **`indigo-stream-collect`** - Gather all elements into a list (already in v0.9.0)
- **`indigo-stream-fold`** - Reduce to single accumulated value (left-associative)

**Example (filtering and folding):**
```elisp
(indigo-with-molecule (mol "CCCCCO")
  (indigo-with-atoms-stream (stream mol)
    (let* ((carbons (indigo-stream-filter
                     (lambda (atom) (equal (indigo-symbol atom) "C"))
                     stream))
           (indices (indigo-stream-map #'indigo-index carbons)))
      (indigo-stream-fold #'+ 0 indices))))  ; => 10 (sum of indices)
```

All combinators maintain full laziness and memoization, enabling efficient composition without unnecessary computation.

#### Macro Generator

- **`define-indigo-with*` macro generator**: Automatically generates sequential binding (`*` suffix) versions of any `indigo-with-*` macro, eliminating manual duplication
- **Extended star macro coverage**: All resource types now have `*` versions - iterators (10 macros), reactions, and arrays

### Test Suite

- **460+ tests**
- **new stream combinator tests** covering:
  - `indigo-stream-take`: Basic usage, edge cases, laziness verification, composition
  - `indigo-stream-filter`: Filtering logic, chaining, laziness, memoization
  - `indigo-stream-fold`: Arithmetic, string building, predicates, complex accumulators
- Tests for `define-indigo-with*` macro generator to verify sequential binding and cleanup behavior

---

## Version 0.9.0 (2025-11-15)

### Summary

Version 0.9.0 is a major release that introduces functional programming capabilities, new bindings and improved resource management. Key highlights include a fully lazy stream abstraction with memoization, intuitive `indigo-with-*` macros for resource management, stream-iterator bridge macros for ergonomic one-liner syntax, and bond property functions for complete molecular graph extraction.

This release includes breaking changes: removal of `indigo-let`/`indigo-let*` macros and a keyword-based API for similarity calculations.

### Breaking Changes

#### Removal of `indigo-let` and `indigo-let*` Macros

The `indigo-let` and `indigo-let*` macros have been removed in favor of the safer, more idiomatic `indigo-with-*` macro family. This change eliminates a fundamental memory leak issue with parallel binding (exclusive to `indigo-let`).

```elisp
;; OLD CODE (REMOVED) - Had memory leak issue
(indigo-let ((:molecule mol1 "CCO")              ; Created successfully
             (:molecule mol2 "INVALID_SMILES"))  ; Error here!
  ...)
;; Result: mol1 LEAKS because unwind-protect hasn't been entered yet!
```

The sequential nesting approach ensures each resource is immediately protected:

```elisp
;; NEW CODE - Safe from leaks
(indigo-with-molecule* ((mol1 "CCO")              ; Created and protected
                        (mol2 "INVALID_SMILES"))  ; Error here, but mol1 is safe
  ...)

;; Expands to nested unwind-protect blocks:
(indigo-with-molecule (mol1 "CCO")           ; mol1 protected
  (indigo-with-molecule (mol2 "INVALID")     ; Error here, mol1's unwind-protect triggers
    ...))
```

#### `indigo-similarity` API Change

The `indigo-similarity` function now uses keywords instead of strings for metric specification:

**Old API (string-based):**
```elisp
(indigo-similarity fp1 fp2 "tanimoto")
(indigo-similarity fp1 fp2 "tversky")
```

**New API (keyword-based):**
```elisp
(indigo-similarity fp1 fp2 :tanimoto)
(indigo-similarity fp1 fp2 :tversky)
(indigo-similarity fp1 fp2 :tversky 0.7 0.3)  ; With custom parameters
```

### New Features

#### Lazy Stream Abstraction

A complete lazy stream implementation built on top of the iterator API, enabling functional composition and efficient traversal of molecular structures.

**Core API:**
- **`indigo-stream`** - Create a lazy stream from an iterator (returns memoized thunk)
- **`indigo-stream-force`** - Force a stream thunk, returning `(value . next-thunk)` or nil
- **`indigo-stream-first`** - Get first element from stream
- **`indigo-stream-rest`** - Get rest of stream
- **`indigo-stream-empty-p`** - Check if stream is exhausted
- **`indigo-stream-map`** - Map function over stream (fully lazy with memoization)
- **`indigo-stream-collect`** - Collect all stream elements into a list
- **`indigo-with-stream-from-iterator`** - Macro for automatic cleanup of forced elements

**Design Principles:**
- Fully lazy: No computation until explicitly forced (both head and tail are lazy)
- Memoization: Each stream node is forced only once, even if accessed multiple times
- Clear ownership: Stream does NOT own the iterator (caller is responsible for freeing it. For simplified cleanup, see `stream-iterator` bridge
  macros below).
- Composable: Combinators like `map` preserve laziness and can be chained

**Example:**
```elisp
(indigo-with-molecule (mol "c1ccccc1")  ; Benzene
  (indigo-with-atoms-stream (stream mol)
    (let* ((symbols (indigo-stream-map #'indigo-symbol stream))
           (all-symbols (indigo-stream-collect symbols)))
      all-symbols)))  ; => ("C" "C" "C" "C" "C" "C")
```

#### Stream-Iterator Bridge Macros

A new module `indigo-stream-iter.el` provides 10 high-level macros that combine iterator creation with stream abstraction for convenient one-liner syntax:

**Molecule-based (7 macros):**
- `indigo-with-atoms-stream`, `indigo-with-bonds-stream`
- `indigo-with-components-stream`, `indigo-with-sssr-stream`
- `indigo-with-rings-stream`, `indigo-with-subtrees-stream`
- `indigo-with-stereocenters-stream`

**Atom-based (1 macro):**
- `indigo-with-neighbors-stream`

**Reaction-based (2 macros):**
- `indigo-with-reactants-stream`, `indigo-with-products-stream`

**Comparison:**

```elisp
;; Explicit iterator + stream (still supported)
(indigo-with-molecule (mol "c1ccccc1")
  (indigo-with-atoms-iterator (atoms mol)
    (indigo-with-stream-from-iterator (stream atoms)
      (let ((symbols (indigo-stream-map #'indigo-symbol stream)))
        ...))))

;; One-liner syntax (ergonomic)
(indigo-with-molecule (mol "c1ccccc1")
  (indigo-with-atoms-stream (stream mol)
    (let ((symbols (indigo-stream-map #'indigo-symbol stream)))
      ...)))
```

**Benefits:**
- Reduces nesting depth by combining iterator and stream creation
- Clear intent - using streams signals functional processing
- Automatic cleanup of both iterators and forced stream elements
- Fully compatible with existing stream combinators

#### Resource Management Macros

Comprehensive `indigo-with-*` macros for all resource types, providing clearer intent and automatic cleanup:

**Single resource macros:**
- `indigo-with-molecule`, `indigo-with-mol-file`
- `indigo-with-query`, `indigo-with-query-file`
- `indigo-with-smarts`, `indigo-with-smarts-file`
- `indigo-with-fingerprint`
- `indigo-with-matcher`
- `indigo-with-reaction`, `indigo-with-reaction-file`

**Multiple resource macros (sequential nesting with `*`):**
- `indigo-with-molecule*`, `indigo-with-mol-file*`
- `indigo-with-query*`, `indigo-with-query-file*`
- `indigo-with-smarts*`, `indigo-with-smarts-file*`
- `indigo-with-fingerprint*`
- `indigo-with-matcher*`

**Iterator macros:**
- `indigo-with-atoms-iterator`, `indigo-with-bonds-iterator`
- `indigo-with-neighbors-iterator`, `indigo-with-components-iterator`
- `indigo-with-sssr-iterator`, `indigo-with-rings-iterator`
- `indigo-with-subtrees-iterator`, `indigo-with-stereocenters-iterator`
- `indigo-with-reactants-iterator`, `indigo-with-products-iterator`
- And more (25+ iterator types total)

**Benefits:**
1. **Memory safety** - No leaks even when construction fails
2. **Explicit cleanup** - Each resource has its own `unwind-protect`
3. **Better error messages** - Failures point to specific resource
4. **More idiomatic** - Follows standard Emacs macro patterns
5. **Type-specific macros** - Clear intent (molecule vs query vs reaction)

#### Bond Property Functions

A complete bond property API with keyword-based return values for idiomatic Elisp:

**Core functions:**
- **`indigo-source`** - Get source atom handle from a bond (use `indigo-index` to get atom index)
- **`indigo-destination`** - Get destination atom handle from a bond (use `indigo-index` to get atom index)
- **`indigo-bond-order`** - Get bond order as keyword: `:single`, `:double`, `:triple`, `:aromatic`, or `:query`
- **`indigo-bond-stereo`** - Get stereochemistry as keyword: `:none`, `:up`, `:down`, `:either`, `:cis`, or `:trans`

**Convenience predicates:**
- **`indigo-bond-single-p`**, **`indigo-bond-double-p`**, **`indigo-bond-triple-p`**, **`indigo-bond-aromatic-p`**
- **`indigo-bond-has-stereo-p`** - Check if bond has stereochemistry

**Example:**
```elisp
(indigo-with-molecule (mol "C=C")
  (indigo-with-bonds-iterator (bonds-iter mol)
    (let ((bond (indigo-next bonds-iter)))
      (indigo-bond-order bond))))
;; => :double
```

**Important:** `indigo-source` and `indigo-destination` return atom **handles**, not indices. Call `indigo-index` on the returned handles to get atom indices when needed.

**Related atom property functions:**
- **`indigo-index`** - Get atom index in molecule
- **`indigo-xyz`** - Get XYZ coordinates as `(x y z)` list
- **`indigo-charge`** - Get formal charge

### Test Suite Enhancements

- **400+ tests** (up from 150+ in v0.8.0)
- Comprehensive stream testing with tests covering:
  - Stream creation, iteration, and empty detection
  - Map operations with laziness verification
  - Map chaining with multiple transformations
  - Stream collection operations
  - Complex molecule iteration patterns
- Stream-iterator bridge macro tests (15 tests) covering all 10 macro variants
- Nested scope cleanup tests verifying:
  - Cleanup at each nesting level (1-5 levels deep)
  - Error handling for construction failures and body errors
  - Mixed resource type tests (molecules + iterators + fingerprints + reactions)
  - Reference counting verification at every scope level
- Added `test-readme-examples.el` to ensure all documentation examples work correctly
- All test files refactored to use explicit `indigo-with-*` macros for clarity

### Contributors

- Giovanni Crisalfi

---

## Previous Versions

See git history for changes in versions 0.8.0 and earlier.
