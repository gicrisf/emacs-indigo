/*
 * indigo-module.c - Core infrastructure and string utilities for Indigo module
 *
 * This module provides the core infrastructure for the Indigo Emacs module,
 * including string operation utilities and module initialization. The actual
 * operation implementations and Emacs wrapper functions are in separate files.
 *
 * Copyright (C) 2025 Giovanni Crisalfi
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "indigo-module.h"

/* Declare the module is GPL compatible */
int plugin_is_GPL_compatible;

/* Core utility function for registering Emacs functions */
void mkfn(emacs_env *env,
          ptrdiff_t nargs_min,
          ptrdiff_t nargs_max,
          emacs_value (*func)(emacs_env *env, ptrdiff_t nargs, emacs_value* args, void *data),
          const char *name,
          const char *docstring,
          void *data)
{
    /* Make function */
    emacs_value Sfun = env->make_function(env, nargs_min, nargs_max, func, docstring, data);
    /* Bind function */
    emacs_value Qfset = env->intern(env, "fset");
    emacs_value Qsym = env->intern(env, name);
    emacs_value args[] = { Qsym, Sfun };
    env->funcall(env, Qfset, 2, args);
}


/* Module initialization */
int emacs_module_init(struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);

    mkfn(env, 1, 1, Fdo_molecular_formula, "indigo-do-molecular-formula",
         "Get molecular formula from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_molecular_weight, "indigo-do-molecular-weight",
         "Get molecular weight from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_canonical_smiles, "indigo-do-canonical-smiles",
         "Get canonical SMILES from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_atom_count, "indigo-do-atom-count",
         "Get heavy atom count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_bond_count, "indigo-do-bond-count",
         "Get explicit bond count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_molfile, "indigo-do-molfile",
         "Get MOL file format from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_hydrogen_count, "indigo-do-hydrogen-count",
         "Get hydrogen count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_total_atom_count, "indigo-do-total-atom-count",
         "Get total atom count (heavy atoms + hydrogens) from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_ring_count, "indigo-do-ring-count",
         "Get ring count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_aromatic_ring_count, "indigo-do-aromatic-ring-count",
         "Get aromatic ring count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_chiral_center_count, "indigo-do-chiral-center-count",
         "Get chiral center count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_formal_charge, "indigo-do-formal-charge",
         "Get total formal charge from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_hbd_count, "indigo-do-hbd-count",
         "Get hydrogen bond donor count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_hba_count, "indigo-do-hba-count",
         "Get hydrogen bond acceptor count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_smiles, "indigo-do-smiles",
         "Get SMILES format from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_cml, "indigo-do-cml",
         "Get CML format from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_has_stereochemistry, "indigo-do-has-stereochemistry",
         "Check if molecule has stereochemistry from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_is_chiral, "indigo-do-is-chiral",
         "Check if molecule is chiral from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_has_coordinates, "indigo-do-has-coordinates",
         "Check if molecule has coordinates from molecular string", NULL);

    mkfn(env, 2, 2, Fdo_substructure_match, "indigo-do-substructure-match",
         "Check if first molecule contains second molecule as substructure", NULL);

    mkfn(env, 2, 2, Fdo_exact_match, "indigo-do-exact-match",
         "Check if two molecules are exactly the same", NULL);

    mkfn(env, 2, 2, Fdo_similarity, "indigo-do-similarity",
         "Calculate Tanimoto similarity between two molecules", NULL);

    mkfn(env, 0, 0, Findigo_version, "indigo-version",
         "Get Indigo version", NULL);

    mkfn(env, 1, 1, Fdo_reaction_products_count, "indigo-do-reaction-products-count",
         "Get number of products from reaction SMILES", NULL);

    mkfn(env, 1, 1, Fdo_reaction_reactants_count, "indigo-do-reaction-reactants-count",
         "Get number of reactants from reaction SMILES", NULL);

    mkfn(env, 1, 1, Fdo_most_abundant_mass, "indigo-do-most-abundant-mass",
         "Get most abundant mass from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_monoisotopic_mass, "indigo-do-monoisotopic-mass", 
         "Get monoisotopic mass from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_layered_code, "indigo-do-layered-code",
         "Get layered code from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_has_z_coord, "indigo-do-has-z-coord",
         "Check if molecule has Z coordinates from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_heavy_atom_count, "indigo-do-heavy-atom-count",
         "Get heavy atom count from molecular string", NULL);

    mkfn(env, 1, 1, Fdo_symmetry_classes, "indigo-do-symmetry-classes",
         "Get symmetry classes from molecular string", NULL);

    /* Stateful functions */
    mkfn(env, 1, 1, Findigo_free, "indigo-free",
         "Free an Indigo object handle", NULL);

    /* Molecule creation */
    /* Private API - I/O functions (exposed with -- prefix) */
    mkfn(env, 0, 0, Findigo_create_molecule, "indigo--create-molecule",
         "Create empty molecule and return handle (private API)", NULL);

    mkfn(env, 0, 0, Findigo_create_query_molecule, "indigo--create-query-molecule",
         "Create empty query molecule and return handle (private API)", NULL);

    /* Molecule loading from strings */
    mkfn(env, 1, 1, Findigo_load_molecule_from_string, "indigo--load-molecule-from-string",
         "Load molecule from string and return handle (private API)", NULL);

    mkfn(env, 1, 1, Findigo_load_query_molecule_from_string, "indigo--load-query-molecule-from-string",
         "Load query molecule from string and return handle (private API)", NULL);

    /* Molecule loading from files */
    mkfn(env, 1, 1, Findigo_load_molecule_from_file, "indigo--load-molecule-from-file",
         "Load molecule from file and return handle (private API)", NULL);

    mkfn(env, 1, 1, Findigo_load_query_molecule_from_file, "indigo--load-query-molecule-from-file",
         "Load query molecule from file and return handle (private API)", NULL);

    /* Molecule loading from buffers - commented out (redundant with string versions) */
    /* mkfn(env, 1, 1, Findigo_load_molecule_from_buffer, "indigo--load-molecule-from-buffer",
         "Load molecule from buffer and return handle (private API)", NULL);

    mkfn(env, 1, 1, Findigo_load_query_molecule_from_buffer, "indigo--load-query-molecule-from-buffer",
         "Load query molecule from buffer and return handle (private API)", NULL); */

    /* SMARTS loading */
    mkfn(env, 1, 1, Findigo_load_smarts_from_string, "indigo--load-smarts-from-string",
         "Load SMARTS from string and return handle (private API)", NULL);

    mkfn(env, 1, 1, Findigo_load_smarts_from_file, "indigo--load-smarts-from-file",
         "Load SMARTS from file and return handle (private API)", NULL);

    /* mkfn(env, 1, 1, Findigo_load_smarts_from_buffer, "indigo--load-smarts-from-buffer",
         "Load SMARTS from buffer and return handle (private API)", NULL); */

    mkfn(env, 1, 1, Findigo_load_reaction_from_string, "indigo--load-reaction-from-string",
         "Load reaction from string and return handle (private API)", NULL);

    mkfn(env, 1, 1, Findigo_load_reaction_from_file, "indigo--load-reaction-from-file",
         "Load reaction from file and return handle (private API)", NULL);

    /* File saving */
    mkfn(env, 2, 2, Findigo_save_molfile_to_file, "indigo-save-molfile-to-file",
         "Save molecule to file in MOL format", NULL);

    mkfn(env, 1, 1, Findigo_canonical_smiles, "indigo-canonical-smiles",
         "Get canonical SMILES from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_smiles, "indigo-smiles",
         "Get SMILES from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_molfile, "indigo-molfile",
         "Get MOL file from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_cml, "indigo-cml",
         "Get CML from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_molecular_weight, "indigo-molecular-weight",
         "Get molecular weight from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_gross_formula, "indigo-gross-formula",
         "Get gross formula from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_count_atoms, "indigo-count-atoms",
         "Get atom count from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_count_bonds, "indigo-count-bonds",
         "Get bond count from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_count_implicit_hydrogens, "indigo-count-implicit-hydrogens",
         "Get implicit hydrogen count from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_count_sssr, "indigo-count-sssr",
         "Get SSSR ring count from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_count_stereocenters, "indigo-count-stereocenters",
         "Get stereocenter count from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_is_chiral, "indigo-is-chiral",
         "Check if molecule is chiral", NULL);

    mkfn(env, 1, 1, Findigo_has_coordinates, "indigo-has-coordinates",
         "Check if molecule has coordinates", NULL);

    mkfn(env, 1, 1, Findigo_most_abundant_mass, "indigo-most-abundant-mass",
         "Get most abundant mass from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_monoisotopic_mass, "indigo-monoisotopic-mass", 
         "Get monoisotopic mass from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_layered_code, "indigo-layered-code",
         "Get layered code from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_has_z_coord, "indigo-has-z-coord",
         "Check if molecule has 3D coordinates", NULL);

    mkfn(env, 1, 1, Findigo_count_heavy_atoms, "indigo-count-heavy-atoms",
         "Get heavy atom count from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_index, "indigo-index",
         "Get the index of an item (atom, bond, etc.)", NULL);

    mkfn(env, 1, 1, Findigo_charge, "indigo-charge",
         "Get formal charge of an atom", NULL);

    mkfn(env, 1, 1, Findigo_radical, "indigo--radical-raw",
         "Get raw radical integer: 0 (none), 101 (singlet), 102 (doublet), 103 (triplet)", NULL);

    mkfn(env, 1, 1, Findigo_radical_electrons, "indigo-radical-electrons",
         "Get number of radical electrons on an atom", NULL);

    mkfn(env, 1, 1, Findigo_xyz, "indigo-xyz",
         "Get XYZ coordinates of an atom as a list (x y z)", NULL);

    mkfn(env, 1, 1, Findigo_source, "indigo-source",
         "Get the source atom index of a bond", NULL);

    mkfn(env, 1, 1, Findigo_destination, "indigo-destination",
         "Get the destination atom index of a bond", NULL);

    mkfn(env, 1, 1, Findigo_bond_order, "indigo--bond-order-raw",
         "Get raw bond order integer: 1 (single), 2 (double), 3 (triple), 4 (aromatic), 0 (query)", NULL);

    mkfn(env, 1, 1, Findigo_bond_stereo, "indigo--bond-stereo-raw",
         "Get raw bond stereochemistry integer: 5 (up), 6 (down), 4 (either), 7 (cis), 8 (trans), 0 (none)", NULL);

    mkfn(env, 1, 1, Findigo_symmetry_classes, "indigo-symmetry-classes",
         "Get symmetry classes from molecule handle", NULL);

    mkfn(env, 1, 1, Findigo_aromatize, "indigo--aromatize",
         "Internal: Aromatize molecule (detect and mark aromatic rings)", NULL);

    mkfn(env, 1, 1, Findigo_layout, "indigo--layout",
         "Internal: Calculate 2D coordinates for molecule", NULL);

    mkfn(env, 1, 1, Findigo_fold_hydrogens, "indigo--fold-hydrogens",
         "Internal: Remove explicit hydrogens (convert to implicit)", NULL);

    mkfn(env, 1, 1, Findigo_unfold_hydrogens, "indigo--unfold-hydrogens",
         "Internal: Add explicit hydrogens to molecule", NULL);

    mkfn(env, 2, 3, Findigo_exact_match, "indigo-exact-match",
         "Check exact match between two molecules", NULL);

    mkfn(env, 1, 1, Findigo_substructure_matcher, "indigo--substructure-matcher",
         "Create substructure matcher for molecule (private API)", NULL);

    mkfn(env, 2, 2, Findigo_fingerprint, "indigo--fingerprint",
         "Generate fingerprint for molecule (private API)", NULL);

    mkfn(env, 3, 3, Findigo_similarity, "indigo-similarity",
         "Calculate similarity between two fingerprints", NULL);

    mkfn(env, 1, 1, Findigo_clone, "indigo-clone",
         "Clone an Indigo object", NULL);

    mkfn(env, 1, 1, Findigo_to_string, "indigo-to-string",
         "Convert Indigo object to string", NULL);

    mkfn(env, 1, 1, Findigo_symbol, "indigo-symbol",
         "Get atom symbol", NULL);
    
    /* Normalization functions */
    mkfn(env, 1, 2, Findigo_normalize, "indigo--normalize",
         "Internal: Normalize molecule structure (MOLECULE [OPTIONS])", NULL);
    mkfn(env, 1, 1, Findigo_standardize, "indigo--standardize",
         "Internal: Standardize molecule charges, stereo etc. (MOLECULE)", NULL);
    mkfn(env, 3, 3, Findigo_ionize, "indigo--ionize",
         "Internal: Ionize molecule at specified pH (MOLECULE PH PH-TOLERANCE)", NULL);

    /* System functions */
    mkfn(env, 0, 0, Findigo_alloc_session_id, "indigo-alloc-session-id",
         "Allocate a new session ID", NULL);

    mkfn(env, 1, 1, Findigo_set_session_id, "indigo-set-session-id",
         "Switch to another session", NULL);

    mkfn(env, 1, 1, Findigo_release_session_id, "indigo-release-session-id",
         "Release a session", NULL);

    mkfn(env, 0, 0, Findigo_get_last_error, "indigo-get-last-error",
         "Get the last error message", NULL);

    mkfn(env, 0, 0, Findigo_count_references, "indigo-count-references",
         "Count objects currently allocated", NULL);

    mkfn(env, 0, 0, Findigo_free_all_objects, "indigo-free-all-objects",
         "Deallocate all objects in current session", NULL);

    /* Option functions */
    mkfn(env, 2, 2, Findigo_set_option, "indigo-set-option",
         "Set an Indigo option with a string value", NULL);

    mkfn(env, 2, 2, Findigo_set_option_int, "indigo-set-option-int",
         "Set an Indigo option with an integer value", NULL);

    mkfn(env, 2, 2, Findigo_set_option_bool, "indigo-set-option-bool",
         "Set an Indigo option with a boolean value", NULL);

    mkfn(env, 2, 2, Findigo_set_option_float, "indigo-set-option-float",
         "Set an Indigo option with a float value", NULL);

    mkfn(env, 4, 4, Findigo_set_option_color, "indigo-set-option-color",
         "Set an Indigo option with RGB color values", NULL);

    mkfn(env, 3, 3, Findigo_set_option_xy, "indigo-set-option-xy",
         "Set an Indigo option with X,Y coordinate values", NULL);

    /* Iterator functions */
    mkfn(env, 1, 1, Findigo_next, "indigo-next",
         "Get the next item from an iterator", NULL);

    /* Private API - Iterator functions (exposed with -- prefix) */
    mkfn(env, 1, 1, Findigo_iterate_atoms, "indigo--iterate-atoms",
         "Create an iterator over atoms in a molecule (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_bonds, "indigo--iterate-bonds",
         "Create an iterator over bonds in a molecule (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_neighbors, "indigo--iterate-neighbors",
         "Create an iterator over neighbors of an atom (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_components, "indigo--iterate-components",
         "Create an iterator over connected components in a molecule (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_sssr, "indigo--iterate-sssr",
         "Create an iterator over SSSR rings in a molecule (private API)", NULL);

    mkfn(env, 3, 3, Findigo_iterate_subtrees, "indigo--iterate-subtrees",
         "Create an iterator over subtrees in a molecule with atom count range (private API)", NULL);

    mkfn(env, 3, 3, Findigo_iterate_rings, "indigo--iterate-rings",
         "Create an iterator over rings in a molecule with atom count range (private API)", NULL);

    mkfn(env, 3, 3, Findigo_iterate_edge_submolecules, "indigo-iterate-edge-submolecules",
         "Create an iterator over edge submolecules with bond count range", NULL);

    mkfn(env, 1, 1, Findigo_iterate_pseudoatoms, "indigo-iterate-pseudoatoms",
         "Create an iterator over pseudoatoms in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_rsites, "indigo-iterate-rsites",
         "Create an iterator over R-sites in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_stereocenters, "indigo--iterate-stereocenters",
         "Create an iterator over stereocenters in a molecule (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_allene_centers, "indigo-iterate-allene-centers",
         "Create an iterator over allene centers in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_rgroups, "indigo-iterate-rgroups",
         "Create an iterator over R-groups in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_rgroup_fragments, "indigo-iterate-rgroup-fragments",
         "Create an iterator over fragments in an R-group", NULL);

    mkfn(env, 2, 2, Findigo_iterate_attachment_points, "indigo-iterate-attachment-points",
         "Create an iterator over attachment points of an item", NULL);

    mkfn(env, 1, 1, Findigo_iterate_data_sgroups, "indigo-iterate-data-sgroups",
         "Create an iterator over data S-groups in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_superatoms, "indigo-iterate-superatoms",
         "Create an iterator over superatoms in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_generic_sgroups, "indigo-iterate-generic-sgroups",
         "Create an iterator over generic S-groups in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_repeating_units, "indigo-iterate-repeating-units",
         "Create an iterator over repeating units in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_multiple_groups, "indigo-iterate-multiple-groups",
         "Create an iterator over multiple groups in a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_properties, "indigo-iterate-properties",
         "Create an iterator over properties of an object", NULL);

    mkfn(env, 1, 1, Findigo_iterate_reactants, "indigo--iterate-reactants",
         "Create an iterator over reactants in a reaction (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_products, "indigo--iterate-products",
         "Create an iterator over products in a reaction (private API)", NULL);

    mkfn(env, 1, 1, Findigo_iterate_catalysts, "indigo-iterate-catalysts",
         "Create an iterator over catalysts in a reaction", NULL);

    mkfn(env, 1, 1, Findigo_iterate_molecules, "indigo-iterate-molecules",
         "Create an iterator over all molecules in a reaction", NULL);

    mkfn(env, 1, 1, Findigo_iterate_array, "indigo-iterate-array",
         "Create an iterator over elements in an array", NULL);

    mkfn(env, 2, 2, Findigo_iterate_matches, "indigo-iterate-matches",
         "Create an iterator over substructure matches", NULL);

    mkfn(env, 2, 2, Findigo_iterate_tautomers, "indigo-iterate-tautomers",
         "Create an iterator over tautomers of a molecule", NULL);

    mkfn(env, 1, 1, Findigo_iterate_decomposed_molecules, "indigo-iterate-decomposed-molecules",
         "Create an iterator over decomposed molecules", NULL);

    mkfn(env, 1, 1, Findigo_iterate_decompositions, "indigo-iterate-decompositions",
         "Create an iterator over decompositions", NULL);

    /* PKA functions */
    mkfn(env, 3, 3, Findigo_build_pka_model, "indigo-build-pka-model",
         "Build PKA model with max level, threshold, and filename", NULL);

    mkfn(env, 4, 4, Findigo_get_acid_pka_value, "indigo-get-acid-pka-value",
         "Get acid PKA value for atom at specified level", NULL);

    mkfn(env, 4, 4, Findigo_get_basic_pka_value, "indigo-get-basic-pka-value",
         "Get basic PKA value for atom at specified level", NULL);

    /* Reaction mapping functions */
    mkfn(env, 2, 2, Findigo_automap, "indigo-automap",
         "Perform automatic atom-to-atom mapping on reaction", NULL);

    mkfn(env, 2, 2, Findigo_get_atom_mapping_number, "indigo-get-atom-mapping-number",
         "Get atom mapping number for reaction atom", NULL);

    mkfn(env, 3, 3, Findigo_set_atom_mapping_number, "indigo-set-atom-mapping-number",
         "Set atom mapping number for reaction atom", NULL);

    mkfn(env, 1, 1, Findigo_clear_aam, "indigo-clear-aam",
         "Clear all atom-to-atom mapping information from reaction", NULL);

    mkfn(env, 1, 1, Findigo_correct_reacting_centers, "indigo-correct-reacting-centers",
         "Correct reacting centers according to atom-to-atom mapping", NULL);

    /* Reacting center functions */
    mkfn(env, 2, 2, Findigo_get_reacting_center, "indigo-get-reacting-center",
         "Get reacting center value for reaction bond", NULL);

    mkfn(env, 3, 3, Findigo_set_reacting_center, "indigo-set-reacting-center",
         "Set reacting center value for reaction bond", NULL);

    /* Renderer functions */
    mkfn(env, 1, 1, Findigo_write_file, "indigo-write-file",
         "Create file output writer", NULL);

    mkfn(env, 0, 0, Findigo_write_buffer, "indigo-write-buffer",
         "Create buffer output writer", NULL);

    mkfn(env, 1, 1, Findigo_to_buffer, "indigo-to-buffer",
         "Convert output handle to buffer string", NULL);

    mkfn(env, 0, 0, Findigo_create_array, "indigo--create-array",
         "Create array for multiple objects (private API)", NULL);

    mkfn(env, 2, 2, Findigo_array_add, "indigo-array-add",
         "Add object to array", NULL);

    mkfn(env, 2, 2, Findigo_render, "indigo-render",
         "Render object to output", NULL);

    mkfn(env, 2, 2, Findigo_render_to_file, "indigo-render-to-file",
         "Render object directly to file", NULL);

    mkfn(env, 4, 4, Findigo_render_grid, "indigo-render-grid",
         "Render objects as grid to output", NULL);

    mkfn(env, 4, 4, Findigo_render_grid_to_file, "indigo-render-grid-to-file",
         "Render objects as grid directly to file", NULL);

    mkfn(env, 0, 0, Findigo_render_reset, "indigo-render-reset",
         "Reset renderer settings", NULL);

    mkfn(env, 2, 2, Findigo_render_write_hdc, "indigo-render-write-hdc",
         "Create HDC output writer (Windows-specific)", NULL);

    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value Qfeat = env->intern(env, "indigo");
    env->funcall(env, Qprovide, 1, &Qfeat);

    return 0;
}
