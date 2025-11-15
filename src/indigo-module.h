/*
 * indigo-module-core.h - Core declarations for Indigo Emacs module
 *
 * This header provides shared declarations for the Indigo cheminformatics
 * module components, including operation function signatures and common
 * type definitions.
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

#ifndef INDIGO_MODULE_H
#define INDIGO_MODULE_H

#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>
#include <indigo.h>

/* Single-molecule operation function type */
typedef emacs_value (*single_mol_operation)(emacs_env *env, int molecule);

/* Two-molecule operation function type */
typedef emacs_value (*dual_mol_operation)(emacs_env *env, int mol1, int mol2);

/* Reaction string operation function type */
typedef emacs_value (*reaction_operation)(emacs_env *env, const char *reaction_string);

/* Core utility function for registering Emacs functions */
void mkfn(emacs_env *env,
          ptrdiff_t nargs_min,
          ptrdiff_t nargs_max,
          emacs_value (*func)(emacs_env *env, ptrdiff_t nargs, emacs_value* args, void *data),
          const char *name,
          const char *docstring,
          void *data);

/* String operation utilities */
emacs_value molstring_op(emacs_env *env, emacs_value molstring_arg, single_mol_operation operation);
emacs_value dual_molstring_op(emacs_env *env, emacs_value molstring1_arg, emacs_value molstring2_arg, dual_mol_operation operation);
emacs_value substructure_match_op(emacs_env *env, emacs_value target_arg, emacs_value query_arg, dual_mol_operation operation);
emacs_value reactionstring_op(emacs_env *env, emacs_value rxnstring_arg, reaction_operation operation);

/* Single-molecule operation implementations */
emacs_value op_do_molecular_formula(emacs_env *env, int mol);
emacs_value op_do_molecular_weight(emacs_env *env, int mol);
emacs_value op_do_canonical_smiles(emacs_env *env, int mol);
emacs_value op_do_atom_count(emacs_env *env, int mol);
emacs_value op_do_bond_count(emacs_env *env, int mol);
emacs_value op_do_molfile(emacs_env *env, int mol);
emacs_value op_do_hydrogen_count(emacs_env *env, int mol);
emacs_value op_do_total_atom_count(emacs_env *env, int mol);
emacs_value op_do_ring_count(emacs_env *env, int mol);
emacs_value op_do_aromatic_ring_count(emacs_env *env, int mol);
emacs_value op_do_chiral_center_count(emacs_env *env, int mol);
emacs_value op_do_formal_charge(emacs_env *env, int mol);
emacs_value op_do_hbd_count(emacs_env *env, int mol);
emacs_value op_do_hba_count(emacs_env *env, int mol);
emacs_value op_do_smiles(emacs_env *env, int mol);
emacs_value op_do_cml(emacs_env *env, int mol);
emacs_value op_do_has_stereochemistry(emacs_env *env, int mol);
emacs_value op_do_is_chiral(emacs_env *env, int mol);
emacs_value op_do_has_coordinates(emacs_env *env, int mol);
emacs_value op_do_most_abundant_mass(emacs_env *env, int mol);
emacs_value op_do_monoisotopic_mass(emacs_env *env, int mol);
emacs_value op_do_layered_code(emacs_env *env, int mol);
emacs_value op_do_has_z_coord(emacs_env *env, int mol);
emacs_value op_do_heavy_atom_count(emacs_env *env, int mol);
emacs_value op_do_symmetry_classes(emacs_env *env, int mol);

/* Two-molecule operation implementations */
emacs_value op_do_substructure_match(emacs_env *env, int target_mol, int query_mol);
emacs_value op_do_exact_match(emacs_env *env, int mol1, int mol2);
emacs_value op_do_similarity(emacs_env *env, int mol1, int mol2);

/* Reaction operation implementations */
emacs_value op_do_reaction_products_count(emacs_env *env, const char *reaction_string);
emacs_value op_do_reaction_reactants_count(emacs_env *env, const char *reaction_string);

/* Stateful operation function declarations */
emacs_value op_indigo_free(emacs_env *env, int handle);
emacs_value op_indigo_create_molecule(emacs_env *env);
emacs_value op_indigo_create_query_molecule(emacs_env *env);
emacs_value op_indigo_load_molecule_from_string(emacs_env *env, const char *string);
emacs_value op_indigo_load_molecule_from_file(emacs_env *env, const char *filename);
/* emacs_value op_indigo_load_molecule_from_buffer(emacs_env *env, const char *buffer, int size); */
emacs_value op_indigo_load_query_molecule_from_string(emacs_env *env, const char *string);
emacs_value op_indigo_load_query_molecule_from_file(emacs_env *env, const char *filename);
/* emacs_value op_indigo_load_query_molecule_from_buffer(emacs_env *env, const char *buffer, int size); */
emacs_value op_indigo_load_smarts_from_string(emacs_env *env, const char *string);
emacs_value op_indigo_load_smarts_from_file(emacs_env *env, const char *filename);
/* emacs_value op_indigo_load_smarts_from_buffer(emacs_env *env, const char *buffer, int size); */
emacs_value op_indigo_load_reaction_from_string(emacs_env *env, const char *string);
emacs_value op_indigo_load_reaction_from_file(emacs_env *env, const char *filename);
emacs_value op_indigo_save_molfile_to_file(emacs_env *env, int molecule, const char *filename);
emacs_value op_indigo_canonical_smiles(emacs_env *env, int mol);
emacs_value op_indigo_smiles(emacs_env *env, int mol);
emacs_value op_indigo_molfile(emacs_env *env, int mol);
emacs_value op_indigo_cml(emacs_env *env, int mol);
emacs_value op_indigo_molecular_weight(emacs_env *env, int mol);
emacs_value op_indigo_gross_formula(emacs_env *env, int mol);
emacs_value op_indigo_count_atoms(emacs_env *env, int mol);
emacs_value op_indigo_count_bonds(emacs_env *env, int mol);
emacs_value op_indigo_count_implicit_hydrogens(emacs_env *env, int mol);
emacs_value op_indigo_count_sssr(emacs_env *env, int mol);
emacs_value op_indigo_count_stereocenters(emacs_env *env, int mol);
emacs_value op_indigo_is_chiral(emacs_env *env, int mol);
emacs_value op_indigo_has_coordinates(emacs_env *env, int mol);
emacs_value op_indigo_exact_match(emacs_env *env, int mol1, int mol2, const char *flags);
emacs_value op_indigo_substructure_matcher(emacs_env *env, int target);
emacs_value op_indigo_fingerprint(emacs_env *env, int mol, const char *type);
emacs_value op_indigo_similarity(emacs_env *env, int fp1, int fp2, const char *metrics);
emacs_value op_indigo_clone(emacs_env *env, int obj);
emacs_value op_indigo_to_string(emacs_env *env, int handle);
emacs_value op_indigo_symbol(emacs_env *env, int atom);
emacs_value op_indigo_normalize(emacs_env *env, int structure, const char *options);
emacs_value op_indigo_standardize(emacs_env *env, int item);
emacs_value op_indigo_ionize(emacs_env *env, int item, float pH, float pH_toll);
emacs_value op_indigo_most_abundant_mass(emacs_env *env, int mol);
emacs_value op_indigo_monoisotopic_mass(emacs_env *env, int mol);
emacs_value op_indigo_layered_code(emacs_env *env, int mol);
emacs_value op_indigo_has_z_coord(emacs_env *env, int mol);
emacs_value op_indigo_count_heavy_atoms(emacs_env *env, int mol);
emacs_value op_indigo_symmetry_classes(emacs_env *env, int mol);
emacs_value op_indigo_index(emacs_env *env, int item);
emacs_value op_indigo_charge(emacs_env *env, int atom);
emacs_value op_indigo_xyz(emacs_env *env, int atom);
emacs_value op_indigo_source(emacs_env *env, int bond);
emacs_value op_indigo_destination(emacs_env *env, int bond);
emacs_value op_indigo_bond_order(emacs_env *env, int bond);
emacs_value op_indigo_bond_stereo(emacs_env *env, int bond);

/* System operation function declarations */
emacs_value op_indigo_version(emacs_env *env);
emacs_value op_indigo_alloc_session_id(emacs_env *env);
emacs_value op_indigo_set_session_id(emacs_env *env, qword session_id);
emacs_value op_indigo_release_session_id(emacs_env *env, qword session_id);
emacs_value op_indigo_get_last_error(emacs_env *env);
emacs_value op_indigo_count_references(emacs_env *env);
emacs_value op_indigo_free_all_objects(emacs_env *env);

/* Option operation function declarations */
emacs_value op_indigo_set_option(emacs_env *env, const char *name, const char *value);
emacs_value op_indigo_set_option_int(emacs_env *env, const char *name, int value);
emacs_value op_indigo_set_option_bool(emacs_env *env, const char *name, int value);
emacs_value op_indigo_set_option_float(emacs_env *env, const char *name, float value);
emacs_value op_indigo_set_option_color(emacs_env *env, const char *name, float r, float g, float b);
emacs_value op_indigo_set_option_xy(emacs_env *env, const char *name, int x, int y);

/* Iterator operation function declarations */
emacs_value op_indigo_next(emacs_env *env, int iterator);
emacs_value op_indigo_iterate_atoms(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_bonds(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_neighbors(emacs_env *env, int atom);
emacs_value op_indigo_iterate_components(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_sssr(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_subtrees(emacs_env *env, int molecule, int min_atoms, int max_atoms);
emacs_value op_indigo_iterate_rings(emacs_env *env, int molecule, int min_atoms, int max_atoms);
emacs_value op_indigo_iterate_edge_submolecules(emacs_env *env, int molecule, int min_bonds, int max_bonds);
emacs_value op_indigo_iterate_pseudoatoms(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_rsites(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_stereocenters(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_allene_centers(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_rgroups(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_rgroup_fragments(emacs_env *env, int rgroup);
emacs_value op_indigo_iterate_attachment_points(emacs_env *env, int item, int order);
emacs_value op_indigo_iterate_data_sgroups(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_superatoms(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_generic_sgroups(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_repeating_units(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_multiple_groups(emacs_env *env, int molecule);
emacs_value op_indigo_iterate_properties(emacs_env *env, int handle);
emacs_value op_indigo_iterate_reactants(emacs_env *env, int reaction);
emacs_value op_indigo_iterate_products(emacs_env *env, int reaction);
emacs_value op_indigo_iterate_catalysts(emacs_env *env, int reaction);
emacs_value op_indigo_iterate_molecules(emacs_env *env, int reaction);
emacs_value op_indigo_iterate_array(emacs_env *env, int array);
emacs_value op_indigo_iterate_matches(emacs_env *env, int matcher, int query);
emacs_value op_indigo_iterate_tautomers(emacs_env *env, int molecule, const char *options);
emacs_value op_indigo_iterate_decomposed_molecules(emacs_env *env, int decomp);
emacs_value op_indigo_iterate_decompositions(emacs_env *env, int deco_item);

/* PKA operation function declarations */
emacs_value op_indigo_build_pka_model(emacs_env *env, int max_level, float threshold, const char *filename);
emacs_value op_indigo_get_acid_pka_value(emacs_env *env, int item, int atom, int level, int min_level);
emacs_value op_indigo_get_basic_pka_value(emacs_env *env, int item, int atom, int level, int min_level);

/* Reaction mapping operation function declarations */
emacs_value op_indigo_automap(emacs_env *env, int reaction, const char *mode);
emacs_value op_indigo_get_atom_mapping_number(emacs_env *env, int reaction, int reaction_atom);
emacs_value op_indigo_set_atom_mapping_number(emacs_env *env, int reaction, int reaction_atom, int number);
emacs_value op_indigo_clear_aam(emacs_env *env, int reaction);
emacs_value op_indigo_correct_reacting_centers(emacs_env *env, int reaction);

/* Reacting center operation function declarations */
emacs_value op_indigo_get_reacting_center(emacs_env *env, int reaction, int reaction_bond);
emacs_value op_indigo_set_reacting_center(emacs_env *env, int reaction, int reaction_bond, int rc);

/* Renderer operation function declarations */
emacs_value op_indigo_write_file(emacs_env *env, const char *filename);
emacs_value op_indigo_write_buffer(emacs_env *env);
emacs_value op_indigo_to_buffer(emacs_env *env, int handle);
emacs_value op_indigo_create_array(emacs_env *env);
emacs_value op_indigo_array_add(emacs_env *env, int array, int object);
emacs_value op_indigo_render(emacs_env *env, int object, int output);
emacs_value op_indigo_render_to_file(emacs_env *env, int object, const char *filename);
emacs_value op_indigo_render_grid(emacs_env *env, int objects, int *ref_atoms, int n_columns, int output);
emacs_value op_indigo_render_grid_to_file(emacs_env *env, int objects, int *ref_atoms, int n_columns, const char *filename);
emacs_value op_indigo_render_reset(emacs_env *env);
emacs_value op_indigo_render_write_hdc(emacs_env *env, void *hdc, int printing_hdc);

/* Stateless Emacs wrapper function declarations */
emacs_value Fdo_molecular_formula(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_molecular_weight(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_canonical_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_atom_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_bond_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_molfile(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_hydrogen_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_total_atom_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_ring_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_aromatic_ring_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_chiral_center_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_formal_charge(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_hbd_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_hba_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_cml(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_has_stereochemistry(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_is_chiral(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_has_coordinates(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_most_abundant_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_monoisotopic_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_layered_code(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_has_z_coord(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_heavy_atom_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_symmetry_classes(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_substructure_match(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_exact_match(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_similarity(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_reaction_products_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Fdo_reaction_reactants_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* Stateful Emacs wrapper function declarations */
emacs_value Findigo_free(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_create_molecule(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_create_query_molecule(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_load_molecule_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_load_molecule_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
/* emacs_value Findigo_load_molecule_from_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data); */
emacs_value Findigo_load_query_molecule_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_load_query_molecule_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
/* emacs_value Findigo_load_query_molecule_from_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data); */
emacs_value Findigo_load_smarts_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_load_smarts_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
/* emacs_value Findigo_load_smarts_from_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data); */
emacs_value Findigo_load_reaction_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_load_reaction_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_save_molfile_to_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_canonical_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_molfile(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_cml(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_molecular_weight(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_gross_formula(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_atoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_bonds(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_implicit_hydrogens(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_sssr(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_stereocenters(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_is_chiral(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_has_coordinates(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_exact_match(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_substructure_matcher(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_fingerprint(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_similarity(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_clone(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_to_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_symbol(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_normalize(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_standardize(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_ionize(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_most_abundant_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_monoisotopic_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_layered_code(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_has_z_coord(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_heavy_atoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_symmetry_classes(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_index(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_charge(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_xyz(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_source(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_destination(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_bond_order(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_bond_stereo(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* System Emacs wrapper function declarations */
emacs_value Findigo_version(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_alloc_session_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_session_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_release_session_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_get_last_error(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_count_references(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_free_all_objects(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* Option Emacs wrapper function declarations */
emacs_value Findigo_set_option(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_option_int(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_option_bool(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_option_float(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_option_color(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_option_xy(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* Iterator Emacs wrapper function declarations */
emacs_value Findigo_next(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_atoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_bonds(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_neighbors(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_components(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_sssr(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_subtrees(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_rings(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_edge_submolecules(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_pseudoatoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_rsites(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_stereocenters(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_allene_centers(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_rgroups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_rgroup_fragments(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_attachment_points(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_data_sgroups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_superatoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_generic_sgroups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_repeating_units(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_multiple_groups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_properties(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_reactants(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_products(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_catalysts(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_molecules(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_array(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_matches(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_tautomers(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_decomposed_molecules(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_iterate_decompositions(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* PKA Emacs wrapper function declarations */
emacs_value Findigo_build_pka_model(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_get_acid_pka_value(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_get_basic_pka_value(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* Reaction mapping Emacs wrapper function declarations */
emacs_value Findigo_automap(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_get_atom_mapping_number(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_atom_mapping_number(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_clear_aam(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_correct_reacting_centers(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* Reacting center Emacs wrapper function declarations */
emacs_value Findigo_get_reacting_center(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_set_reacting_center(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

/* Renderer Emacs wrapper function declarations */
emacs_value Findigo_write_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_write_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_to_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_create_array(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_array_add(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_render(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_render_to_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_render_grid(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_render_grid_to_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_render_reset(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);
emacs_value Findigo_render_write_hdc(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data);

#endif /* INDIGO_MODULE_H */