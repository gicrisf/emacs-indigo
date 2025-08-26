/*
 * indigo-stateless-wrappers.c - Emacs wrapper functions for Indigo operations
 *
 * This file contains all the Emacs-facing wrapper functions that bridge
 * between Emacs Lisp and the core Indigo operations. These functions handle
 * Emacs value conversion and delegate to the appropriate operation functions.
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

/* Core molecular property wrapper functions */
emacs_value Fdo_molecular_formula(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_molecular_formula);
}

emacs_value Fdo_molecular_weight(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_molecular_weight);
}

emacs_value Fdo_canonical_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_canonical_smiles);
}

emacs_value Fdo_molfile(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_molfile);
}

emacs_value Fdo_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_smiles);
}

emacs_value Fdo_cml(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_cml);
}

/* Counting function wrappers */
emacs_value Fdo_atom_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_atom_count);
}

emacs_value Fdo_bond_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_bond_count);
}

emacs_value Fdo_hydrogen_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_hydrogen_count);
}

emacs_value Fdo_total_atom_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_total_atom_count);
}

emacs_value Fdo_ring_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_ring_count);
}

emacs_value Fdo_aromatic_ring_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_aromatic_ring_count);
}

emacs_value Fdo_chiral_center_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_chiral_center_count);
}

/* Chemical property wrapper functions */
emacs_value Fdo_formal_charge(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_formal_charge);
}

emacs_value Fdo_hbd_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_hbd_count);
}

emacs_value Fdo_hba_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_hba_count);
}

/* Structural analysis wrapper functions */
emacs_value Fdo_has_stereochemistry(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_has_stereochemistry);
}

emacs_value Fdo_is_chiral(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_is_chiral);
}

emacs_value Fdo_has_coordinates(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_has_coordinates);
}

/* Two-molecule wrapper functions */
emacs_value Fdo_substructure_match(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return substructure_match_op(env, args[0], args[1], op_do_substructure_match);
}

emacs_value Fdo_exact_match(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return dual_molstring_op(env, args[0], args[1], op_do_exact_match);
}

emacs_value Fdo_similarity(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return dual_molstring_op(env, args[0], args[1], op_do_similarity);
}

/* Reaction wrapper functions */
emacs_value Fdo_reaction_products_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return reactionstring_op(env, args[0], op_do_reaction_products_count);
}

emacs_value Fdo_reaction_reactants_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return reactionstring_op(env, args[0], op_do_reaction_reactants_count);
}

/* Additional molecular property wrapper functions */
emacs_value Fdo_most_abundant_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_most_abundant_mass);
}

emacs_value Fdo_monoisotopic_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_monoisotopic_mass);
}

emacs_value Fdo_layered_code(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_layered_code);
}

emacs_value Fdo_has_z_coord(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_has_z_coord);
}

emacs_value Fdo_heavy_atom_count(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_heavy_atom_count);
}

emacs_value Fdo_symmetry_classes(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_do_symmetry_classes);
}

