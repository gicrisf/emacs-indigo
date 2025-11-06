/*
 * indigo-iterators-wrappers.c - Emacs wrapper functions for iterator operations
 *
 * This file contains Emacs-facing wrapper functions for iterator operations
 * that traverse molecular structures and features.
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

/* Helper function to extract string from Emacs value */
static char* extract_string(emacs_env *env, emacs_value str_arg) {
    ptrdiff_t size = 0;
    if (!env->copy_string_contents(env, str_arg, NULL, &size)) {
        return NULL;
    }
    
    char *str = malloc(size);
    if (!str || !env->copy_string_contents(env, str_arg, str, &size)) {
        free(str);
        return NULL;
    }
    
    return str;
}

/* Iterator wrapper functions */
emacs_value Findigo_next(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int iterator = env->extract_integer(env, args[0]);
    return op_indigo_next(env, iterator);
}

emacs_value Findigo_iterate_atoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_atoms(env, molecule);
}

emacs_value Findigo_iterate_bonds(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_bonds(env, molecule);
}

emacs_value Findigo_iterate_neighbors(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int atom = env->extract_integer(env, args[0]);
    return op_indigo_iterate_neighbors(env, atom);
}

emacs_value Findigo_iterate_components(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_components(env, molecule);
}

emacs_value Findigo_iterate_sssr(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_sssr(env, molecule);
}

emacs_value Findigo_iterate_subtrees(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    int min_atoms = env->extract_integer(env, args[1]);
    int max_atoms = env->extract_integer(env, args[2]);
    return op_indigo_iterate_subtrees(env, molecule, min_atoms, max_atoms);
}

emacs_value Findigo_iterate_rings(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    int min_atoms = env->extract_integer(env, args[1]);
    int max_atoms = env->extract_integer(env, args[2]);
    return op_indigo_iterate_rings(env, molecule, min_atoms, max_atoms);
}

emacs_value Findigo_iterate_edge_submolecules(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    int min_bonds = env->extract_integer(env, args[1]);
    int max_bonds = env->extract_integer(env, args[2]);
    return op_indigo_iterate_edge_submolecules(env, molecule, min_bonds, max_bonds);
}

emacs_value Findigo_iterate_pseudoatoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_pseudoatoms(env, molecule);
}

emacs_value Findigo_iterate_rsites(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_rsites(env, molecule);
}

emacs_value Findigo_iterate_stereocenters(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_stereocenters(env, molecule);
}

emacs_value Findigo_iterate_allene_centers(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_allene_centers(env, molecule);
}

emacs_value Findigo_iterate_rgroups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_rgroups(env, molecule);
}

emacs_value Findigo_iterate_rgroup_fragments(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int rgroup = env->extract_integer(env, args[0]);
    return op_indigo_iterate_rgroup_fragments(env, rgroup);
}

emacs_value Findigo_iterate_attachment_points(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int item = env->extract_integer(env, args[0]);
    int order = env->extract_integer(env, args[1]);
    return op_indigo_iterate_attachment_points(env, item, order);
}

emacs_value Findigo_iterate_data_sgroups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_data_sgroups(env, molecule);
}

emacs_value Findigo_iterate_superatoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_superatoms(env, molecule);
}

emacs_value Findigo_iterate_generic_sgroups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_generic_sgroups(env, molecule);
}

emacs_value Findigo_iterate_repeating_units(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_repeating_units(env, molecule);
}

emacs_value Findigo_iterate_multiple_groups(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    return op_indigo_iterate_multiple_groups(env, molecule);
}

emacs_value Findigo_iterate_properties(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int handle = env->extract_integer(env, args[0]);
    return op_indigo_iterate_properties(env, handle);
}

emacs_value Findigo_iterate_reactants(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    return op_indigo_iterate_reactants(env, reaction);
}

emacs_value Findigo_iterate_products(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    return op_indigo_iterate_products(env, reaction);
}

emacs_value Findigo_iterate_catalysts(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    return op_indigo_iterate_catalysts(env, reaction);
}

emacs_value Findigo_iterate_molecules(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    return op_indigo_iterate_molecules(env, reaction);
}

emacs_value Findigo_iterate_array(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int array = env->extract_integer(env, args[0]);
    return op_indigo_iterate_array(env, array);
}

emacs_value Findigo_iterate_matches(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int matcher = env->extract_integer(env, args[0]);
    int query = env->extract_integer(env, args[1]);
    return op_indigo_iterate_matches(env, matcher, query);
}

emacs_value Findigo_iterate_tautomers(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    char *options = extract_string(env, args[1]);
    if (!options) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_iterate_tautomers(env, molecule, options);
    
    free(options);
    return result;
}

emacs_value Findigo_iterate_decomposed_molecules(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int decomp = env->extract_integer(env, args[0]);
    return op_indigo_iterate_decomposed_molecules(env, decomp);
}

emacs_value Findigo_iterate_decompositions(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int deco_item = env->extract_integer(env, args[0]);
    return op_indigo_iterate_decompositions(env, deco_item);
}