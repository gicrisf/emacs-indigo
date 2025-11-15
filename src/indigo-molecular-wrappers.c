/*
 * indigo-molecular-wrappers.c - Emacs wrapper functions for molecular operations
 *
 * This file contains Emacs-facing wrapper functions for molecular operations
 * including format conversions, properties, calculations, matching, and fingerprints.
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

/* Basic molecule operations */
emacs_value Findigo_canonical_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_canonical_smiles(env, mol);
}

emacs_value Findigo_smiles(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_smiles(env, mol);
}

emacs_value Findigo_molfile(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_molfile(env, mol);
}

emacs_value Findigo_cml(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_cml(env, mol);
}

/* Molecular properties */
emacs_value Findigo_molecular_weight(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_molecular_weight(env, mol);
}

emacs_value Findigo_gross_formula(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_gross_formula(env, mol);
}

emacs_value Findigo_most_abundant_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_most_abundant_mass(env, mol);
}

emacs_value Findigo_monoisotopic_mass(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_monoisotopic_mass(env, mol);
}

emacs_value Findigo_layered_code(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_layered_code(env, mol);
}

/* Counting functions */
emacs_value Findigo_count_atoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_count_atoms(env, mol);
}

emacs_value Findigo_count_bonds(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_count_bonds(env, mol);
}

emacs_value Findigo_count_implicit_hydrogens(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_count_implicit_hydrogens(env, mol);
}

emacs_value Findigo_count_sssr(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_count_sssr(env, mol);
}

emacs_value Findigo_count_stereocenters(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_count_stereocenters(env, mol);
}

emacs_value Findigo_count_heavy_atoms(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_count_heavy_atoms(env, mol);
}

/* Atom properties */
emacs_value Findigo_index(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int item = env->extract_integer(env, args[0]);
    return op_indigo_index(env, item);
}

emacs_value Findigo_charge(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int atom = env->extract_integer(env, args[0]);
    return op_indigo_charge(env, atom);
}

emacs_value Findigo_xyz(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int atom = env->extract_integer(env, args[0]);
    return op_indigo_xyz(env, atom);
}

/* Boolean properties */
emacs_value Findigo_is_chiral(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_is_chiral(env, mol);
}

emacs_value Findigo_has_coordinates(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_has_coordinates(env, mol);
}

emacs_value Findigo_has_z_coord(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_has_z_coord(env, mol);
}

/* Bond property functions */
emacs_value Findigo_source(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int bond = env->extract_integer(env, args[0]);
    return op_indigo_source(env, bond);
}

emacs_value Findigo_destination(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int bond = env->extract_integer(env, args[0]);
    return op_indigo_destination(env, bond);
}

emacs_value Findigo_bond_order(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int bond = env->extract_integer(env, args[0]);
    return op_indigo_bond_order(env, bond);
}

emacs_value Findigo_bond_stereo(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int bond = env->extract_integer(env, args[0]);
    return op_indigo_bond_stereo(env, bond);
}

/* Matching functions */
emacs_value Findigo_exact_match(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol1 = env->extract_integer(env, args[0]);
    int mol2 = env->extract_integer(env, args[1]);
    
    char *flags = "";
    if (nargs > 2) {
        flags = extract_string(env, args[2]);
        if (!flags) flags = "";
    }
    
    emacs_value result = op_indigo_exact_match(env, mol1, mol2, flags);
    
    if (nargs > 2 && flags != NULL && *flags != '\0') {
        free(flags);
    }
    
    return result;
}

emacs_value Findigo_substructure_matcher(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int target = env->extract_integer(env, args[0]);
    return op_indigo_substructure_matcher(env, target);
}

/* Fingerprints and similarity */
emacs_value Findigo_fingerprint(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    char *type = extract_string(env, args[1]);
    if (!type) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_fingerprint(env, mol, type);
    free(type);
    return result;
}

emacs_value Findigo_similarity(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int fp1 = env->extract_integer(env, args[0]);
    int fp2 = env->extract_integer(env, args[1]);
    char *metrics = extract_string(env, args[2]);
    if (!metrics) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_similarity(env, fp1, fp2, metrics);
    free(metrics);
    return result;
}

/* Symmetry analysis */
emacs_value Findigo_symmetry_classes(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_symmetry_classes(env, mol);
}