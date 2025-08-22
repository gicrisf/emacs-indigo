/*
 * indigo-stateful-wrappers.c - Emacs wrapper functions for stateful operations
 *
 * This file contains Emacs-facing wrapper functions that handle parameter
 * conversion and call the corresponding operation functions.
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

/* Memory management */
emacs_value Findigo_free(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int handle = env->extract_integer(env, args[0]);
    return op_indigo_free(env, handle);
}

/* Molecule creation and loading */
emacs_value Findigo_create_molecule(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_create_molecule(env);
}

emacs_value Findigo_create_query_molecule(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_create_query_molecule(env);
}

emacs_value Findigo_load_molecule_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *string = extract_string(env, args[0]);
    if (!string) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_molecule_from_string(env, string);
    free(string);
    return result;
}

emacs_value Findigo_load_molecule_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *filename = extract_string(env, args[0]);
    if (!filename) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_molecule_from_file(env, filename);
    free(filename);
    return result;
}

/* emacs_value Findigo_load_molecule_from_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *buffer = extract_string(env, args[0]);
    if (!buffer) return env->intern(env, "nil");
    
    ptrdiff_t size = strlen(buffer);
    emacs_value result = op_indigo_load_molecule_from_buffer(env, buffer, size);
    free(buffer);
    return result;
} */

emacs_value Findigo_load_query_molecule_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *string = extract_string(env, args[0]);
    if (!string) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_query_molecule_from_string(env, string);
    free(string);
    return result;
}

emacs_value Findigo_load_query_molecule_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *filename = extract_string(env, args[0]);
    if (!filename) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_query_molecule_from_file(env, filename);
    free(filename);
    return result;
}

/* emacs_value Findigo_load_query_molecule_from_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *buffer = extract_string(env, args[0]);
    if (!buffer) return env->intern(env, "nil");
    
    ptrdiff_t size = strlen(buffer);
    emacs_value result = op_indigo_load_query_molecule_from_buffer(env, buffer, size);
    free(buffer);
    return result;
} */

emacs_value Findigo_load_smarts_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *string = extract_string(env, args[0]);
    if (!string) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_smarts_from_string(env, string);
    free(string);
    return result;
}

emacs_value Findigo_load_smarts_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *filename = extract_string(env, args[0]);
    if (!filename) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_smarts_from_file(env, filename);
    free(filename);
    return result;
}

/* emacs_value Findigo_load_smarts_from_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *buffer = extract_string(env, args[0]);
    if (!buffer) return env->intern(env, "nil");
    
    ptrdiff_t size = strlen(buffer);
    emacs_value result = op_indigo_load_smarts_from_buffer(env, buffer, size);
    free(buffer);
    return result;
} */

emacs_value Findigo_save_molfile_to_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int molecule = env->extract_integer(env, args[0]);
    char *filename = extract_string(env, args[1]);
    if (!filename) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_save_molfile_to_file(env, molecule, filename);
    free(filename);
    return result;
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

/* Boolean properties */
emacs_value Findigo_is_chiral(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_is_chiral(env, mol);
}

emacs_value Findigo_has_coordinates(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int mol = env->extract_integer(env, args[0]);
    return op_indigo_has_coordinates(env, mol);
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

/* Utility functions */
emacs_value Findigo_clone(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int obj = env->extract_integer(env, args[0]);
    return op_indigo_clone(env, obj);
}

emacs_value Findigo_to_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int handle = env->extract_integer(env, args[0]);
    return op_indigo_to_string(env, handle);
}

/* System functions */
emacs_value Findigo_version(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_version(env);
}

emacs_value Findigo_alloc_session_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_alloc_session_id(env);
}

emacs_value Findigo_set_session_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    qword session_id = env->extract_integer(env, args[0]);
    return op_indigo_set_session_id(env, session_id);
}

emacs_value Findigo_release_session_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    qword session_id = env->extract_integer(env, args[0]);
    return op_indigo_release_session_id(env, session_id);
}

emacs_value Findigo_get_last_error(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_get_last_error(env);
}

emacs_value Findigo_count_references(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_count_references(env);
}

emacs_value Findigo_free_all_objects(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_free_all_objects(env);
}

/* Option wrapper functions */
emacs_value Findigo_set_option(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *name;
    char *value;
    size_t name_len, value_len;
    
    env->copy_string_contents(env, args[0], NULL, &name_len);
    name = malloc(name_len);
    env->copy_string_contents(env, args[0], name, &name_len);
    
    env->copy_string_contents(env, args[1], NULL, &value_len);
    value = malloc(value_len);
    env->copy_string_contents(env, args[1], value, &value_len);
    
    emacs_value result = op_indigo_set_option(env, name, value);
    
    free(name);
    free(value);
    return result;
}

emacs_value Findigo_set_option_int(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *name;
    size_t name_len;
    
    env->copy_string_contents(env, args[0], NULL, &name_len);
    name = malloc(name_len);
    env->copy_string_contents(env, args[0], name, &name_len);
    
    int value = env->extract_integer(env, args[1]);
    
    emacs_value result = op_indigo_set_option_int(env, name, value);
    
    free(name);
    return result;
}

emacs_value Findigo_set_option_bool(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *name;
    size_t name_len;
    
    env->copy_string_contents(env, args[0], NULL, &name_len);
    name = malloc(name_len);
    env->copy_string_contents(env, args[0], name, &name_len);
    
    int value = env->extract_integer(env, args[1]);
    
    emacs_value result = op_indigo_set_option_bool(env, name, value);
    
    free(name);
    return result;
}

emacs_value Findigo_set_option_float(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *name;
    size_t name_len;
    
    env->copy_string_contents(env, args[0], NULL, &name_len);
    name = malloc(name_len);
    env->copy_string_contents(env, args[0], name, &name_len);
    
    float value = (float)env->extract_float(env, args[1]);
    
    emacs_value result = op_indigo_set_option_float(env, name, value);
    
    free(name);
    return result;
}

emacs_value Findigo_set_option_color(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *name;
    size_t name_len;
    
    env->copy_string_contents(env, args[0], NULL, &name_len);
    name = malloc(name_len);
    env->copy_string_contents(env, args[0], name, &name_len);
    
    float r = (float)env->extract_float(env, args[1]);
    float g = (float)env->extract_float(env, args[2]);
    float b = (float)env->extract_float(env, args[3]);
    
    emacs_value result = op_indigo_set_option_color(env, name, r, g, b);
    
    free(name);
    return result;
}

emacs_value Findigo_set_option_xy(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *name;
    size_t name_len;
    
    env->copy_string_contents(env, args[0], NULL, &name_len);
    name = malloc(name_len);
    env->copy_string_contents(env, args[0], name, &name_len);
    
    int x = env->extract_integer(env, args[1]);
    int y = env->extract_integer(env, args[2]);
    
    emacs_value result = op_indigo_set_option_xy(env, name, x, y);
    
    free(name);
    return result;
}