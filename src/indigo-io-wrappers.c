/*
 * indigo-io-wrappers.c - Emacs wrapper functions for I/O operations
 *
 * This file contains Emacs-facing wrapper functions for I/O operations
 * including molecule creation, loading, and file saving.
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

emacs_value Findigo_load_reaction_from_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *string = extract_string(env, args[0]);
    if (!string) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_reaction_from_string(env, string);
    free(string);
    return result;
}

emacs_value Findigo_load_reaction_from_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *filename = extract_string(env, args[0]);
    if (!filename) return env->intern(env, "nil");
    
    emacs_value result = op_indigo_load_reaction_from_file(env, filename);
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