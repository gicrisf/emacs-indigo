/*
 * indigo-reactions-wrappers.c - Emacs wrapper functions for reaction operations
 *
 * This file contains Emacs-facing wrapper functions for reaction operations
 * including normalization, PKA calculations, and reaction mapping.
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

/* Normalization functions */
emacs_value Findigo_normalize(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int structure = env->extract_integer(env, args[0]);
    
    char *options = NULL;
    if (nargs > 1 && env->is_not_nil(env, args[1])) {
        ptrdiff_t size;
        env->copy_string_contents(env, args[1], NULL, &size);
        options = malloc(size);
        env->copy_string_contents(env, args[1], options, &size);
    } else {
        options = strdup("");
    }
    
    emacs_value result = op_indigo_normalize(env, structure, options);
    free(options);
    return result;
}

emacs_value Findigo_standardize(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int item = env->extract_integer(env, args[0]);
    return op_indigo_standardize(env, item);
}

emacs_value Findigo_ionize(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int item = env->extract_integer(env, args[0]);
    double pH = env->extract_float(env, args[1]);
    double pH_toll = env->extract_float(env, args[2]);
    return op_indigo_ionize(env, item, (float)pH, (float)pH_toll);
}

/* PKA functions */
emacs_value Findigo_build_pka_model(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int max_level = env->extract_integer(env, args[0]);
    double threshold = env->extract_float(env, args[1]);
    
    ptrdiff_t size;
    env->copy_string_contents(env, args[2], NULL, &size);
    char *filename = malloc(size);
    env->copy_string_contents(env, args[2], filename, &size);
    
    emacs_value result = op_indigo_build_pka_model(env, max_level, (float)threshold, filename);
    free(filename);
    return result;
}

emacs_value Findigo_get_acid_pka_value(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int item = env->extract_integer(env, args[0]);
    int atom = env->extract_integer(env, args[1]);
    int level = env->extract_integer(env, args[2]);
    int min_level = env->extract_integer(env, args[3]);
    return op_indigo_get_acid_pka_value(env, item, atom, level, min_level);
}

emacs_value Findigo_get_basic_pka_value(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int item = env->extract_integer(env, args[0]);
    int atom = env->extract_integer(env, args[1]);
    int level = env->extract_integer(env, args[2]);
    int min_level = env->extract_integer(env, args[3]);
    return op_indigo_get_basic_pka_value(env, item, atom, level, min_level);
}

/* Reaction mapping functions */
emacs_value Findigo_automap(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    
    ptrdiff_t size;
    env->copy_string_contents(env, args[1], NULL, &size);
    char *mode = malloc(size);
    env->copy_string_contents(env, args[1], mode, &size);
    
    emacs_value result = op_indigo_automap(env, reaction, mode);
    free(mode);
    return result;
}

emacs_value Findigo_get_atom_mapping_number(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    int reaction_atom = env->extract_integer(env, args[1]);
    return op_indigo_get_atom_mapping_number(env, reaction, reaction_atom);
}

emacs_value Findigo_set_atom_mapping_number(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    int reaction_atom = env->extract_integer(env, args[1]);
    int number = env->extract_integer(env, args[2]);
    return op_indigo_set_atom_mapping_number(env, reaction, reaction_atom, number);
}

emacs_value Findigo_clear_aam(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    return op_indigo_clear_aam(env, reaction);
}

emacs_value Findigo_correct_reacting_centers(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    return op_indigo_correct_reacting_centers(env, reaction);
}

/* Reacting center functions */
emacs_value Findigo_get_reacting_center(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    int reaction_bond = env->extract_integer(env, args[1]);
    return op_indigo_get_reacting_center(env, reaction, reaction_bond);
}

emacs_value Findigo_set_reacting_center(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int reaction = env->extract_integer(env, args[0]);
    int reaction_bond = env->extract_integer(env, args[1]);
    int rc = env->extract_integer(env, args[2]);
    return op_indigo_set_reacting_center(env, reaction, reaction_bond, rc);
}