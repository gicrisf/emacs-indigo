/*
 * indigo-system-wrappers.c - Emacs wrapper functions for system operations
 *
 * This file contains Emacs-facing wrapper functions for system-level operations
 * including memory management, session handling, error management, and options.
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

emacs_value Findigo_clone(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int obj = env->extract_integer(env, args[0]);
    return op_indigo_clone(env, obj);
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

/* Utility functions */
emacs_value Findigo_to_string(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int handle = env->extract_integer(env, args[0]);
    return op_indigo_to_string(env, handle);
}

emacs_value Findigo_symbol(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int atom = env->extract_integer(env, args[0]);
    return op_indigo_symbol(env, atom);
}