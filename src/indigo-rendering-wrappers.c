/*
 * indigo-rendering-wrappers.c - Emacs wrapper functions for rendering operations
 *
 * This file contains Emacs-facing wrapper functions for rendering operations
 * including file/buffer output creation, basic and grid rendering, and
 * rendering configuration.
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

/* Helper function to extract integer array from Emacs list */
static int* extract_integer_array(emacs_env *env, emacs_value list_arg, int *length) {
    emacs_value length_val = env->funcall(env, env->intern(env, "length"), 1, &list_arg);
    if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
        return NULL;
    }
    
    *length = env->extract_integer(env, length_val);
    if (*length <= 0) {
        return NULL;
    }
    
    int *array = malloc(*length * sizeof(int));
    if (!array) {
        return NULL;
    }
    
    emacs_value nth_func = env->intern(env, "nth");
    for (int i = 0; i < *length; i++) {
        emacs_value args[2] = {
            env->make_integer(env, i),
            list_arg
        };
        emacs_value element = env->funcall(env, nth_func, 2, args);
        if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
            free(array);
            return NULL;
        }
        array[i] = env->extract_integer(env, element);
    }
    
    return array;
}

/* Writer and Output Functions */

emacs_value Findigo_write_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    char *filename = extract_string(env, args[0]);
    if (!filename) {
        return env->intern(env, "nil");
    }
    
    emacs_value result = op_indigo_write_file(env, filename);
    free(filename);
    return result;
}

emacs_value Findigo_write_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_write_buffer(env);
}

emacs_value Findigo_to_buffer(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int handle = env->extract_integer(env, args[0]);
    return op_indigo_to_buffer(env, handle);
}

/* Array Functions for Grid Rendering */

emacs_value Findigo_create_array(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_create_array(env);
}

emacs_value Findigo_array_add(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int array = env->extract_integer(env, args[0]);
    int object = env->extract_integer(env, args[1]);
    
    /* Validate handles */
    if (array <= 0 || object <= 0) {
        env->non_local_exit_signal(env, env->intern(env, "args-out-of-range"), 
                                  env->intern(env, "Invalid handle"));
        return env->intern(env, "nil");
    }
    
    return op_indigo_array_add(env, array, object);
}

/* Basic Rendering Functions */

emacs_value Findigo_render(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int object = env->extract_integer(env, args[0]);
    int output = env->extract_integer(env, args[1]);
    
    /* Validate handles */
    if (object <= 0 || output <= 0) {
        env->non_local_exit_signal(env, env->intern(env, "args-out-of-range"), 
                                  env->intern(env, "Invalid handle"));
        return env->intern(env, "nil");
    }
    
    return op_indigo_render(env, object, output);
}

emacs_value Findigo_render_to_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int object = env->extract_integer(env, args[0]);
    char *filename = extract_string(env, args[1]);
    if (!filename) {
        return env->intern(env, "nil");
    }
    
    emacs_value result = op_indigo_render_to_file(env, object, filename);
    free(filename);
    return result;
}

/* Grid Rendering Functions */

emacs_value Findigo_render_grid(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int objects = env->extract_integer(env, args[0]);
    
    int ref_atoms_length;
    int *ref_atoms = NULL;
    
    // Handle optional ref_atoms parameter (can be nil)
    if (nargs >= 2 && env->is_not_nil(env, args[1])) {
        ref_atoms = extract_integer_array(env, args[1], &ref_atoms_length);
        if (!ref_atoms && ref_atoms_length > 0) {
            return env->intern(env, "nil");
        }
    }
    
    int n_columns = env->extract_integer(env, args[2]);
    int output = env->extract_integer(env, args[3]);
    
    emacs_value result = op_indigo_render_grid(env, objects, ref_atoms, n_columns, output);
    
    if (ref_atoms) {
        free(ref_atoms);
    }
    
    return result;
}

emacs_value Findigo_render_grid_to_file(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    int objects = env->extract_integer(env, args[0]);
    
    int ref_atoms_length;
    int *ref_atoms = NULL;
    
    // Handle optional ref_atoms parameter (can be nil)
    if (nargs >= 2 && env->is_not_nil(env, args[1])) {
        ref_atoms = extract_integer_array(env, args[1], &ref_atoms_length);
        if (!ref_atoms && ref_atoms_length > 0) {
            return env->intern(env, "nil");
        }
    }
    
    int n_columns = env->extract_integer(env, args[2]);
    char *filename = extract_string(env, args[3]);
    if (!filename) {
        if (ref_atoms) free(ref_atoms);
        return env->intern(env, "nil");
    }
    
    emacs_value result = op_indigo_render_grid_to_file(env, objects, ref_atoms, n_columns, filename);
    
    if (ref_atoms) {
        free(ref_atoms);
    }
    free(filename);
    
    return result;
}

/* Rendering Configuration */

emacs_value Findigo_render_reset(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    return op_indigo_render_reset(env);
}

/* HDC Rendering (Windows-specific) */

emacs_value Findigo_render_write_hdc(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
    // Note: HDC handling in Emacs modules is complex and platform-specific
    // This is a placeholder implementation that returns nil for now
    // In practice, this would require extracting a void* from the Emacs value
    // which is not straightforward without additional platform-specific code
    return env->intern(env, "nil");
}