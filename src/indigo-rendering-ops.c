/*
 * indigo-rendering-ops.c - Indigo rendering operation implementations
 *
 * This file contains the core rendering operation implementations for the
 * Indigo Emacs module. These functions handle molecule and reaction rendering
 * to various output formats including files, buffers, and grids.
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
#include <indigo-renderer.h>

/* Writer and Output Functions */

emacs_value op_indigo_write_file(emacs_env *env, const char *filename) {
    int writer = indigoWriteFile(filename);
    if (writer == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, writer);
}

emacs_value op_indigo_write_buffer(emacs_env *env) {
    int buffer = indigoWriteBuffer();
    if (buffer == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, buffer);
}

emacs_value op_indigo_to_buffer(emacs_env *env, int handle) {
    char *buffer_data = NULL;
    int size = 0;
    int result = indigoToBuffer(handle, &buffer_data, &size);
    if (result != 1 || buffer_data == NULL || size <= 0) {
        return env->intern(env, "nil");
    }
    
    /* Check if data looks like binary (contains null bytes or non-printable chars) */
    int is_binary = 0;
    for (int i = 0; i < size && i < 100; i++) {
        if (buffer_data[i] == '\0' || ((unsigned char)buffer_data[i] > 127)) {
            is_binary = 1;
            break;
        }
    }
    
    if (is_binary) {
        return env->make_unibyte_string(env, buffer_data, size);
    } else {
        return env->make_string(env, buffer_data, size);
    }
}

/* Array Functions for Grid Rendering */

emacs_value op_indigo_create_array(emacs_env *env) {
    int array = indigoCreateArray();
    if (array == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, array);
}

emacs_value op_indigo_array_add(emacs_env *env, int array, int object) {
    int result = indigoArrayAdd(array, object);
    if (result == -1) {
        return env->make_integer(env, 0);  /* Failure */
    }
    return env->make_integer(env, 1);  /* Success */
}

/* Basic Rendering Functions */

emacs_value op_indigo_render(emacs_env *env, int object, int output) {
    int result = indigoRender(object, output);
    return env->make_integer(env, result);
}

emacs_value op_indigo_render_to_file(emacs_env *env, int object, const char *filename) {
    int result = indigoRenderToFile(object, filename);
    return env->make_integer(env, result);
}

/* Grid Rendering Functions */

emacs_value op_indigo_render_grid(emacs_env *env, int objects, int *ref_atoms, int n_columns, int output) {
    int result = indigoRenderGrid(objects, ref_atoms, n_columns, output);
    return env->make_integer(env, result);
}

emacs_value op_indigo_render_grid_to_file(emacs_env *env, int objects, int *ref_atoms, int n_columns, const char *filename) {
    int result = indigoRenderGridToFile(objects, ref_atoms, n_columns, filename);
    return env->make_integer(env, result);
}

/* Rendering Configuration */

emacs_value op_indigo_render_reset(emacs_env *env) {
    int result = indigoRenderReset();
    if (result == 1) {
        /* Set a default output format after reset */
        indigoSetOption("render-output-format", "svg");
    }
    return env->make_integer(env, result);
}

/* HDC Rendering (Windows-specific) */

emacs_value op_indigo_render_write_hdc(emacs_env *env, void *hdc, int printing_hdc) {
    int result = indigoRenderWriteHDC(hdc, printing_hdc);
    if (result == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, result);
}