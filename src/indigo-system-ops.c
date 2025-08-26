/*
 * indigo-system-ops.c - System operation functions
 *
 * This file contains system-level operations including memory management,
 * session handling, error management, and option configuration functions.
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

/* Memory management */
emacs_value op_indigo_free(emacs_env *env, int handle) {
    indigoFree(handle);
    return env->intern(env, "t");
}

emacs_value op_indigo_clone(emacs_env *env, int obj) {
    int cloned = indigoClone(obj);
    return env->make_integer(env, cloned);
}

/* System functions */
emacs_value op_indigo_version(emacs_env *env) {
    const char* version = indigoVersion();
    if (!version) return env->intern(env, "nil");
    return env->make_string(env, version, strlen(version));
}

emacs_value op_indigo_alloc_session_id(emacs_env *env) {
    qword session_id = indigoAllocSessionId();
    return env->make_integer(env, session_id);
}

emacs_value op_indigo_set_session_id(emacs_env *env, qword session_id) {
    indigoSetSessionId(session_id);
    return env->intern(env, "t");
}

emacs_value op_indigo_release_session_id(emacs_env *env, qword session_id) {
    indigoReleaseSessionId(session_id);
    return env->intern(env, "t");
}

emacs_value op_indigo_get_last_error(emacs_env *env) {
    const char* error = indigoGetLastError();
    if (!error) return env->intern(env, "nil");
    return env->make_string(env, error, strlen(error));
}

emacs_value op_indigo_count_references(emacs_env *env) {
    int count = indigoCountReferences();
    return env->make_integer(env, count);
}

emacs_value op_indigo_free_all_objects(emacs_env *env) {
    int result = indigoFreeAllObjects();
    return env->make_integer(env, result);
}

/* Option functions */
emacs_value op_indigo_set_option(emacs_env *env, const char *name, const char *value) {
    int result = indigoSetOption(name, value);
    return env->make_integer(env, result);
}

emacs_value op_indigo_set_option_int(emacs_env *env, const char *name, int value) {
    int result = indigoSetOptionInt(name, value);
    return env->make_integer(env, result);
}

emacs_value op_indigo_set_option_bool(emacs_env *env, const char *name, int value) {
    int result = indigoSetOptionBool(name, value);
    return env->make_integer(env, result);
}

emacs_value op_indigo_set_option_float(emacs_env *env, const char *name, float value) {
    int result = indigoSetOptionFloat(name, value);
    return env->make_integer(env, result);
}

emacs_value op_indigo_set_option_color(emacs_env *env, const char *name, float r, float g, float b) {
    int result = indigoSetOptionColor(name, r, g, b);
    return env->make_integer(env, result);
}

emacs_value op_indigo_set_option_xy(emacs_env *env, const char *name, int x, int y) {
    int result = indigoSetOptionXY(name, x, y);
    return env->make_integer(env, result);
}

/* Utility functions */
emacs_value op_indigo_to_string(emacs_env *env, int handle) {
    const char* str = indigoToString(handle);
    if (!str) return env->intern(env, "nil");
    return env->make_string(env, str, strlen(str));
}

emacs_value op_indigo_symbol(emacs_env *env, int atom) {
    const char* symbol = indigoSymbol(atom);
    if (!symbol) return env->intern(env, "nil");
    return env->make_string(env, symbol, strlen(symbol));
}