/*
 * indigo-reactions-ops.c - Reaction operation functions
 *
 * This file contains reaction-specific operations including normalization,
 * PKA calculations, reaction mapping, and reacting center functions.
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
emacs_value op_indigo_normalize(emacs_env *env, int structure, const char *options) {
    int result = indigoNormalize(structure, options);
    return env->make_integer(env, result);
}

emacs_value op_indigo_standardize(emacs_env *env, int item) {
    int result = indigoStandardize(item);
    return env->make_integer(env, result);
}

emacs_value op_indigo_ionize(emacs_env *env, int item, float pH, float pH_toll) {
    int result = indigoIonize(item, pH, pH_toll);
    return env->make_integer(env, result);
}

/* PKA functions */
emacs_value op_indigo_build_pka_model(emacs_env *env, int max_level, float threshold, const char *filename) {
    int result = indigoBuildPkaModel(max_level, threshold, filename);
    return env->make_integer(env, result);
}

emacs_value op_indigo_get_acid_pka_value(emacs_env *env, int item, int atom, int level, int min_level) {
    float *pka_value = indigoGetAcidPkaValue(item, atom, level, min_level);
    if (pka_value == NULL) {
        return env->intern(env, "nil");
    }
    return env->make_float(env, *pka_value);
}

emacs_value op_indigo_get_basic_pka_value(emacs_env *env, int item, int atom, int level, int min_level) {
    float *pka_value = indigoGetBasicPkaValue(item, atom, level, min_level);
    if (pka_value == NULL) {
        return env->intern(env, "nil");
    }
    return env->make_float(env, *pka_value);
}

/* Reaction mapping functions */
emacs_value op_indigo_automap(emacs_env *env, int reaction, const char *mode) {
    int result = indigoAutomap(reaction, mode);
    return env->make_integer(env, result);
}

emacs_value op_indigo_get_atom_mapping_number(emacs_env *env, int reaction, int reaction_atom) {
    int mapping_number = indigoGetAtomMappingNumber(reaction, reaction_atom);
    return env->make_integer(env, mapping_number);
}

emacs_value op_indigo_set_atom_mapping_number(emacs_env *env, int reaction, int reaction_atom, int number) {
    int result = indigoSetAtomMappingNumber(reaction, reaction_atom, number);
    return env->make_integer(env, result);
}

emacs_value op_indigo_clear_aam(emacs_env *env, int reaction) {
    int result = indigoClearAAM(reaction);
    return env->make_integer(env, result);
}

emacs_value op_indigo_correct_reacting_centers(emacs_env *env, int reaction) {
    int result = indigoCorrectReactingCenters(reaction);
    return env->make_integer(env, result);
}

/* Reacting center functions */
emacs_value op_indigo_get_reacting_center(emacs_env *env, int reaction, int reaction_bond) {
    int rc;
    int result = indigoGetReactingCenter(reaction, reaction_bond, &rc);
    if (result == 1) {
        return env->make_integer(env, rc);
    }
    return env->intern(env, "nil");
}

emacs_value op_indigo_set_reacting_center(emacs_env *env, int reaction, int reaction_bond, int rc) {
    int result = indigoSetReactingCenter(reaction, reaction_bond, rc);
    return env->make_integer(env, result);
}