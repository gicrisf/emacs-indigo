/*
 * indigo-io-ops.c - Input/Output operation functions
 *
 * This file contains I/O operations including molecule creation, loading from
 * strings/files, SMARTS loading, reaction loading, and file saving operations.
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

/* Molecule creation and loading */
emacs_value op_indigo_create_molecule(emacs_env *env) {
    int mol = indigoCreateMolecule();
    return env->make_integer(env, mol);
}

emacs_value op_indigo_create_query_molecule(emacs_env *env) {
    int mol = indigoCreateQueryMolecule();
    return env->make_integer(env, mol);
}

emacs_value op_indigo_load_molecule_from_string(emacs_env *env, const char *string) {
    int mol = indigoLoadMoleculeFromString(string);
    return env->make_integer(env, mol);
}

emacs_value op_indigo_load_molecule_from_file(emacs_env *env, const char *filename) {
    int mol = indigoLoadMoleculeFromFile(filename);
    return env->make_integer(env, mol);
}

/* emacs_value op_indigo_load_molecule_from_buffer(emacs_env *env, const char *buffer, int size) {
    int mol = indigoLoadMoleculeFromBuffer(buffer, size);
    return env->make_integer(env, mol);
} */

emacs_value op_indigo_load_query_molecule_from_string(emacs_env *env, const char *string) {
    int mol = indigoLoadQueryMoleculeFromString(string);
    return env->make_integer(env, mol);
}

emacs_value op_indigo_load_query_molecule_from_file(emacs_env *env, const char *filename) {
    int mol = indigoLoadQueryMoleculeFromFile(filename);
    return env->make_integer(env, mol);
}

/* emacs_value op_indigo_load_query_molecule_from_buffer(emacs_env *env, const char *buffer, int size) {
    int mol = indigoLoadQueryMoleculeFromBuffer(buffer, size);
    return env->make_integer(env, mol);
} */

emacs_value op_indigo_load_smarts_from_string(emacs_env *env, const char *string) {
    int mol = indigoLoadSmartsFromString(string);
    return env->make_integer(env, mol);
}

emacs_value op_indigo_load_smarts_from_file(emacs_env *env, const char *filename) {
    int mol = indigoLoadSmartsFromFile(filename);
    return env->make_integer(env, mol);
}

emacs_value op_indigo_load_reaction_from_string(emacs_env *env, const char *string) {
    int rxn = indigoLoadReactionFromString(string);
    return env->make_integer(env, rxn);
}

emacs_value op_indigo_load_reaction_from_file(emacs_env *env, const char *filename) {
    int rxn = indigoLoadReactionFromFile(filename);
    return env->make_integer(env, rxn);
}

/* emacs_value op_indigo_load_smarts_from_buffer(emacs_env *env, const char *buffer, int size) {
    int mol = indigoLoadSmartsFromBuffer(buffer, size);
    return env->make_integer(env, mol);
} */

emacs_value op_indigo_save_molfile_to_file(emacs_env *env, int molecule, const char *filename) {
    int result = indigoSaveMolfileToFile(molecule, filename);
    return env->make_integer(env, result);
}