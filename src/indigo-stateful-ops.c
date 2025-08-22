/*
 * indigo-stateful-ops.c - Direct wrappers around native Indigo C functions
 *
 * This file contains simple wrappers that expose native Indigo library functions
 * directly to Emacs, maintaining the original function semantics and return types.
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

/* emacs_value op_indigo_load_smarts_from_buffer(emacs_env *env, const char *buffer, int size) {
    int mol = indigoLoadSmartsFromBuffer(buffer, size);
    return env->make_integer(env, mol);
} */

emacs_value op_indigo_save_molfile_to_file(emacs_env *env, int molecule, const char *filename) {
    int result = indigoSaveMolfileToFile(molecule, filename);
    return env->make_integer(env, result);
}

/* Basic molecule operations */
emacs_value op_indigo_canonical_smiles(emacs_env *env, int mol) {
    const char* smiles = indigoCanonicalSmiles(mol);
    if (!smiles) return env->intern(env, "nil");
    return env->make_string(env, smiles, strlen(smiles));
}

emacs_value op_indigo_smiles(emacs_env *env, int mol) {
    const char* smiles = indigoSmiles(mol);
    if (!smiles) return env->intern(env, "nil");
    return env->make_string(env, smiles, strlen(smiles));
}

emacs_value op_indigo_molfile(emacs_env *env, int mol) {
    const char* molfile = indigoMolfile(mol);
    if (!molfile) return env->intern(env, "nil");
    return env->make_string(env, molfile, strlen(molfile));
}

emacs_value op_indigo_cml(emacs_env *env, int mol) {
    const char* cml = indigoCml(mol);
    if (!cml) return env->intern(env, "nil");
    return env->make_string(env, cml, strlen(cml));
}

/* Molecular properties */
emacs_value op_indigo_molecular_weight(emacs_env *env, int mol) {
    double weight = indigoMolecularWeight(mol);
    return env->make_float(env, weight);
}

emacs_value op_indigo_gross_formula(emacs_env *env, int mol) {
    int formula_handle = indigoGrossFormula(mol);
    if (formula_handle <= 0) return env->intern(env, "nil");
    const char* formula = indigoToString(formula_handle);
    emacs_value result = env->make_string(env, formula, strlen(formula));
    indigoFree(formula_handle);
    return result;
}

/* Counting functions */
emacs_value op_indigo_count_atoms(emacs_env *env, int mol) {
    int count = indigoCountAtoms(mol);
    return env->make_integer(env, count);
}

emacs_value op_indigo_count_bonds(emacs_env *env, int mol) {
    int count = indigoCountBonds(mol);
    return env->make_integer(env, count);
}

emacs_value op_indigo_count_implicit_hydrogens(emacs_env *env, int mol) {
    int count = indigoCountImplicitHydrogens(mol);
    return env->make_integer(env, count);
}

emacs_value op_indigo_count_sssr(emacs_env *env, int mol) {
    int count = indigoCountSSSR(mol);
    return env->make_integer(env, count);
}

emacs_value op_indigo_count_stereocenters(emacs_env *env, int mol) {
    int count = indigoCountStereocenters(mol);
    return env->make_integer(env, count);
}

/* Boolean properties */
emacs_value op_indigo_is_chiral(emacs_env *env, int mol) {
    return indigoIsChiral(mol) ? env->intern(env, "t") : env->intern(env, "nil");
}

emacs_value op_indigo_has_coordinates(emacs_env *env, int mol) {
    if (mol <= 0) return env->intern(env, "nil");
    int result = indigoHasCoord(mol);
    return result ? env->intern(env, "t") : env->intern(env, "nil");
}

/* Matching functions */
emacs_value op_indigo_exact_match(emacs_env *env, int mol1, int mol2, const char *flags) {
    int match = indigoExactMatch(mol1, mol2, flags);
    return env->make_integer(env, match);
}

emacs_value op_indigo_substructure_matcher(emacs_env *env, int target) {
    int matcher = indigoSubstructureMatcher(target, "");
    return env->make_integer(env, matcher);
}

/* Fingerprints and similarity */
emacs_value op_indigo_fingerprint(emacs_env *env, int mol, const char *type) {
    int fp = indigoFingerprint(mol, type);
    return env->make_integer(env, fp);
}

emacs_value op_indigo_similarity(emacs_env *env, int fp1, int fp2, const char *metrics) {
    float similarity = indigoSimilarity(fp1, fp2, metrics);
    return env->make_float(env, similarity);
}

/* Utility functions */
emacs_value op_indigo_clone(emacs_env *env, int obj) {
    int cloned = indigoClone(obj);
    return env->make_integer(env, cloned);
}

emacs_value op_indigo_to_string(emacs_env *env, int handle) {
    const char* str = indigoToString(handle);
    if (!str) return env->intern(env, "nil");
    return env->make_string(env, str, strlen(str));
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