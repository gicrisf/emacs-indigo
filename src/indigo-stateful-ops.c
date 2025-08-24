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

emacs_value op_indigo_symbol(emacs_env *env, int atom) {
    const char* symbol = indigoSymbol(atom);
    if (!symbol) return env->intern(env, "nil");
    return env->make_string(env, symbol, strlen(symbol));
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

/* Iterator functions */
emacs_value op_indigo_next(emacs_env *env, int iterator) {
    int item = indigoNext(iterator);
    if (item == 0) {
        return env->intern(env, "nil");
    } else if (item == -1) {
        /* TODO or is it better raising an error? */
        return env->intern(env, "nil");
    }
    return env->make_integer(env, item);
}

emacs_value op_indigo_iterate_atoms(emacs_env *env, int molecule) {
    int iterator = indigoIterateAtoms(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_bonds(emacs_env *env, int molecule) {
    int iterator = indigoIterateBonds(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_neighbors(emacs_env *env, int atom) {
    int iterator = indigoIterateNeighbors(atom);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_components(emacs_env *env, int molecule) {
    int iterator = indigoIterateComponents(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_sssr(emacs_env *env, int molecule) {
    int iterator = indigoIterateSSSR(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_subtrees(emacs_env *env, int molecule, int min_atoms, int max_atoms) {
    int iterator = indigoIterateSubtrees(molecule, min_atoms, max_atoms);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_rings(emacs_env *env, int molecule, int min_atoms, int max_atoms) {
    int iterator = indigoIterateRings(molecule, min_atoms, max_atoms);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_edge_submolecules(emacs_env *env, int molecule, int min_bonds, int max_bonds) {
    int iterator = indigoIterateEdgeSubmolecules(molecule, min_bonds, max_bonds);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_pseudoatoms(emacs_env *env, int molecule) {
    int iterator = indigoIteratePseudoatoms(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_rsites(emacs_env *env, int molecule) {
    int iterator = indigoIterateRSites(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_stereocenters(emacs_env *env, int molecule) {
    int iterator = indigoIterateStereocenters(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_allene_centers(emacs_env *env, int molecule) {
    int iterator = indigoIterateAlleneCenters(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_rgroups(emacs_env *env, int molecule) {
    int iterator = indigoIterateRGroups(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_rgroup_fragments(emacs_env *env, int rgroup) {
    int iterator = indigoIterateRGroupFragments(rgroup);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_attachment_points(emacs_env *env, int item, int order) {
    int iterator = indigoIterateAttachmentPoints(item, order);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_data_sgroups(emacs_env *env, int molecule) {
    int iterator = indigoIterateDataSGroups(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_superatoms(emacs_env *env, int molecule) {
    int iterator = indigoIterateSuperatoms(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_generic_sgroups(emacs_env *env, int molecule) {
    int iterator = indigoIterateGenericSGroups(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_repeating_units(emacs_env *env, int molecule) {
    int iterator = indigoIterateRepeatingUnits(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_multiple_groups(emacs_env *env, int molecule) {
    int iterator = indigoIterateMultipleGroups(molecule);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_properties(emacs_env *env, int handle) {
    int iterator = indigoIterateProperties(handle);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_reactants(emacs_env *env, int reaction) {
    int iterator = indigoIterateReactants(reaction);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_products(emacs_env *env, int reaction) {
    int iterator = indigoIterateProducts(reaction);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_catalysts(emacs_env *env, int reaction) {
    int iterator = indigoIterateCatalysts(reaction);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_molecules(emacs_env *env, int reaction) {
    int iterator = indigoIterateMolecules(reaction);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_array(emacs_env *env, int array) {
    int iterator = indigoIterateArray(array);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_matches(emacs_env *env, int matcher, int query) {
    int iterator = indigoIterateMatches(matcher, query);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_tautomers(emacs_env *env, int molecule, const char *options) {
    int iterator = indigoIterateTautomers(molecule, options);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_decomposed_molecules(emacs_env *env, int decomp) {
    int iterator = indigoIterateDecomposedMolecules(decomp);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

emacs_value op_indigo_iterate_decompositions(emacs_env *env, int deco_item) {
    int iterator = indigoIterateDecompositions(deco_item);
    if (iterator == -1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, iterator);
}

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

/* Core functions, maybe I should move these up */
emacs_value op_indigo_most_abundant_mass(emacs_env *env, int mol) {
    float mass = indigoMostAbundantMass(mol);
    return env->make_float(env, mass);
}

emacs_value op_indigo_monoisotopic_mass(emacs_env *env, int mol) {
    float mass = indigoMonoisotopicMass(mol);
    return env->make_float(env, mass);
}

emacs_value op_indigo_layered_code(emacs_env *env, int mol) {
    const char* code = indigoLayeredCode(mol);
    return env->make_string(env, code, strlen(code));
}

emacs_value op_indigo_has_z_coord(emacs_env *env, int mol) {
    int has_z = indigoHasZCoord(mol);
    if (has_z) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

emacs_value op_indigo_count_heavy_atoms(emacs_env *env, int mol) {
    int count = indigoCountHeavyAtoms(mol);
    return env->make_integer(env, count);
}

emacs_value op_indigo_symmetry_classes(emacs_env *env, int mol) {
    int count_out;
    const int* classes = indigoSymmetryClasses(mol, &count_out);
    if (classes == NULL) {
        return env->intern(env, "nil");
    }
    
    /* Create a list of symmetry classes */
    emacs_value result = env->intern(env, "nil");
    for (int i = count_out - 1; i >= 0; i--) {
        emacs_value class_val = env->make_integer(env, classes[i]);
        result = env->funcall(env, env->intern(env, "cons"), 2, (emacs_value[]){class_val, result});
    }
    
    return result;
}
