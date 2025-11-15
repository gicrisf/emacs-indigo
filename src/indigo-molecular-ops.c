/*
 * indigo-molecular-ops.c - Molecular operation functions
 *
 * This file contains molecular operations including format conversions,
 * property calculations, counting functions, matching, and fingerprints.
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

emacs_value op_indigo_count_heavy_atoms(emacs_env *env, int mol) {
    int count = indigoCountHeavyAtoms(mol);
    return env->make_integer(env, count);
}

/* Atom properties */
emacs_value op_indigo_index(emacs_env *env, int item) {
    int index = indigoIndex(item);
    if (index < 0) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, index);
}

emacs_value op_indigo_charge(emacs_env *env, int atom) {
    int charge = 0;
    if (indigoGetCharge(atom, &charge) != 1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, charge);
}

emacs_value op_indigo_radical(emacs_env *env, int atom) {
    int radical = 0;
    if (indigoGetRadical(atom, &radical) != 1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, radical);
}

emacs_value op_indigo_radical_electrons(emacs_env *env, int atom) {
    int electrons = 0;
    if (indigoGetRadicalElectrons(atom, &electrons) != 1) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, electrons);
}

emacs_value op_indigo_xyz(emacs_env *env, int atom) {
    float *xyz = indigoXYZ(atom);
    if (!xyz) {
        return env->intern(env, "nil");
    }

    /* Build list (x y z) from right to left */
    emacs_value result = env->intern(env, "nil");
    emacs_value z = env->make_float(env, xyz[2]);
    result = env->funcall(env, env->intern(env, "cons"), 2, (emacs_value[]){z, result});
    emacs_value y = env->make_float(env, xyz[1]);
    result = env->funcall(env, env->intern(env, "cons"), 2, (emacs_value[]){y, result});
    emacs_value x = env->make_float(env, xyz[0]);
    result = env->funcall(env, env->intern(env, "cons"), 2, (emacs_value[]){x, result});

    return result;
}

/* Bond properties */
emacs_value op_indigo_source(emacs_env *env, int bond) {
    int source = indigoSource(bond);
    if (source < 0) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, source);
}

emacs_value op_indigo_destination(emacs_env *env, int bond) {
    int dest = indigoDestination(bond);
    if (dest < 0) {
        return env->intern(env, "nil");
    }
    return env->make_integer(env, dest);
}

emacs_value op_indigo_bond_order(emacs_env *env, int bond) {
    int order = indigoBondOrder(bond);
    return env->make_integer(env, order);
}

emacs_value op_indigo_bond_stereo(emacs_env *env, int bond) {
    int stereo = indigoBondStereo(bond);
    return env->make_integer(env, stereo);
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

emacs_value op_indigo_has_z_coord(emacs_env *env, int mol) {
    int has_z = indigoHasZCoord(mol);
    if (has_z) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
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

/* Symmetry analysis */
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

/* Structure manipulation functions */
emacs_value op_indigo_aromatize(emacs_env *env, int item) {
    int status = indigoAromatize(item);
    return env->make_integer(env, status);
}

emacs_value op_indigo_layout(emacs_env *env, int object) {
    int status = indigoLayout(object);
    return env->make_integer(env, status);
}

emacs_value op_indigo_fold_hydrogens(emacs_env *env, int item) {
    int status = indigoFoldHydrogens(item);
    return env->make_integer(env, status);
}

emacs_value op_indigo_unfold_hydrogens(emacs_env *env, int item) {
    int status = indigoUnfoldHydrogens(item);
    return env->make_integer(env, status);
}
