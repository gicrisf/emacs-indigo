/*
 * indigo-stateless-ops.c - Stateless operation implementations for Indigo module
 *
 * This file contains all the core operation functions that work directly with
 * Indigo molecule and reaction handles. These functions are format-agnostic
 * and focus purely on chemical computations without Emacs integration.
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

/* Temp */
#include <stdio.h>

/* Core molecular property operations */
emacs_value op_do_molecular_formula(emacs_env *env, int mol) {
    int formula_handle = indigoGrossFormula(mol);
    const char* formula = indigoToString(formula_handle);
    emacs_value result = env->make_string(env, formula, strlen(formula));
    indigoFree(formula_handle);
    return result;
}

emacs_value op_do_molecular_weight(emacs_env *env, int mol) {
    double weight = indigoMolecularWeight(mol);
    return env->make_float(env, weight);
}

emacs_value op_do_canonical_smiles(emacs_env *env, int mol) {
    const char* canonical = indigoCanonicalSmiles(mol);
    return env->make_string(env, canonical, strlen(canonical));
}

emacs_value op_do_molfile(emacs_env *env, int mol) {
    const char* molfile = indigoMolfile(mol);
    return env->make_string(env, molfile, strlen(molfile));
}

emacs_value op_do_smiles(emacs_env *env, int mol) {
    const char* smiles = indigoSmiles(mol);
    return env->make_string(env, smiles, strlen(smiles));
}

emacs_value op_do_cml(emacs_env *env, int mol) {
    const char* cml = indigoCml(mol);
    return env->make_string(env, cml, strlen(cml));
}

/* Counting operations */
emacs_value op_do_atom_count(emacs_env *env, int mol) {
    int count = indigoCountAtoms(mol);
    return env->make_integer(env, count);
}

emacs_value op_do_bond_count(emacs_env *env, int mol) {
    int count = indigoCountBonds(mol);
    return env->make_integer(env, count);
}

emacs_value op_do_hydrogen_count(emacs_env *env, int mol) {
    int count = indigoCountImplicitHydrogens(mol);
    return env->make_integer(env, count);
}

emacs_value op_do_total_atom_count(emacs_env *env, int mol) {
    int heavy_atoms = indigoCountAtoms(mol);
    int hydrogens = indigoCountImplicitHydrogens(mol);
    return env->make_integer(env, heavy_atoms + hydrogens);
}

emacs_value op_do_ring_count(emacs_env *env, int mol) {
    int count = indigoCountSSSR(mol);
    return env->make_integer(env, count);
}

emacs_value op_do_aromatic_ring_count(emacs_env *env, int mol) {
    int count = 0;
    int rings = indigoIterateSSSR(mol);
    for (int ring = indigoNext(rings); ring; ring = indigoNext(rings)) {
        /* Check if any bond in this ring is aromatic (bond order 4) */
        int bonds = indigoIterateBonds(ring);
        int is_aromatic = 0;
        for (int bond = indigoNext(bonds); bond && !is_aromatic; bond = indigoNext(bonds)) {
            if (indigoBondOrder(bond) == 4) {
                is_aromatic = 1;
            }
        }
        indigoFree(bonds);
        if (is_aromatic) {
            count++;
        }
    }
    indigoFree(rings);
    return env->make_integer(env, count);
}

emacs_value op_do_chiral_center_count(emacs_env *env, int mol) {
    int count = 0;
    int atoms = indigoIterateAtoms(mol);
    for (int atom = indigoNext(atoms); atom; atom = indigoNext(atoms)) {
        int stereo = indigoStereocenterType(atom);
        if (stereo == INDIGO_ABS || stereo == INDIGO_OR || stereo == INDIGO_AND) {
            count++;
        }
    }
    indigoFree(atoms);
    return env->make_integer(env, count);
}

/* Chemical property operations */
emacs_value op_do_formal_charge(emacs_env *env, int mol) {
    int total_charge = 0;
    int atoms = indigoIterateAtoms(mol);
    for (int atom = indigoNext(atoms); atom; atom = indigoNext(atoms)) {
        int charge = 0;
        if (indigoGetCharge(atom, &charge) == 1) {
            total_charge += charge;
        }
    }
    indigoFree(atoms);
    return env->make_integer(env, total_charge);
}

emacs_value op_do_hbd_count(emacs_env *env, int mol) {
    int count = 0;
    int atoms = indigoIterateAtoms(mol);
    for (int atom = indigoNext(atoms); atom; atom = indigoNext(atoms)) {
        /* Check if atom is N, O, or S with at least one hydrogen */
        const char* symbol = indigoSymbol(atom);
        if (symbol && (strcmp(symbol, "N") == 0 || strcmp(symbol, "O") == 0 || strcmp(symbol, "S") == 0)) {
            /* Count implicit and explicit hydrogens on this atom */
            int h_count = 0;
            if (indigoCountHydrogens(atom, &h_count) == 1 && h_count > 0) {
                count++; /* This atom can donate hydrogen bonds */
            }
        }
    }
    indigoFree(atoms);
    return env->make_integer(env, count);
}

emacs_value op_do_hba_count(emacs_env *env, int mol) {
    int count = 0;
    int atoms = indigoIterateAtoms(mol);
    for (int atom = indigoNext(atoms); atom; atom = indigoNext(atoms)) {
        /* Check if atom is N, O, or F (common hydrogen bond acceptors) */
        const char* symbol = indigoSymbol(atom);
        if (symbol && (strcmp(symbol, "N") == 0 || strcmp(symbol, "O") == 0 || strcmp(symbol, "F") == 0)) {
            /* Check if atom has lone pairs (simplified: assume N,O,F can accept) */
            count++;
        }
    }
    indigoFree(atoms);
    return env->make_integer(env, count);
}

/* Structural analysis operations */
emacs_value op_do_has_stereochemistry(emacs_env *env, int mol) {
    /* Check if molecule has any stereocenters */
    int stereo_count = indigoCountStereocenters(mol);
    if (stereo_count > 0) {
        return env->intern(env, "t");
    }
    
    /* Also check for stereobonds (cis/trans double bonds) */
    int bonds = indigoIterateBonds(mol);
    for (int bond = indigoNext(bonds); bond; bond = indigoNext(bonds)) {
        int stereo = indigoBondStereo(bond);
        if (stereo != 0) {
            indigoFree(bonds);
            return env->intern(env, "t");
        }
    }
    indigoFree(bonds);
    
    return env->intern(env, "nil");
}

emacs_value op_do_is_chiral(emacs_env *env, int mol) {
    int is_chiral = indigoIsChiral(mol);
    if (is_chiral) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

emacs_value op_do_has_coordinates(emacs_env *env, int mol) {
    int has_coord = indigoHasCoord(mol);
    if (has_coord) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

/* Two-molecule operation implementations */
emacs_value op_do_substructure_match(emacs_env *env, int target_mol, int query_mol) {
    /* Create substructure matcher for the target molecule */
    int matcher = indigoSubstructureMatcher(target_mol, "");
    if (matcher == -1) {
        printf("DEBUG: indigoSubstructureMatcher failed\n");
        return env->intern(env, "nil");
    }
    
    /* Check if query matches target */
    int match_result = indigoMatch(matcher, query_mol);
    
    indigoFree(matcher);
    
    if (match_result > 0) {
        /* Positive value means match found, need to free the match object */
        indigoFree(match_result);
        return env->intern(env, "t");
    } else {
        /* Zero means no match, negative means error */
        return env->intern(env, "nil");
    }
}

emacs_value op_do_exact_match(emacs_env *env, int mol1, int mol2) {
    /* Check if two molecules are exactly the same */
    int exact_match = indigoExactMatch(mol1, mol2, "");
    if (exact_match) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

emacs_value op_do_similarity(emacs_env *env, int mol1, int mol2) {
    /* Calculate Tanimoto similarity using fingerprints */
    int fp1 = indigoFingerprint(mol1, "sim");
    int fp2 = indigoFingerprint(mol2, "sim");
    
    if (fp1 == -1 || fp2 == -1) {
        if (fp1 != -1) indigoFree(fp1);
        if (fp2 != -1) indigoFree(fp2);
        return env->intern(env, "nil");
    }
    
    float similarity = indigoSimilarity(fp1, fp2, "tanimoto");
    indigoFree(fp1);
    indigoFree(fp2);
    
    return env->make_float(env, similarity);
}

/* Reaction operation functions */
emacs_value op_do_reaction_products_count(emacs_env *env, const char *reaction_string) {
    int reaction = indigoLoadReactionFromString(reaction_string);
    if (reaction == -1) {
        return env->intern(env, "nil");
    }
    
    int count = indigoCountProducts(reaction);
    indigoFree(reaction);
    return env->make_integer(env, count);
}

emacs_value op_do_reaction_reactants_count(emacs_env *env, const char *reaction_string) {
    int reaction = indigoLoadReactionFromString(reaction_string);
    if (reaction == -1) {
        return env->intern(env, "nil");
    }
    
    int count = indigoCountReactants(reaction);
    indigoFree(reaction);
    return env->make_integer(env, count);
}

/* Additional molecular property operations */
emacs_value op_do_most_abundant_mass(emacs_env *env, int mol) {
    float mass = indigoMostAbundantMass(mol);
    return env->make_float(env, mass);
}

emacs_value op_do_monoisotopic_mass(emacs_env *env, int mol) {
    float mass = indigoMonoisotopicMass(mol);
    return env->make_float(env, mass);
}

emacs_value op_do_layered_code(emacs_env *env, int mol) {
    const char* code = indigoLayeredCode(mol);
    return env->make_string(env, code, strlen(code));
}

emacs_value op_do_has_z_coord(emacs_env *env, int mol) {
    int has_z = indigoHasZCoord(mol);
    return has_z ? env->intern(env, "t") : env->intern(env, "nil");
}

emacs_value op_do_heavy_atom_count(emacs_env *env, int mol) {
    int count = indigoCountHeavyAtoms(mol);
    return env->make_integer(env, count);
}

emacs_value op_do_symmetry_classes(emacs_env *env, int mol) {
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