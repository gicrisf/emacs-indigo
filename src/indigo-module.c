/*
 * indigo-module.c - Emacs dynamic module interface to Indigo cheminformatics library
 *
 * This module provides Emacs Lisp bindings for the Indigo toolkit,
 * enabling molecular structure manipulation, chemical file I/O,
 * and cheminformatics operations from within Emacs.
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

#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>
#include <indigo.h>

// Temp
#include <stdio.h>

/* Declare the module is GPL compatible */
int plugin_is_GPL_compatible;

static void
mkfn (emacs_env *env,
      ptrdiff_t nargs_min,
      ptrdiff_t nargs_max,
      emacs_value (*func) (emacs_env *env,
                           ptrdiff_t nargs,
                           emacs_value* args,
                           void *data),
      const char *name,
      const char *docstring,
      void *data)
{
    /* Make function */
    emacs_value Sfun = env->make_function
        (env, nargs_min, nargs_max, func, docstring, data);
    /* Bind function */
    emacs_value Qfset = env->intern (env, "fset");
    emacs_value Qsym = env->intern (env, name);
    emacs_value args[] = { Qsym, Sfun };
    env->funcall (env, Qfset, 2, args);
}

/* Utility function to execute operations on molecules from molecular format strings */
static emacs_value
molstring_op (emacs_env *env,
              emacs_value molstring_arg,
              emacs_value (*operation) (emacs_env *env, int molecule))
{
    // Step 1: Find out how long the molecular string is
    ptrdiff_t molstring_len = 0;
    env->copy_string_contents(env, molstring_arg, NULL, &molstring_len);
    
    // Step 2: Allocate memory to hold the molecular string
    char *molstring = malloc(molstring_len);
    if (!molstring) {
        return env->intern(env, "nil");
    }
    
    // Step 3: Actually copy the molecular string from Emacs to our buffer
    env->copy_string_contents(env, molstring_arg, molstring, &molstring_len);
    
    // Step 4: Ask Indigo to parse the molecular string into a molecule
    int mol = indigoLoadMoleculeFromString(molstring);
    free(molstring);
    
    // Step 5: Check if Indigo failed to parse the molecule
    if (mol == -1) {
        return env->intern(env, "nil");
    }
    
    // Step 6: Execute the operation on the molecule
    emacs_value result = operation(env, mol);
    
    // Step 7: Clean up Indigo objects to prevent memory leaks
    indigoFree(mol);
    
    // Step 8: Return the result to Emacs
    return result;
}

/* Utility function to execute operations on two molecules from molecular format strings */
static emacs_value
dual_molstring_op (emacs_env *env,
                   emacs_value molstring1_arg,
                   emacs_value molstring2_arg,
                   emacs_value (*operation) (emacs_env *env, int mol1, int mol2))
{
    // Step 1: Get first molecule string length and allocate memory
    ptrdiff_t molstring1_len = 0;
    env->copy_string_contents(env, molstring1_arg, NULL, &molstring1_len);
    char *molstring1 = malloc(molstring1_len);
    if (!molstring1) {
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, molstring1_arg, molstring1, &molstring1_len);
    
    // Step 2: Get second molecule string length and allocate memory
    ptrdiff_t molstring2_len = 0;
    env->copy_string_contents(env, molstring2_arg, NULL, &molstring2_len);
    char *molstring2 = malloc(molstring2_len);
    if (!molstring2) {
        free(molstring1);
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, molstring2_arg, molstring2, &molstring2_len);
    
    // Step 3: Parse both molecules with Indigo
    int mol1 = indigoLoadMoleculeFromString(molstring1);
    int mol2 = indigoLoadMoleculeFromString(molstring2);
    free(molstring1);
    free(molstring2);
    
    // Step 4: Check if either molecule failed to parse
    if (mol1 == -1 || mol2 == -1) {
        if (mol1 != -1) indigoFree(mol1);
        if (mol2 != -1) indigoFree(mol2);
        return env->intern(env, "nil");
    }
    
    // Step 5: Execute the operation on both molecules
    emacs_value result = operation(env, mol1, mol2);
    
    // Step 6: Clean up Indigo objects
    indigoFree(mol1);
    indigoFree(mol2);
    
    // Step 7: Return the result
    return result;
}

/* Utility function for substructure matching - query molecule loaded as query type */
static emacs_value
substructure_match_op (emacs_env *env,
                       emacs_value target_arg,
                       emacs_value query_arg,
                       emacs_value (*operation) (emacs_env *env, int target_mol, int query_mol))
{
    // Step 1: Get target molecule string and allocate memory
    ptrdiff_t target_len = 0;
    env->copy_string_contents(env, target_arg, NULL, &target_len);
    char *target_string = malloc(target_len);
    if (!target_string) {
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, target_arg, target_string, &target_len);
    
    // Step 2: Get query molecule string and allocate memory
    ptrdiff_t query_len = 0;
    env->copy_string_contents(env, query_arg, NULL, &query_len);
    char *query_string = malloc(query_len);
    if (!query_string) {
        free(target_string);
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, query_arg, query_string, &query_len);
    
    // Step 3: Parse target as regular molecule, query as query molecule
    int target_mol = indigoLoadMoleculeFromString(target_string);
    int query_mol = indigoLoadQueryMoleculeFromString(query_string);
    free(target_string);
    free(query_string);
    
    // Step 4: Check if either molecule failed to parse
    if (target_mol == -1 || query_mol == -1) {
        if (target_mol != -1) indigoFree(target_mol);
        if (query_mol != -1) indigoFree(query_mol);
        return env->intern(env, "nil");
    }
    
    // Step 5: Execute the operation
    emacs_value result = operation(env, target_mol, query_mol);
    
    // Step 6: Clean up
    indigoFree(target_mol);
    indigoFree(query_mol);
    
    // Step 7: Return result
    return result;
}

/* Utility function for reaction string operations */
static emacs_value
reactionstring_op (emacs_env *env,
                   emacs_value rxnstring_arg,
                   emacs_value (*operation) (emacs_env *env, const char *reaction_string))
{
    // Step 1: Get reaction string and allocate memory
    ptrdiff_t len = 0;
    env->copy_string_contents(env, rxnstring_arg, NULL, &len);
    char *reaction_string = malloc(len);
    if (!reaction_string) {
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, rxnstring_arg, reaction_string, &len);
    
    // Step 2: Execute the operation
    emacs_value result = operation(env, reaction_string);
    
    // Step 3: Clean up
    free(reaction_string);
    
    // Step 4: Return result
    return result;
}

/* Operation implementations */
static emacs_value op_molecular_formula(emacs_env *env, int mol) {
    int formula_handle = indigoGrossFormula(mol);
    const char* formula = indigoToString(formula_handle);
    emacs_value result = env->make_string(env, formula, strlen(formula));
    indigoFree(formula_handle);
    return result;
}

static emacs_value op_molecular_weight(emacs_env *env, int mol) {
    double weight = indigoMolecularWeight(mol);
    return env->make_float(env, weight);
}

static emacs_value op_canonical_smiles(emacs_env *env, int mol) {
    const char* canonical = indigoCanonicalSmiles(mol);
    return env->make_string(env, canonical, strlen(canonical));
}

static emacs_value op_atom_count(emacs_env *env, int mol) {
    int count = indigoCountAtoms(mol);
    return env->make_integer(env, count);
}

static emacs_value op_bond_count(emacs_env *env, int mol) {
    int count = indigoCountBonds(mol);
    return env->make_integer(env, count);
}

static emacs_value op_molfile(emacs_env *env, int mol) {
    const char* molfile = indigoMolfile(mol);
    return env->make_string(env, molfile, strlen(molfile));
}

static emacs_value op_hydrogen_count(emacs_env *env, int mol) {
    int count = indigoCountImplicitHydrogens(mol);
    return env->make_integer(env, count);
}

static emacs_value op_total_atom_count(emacs_env *env, int mol) {
    int heavy_atoms = indigoCountAtoms(mol);
    int hydrogens = indigoCountImplicitHydrogens(mol);
    return env->make_integer(env, heavy_atoms + hydrogens);
}

static emacs_value op_ring_count(emacs_env *env, int mol) {
    int count = indigoCountSSSR(mol);
    return env->make_integer(env, count);
}

static emacs_value op_aromatic_ring_count(emacs_env *env, int mol) {
    int count = 0;
    int rings = indigoIterateSSSR(mol);
    for (int ring = indigoNext(rings); ring; ring = indigoNext(rings)) {
        // Check if any bond in this ring is aromatic (bond order 4)
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

static emacs_value op_chiral_center_count(emacs_env *env, int mol) {
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

static emacs_value op_formal_charge(emacs_env *env, int mol) {
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

static emacs_value op_hbd_count(emacs_env *env, int mol) {
    int count = 0;
    int atoms = indigoIterateAtoms(mol);
    for (int atom = indigoNext(atoms); atom; atom = indigoNext(atoms)) {
        // Check if atom is N, O, or S with at least one hydrogen
        const char* symbol = indigoSymbol(atom);
        if (symbol && (strcmp(symbol, "N") == 0 || strcmp(symbol, "O") == 0 || strcmp(symbol, "S") == 0)) {
            // Count implicit and explicit hydrogens on this atom
            int h_count = 0;
            if (indigoCountHydrogens(atom, &h_count) == 1 && h_count > 0) {
                count++; // This atom can donate hydrogen bonds
            }
        }
    }
    indigoFree(atoms);
    return env->make_integer(env, count);
}

static emacs_value op_hba_count(emacs_env *env, int mol) {
    int count = 0;
    int atoms = indigoIterateAtoms(mol);
    for (int atom = indigoNext(atoms); atom; atom = indigoNext(atoms)) {
        // Check if atom is N, O, or F (common hydrogen bond acceptors)
        const char* symbol = indigoSymbol(atom);
        if (symbol && (strcmp(symbol, "N") == 0 || strcmp(symbol, "O") == 0 || strcmp(symbol, "F") == 0)) {
            // Check if atom has lone pairs (simplified: assume N,O,F can accept)
            count++;
        }
    }
    indigoFree(atoms);
    return env->make_integer(env, count);
}

static emacs_value op_smiles(emacs_env *env, int mol) {
    const char* smiles = indigoSmiles(mol);
    return env->make_string(env, smiles, strlen(smiles));
}

static emacs_value op_cml(emacs_env *env, int mol) {
    const char* cml = indigoCml(mol);
    return env->make_string(env, cml, strlen(cml));
}

static emacs_value op_has_stereochemistry(emacs_env *env, int mol) {
    // Check if molecule has any stereocenters
    int stereo_count = indigoCountStereocenters(mol);
    if (stereo_count > 0) {
        return env->intern(env, "t");
    }
    
    // Also check for stereobonds (cis/trans double bonds)
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

static emacs_value op_is_chiral(emacs_env *env, int mol) {
    int is_chiral = indigoIsChiral(mol);
    if (is_chiral) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

static emacs_value op_has_coordinates(emacs_env *env, int mol) {
    int has_coord = indigoHasCoord(mol);
    if (has_coord) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

/* Two-molecule operation implementations */
static emacs_value op_substructure_match(emacs_env *env, int target_mol, int query_mol) {
    // Create substructure matcher for the target molecule
    int matcher = indigoSubstructureMatcher(target_mol, "");
    if (matcher == -1) {
        printf("DEBUG: indigoSubstructureMatcher failed\n");
        return env->intern(env, "nil");
    }
    
    // Check if query matches target
    int match_result = indigoMatch(matcher, query_mol);
    
    indigoFree(matcher);
    
    if (match_result > 0) {
        // Positive value means match found, need to free the match object
        indigoFree(match_result);
        return env->intern(env, "t");
    } else {
        // Zero means no match, negative means error
        return env->intern(env, "nil");
    }
}

static emacs_value op_exact_match(emacs_env *env, int mol1, int mol2) {
    // Check if two molecules are exactly the same
    int exact_match = indigoExactMatch(mol1, mol2, "");
    if (exact_match) {
        return env->intern(env, "t");
    } else {
        return env->intern(env, "nil");
    }
}

static emacs_value op_similarity(emacs_env *env, int mol1, int mol2) {
    // Calculate Tanimoto similarity using fingerprints
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

// Reaction operation functions
static emacs_value op_reaction_products_count(emacs_env *env, const char *reaction_string) {
    int reaction = indigoLoadReactionFromString(reaction_string);
    if (reaction == -1) {
        return env->intern(env, "nil");
    }
    
    int count = indigoCountProducts(reaction);
    indigoFree(reaction);
    return env->make_integer(env, count);
}

static emacs_value op_reaction_reactants_count(emacs_env *env, const char *reaction_string) {
    int reaction = indigoLoadReactionFromString(reaction_string);
    if (reaction == -1) {
        return env->intern(env, "nil");
    }
    
    int count = indigoCountReactants(reaction);
    indigoFree(reaction);
    return env->make_integer(env, count);
}

static emacs_value Fmolecular_formula(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_molecular_formula);
}

static emacs_value Findigo_version(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)args; (void)data;

    const char *version = indigoVersion();
    if (!version) version = "unknown";
    return env->make_string(env, version, strlen(version));
}

static emacs_value Fmolecular_weight(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_molecular_weight);
}

static emacs_value Fcanonical_smiles(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_canonical_smiles);
}

static emacs_value Fatom_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_atom_count);
}

static emacs_value Fbond_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_bond_count);
}

static emacs_value Fmolfile(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_molfile);
}

static emacs_value Fhydrogen_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_hydrogen_count);
}

static emacs_value Ftotal_atom_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_total_atom_count);
}

static emacs_value Fring_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_ring_count);
}

static emacs_value Faromatic_ring_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_aromatic_ring_count);
}

static emacs_value Fchiral_center_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_chiral_center_count);
}

static emacs_value Fformal_charge(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_formal_charge);
}

static emacs_value Fhbd_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_hbd_count);
}

static emacs_value Fhba_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_hba_count);
}

static emacs_value Fsmiles(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_smiles);
}

static emacs_value Fcml(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_cml);
}

static emacs_value Fhas_stereochemistry(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_has_stereochemistry);
}

static emacs_value Fis_chiral(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_is_chiral);
}

static emacs_value Fhas_coordinates(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return molstring_op(env, args[0], op_has_coordinates);
}

static emacs_value Fsubstructure_match(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return substructure_match_op(env, args[0], args[1], op_substructure_match);
}

static emacs_value Fexact_match(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return dual_molstring_op(env, args[0], args[1], op_exact_match);
}

static emacs_value Fsimilarity(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return dual_molstring_op(env, args[0], args[1], op_similarity);
}

static emacs_value Freaction_products_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return reactionstring_op(env, args[0], op_reaction_products_count);
}

static emacs_value Freaction_reactants_count(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;
    return reactionstring_op(env, args[0], op_reaction_reactants_count);
}

int emacs_module_init( struct emacs_runtime *ert ) {
    emacs_env *env = ert->get_environment(ert);

    mkfn(env, 1, 1, Fmolecular_formula, "indigo-molecular-formula",
         "Get molecular formula from molecular string", NULL);

    mkfn(env, 1, 1, Fmolecular_weight, "indigo-molecular-weight",
         "Get molecular weight from molecular string", NULL);

    mkfn(env, 1, 1, Fcanonical_smiles, "indigo-canonical-smiles",
         "Get canonical SMILES from molecular string", NULL);

    mkfn(env, 1, 1, Fatom_count, "indigo-atom-count",
         "Get heavy atom count from molecular string", NULL);

    mkfn(env, 1, 1, Fbond_count, "indigo-bond-count",
         "Get explicit bond count from molecular string", NULL);

    mkfn(env, 1, 1, Fmolfile, "indigo-molfile",
         "Get MOL file format from molecular string", NULL);

    mkfn(env, 1, 1, Fhydrogen_count, "indigo-hydrogen-count",
         "Get hydrogen count from molecular string", NULL);

    mkfn(env, 1, 1, Ftotal_atom_count, "indigo-total-atom-count",
         "Get total atom count (heavy atoms + hydrogens) from molecular string", NULL);

    mkfn(env, 1, 1, Fring_count, "indigo-ring-count",
         "Get ring count from molecular string", NULL);

    mkfn(env, 1, 1, Faromatic_ring_count, "indigo-aromatic-ring-count",
         "Get aromatic ring count from molecular string", NULL);

    mkfn(env, 1, 1, Fchiral_center_count, "indigo-chiral-center-count",
         "Get chiral center count from molecular string", NULL);

    mkfn(env, 1, 1, Fformal_charge, "indigo-formal-charge",
         "Get total formal charge from molecular string", NULL);

    mkfn(env, 1, 1, Fhbd_count, "indigo-hbd-count",
         "Get hydrogen bond donor count from molecular string", NULL);

    mkfn(env, 1, 1, Fhba_count, "indigo-hba-count",
         "Get hydrogen bond acceptor count from molecular string", NULL);

    mkfn(env, 1, 1, Fsmiles, "indigo-smiles",
         "Get SMILES format from molecular string", NULL);

    mkfn(env, 1, 1, Fcml, "indigo-cml",
         "Get CML format from molecular string", NULL);

    mkfn(env, 1, 1, Fhas_stereochemistry, "indigo-has-stereochemistry",
         "Check if molecule has stereochemistry from molecular string", NULL);

    mkfn(env, 1, 1, Fis_chiral, "indigo-is-chiral",
         "Check if molecule is chiral from molecular string", NULL);

    mkfn(env, 1, 1, Fhas_coordinates, "indigo-has-coordinates",
         "Check if molecule has coordinates from molecular string", NULL);

    mkfn(env, 2, 2, Fsubstructure_match, "indigo-substructure-match",
         "Check if first molecule contains second molecule as substructure", NULL);

    mkfn(env, 2, 2, Fexact_match, "indigo-exact-match",
         "Check if two molecules are exactly the same", NULL);

    mkfn(env, 2, 2, Fsimilarity, "indigo-similarity",
         "Calculate Tanimoto similarity between two molecules", NULL);

    mkfn(env, 0, 0, Findigo_version, "indigo-version",
         "Get Indigo version", NULL);

    mkfn(env, 1, 1, Freaction_products_count, "indigo-reaction-products-count",
         "Get number of products from reaction SMILES", NULL);

    mkfn(env, 1, 1, Freaction_reactants_count, "indigo-reaction-reactants-count",
         "Get number of reactants from reaction SMILES", NULL);

    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value Qfeat = env->intern(env, "indigo");
    env->funcall(env, Qprovide, 1, &Qfeat);

    return 0;
}
