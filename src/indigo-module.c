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

static emacs_value Fmolecular_formula(
    emacs_env *env,
    ptrdiff_t nargs,
    emacs_value *args,
    void *data
) {
    (void)nargs; (void)data;

    // Step 1: Find out how long the SMILES string is
    ptrdiff_t smiles_len = 0;
    env->copy_string_contents(env, args[0], NULL, &smiles_len);
    
    // Step 2: Allocate memory to hold the SMILES string
    char *smiles = malloc(smiles_len);
    if (!smiles) {
        return env->intern(env, "nil");
    }
    
    // Step 3: Actually copy the SMILES string from Emacs to our buffer
    env->copy_string_contents(env, args[0], smiles, &smiles_len);
    
    // Step 4: Ask Indigo to parse the SMILES string into a molecule
    int mol = indigoLoadMoleculeFromString(smiles);
    free(smiles);
    
    // Step 5: Check if Indigo failed to parse the molecule
    if (mol == -1) {
        return env->intern(env, "nil");
    }
    
    // Step 6: Ask Indigo to calculate the molecular formula
    int formula_handle = indigoGrossFormula(mol);
    const char* formula = indigoToString(formula_handle);
    
    // Step 7: Convert the C string to an Emacs string
    emacs_value result = env->make_string(env, formula, strlen(formula));
    
    // Step 8: Clean up Indigo objects to prevent memory leaks
    indigoFree(formula_handle);
    indigoFree(mol);
    
    // Step 9: Return the molecular formula string to Emacs
    return result;
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

    // Step 1: Find out how long the SMILES string is
    ptrdiff_t smiles_len = 0;
    env->copy_string_contents(env, args[0], NULL, &smiles_len);
    
    // Step 2: Allocate memory to hold the SMILES string
    char *smiles = malloc(smiles_len);
    if (!smiles) {
        return env->intern(env, "nil");
    }
    
    // Step 3: Actually copy the SMILES string from Emacs to our buffer
    env->copy_string_contents(env, args[0], smiles, &smiles_len);
    
    // Step 4: Ask Indigo to parse the SMILES string into a molecule
    int mol = indigoLoadMoleculeFromString(smiles);
    free(smiles);
    
    // Step 5: Check if Indigo failed to parse the molecule
    if (mol == -1) {
        return env->intern(env, "nil");
    }
    
    // Step 6: Ask Indigo to calculate the molecular weight
    double weight = indigoMolecularWeight(mol);
    
    // Step 7: Convert the C double to an Emacs float
    emacs_value result = env->make_float(env, weight);
    
    // Step 8: Clean up Indigo objects to prevent memory leaks
    indigoFree(mol);
    
    // Step 9: Return the molecular weight to Emacs
    return result;
}

int emacs_module_init( struct emacs_runtime *ert ) {
    emacs_env *env = ert->get_environment(ert);

    mkfn(env, 1, 1, Fmolecular_formula, "indigo-molecular-formula",
         "Get molecular formula from SMILES string", NULL);

    mkfn(env, 1, 1, Fmolecular_weight, "indigo-molecular-weight",
         "Get molecular weight from SMILES string", NULL);

    mkfn(env, 0, 0, Findigo_version, "indigo-version",
         "Get Indigo version", NULL);

    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value Qfeat = env->intern(env, "indigo");
    env->funcall(env, Qprovide, 1, &Qfeat);

    return 0;
}
