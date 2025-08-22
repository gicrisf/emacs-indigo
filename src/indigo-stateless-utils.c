/*
 * indigo-stateless-utils.c - String operation utilities for stateless operations
 *
 * This file contains the string parsing utilities that handle conversion between
 * Emacs string values and Indigo molecule/reaction handles. These utilities provide
 * a clean abstraction layer for stateless operations.
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

/* String operation utilities for stateless operations */

/* Utility function to execute operations on molecules from molecular format strings */
emacs_value molstring_op(emacs_env *env,
                        emacs_value molstring_arg,
                        single_mol_operation operation)
{
    /* Step 1: Find out how long the molecular string is */
    ptrdiff_t molstring_len = 0;
    env->copy_string_contents(env, molstring_arg, NULL, &molstring_len);
    
    /* Step 2: Allocate memory to hold the molecular string */
    char *molstring = malloc(molstring_len);
    if (!molstring) {
        return env->intern(env, "nil");
    }
    
    /* Step 3: Actually copy the molecular string from Emacs to our buffer */
    env->copy_string_contents(env, molstring_arg, molstring, &molstring_len);
    
    /* Step 4: Ask Indigo to parse the molecular string into a molecule */
    int mol = indigoLoadMoleculeFromString(molstring);
    free(molstring);
    
    /* Step 5: Check if Indigo failed to parse the molecule */
    if (mol == -1) {
        return env->intern(env, "nil");
    }
    
    /* Step 6: Execute the operation on the molecule */
    emacs_value result = operation(env, mol);
    
    /* Step 7: Clean up Indigo objects to prevent memory leaks */
    indigoFree(mol);
    
    /* Step 8: Return the result to Emacs */
    return result;
}

/* Utility function to execute operations on two molecules from molecular format strings */
emacs_value dual_molstring_op(emacs_env *env,
                             emacs_value molstring1_arg,
                             emacs_value molstring2_arg,
                             dual_mol_operation operation)
{
    /* Step 1: Get first molecule string length and allocate memory */
    ptrdiff_t molstring1_len = 0;
    env->copy_string_contents(env, molstring1_arg, NULL, &molstring1_len);
    char *molstring1 = malloc(molstring1_len);
    if (!molstring1) {
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, molstring1_arg, molstring1, &molstring1_len);
    
    /* Step 2: Get second molecule string length and allocate memory */
    ptrdiff_t molstring2_len = 0;
    env->copy_string_contents(env, molstring2_arg, NULL, &molstring2_len);
    char *molstring2 = malloc(molstring2_len);
    if (!molstring2) {
        free(molstring1);
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, molstring2_arg, molstring2, &molstring2_len);
    
    /* Step 3: Parse both molecules with Indigo */
    int mol1 = indigoLoadMoleculeFromString(molstring1);
    int mol2 = indigoLoadMoleculeFromString(molstring2);
    free(molstring1);
    free(molstring2);
    
    /* Step 4: Check if either molecule failed to parse */
    if (mol1 == -1 || mol2 == -1) {
        if (mol1 != -1) indigoFree(mol1);
        if (mol2 != -1) indigoFree(mol2);
        return env->intern(env, "nil");
    }
    
    /* Step 5: Execute the operation on both molecules */
    emacs_value result = operation(env, mol1, mol2);
    
    /* Step 6: Clean up Indigo objects */
    indigoFree(mol1);
    indigoFree(mol2);
    
    /* Step 7: Return the result */
    return result;
}

/* Utility function for substructure matching - query molecule loaded as query type */
emacs_value substructure_match_op(emacs_env *env,
                                  emacs_value target_arg,
                                  emacs_value query_arg,
                                  dual_mol_operation operation)
{
    /* Step 1: Get target molecule string and allocate memory */
    ptrdiff_t target_len = 0;
    env->copy_string_contents(env, target_arg, NULL, &target_len);
    char *target_string = malloc(target_len);
    if (!target_string) {
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, target_arg, target_string, &target_len);
    
    /* Step 2: Get query molecule string and allocate memory */
    ptrdiff_t query_len = 0;
    env->copy_string_contents(env, query_arg, NULL, &query_len);
    char *query_string = malloc(query_len);
    if (!query_string) {
        free(target_string);
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, query_arg, query_string, &query_len);
    
    /* Step 3: Parse target as regular molecule, query as query molecule */
    int target_mol = indigoLoadMoleculeFromString(target_string);
    int query_mol = indigoLoadQueryMoleculeFromString(query_string);
    free(target_string);
    free(query_string);
    
    /* Step 4: Check if either molecule failed to parse */
    if (target_mol == -1 || query_mol == -1) {
        if (target_mol != -1) indigoFree(target_mol);
        if (query_mol != -1) indigoFree(query_mol);
        return env->intern(env, "nil");
    }
    
    /* Step 5: Execute the operation */
    emacs_value result = operation(env, target_mol, query_mol);
    
    /* Step 6: Clean up */
    indigoFree(target_mol);
    indigoFree(query_mol);
    
    /* Step 7: Return result */
    return result;
}

/* Utility function for reaction string operations */
emacs_value reactionstring_op(emacs_env *env,
                              emacs_value rxnstring_arg,
                              reaction_operation operation)
{
    /* Step 1: Get reaction string and allocate memory */
    ptrdiff_t len = 0;
    env->copy_string_contents(env, rxnstring_arg, NULL, &len);
    char *reaction_string = malloc(len);
    if (!reaction_string) {
        return env->intern(env, "nil");
    }
    env->copy_string_contents(env, rxnstring_arg, reaction_string, &len);
    
    /* Step 2: Execute the operation */
    emacs_value result = operation(env, reaction_string);
    
    /* Step 3: Clean up */
    free(reaction_string);
    
    /* Step 4: Return result */
    return result;
}