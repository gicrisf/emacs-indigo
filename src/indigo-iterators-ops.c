/*
 * indigo-iterators-ops.c - Iterator operation functions
 *
 * This file contains iterator operations for traversing molecular structures
 * including atoms, bonds, rings, components, and various molecular features.
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