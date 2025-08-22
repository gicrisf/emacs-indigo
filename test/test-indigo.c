#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "indigo.h"

int main() {
    printf("=== Indigo Installation Test ===\n");

    // Test basic Indigo functionality
    printf("Testing Indigo library...\n");
    
    // Get and display Indigo version
    const char* version = indigoVersion();
    if (version) {
        printf("Indigo version: %s\n", version);
    } else {
        printf("Error: Could not get Indigo version\n");
        return 1;
    }
    
    // Test molecule creation from SMILES
    const char* smiles = "CCO";  // Ethanol
    printf("Creating molecule from SMILES: %s\n", smiles);
    
    int mol = indigoLoadMoleculeFromString(smiles);
    if (mol == -1) {
        printf("Error: Failed to create molecule from SMILES\n");
        printf("Error message: %s\n", indigoGetLastError());
        return 1;
    }
    
    // Test molecular formula calculation
    int formula_handle = indigoGrossFormula(mol);
    const char* formula = indigoToString(formula_handle);
    printf("Molecular formula: %s\n", formula);
    
    // Verify expected result
    if (formula && strcmp(formula, "C2 H6 O") == 0) {
        printf("✓ Molecular formula calculation: PASSED\n");
    } else {
        printf("✗ Molecular formula calculation: FAILED (expected 'C2 H6 O', got '%s')\n", formula ? formula : "NULL");
        indigoFree(formula_handle);
        indigoFree(mol);
        return 1;
    }
    
    // Clean up
    indigoFree(formula_handle);
    indigoFree(mol);
    
    printf("\n✓ Indigo installation test completed successfully!\n");
    printf("  The Indigo library is properly installed and functional.\n");
    return 0;
}