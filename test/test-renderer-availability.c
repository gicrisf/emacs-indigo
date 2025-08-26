#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>  // for access()
#include "indigo.h"
#include "indigo-renderer.h"

// Helper function to check if a file exists and has content
int check_file_exists_with_content(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) return 0;
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fclose(file);
    
    return size > 0;
}

// Helper function to safely get error message
const char* get_safe_error() {
    const char* error = indigoGetLastError();
    return error ? error : "no error reported";
}

int main() {
    printf("=== Indigo Rendering Module Installation Test ===\n");
    
    // First verify basic Indigo works
    printf("\n1. Testing basic Indigo functionality...\n");
    const char* version = indigoVersion();
    if (version) {
        printf("   Indigo version: %s\n", version);
    } else {
        printf("   ✗ CRITICAL: Basic Indigo not working\n");
        return 1;
    }
    
    // Test molecule creation (prerequisite for rendering)
    printf("\n2. Testing molecule creation...\n");
    int mol = indigoLoadMoleculeFromString("CCO");
    if (mol == -1) {
        printf("   ✗ CRITICAL: Cannot create molecules - %s\n", get_safe_error());
        return 1;
    }
    printf("   ✓ Molecule creation: SUCCESS (handle %d)\n", mol);
    
    // Test writer creation functions
    printf("\n3. Testing writer creation functions...\n");
    
    // Test buffer writer
    printf("   Testing indigoWriteBuffer()...\n");
    indigoGetLastError(); // Clear previous errors
    int buffer_writer = indigoWriteBuffer();
    if (buffer_writer == -1) {
        printf("   ✗ indigoWriteBuffer: FAILED - %s\n", get_safe_error());
    } else {
        printf("   ✓ indigoWriteBuffer: SUCCESS (handle %d)\n", buffer_writer);
        indigoFree(buffer_writer);
    }
    
    // Test file writer
    printf("   Testing indigoWriteFile()...\n");
    indigoGetLastError(); // Clear
    int file_writer = indigoWriteFile("test_output.tmp");
    if (file_writer == -1) {
        printf("   ✗ indigoWriteFile: FAILED - %s\n", get_safe_error());
    } else {
        printf("   ✓ indigoWriteFile: SUCCESS (handle %d)\n", file_writer);
        indigoFree(file_writer);
        // Clean up temp file
        remove("test_output.tmp");
    }
    
    // Test array creation (for grid rendering)
    printf("\n4. Testing array creation functions...\n");
    printf("   Testing indigoCreateArray()...\n");
    indigoGetLastError(); // Clear
    int array = indigoCreateArray();
    if (array == -1) {
        printf("   ✗ indigoCreateArray: FAILED - %s\n", get_safe_error());
    } else {
        printf("   ✓ indigoCreateArray: SUCCESS (handle %d)\n", array);
        
        // Test array add (according to indigo.h, it auto-clones internally)
        printf("   Testing indigoArrayAdd() with original molecule...\n");
        indigoGetLastError(); // Clear
        
        // Check molecule is valid before adding
        const char* mol_smiles = indigoCanonicalSmiles(mol);
        if (mol_smiles) {
            printf("   → Molecule SMILES: %s\n", mol_smiles);
        } else {
            printf("   → Warning: Cannot get SMILES from molecule\n");
        }
        
        int add_result = indigoArrayAdd(array, mol);
        printf("   → indigoArrayAdd returned: %d\n", add_result);
        if (add_result != 1) {
            printf("   ✗ indigoArrayAdd: FAILED - %s\n", get_safe_error());
            
            // Try with a fresh molecule  
            printf("   → Trying with fresh molecule...\n");
            indigoGetLastError(); // Clear
            int fresh_mol = indigoLoadMoleculeFromString("CCO");
            if (fresh_mol != -1) {
                printf("   → Fresh molecule handle: %d\n", fresh_mol);
                int fresh_add = indigoArrayAdd(array, fresh_mol);
                printf("   → Fresh add result: %d\n", fresh_add);
                if (fresh_add == 1) {
                    printf("   ✓ Fresh molecule add: SUCCESS\n");
                } else {
                    printf("   ✗ Fresh molecule add: FAILED - %s\n", get_safe_error());
                }
                indigoFree(fresh_mol); // Safe to free - array has clone
            }
        } else {
            printf("   ✓ indigoArrayAdd: SUCCESS\n");
            
            // Test array count
            printf("   Testing indigoCount() on array...\n");
            int count = indigoCount(array);
            if (count == -1) {
                printf("      ✗ indigoCount: FAILED - %s\n", get_safe_error());
            } else {
                printf("      ✓ indigoCount: SUCCESS (count: %d)\n", count);
                
                // Test array access
                if (count > 0) {
                    printf("      Testing indigoAt() on array...\n");
                    int first_item = indigoAt(array, 0);
                    if (first_item == -1) {
                        printf("         ✗ indigoAt: FAILED - %s\n", get_safe_error());
                    } else {
                        printf("         ✓ indigoAt: SUCCESS (handle %d)\n", first_item);
                    }
                }
            }
        }
        
        indigoFree(array);
    }
    
    // Test rendering functions (this is where we expect failures)
    printf("\n5. Testing rendering functions...\n");
    
    // Test render reset
    printf("   Testing indigoRenderReset()...\n");
    indigoGetLastError(); // Clear
    int reset_result = indigoRenderReset();
    if (reset_result != 1) {
        printf("   ✗ indigoRenderReset: FAILED - %s\n", get_safe_error());
        printf("   → This indicates the renderer library is NOT available\n");
    } else {
        printf("   ✓ indigoRenderReset: SUCCESS\n");
    }
    
    // Test basic rendering to buffer (need to set format first)
    printf("   Testing indigoRender() to buffer...\n");
    
    // Set render format before rendering
    indigoSetOption("render-output-format", "png");
    
    int buffer_writer2 = indigoWriteBuffer();
    if (buffer_writer2 != -1) {
        indigoGetLastError(); // Clear
        int render_result = indigoRender(mol, buffer_writer2);
        if (render_result != 1) {
            printf("   ✗ indigoRender: FAILED - %s\n", get_safe_error());
            printf("      → This usually means render format not set properly\n");
        } else {
            printf("   ✓ indigoRender: SUCCESS\n");
            
            // Test buffer extraction
            printf("   Testing indigoToBuffer()...\n");
            char* buffer_data = NULL;
            int buffer_size = 0;
            indigoGetLastError(); // Clear
            int buffer_result = indigoToBuffer(buffer_writer2, &buffer_data, &buffer_size);
            if (buffer_result != 1 || !buffer_data || buffer_size <= 0) {
                printf("   ✗ indigoToBuffer: FAILED - %s\n", get_safe_error());
            } else {
                printf("   ✓ indigoToBuffer: SUCCESS (size: %d bytes)\n", buffer_size);
                // Don't print binary PNG data
                if (buffer_size >= 8 && strncmp(buffer_data, "\x89PNG\r\n\x1a\n", 8) == 0) {
                    printf("      → Valid PNG header detected\n");
                } else {
                    printf("      → Buffer preview: %.50s%s\n", buffer_data, buffer_size > 50 ? "..." : "");
                }
            }
        }
        indigoFree(buffer_writer2);
    }
    
    // Test direct file rendering with different formats
    printf("   Testing indigoRenderToFile() with PNG...\n");
    indigoSetOption("render-output-format", "png");
    const char* test_file_png = "test_render_output.png";
    indigoGetLastError(); // Clear
    int file_render_result = indigoRenderToFile(mol, test_file_png);
    if (file_render_result != 1) {
        printf("   ✗ indigoRenderToFile (PNG): FAILED - %s\n", get_safe_error());
    } else {
        printf("   ✓ indigoRenderToFile (PNG): SUCCESS\n");
        if (check_file_exists_with_content(test_file_png)) {
            FILE* f = fopen(test_file_png, "rb");
            if (f) {
                char header[8];
                fread(header, 1, 8, f);
                fclose(f);
                if (strncmp(header, "\x89PNG\r\n\x1a\n", 8) == 0) {
                    printf("      → Valid PNG file created\n");
                } else {
                    printf("      → File created but invalid PNG header\n");
                }
            }
        } else {
            printf("      → Output file missing or empty\n");
        }
        remove(test_file_png); // Clean up
    }
    
    // Test SVG rendering
    printf("   Testing indigoRenderToFile() with SVG...\n");
    indigoSetOption("render-output-format", "svg");
    const char* test_file_svg = "test_render_output.svg";
    indigoGetLastError(); // Clear
    int svg_render_result = indigoRenderToFile(mol, test_file_svg);
    if (svg_render_result != 1) {
        printf("   ✗ indigoRenderToFile (SVG): FAILED - %s\n", get_safe_error());
    } else {
        printf("   ✓ indigoRenderToFile (SVG): SUCCESS\n");
        if (check_file_exists_with_content(test_file_svg)) {
            FILE* f = fopen(test_file_svg, "r");
            if (f) {
                char header[6];
                fread(header, 1, 5, f);
                header[5] = '\0';
                fclose(f);
                if (strncmp(header, "<?xml", 5) == 0 || strncmp(header, "<svg", 4) == 0) {
                    printf("      → Valid SVG file created\n");
                } else {
                    printf("      → File created but unusual SVG format\n");
                }
            }
        } else {
            printf("      → Output file missing or empty\n");
        }
        remove(test_file_svg); // Clean up
    }
    
    // Test grid rendering with proper setup
    printf("   Testing indigoRenderGrid()...\n");
    
    // Set up for grid rendering
    indigoSetOption("render-output-format", "png");
    indigoSetOption("render-image-width", "600");
    indigoSetOption("render-image-height", "400");
    
    int grid_array = indigoCreateArray();
    if (grid_array != -1) {
        // Add molecule directly (cloning doesn't work with arrays)
        int add_result = indigoArrayAdd(grid_array, mol);
        if (add_result == 1) {
            int grid_writer = indigoWriteBuffer();
            if (grid_writer != -1) {
                indigoGetLastError(); // Clear
                int grid_result = indigoRenderGrid(grid_array, NULL, 1, grid_writer);
                if (grid_result != 1) {
                    printf("   ✗ indigoRenderGrid: FAILED - %s\n", get_safe_error());
                } else {
                    printf("   ✓ indigoRenderGrid: SUCCESS\n");
                    
                    // Test grid buffer extraction
                    char* grid_buffer = NULL;
                    int grid_size = 0;
                    if (indigoToBuffer(grid_writer, &grid_buffer, &grid_size) == 1 && grid_size > 0) {
                        printf("      → Grid buffer size: %d bytes\n", grid_size);
                    }
                }
                indigoFree(grid_writer);
            }
        } else {
            printf("   → Could not add molecule to array\n");
        }
        indigoFree(grid_array);
    }
    
    // Test comprehensive rendering options
    printf("\n6. Testing rendering options...\n");
    
    // Basic format options
    const char* format_options[][2] = {
        {"render-output-format", "svg"},
        {"render-output-format", "png"},
        {"render-output-format", "pdf"}
    };
    
    printf("   Testing format options...\n");
    for (int i = 0; i < 3; i++) {
        indigoGetLastError(); // Clear
        int option_result = indigoSetOption(format_options[i][0], format_options[i][1]);
        if (option_result != 1) {
            printf("      ✗ %s: FAILED - %s\n", format_options[i][1], get_safe_error());
        } else {
            printf("      ✓ %s: SUCCESS\n", format_options[i][1]);
        }
    }
    
    // Size and appearance options
    const char* appearance_options[][2] = {
        {"render-image-width", "400"},
        {"render-image-height", "300"},
        {"render-bond-length", "25.0"},
        {"render-label-mode", "hetero"},
        {"render-implicit-hydrogens-visible", "false"},
        {"render-coloring", "true"},
        {"render-background-color", "1.0, 1.0, 1.0"}
    };
    
    printf("   Testing appearance options...\n");
    int num_appearance = sizeof(appearance_options) / sizeof(appearance_options[0]);
    for (int i = 0; i < num_appearance; i++) {
        indigoGetLastError(); // Clear
        int option_result = indigoSetOption(appearance_options[i][0], appearance_options[i][1]);
        if (option_result != 1) {
            printf("      ✗ %s: FAILED - %s\n", appearance_options[i][0], get_safe_error());
        } else {
            printf("      ✓ %s: SUCCESS\n", appearance_options[i][0]);
        }
    }
    
    // Test a final render with all options set
    printf("   Testing final render with all options...\n");
    indigoSetOption("render-output-format", "png");
    const char* final_test_file = "test_final_render.png";
    if (indigoRenderToFile(mol, final_test_file) == 1) {
        printf("      ✓ Final render: SUCCESS\n");
        remove(final_test_file);
    } else {
        printf("      ✗ Final render: FAILED - %s\n", get_safe_error());
    }
    
    // Clean up
    indigoFree(mol);
    
    // Summary
    printf("\n=== RENDERING MODULE TEST SUMMARY ===\n");
    printf("RENDERER STATUS:\n");
    if (reset_result == 1) {
        printf("  ✓ Renderer library: AVAILABLE\n");
        printf("  ✓ Basic functions: Working\n");
        if (file_render_result == 1) {
            printf("  ✓ File rendering: Working\n");
        }
        printf("\nRECOMMENDATION: Implement rendering functions in Emacs module\n");
        printf("Key functions to implement:\n");
        printf("  - indigo-render, indigo-render-to-file\n");
        printf("  - indigo-render-grid, indigo-write-buffer\n");
        printf("  - indigo-create-array, indigo-array-add\n");
        printf("  - All rendering option setters\n");
    } else {
        printf("  ✗ Renderer library: NOT AVAILABLE\n");
        printf("\nRECOMMENDATION: Skip rendering functions in Emacs module\n");
        printf("Alternative: Focus on core cheminformatics without visualization\n");
    }
    
    return 0;
}