/**
 * @file p3-analysis.c
 * @brief Compiler phase 3: static analysis
 * @author Mia Pham and Tesha Yeboah
 */
#include "p3-analysis.h"

/**
 * @brief State/data for static analysis visitor
 */
typedef struct AnalysisData
{
    /**
     * @brief List of errors detected
     */
    ErrorList* errors;
    int loop_count;

    /* BOILERPLATE: TODO: add any new desired state information (and clean it up in AnalysisData_free) */

} AnalysisData;

/**
 * @brief Allocate memory for analysis data
 * 
 * @returns Pointer to allocated structure
 */
AnalysisData* AnalysisData_new ()
{
    AnalysisData* data = (AnalysisData*)calloc(1, sizeof(AnalysisData));
    CHECK_MALLOC_PTR(data);
    data->errors = ErrorList_new();
    return data;
}

/**
 * @brief Deallocate memory for analysis data
 * 
 * @param data Pointer to the structure to be deallocated
 */
void AnalysisData_free (AnalysisData* data)
{
    /* free everything in data that is allocated on the heap except the error
     * list; it needs to be returned after the analysis is complete */

    /* free "data" itself */
    free(data);
}

/**
 * @brief Macro for more convenient access to the data inside a @ref AnalysisVisitor
 * data structure
 */
#define DATA ((AnalysisData*)visitor->data)

/**
 * @brief Macro for more convenient access to the error list inside a
 * @ref AnalysisVisitor data structure
 */
#define ERROR_LIST (((AnalysisData*)visitor->data)->errors)

/**
 * @brief Wrapper for @ref lookup_symbol that reports an error if the symbol isn't found
 * 
 * @param visitor Visitor with the error list for reporting
 * @param node AST node to begin the search at
 * @param name Name of symbol to find
 * @returns The @ref Symbol if found, otherwise @c NULL
 */
Symbol* lookup_symbol_with_reporting(NodeVisitor* visitor, ASTNode* node, const char* name)
{
    Symbol* symbol = lookup_symbol(node, name);
    if (symbol == NULL) {
        ErrorList_printf(ERROR_LIST, "Symbol '%s' undefined on line %d", name, node->source_line);
    }
    return symbol;
}

/**
 * @brief Macro for shorter storing of the inferred @c type attribute
 */
#define SET_INFERRED_TYPE(T) ASTNode_set_printable_attribute(node, "type", (void*)(T), \
                                 type_attr_print, dummy_free)

/**
 * @brief Macro for shorter retrieval of the inferred @c type attribute
 */
#define GET_INFERRED_TYPE(N) (DecafType)(long)ASTNode_get_attribute(N, "type")

void Analysis_previsit_literal (NodeVisitor* visitor, ASTNode* node)
{
    SET_INFERRED_TYPE(node->literal.type);
}

void Analysis_postvisit_binaryop (NodeVisitor* visitor, ASTNode* node)
{
    printf("left: %s right: %s\n", 
        DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.left)),
        DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.right)));
}

void AnalysisVisitor_check_vardecl (NodeVisitor* visitor, ASTNode* node)
{
    if (strcmp(DecafType_to_string(node->vardecl.type), "void") == 0) {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", 
            node->vardecl.name, node->source_line);
    }

    // checking for array declarations
    if (node->vardecl.is_array) {

        if (node->vardecl.array_length == 0) {
            ErrorList_printf(ERROR_LIST, "Array '%s' on line %d must have positive non-zero lengthArray variable", 
            node->vardecl.name, node->source_line);
        }
    }
}

void AnalysisVisitor_postvisit_location(NodeVisitor* visitor, ASTNode* node)
{
    lookup_symbol_with_reporting(visitor, node, node->location.name);
}

void AnalysisVisitor_check_main (NodeVisitor* visitor, ASTNode* node)
{
    Symbol* symbol = lookup_symbol(node, "main");
    if (symbol == NULL) {
        ErrorList_printf(ERROR_LIST, "Program does not contain a 'main' function");
    }
}

void AnalysisVisitor_postvisit_conditional (NodeVisitor* visitor, ASTNode* node)
{
    const char* cond_type = DecafType_to_string(GET_INFERRED_TYPE(node->conditional.condition));
    if (strcmp(cond_type, "bool") != 0) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: bool expected but %s found on line %d", 
            cond_type, node->source_line);
    }
}

void Analysis_previsit_loop (NodeVisitor* visitor, ASTNode* node)
{
    DATA->loop_count++;
}

void Analysis_postvisit_loop (NodeVisitor* visitor, ASTNode* node)
{
    const char* cond_type = DecafType_to_string(GET_INFERRED_TYPE(node->whileloop.condition));
    if (strcmp(cond_type, "bool") != 0) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: bool expected but %s found on line %d", 
            cond_type, node->source_line);
    }
}

void Analysis_postvisit_break (NodeVisitor* visitor, ASTNode* node)
{
    if (DATA->loop_count < 1) {
        ErrorList_printf(ERROR_LIST, "Invalid 'break' outside of loop on line %d", node->source_line);
    }
}

void Analysis_postvisit_continue (NodeVisitor* visitor, ASTNode* node)
{
    if (DATA->loop_count < 1) {
        ErrorList_printf(ERROR_LIST, "Invalid 'continue' outside of loop on line %d", node->source_line);
    }
}

// void Analysis_postvisit_location (NodeVisitor* visitor, ASTNode* node)
// {
//     const char* index_type = DecafType_to_string(GET_INFERRED_TYPE(node->location.index));
//     if (strcmp(index_type, "int") != 0) {
//         ErrorList_printf(ERROR_LIST, "Invalid array size '%s' on line %d", 
//             index_type, node->source_line);
//     }
// }

// void Analysis_postvisit_unary_op (NodeVisitor* visitor, ASTNode* node)
// {
//     // change hard coding bool
//     const char* unary_type = DecafType_to_string(GET_INFERRED_TYPE(node->unaryop.child));
//     if (strcmp(unary_type, "bool") != 0) {
//         ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with bool on line %d", 
//             unary_type, node->source_line);
//     }
// }

ErrorList* analyze (ASTNode* tree)
{
    if (tree == NULL) {
        Error_throw_printf("The AST tree is NULL\n");
    }
    
    /* allocate analysis structures */
    NodeVisitor* v = NodeVisitor_new();
    v->data = (void*) AnalysisData_new();
    v->dtor = (Destructor)AnalysisData_free;

    /* BOILERPLATE: TODO: register analysis callbacks */
    v->previsit_vardecl = AnalysisVisitor_check_vardecl;
    v->previsit_location = AnalysisVisitor_postvisit_location;
    v->previsit_program = AnalysisVisitor_check_main;
    v->previsit_literal = Analysis_previsit_literal;
    v->postvisit_conditional = AnalysisVisitor_postvisit_conditional;
    v->postvisit_whileloop = Analysis_postvisit_loop;
    v->postvisit_break = Analysis_postvisit_break;
    v->postvisit_continue = Analysis_postvisit_continue;
    // v->postvisit_location = Analysis_postvisit_location;
    v->postvisit_binaryop = Analysis_postvisit_binaryop;
    // v->postvisit_unaryop = Analysis_postvisit_unary_op;

    /* perform analysis, save error list, clean up, and return errors */
    NodeVisitor_traverse(v, tree);
    ErrorList* errors = ((AnalysisData*)v->data)->errors;
    NodeVisitor_free(v);
    return errors;
}

