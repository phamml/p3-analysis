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

/**
 * Helper functions
*/

/**
 * Traverse through symbol table to check if there are any duplicate symbols
*/
void check_duplicates(NodeVisitor* visitor, ASTNode* node)
{
    SymbolTable*table = (SymbolTable*) ASTNode_get_attribute(node, "symbolTable");
    for (Symbol* s = table->local_symbols->head; s != NULL; s = s->next) {
        for (Symbol* s2 = s->next; s2 != NULL; s2 = s2->next) {
            if (strcmp(s->name, s2->name) == 0) {
                ErrorList_printf(ERROR_LIST, "Duplicate symbols named '%s' in scope started on line %d",
                    s->name, node->source_line);
            }
        }
    }
}

/* 
* Helper method for checking undefined location node / function call node.
*/
bool check_undefined_loc_funccall (NodeVisitor* visitor, ASTNode* node)
{
    if (node->type == LOCATION) {
        Symbol* loc_sym = lookup_symbol(node, node->location.name);
        if (loc_sym == NULL) {
            return true;
        }
    } else if (node->type == FUNCCALL) {
        Symbol* funccall_sym = lookup_symbol(node, node->funccall.name);
        if (funccall_sym == NULL) {
            return true;
        }
    }

    return false;
}

/**
 * Functions for static analysis
 */

void AnalysisVisitor_previsit_literal (NodeVisitor* visitor, ASTNode* node)
{
    SET_INFERRED_TYPE(node->literal.type);
}

void AnalysisVisitor_previsit_unaryop (NodeVisitor* visitor, ASTNode* node) 
{
    UnaryOpType op = node->unaryop.operator;
    switch (op) {
        case NEGOP:
        SET_INFERRED_TYPE(INT);
                break;
        case NOTOP:
            SET_INFERRED_TYPE(BOOL);
                break;
    }
}

void AnalysisVisitor_postvisit_unaryop (NodeVisitor* visitor, ASTNode* node)
{
    if (check_undefined_loc_funccall(visitor, node->unaryop.child)) {
        return;
    }   

    const char* bin_type = DecafType_to_string(GET_INFERRED_TYPE(node));
    const char* child_type = DecafType_to_string(GET_INFERRED_TYPE(node->unaryop.child));

    // child must be same type as unary op type
    if (strcmp(bin_type, child_type) != 0) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
            bin_type, child_type, node->source_line);
        return;
    }
}

void AnalysisVisitor_previsit_binaryop (NodeVisitor* visitor, ASTNode* node)
{
    BinaryOpType op = node->binaryop.operator;
    switch (op) {
        case OROP:
        case ANDOP:
        case EQOP:
        case NEQOP:
        case LTOP:
        case LEOP:
        case GEOP:
        case GTOP:
            SET_INFERRED_TYPE(BOOL);
                break;
        case ADDOP:
        case SUBOP:
        case MULOP:
        case DIVOP:
        case MODOP:
            SET_INFERRED_TYPE(INT);
            break;
    }
}

void AnalysisVisitor_postvisit_binaryop (NodeVisitor* visitor, ASTNode* node)
{
    // checking left side
    if (check_undefined_loc_funccall(visitor, node->binaryop.left)) {
        return;
    }

    // checking right side
    if (check_undefined_loc_funccall(visitor, node->binaryop.right)) {
        return;
    }
    const char* left_type = DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.left));
    const char* right_type = DecafType_to_string(GET_INFERRED_TYPE(node->binaryop.right));
    BinaryOpType op = node->binaryop.operator;
    switch (op) {
        // left and right child must be type bool
        case OROP:
        case ANDOP:
            if (strcmp("bool", left_type) != 0) {
                ErrorList_printf(ERROR_LIST, "Type mismatch: bool expected but %s found on line %d",
                    left_type, node->source_line);
            }
            if (strcmp("bool", right_type) != 0) {
                ErrorList_printf(ERROR_LIST, "Type mismatch: bool expected but %s found on line %d",
                    right_type, node->source_line);
            }
            break;
        // left and right child must be the same type
        case EQOP:
        case NEQOP:
            if (strcmp(left_type, right_type) != 0) {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d",
                    left_type, right_type, node->source_line);
            }
            break;
        // left and right child must be type int 
        case ADDOP:
        case SUBOP:
        case MULOP:
        case DIVOP:
        case MODOP:
        case LTOP:
        case LEOP:
        case GEOP:
        case GTOP:
            if (strcmp("int", left_type) != 0) {
                ErrorList_printf(ERROR_LIST, "Type mismatch: int expected but %s found on line %d",
                    left_type, node->source_line);
            }
            if (strcmp("int", right_type) != 0) {
                ErrorList_printf(ERROR_LIST, "Type mismatch: int expected but %s found on line %d",
                    right_type, node->source_line);
            }
            break;
    }
}

void AnalysisVisitor_previsit_vardecl (NodeVisitor* visitor, ASTNode* node)
{
    // no void var allowed
    if (strcmp(DecafType_to_string(node->vardecl.type), "void") == 0) {
        ErrorList_printf(ERROR_LIST, "Void variable '%s' on line %d", 
            node->vardecl.name, node->source_line);
        return;
    }

    //  invalid main var if main var is global
    int depth = ASTNode_get_int_attribute(node, "depth");
    if (strcmp(node->vardecl.name, "main") == 0 && depth == 1) {
        ErrorList_printf(ERROR_LIST, "'main' must be a function");
        return;
    }

    // checking for array declarations
    if (node->vardecl.is_array) {
        if (node->vardecl.array_length == 0) {
            ErrorList_printf(ERROR_LIST, "Array '%s' on line %d must have positive non-zero lengthArray variable", 
                node->vardecl.name, node->source_line);
            return;
        }

        if (depth != 1) {
            ErrorList_printf(ERROR_LIST, "Local variable '%s' on line %d cannot be an array, arrays must be global", 
                node->vardecl.name, node->source_line);
            return;
        }
    }
}

void AnalysisVisitor_postvisit_assignment (NodeVisitor* visitor, ASTNode* node)
{
    Symbol* symbol = lookup_symbol(node, node->assignment.location->location.name);
    if (symbol == NULL) {
        return;
    }

    // undefined loc or funccall check
    if (check_undefined_loc_funccall(visitor, node->assignment.value)) {
        return;
    }


    // check for type mismatch for variables
    const char* var_type = DecafType_to_string(symbol->type);
    const char* val_type = DecafType_to_string(GET_INFERRED_TYPE(node->assignment.value));
    if (strcmp(var_type, val_type) != 0) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: %s is incompatible with %s on line %d", 
            var_type, val_type, node->source_line);
        return;
    }
}

void AnalysisVisitor_previsit_location(NodeVisitor* visitor, ASTNode* node)
{
    Symbol* sym = lookup_symbol(node, node->location.name);
    if (sym == NULL) {
        return;
    }
    SET_INFERRED_TYPE(sym->type);
}

void AnalysisVisitor_postvisit_location(NodeVisitor* visitor, ASTNode* node)
{
    Symbol* sym = lookup_symbol_with_reporting(visitor, node, node->location.name);
    if (sym == NULL) {
        return;
    }

    if (sym->symbol_type == ARRAY_SYMBOL) {
        if (node->location.index == NULL) {
            ErrorList_printf(ERROR_LIST, "Array '%s' accessed without index on line %d", 
                node->location.name, node->source_line);
        } else if (node->location.index->type == LOCATION) {
            Symbol* loc_sym = lookup_symbol_with_reporting(visitor, node, node->location.name);
            if (loc_sym == NULL) {
                return;
            }
        } else if (node->location.index->type == FUNCCALL) {
            Symbol* funccal_sym = lookup_symbol_with_reporting(visitor, node, node->location.name);
            if (funccal_sym == NULL) {
                return;
            }
        } else {
        // check type of the index
            const char* ind_type = DecafType_to_string(GET_INFERRED_TYPE(node->location.index));
            if (strcmp(ind_type, "int") != 0) {
                // error if index type is not an int
                ErrorList_printf(ERROR_LIST, "Index Type mismatch: int expected but %s found on line %d for node %s", 
                    ind_type, node->source_line, node->location.name);
            }
        }
    }
}

void AnalysisVisitor_postvisit_program (NodeVisitor* visitor, ASTNode* node)
{
    // check that program contains a main function
    Symbol* symbol = lookup_symbol(node, "main");
    if (symbol == NULL) {
        ErrorList_printf(ERROR_LIST, "Program does not contain a 'main' function");
        return;
    }

    check_duplicates(visitor, node);
}

void AnalysisVisitor_postvisit_conditional (NodeVisitor* visitor, ASTNode* node)
{
    if (check_undefined_loc_funccall(visitor, node->conditional.condition)) {
        return;
    }
    // expression inside if condition must be type bool
    const char* cond_type = DecafType_to_string(GET_INFERRED_TYPE(node->conditional.condition));
    if (strcmp(cond_type, "bool") != 0) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: bool expected but %s found on line %d", 
            cond_type, node->source_line);
    }
}

void AnalysisVisitor_previsit_loop (NodeVisitor* visitor, ASTNode* node)
{
    DATA->loop_count++;
}

void AnalysisVisitor_postvisit_loop (NodeVisitor* visitor, ASTNode* node)
{
    if (check_undefined_loc_funccall(visitor, node->whileloop.condition)) {
        return;
    }
    DATA->loop_count--;
    // expression inside while condition must be type bool
    const char* cond_type = DecafType_to_string(GET_INFERRED_TYPE(node->whileloop.condition));
    if (strcmp(cond_type, "bool") != 0) {
        ErrorList_printf(ERROR_LIST, "Type mismatch: bool expected but %s found on line %d", 
            cond_type, node->source_line);
        return;
    }
}

void AnalysisVisitor_postvisit_break (NodeVisitor* visitor, ASTNode* node)
{
    // break must be inside loop
    if (DATA->loop_count < 1) {
        ErrorList_printf(ERROR_LIST, "Invalid 'break' outside of loop on line %d", node->source_line);
        return;
    }
}

void AnalysisVisitor_postvisit_continue (NodeVisitor* visitor, ASTNode* node)
{
    // continue must be inside loop
    if (DATA->loop_count < 1) {
        ErrorList_printf(ERROR_LIST, "Invalid 'continue' outside of loop on line %d", node->source_line);
        return;
    }
}

void AnalysisVisitor_previsit_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    // check that main function has no params
    if (strcmp(node->funcdecl.name, "main") == 0 && node->funcdecl.parameters->size != 0) {
        ErrorList_printf(ERROR_LIST, "'main' must take no parameters");
        return;
    }

     // checks that main returns an int
    if (strcmp(node->funcdecl.name, "main") == 0 && node->funcdecl.return_type != INT) {
        ErrorList_printf(ERROR_LIST, "'main' must return an integer");
        return;
    }

}

void AnalysisVisitor_postvisit_funcdecl (NodeVisitor* visitor, ASTNode* node)
{
    check_duplicates(visitor, node);
}


void AnalysisVisitor_previsit_funccall (NodeVisitor* visitor, ASTNode* node)
{
    Symbol* sym = lookup_symbol(node, node->funccall.name);
    if (sym == NULL) {
       return;
    }
    
   SET_INFERRED_TYPE(sym->type);
}

void AnalysisVisitor_postvisit_funccall (NodeVisitor* visitor, ASTNode* node)
{
    Symbol* sym = lookup_symbol_with_reporting(visitor, node, node->funccall.name);
    if (sym == NULL) {
        return;
    }

    if (sym->symbol_type != FUNCTION_SYMBOL) {
        ErrorList_printf(ERROR_LIST, "Invalid call to non-function '%s' on line %d", node->funccall.name, node->source_line);
        return;
    }
    const char* param_type = NULL;
    const char* arg_type = NULL;
    ASTNode* curr_arg = node->funccall.arguments->head;
    ParameterList* param_list = lookup_symbol(node, node->funccall.name)->parameters;
    Parameter* curr_param = param_list->head;
    int param_num = 0;  // to keep track of where in param list

    // check that number of args matches number of parameters expected
    if (node->funccall.arguments->size != param_list->size) {
        ErrorList_printf(ERROR_LIST, "Invalid number of function arguments on line %d", node->source_line);
        return;
    }
    // check for type mismatches in args passed to funncall
    while (curr_arg != NULL) {
        if (curr_param == NULL) {
            return;
        }
        param_type = DecafType_to_string(curr_param->type);
        arg_type = DecafType_to_string(GET_INFERRED_TYPE(curr_arg));

        if (strcmp(param_type, arg_type) != 0) {
            ErrorList_printf(ERROR_LIST, "Type mismatch in parameter %d of call to '%s': expected %s but found %s on line %d", 
                param_num, node->funccall.name, param_type, arg_type, node->source_line);
            return;
        }
        curr_arg = curr_arg->next;
        curr_param = curr_param->next;
        param_num++;
    }
}


void AnalysisVisitor_postvisit_block (NodeVisitor* visitor, ASTNode* node)
{
    // get symbol table attribute from node and traverse through table to find duplicates
    check_duplicates(visitor, node);
}

void AnalysisVisitor_postvisit_return (NodeVisitor* visitor, ASTNode* node) 
{
    // getting parent node
    ASTNode* grand = (ASTNode*) ASTNode_get_attribute(node, "parent");

    // getting to funcdecl node.
    while (grand->type != FUNCDECL)  {
        grand = (ASTNode*) ASTNode_get_attribute(grand, "parent");
    }

    // checks if function returns void for a non-void funcdecl
    if (node->funcreturn.value == NULL && grand->funcdecl.return_type != VOID) {
        ErrorList_printf(ERROR_LIST, "Invalid void return from non-void function on line %d", node->source_line);
        return;
    // to make sure it does not print for undefined variables which is handled elsewhere
    // filters valid void returns
    } else if (node->funcreturn.value != NULL && ASTNode_has_attribute (node->funcreturn.value, "type")) {
        if (GET_INFERRED_TYPE(node->funcreturn.value) == INT || GET_INFERRED_TYPE(node->funcreturn.value) == BOOL ) {
            if (strcmp(DecafType_to_string(grand->funcdecl.return_type), DecafType_to_string(GET_INFERRED_TYPE(node->funcreturn.value))) != 0) {
                ErrorList_printf(ERROR_LIST, "Type mismatch: %s expected but %s found on line %d",
                    DecafType_to_string(grand->funcdecl.return_type), DecafType_to_string(GET_INFERRED_TYPE(node->funcreturn.value)), node->source_line);
            }
        }
    }
}

ErrorList* analyze (ASTNode* tree)
{
    if (tree == NULL) {
        fprintf(stderr, "Error: tree is null\n");
        exit(0);
    }
    
    /* allocate analysis structures */
    NodeVisitor* v = NodeVisitor_new();
    v->data = (void*) AnalysisData_new();
    v->dtor = (Destructor)AnalysisData_free;

    /* BOILERPLATE: TODO: register analysis callbacks */
    v->previsit_funcdecl = AnalysisVisitor_previsit_funcdecl;
    v->previsit_vardecl = AnalysisVisitor_previsit_vardecl;
    v->previsit_location = AnalysisVisitor_previsit_location;
    v->previsit_whileloop = AnalysisVisitor_previsit_loop;
    v->previsit_literal = AnalysisVisitor_previsit_literal;
    v->previsit_binaryop = AnalysisVisitor_previsit_binaryop;
    v->previsit_unaryop = AnalysisVisitor_previsit_unaryop;
    v->previsit_funccall = AnalysisVisitor_previsit_funccall;

    v->postvisit_program = AnalysisVisitor_postvisit_program;
    v->postvisit_funcdecl = AnalysisVisitor_postvisit_funcdecl;
    v->postvisit_location = AnalysisVisitor_postvisit_location;
    v->postvisit_conditional = AnalysisVisitor_postvisit_conditional;
    v->postvisit_whileloop = AnalysisVisitor_postvisit_loop;
    v->postvisit_binaryop = AnalysisVisitor_postvisit_binaryop;
    v->postvisit_unaryop = AnalysisVisitor_postvisit_unaryop;
    v->postvisit_funccall = AnalysisVisitor_postvisit_funccall;
    v->postvisit_break = AnalysisVisitor_postvisit_break;
    v->postvisit_continue = AnalysisVisitor_postvisit_continue;
    v->postvisit_assignment = AnalysisVisitor_postvisit_assignment;
    v->postvisit_block = AnalysisVisitor_postvisit_block;
    v->postvisit_return = AnalysisVisitor_postvisit_return;

    /* perform analysis, save error list, clean up, and return errors */
    NodeVisitor_traverse(v, tree);
    ErrorList* errors = ((AnalysisData*)v->data)->errors;
    NodeVisitor_free(v);
    return errors;
}

