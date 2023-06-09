/**
 * @file public.c
 * @brief Public test cases (and location for new tests)
 * 
 * This file provides a few basic sanity test cases and a location to add new tests.
 */

#include "testsuite.h"

#ifndef SKIP_IN_DOXYGEN

/*
 * Test a variety of valid programs to make sure the analysis doesn't report
 * an error
 */

TEST_VALID_MAIN(D_trivial, "return 0;")
TEST_VALID_MAIN(C_assign,  "int i; i = 3; return 0;")

/*
 * Test a variety of invalid programs to make sure the analysis reports an
 * error.
 */

TEST_INVALID(D_invalid_no_main,        "int a;")
TEST_INVALID_MAIN(D_invalid_void_var,  "void a;")
TEST_INVALID_MAIN(D_invalid_undef_var, "return a;")
TEST_INVALID_MAIN(C_invalid_break,    "break; return 0;")
TEST_INVALID(C_array_size_O,          "int sums[0]; def int main() { return 0; }")
TEST_INVALID(C_return_type_mismatch,  "def int main() { return true; }")
TEST_INVALID(C_var_type_mismatch,     "int x; def int main() { x=false; return 0; }")
TEST_INVALID(C_bi_return_type_mismatch,  "def int main() { return 3 < 2; }")
TEST_INVALID(C_invalid_conditional,   "def int main() { if (1) { return 0; } }")
TEST_INVALID_MAIN(B_invalid_dup_var_global, "int a; bool b; int a; return 0;")
TEST_INVALID(B_expr_type_mismatch,    "def int main() { int i; i = true+4; return 0; }")
TEST_INVALID(B_mismatched_parameters, "def int main() { foo(true, true); return 0; } "
                                      "def void foo(int i, bool b) { return; } ")

// Added tests ==============================================================================
TEST_INVALID(B_funnccall_of_undefinedvar, "def int main(){ a = 3; return add(a, 2);} def int add(int x, int y){ return x + y;}")
TEST_INVALID(B_array_no_access, "int sums[2]; def int main() { sums = 2; return 0; }")
TEST_INVALID(B_invalid_break_double_loop, "int a; def int main() { while (true) { while (true) { int c; break; } } break; return 0; }")
TEST_INVALID(B_non_global_array, "int a; def int main() { int b[5]; return 0; }")

// checked for duplciate vars and methods at the program level
// checked for duplicate parameters
// fixed verifying type for EQ and NEQ
// 
// ==========================================================================================

TEST_INVALID(A_invalid_main_var,      "int main; def int foo(int a) { return 0; }")

#endif

/**
 * @brief Register all test cases
 * 
 * @param s Test suite to which the tests should be added
 */
void public_tests (Suite *s)
{
    TCase *tc = tcase_create ("Public");

    TEST(D_trivial);
    TEST(D_invalid_no_main);
    TEST(D_invalid_void_var);
    TEST(D_invalid_undef_var);

    TEST(C_array_size_O);
    TEST(C_return_type_mismatch);
    TEST(C_bi_return_type_mismatch);
    TEST(C_assign);
    TEST(C_invalid_break);
    TEST(C_var_type_mismatch);
    TEST(C_invalid_conditional);

    TEST(B_invalid_dup_var_global);
    TEST(B_expr_type_mismatch);
    TEST(B_mismatched_parameters);
    TEST(B_funnccall_of_undefinedvar);
    TEST(B_array_no_access);
    TEST(B_invalid_break_double_loop);
    TEST(B_non_global_array);

    TEST(A_invalid_main_var);

    suite_add_tcase (s, tc);
}

