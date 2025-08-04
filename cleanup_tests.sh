#!/bin/bash
echo "🧹 Cleaning up redundant Keen test files..."
rm -f debug_test.kn debug_block.kn isolated_test.kn minimal_test.kn ultra_minimal.kn
rm -f oneline_test.kn multiline_test.kn progressive_test.kn simple_test.kn minimal_block.kn
rm -f test_simple_block.kn test_both_blocks.kn mixed_test.kn test_single_manual.kn
rm -f test_known_manual.kn then_clause_test.kn var_in_ternary_test.kn variable_test.kn
rm -f simplest_var_test.kn chumsky_ternary_test.kn basic_ternary_test.kn
rm -f test_type_annotations.kn single_typed.kn mixed_typed.kn minimal_type_test.kn
rm -f test_single_type.kn test_one_function.kn test_multi_functions.kn two_typed.kn simple_types.kn
rm -f simple_lambdas.kn test_lambda_call.kn test_simple_call.kn test_zero_args.kn
rm -f test_math_basic.kn test_math_simple.kn test_math_working.kn
rm -f simple_working.kn test_blocks.kn test_case.kn
echo "✅ Cleanup complete!"
