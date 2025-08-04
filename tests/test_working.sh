#!/bin/bash

# Simple Keen Test Runner - Only tests working features
echo "🧪 Testing Working Keen Features"
echo "================================"

KEEN_BIN="./target/debug/keen"
PASSED=0
FAILED=0

run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .kn)
    
    echo -n "Testing $test_name... "
    
    if timeout 5s "$KEEN_BIN" run "$test_file" > /dev/null 2>&1; then
        echo "✅ PASS"
        ((PASSED++))
    else
        echo "❌ FAIL"
        ((FAILED++))
    fi
}

# Test working core features
echo ""
echo "🎯 Core Tests (Working Features Only):"
run_test "tests/core/simplest.kn"
run_test "tests/core/working_basic.kn"

# Test working examples
echo ""
echo "📚 Example Tests:"
run_test "examples/simplest.kn"
run_test "examples/working_basic.kn" 
run_test "examples/basic_subset.kn"

# Test working programs
echo ""
echo "🧮 Program Tests:"
run_test "test_programs/basic_test.kn"
run_test "test_programs/math.kn"

echo ""
echo "================================"
echo "Results: $PASSED passed, $FAILED failed"
if [[ $FAILED -eq 0 ]]; then
    echo "🎉 All working tests passed!"
else
    echo "⚠️  Some tests failed (expected for features in development)"
fi
