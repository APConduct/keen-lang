#!/bin/bash

# Keen Language Test Runner
# Runs organized test suite and reports results

set -e

KEEN_BIN="../target/debug/keen"
PASSED=0
FAILED=0
TOTAL=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Keen Language Test Runner${NC}"
echo "=================================="

# Check if keen binary exists
if [[ ! -f "$KEEN_BIN" ]]; then
    echo -e "${RED}❌ Keen binary not found at $KEEN_BIN${NC}"
    echo "Please run 'cargo build' first from the project root."
    exit 1
fi

# Function to run a single test
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .kn)

    echo -n "  Testing $test_name... "

    if timeout 10s "$KEEN_BIN" run "$test_file" > /dev/null 2>&1; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((PASSED++))
    else
        echo -e "${RED}❌ FAIL${NC}"
        ((FAILED++))
    fi
    ((TOTAL++))
}

# Function to run tests in a category
run_category() {
    local category="$1"
    local description="$2"

    echo ""
    echo -e "${YELLOW}$description${NC}"
    echo "$(printf '%.0s-' {1..40})"

    if [[ -d "$category" ]]; then
        for test in "$category"/*.kn; do
            if [[ -f "$test" ]]; then
                run_test "$test"
            fi
        done
    else
        echo "  No tests found in $category/"
    fi
}

# Run core tests
run_category "core" "📋 Core Language Tests"

# Run feature tests
run_category "features" "🔧 Feature-Specific Tests"

# Run integration tests
run_category "integration" "🔗 Integration Tests"

# Summary
echo ""
echo "=================================="
echo -e "${BLUE}📊 Test Results Summary${NC}"
echo "=================================="
echo "Total tests: $TOTAL"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [[ $FAILED -eq 0 ]]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}💥 $FAILED test(s) failed.${NC}"
    exit 1
fi
