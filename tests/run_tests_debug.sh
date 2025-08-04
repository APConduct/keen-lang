#!/bin/bash

# Keen Language Debug Test Runner
# Provides detailed diagnostics for failing tests

set -e

cd "$(dirname "$0")"

# Adjust path to keen binary relative to tests directory
KEEN_BIN="../target/debug/keen"
PASSED=0
FAILED=0
TOTAL=0
FAILED_TESTS=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 Keen Language Debug Test Runner${NC}"
echo "=================================="

# Check if keen binary exists
if [[ ! -f "$KEEN_BIN" ]]; then
    echo -e "${RED}❌ Keen binary not found at $KEEN_BIN${NC}"
    echo "Please run 'cargo build' first from the project root."
    exit 1
fi

# Function to run a single test with detailed output
run_test() {
    local test_file="$1"
    local test_name=$(basename "$test_file" .kn)
    local category=$(basename "$(dirname "$test_file")")

    echo -e "\n${CYAN}Testing $category/$test_name...${NC}"

    # Capture output for detailed error reporting
    local output
    local exit_code

    if output=$(timeout 10s "$KEEN_BIN" run "$test_file" 2>&1); then
        exit_code=0
    else
        exit_code=$?
    fi

    if [[ $exit_code -eq 0 ]]; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((PASSED++))
        # Show final result line if available
        if [[ -n "$output" ]]; then
            local result_line=$(echo "$output" | grep "Program completed with exit code" | tail -1)
            if [[ -n "$result_line" ]]; then
                echo -e "    ${CYAN}$result_line${NC}"
            fi
        fi
    else
        echo -e "${RED}❌ FAIL${NC}"
        ((FAILED++))
        FAILED_TESTS+=("$category/$test_name")

        # Show error details
        if [[ $exit_code -eq 124 ]]; then
            echo -e "    ${RED}Timeout (10s exceeded)${NC}"
        else
            echo -e "    ${RED}Exit code: $exit_code${NC}"
            if [[ -n "$output" ]]; then
                # Extract the most relevant error info
                local parse_errors=$(echo "$output" | grep -A 10 "Parse errors:" | head -20)
                local debug_info=$(echo "$output" | grep "DEBUG:" | tail -5)
                local error_line=$(echo "$output" | grep "Error:" | head -1)

                if [[ -n "$parse_errors" ]]; then
                    echo -e "    ${RED}Parse Errors:${NC}"
                    echo "$parse_errors" | sed 's/^/      /'
                elif [[ -n "$error_line" ]]; then
                    echo -e "    ${RED}$error_line${NC}"
                fi

                if [[ -n "$debug_info" ]]; then
                    echo -e "    ${YELLOW}Last Debug Info:${NC}"
                    echo "$debug_info" | sed 's/^/      /'
                fi
            fi
        fi
    fi
    ((TOTAL++))
}

# Function to run tests in a category
run_category() {
    local category="$1"
    local description="$2"

    echo -e "\n${YELLOW}$description${NC}"
    echo "$(printf '%.0s-' {1..60})"

    if [[ -d "$category" ]]; then
        local test_count=0
        for test in "$category"/*.kn; do
            if [[ -f "$test" ]]; then
                run_test "$test"
                ((test_count++))
            fi
        done
        echo -e "\n${CYAN}Category Summary: $test_count test(s) in $category/${NC}"
    else
        echo "  No tests found in $category/"
    fi
}

# Parse command line arguments
if [[ $# -eq 0 ]]; then
    # No arguments: run all tests
    run_category "core" "📋 Core Language Tests"
    run_category "features" "🔧 Feature-Specific Tests"
    run_category "integration" "🔗 Integration Tests"
elif [[ $# -eq 1 ]]; then
    # One argument: run specific category
    case "$1" in
        "core")
            run_category "core" "📋 Core Language Tests"
            ;;
        "features")
            run_category "features" "🔧 Feature-Specific Tests"
            ;;
        "integration")
            run_category "integration" "🔗 Integration Tests"
            ;;
        *)
            echo -e "${RED}Unknown category: $1${NC}"
            echo "Available categories: core, features, integration"
            exit 1
            ;;
    esac
elif [[ $# -eq 2 ]]; then
    # Two arguments: run specific test
    category="$1"
    test_name="$2"
    test_file="$category/$test_name.kn"

    if [[ -f "$test_file" ]]; then
        run_test "$test_file"
    else
        echo -e "${RED}Test file not found: $test_file${NC}"
        exit 1
    fi
else
    echo "Usage: $0 [category] [test_name]"
    echo ""
    echo "Examples:"
    echo "  $0                    # Run all tests"
    echo "  $0 core              # Run all core tests"
    echo "  $0 features lambdas  # Run specific test"
    exit 1
fi

# Summary
echo ""
echo "=================================="
echo -e "${BLUE}📊 Test Results Summary${NC}"
echo "=================================="
echo "Total tests: $TOTAL"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [[ $FAILED -gt 0 ]]; then
    echo ""
    echo -e "${RED}Failed tests:${NC}"
    for test in "${FAILED_TESTS[@]}"; do
        echo -e "  ${RED}❌ $test${NC}"
    done

    echo ""
    echo -e "${YELLOW}Debug Tips:${NC}"
    echo "• Run individual tests with: $0 <category> <test_name>"
    echo "• Check for parsing issues in functions with type annotations"
    echo "• Look for unsupported features like complex pattern matching"
    echo "• Verify that case/when expressions are used correctly"
fi

if [[ $TOTAL -gt 0 ]]; then
    success_rate=$((PASSED * 100 / TOTAL))
    echo -e "Success rate: ${success_rate}%"
fi

echo ""
if [[ $FAILED -eq 0 ]]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}💥 $FAILED test(s) failed.${NC}"
    echo -e "${CYAN}Run with specific test names for detailed debugging.${NC}"
    exit 1
fi
