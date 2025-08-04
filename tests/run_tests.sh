#!/bin/bash

# Keen Language Test Runner
# Runs organized test suite and reports results

set -e

# Default behavior: run all tests if no arguments given
# Can be overridden by passing specific category names as arguments
CATEGORIES_TO_RUN=("core" "features" "integration")

# Parse command line arguments
if [[ $# -gt 0 ]]; then
    CATEGORIES_TO_RUN=("$@")
fi

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
    local category=$(basename "$(dirname "$test_file")")

    echo -n "  Testing $test_name... "

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
        # Show first line of output if available
        if [[ -n "$output" ]]; then
            local first_line=$(echo "$output" | head -1)
            echo -e "    ${CYAN}Output: $first_line${NC}"
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
                # Show first few lines of error output
                echo "$output" | head -3 | sed 's/^/    /' | while read line; do
                    echo -e "    ${RED}$line${NC}"
                done
            fi
        fi
    fi
    ((TOTAL++))
}

# Function to run tests in a category
run_category() {
    local category="$1"
    local description="$2"
    local specific_test="$3"

    echo ""
    echo -e "${YELLOW}$description${NC}"
    echo "$(printf '%.0s-' {1..40})"

    if [[ -d "$category" ]]; then
        local test_count=0
        if [[ -n "$specific_test" ]]; then
            # Run only the specific test if provided
            test_file="$category/$specific_test.kn"
            if [[ -f "$test_file" ]]; then
                run_test "$test_file"
                test_count=1
            else
                echo "  Test file $test_file not found"
            fi
        else
            # Run all tests in the category
            for test in "$category"/*.kn; do
                if [[ -f "$test" ]]; then
                    run_test "$test"
                    ((test_count++))
                fi
            done
        fi
        echo -e "  ${CYAN}Found $test_count test(s) in $category/${NC}"
    else
        echo "  No tests found in $category/"
    fi
}

# Show usage if help is requested
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    echo "Usage: $0 [category] [test_name]"
    echo ""
    echo "Categories:"
    echo "  core         Run core language tests"
    echo "  features     Run feature-specific tests"
    echo "  integration  Run integration tests"
    echo ""
    echo "Examples:"
    echo "  $0                    # Run all tests"
    echo "  $0 core              # Run all core tests"
    echo "  $0 features lambdas  # Run specific test"
    echo ""
    exit 0
fi

# Parse arguments to handle specific test names
if [[ $# -eq 2 ]]; then
    # Two arguments: category and specific test name
    category="$1"
    test_name="$2"
    case "$category" in
        "core")
            run_category "core" "📋 Core Language Tests" "$test_name"
            ;;
        "features")
            run_category "features" "🔧 Feature-Specific Tests" "$test_name"
            ;;
        "integration")
            run_category "integration" "🔗 Integration Tests" "$test_name"
            ;;
        *)
            echo -e "${RED}Unknown category: $category${NC}"
            echo "Available categories: core, features, integration"
            exit 1
            ;;
    esac
elif [[ $# -eq 1 ]]; then
    # One argument: category only
    category="$1"
    case "$category" in
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
            echo -e "${RED}Unknown category: $category${NC}"
            echo "Available categories: core, features, integration"
            exit 1
            ;;
    esac
else
    # No arguments: run all tests
    run_category "core" "📋 Core Language Tests"
    run_category "features" "🔧 Feature-Specific Tests"
    run_category "integration" "🔗 Integration Tests"
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
fi

if [[ $TOTAL -gt 0 ]]; then
    success_rate=$((PASSED * 100 / TOTAL))
    echo -e "Success rate: ${success_rate}%"
fi

if [[ $FAILED -eq 0 ]]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}💥 $FAILED test(s) failed.${NC}"
    exit 1
fi
