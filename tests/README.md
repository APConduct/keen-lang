# Keen Language Test Suite

## Structure
- core/ - Basic functionality tests  
- features/ - Language feature tests
- integration/ - Complex integration tests

## Running Tests
```bash
cd tests && ./run_tests.sh
```

## Test Files
### Core Tests
- simplest.kn - Basic arithmetic
- working_basic.kn - Functions and calls
- basic_features.kn - Core features

### Feature Tests  
- lambdas.kn - Lambda expressions
- pipelines.kn - Pipeline operators
- pattern_matching.kn - Case/when expressions
- mutability.kn - Mutability model
- chaining.kn - Method chaining

### Integration Tests
- basic_test.kn - Comprehensive test
- math.kn - Mathematical operations  
- chaining.kn - Complex chaining
- complete_demo.kn - Full demonstration

## Cleanup Summary
Removed ~40 redundant debug and duplicate test files.
Organized remaining tests into logical categories.
