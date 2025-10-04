# MassRename CLI Test Suite

This directory contains tests for the MassRename CLI command.

## What the Test Does

The `basic` test case demonstrates the MassRename CLI command's ability to:

1. **Find datatypes with prefixed fields**: Identifies record types where ALL field names start with `_` (lens-style)
2. **Strip the prefix**: Removes the `_` prefix from all field names across the codebase
3. **Handle various Haskell patterns**:
   - NamedFieldPuns: `Restaurant{_name}` → `Restaurant{name}`
   - RecordWildCards: `Restaurant{..}` (with `_id`, `_name` in scope)
   - OverloadedRecordDot: `r._name` → `r.name`
   - Record construction: `Restaurant { _name = x }` → `Restaurant { name = x }`
   - Record updates: `r { _name = x }` → `r { name = x }`
   - Qualified updates: `r { Types1._name = x }` → `r { Types1.name = x }`
   - Field selectors: `_name r` → `name r`

## Running Tests

### Unit Tests

```bash
cabal test hls-mass-rename-plugin-tests
```

The current test suite verifies that:
- Test data files exist and are properly structured
- Expected output files exist

### Manual Testing

To manually test the MassRename CLI command:

1. Navigate to the test data directory:
   ```bash
   cd plugins/hls-mass-rename-plugin/test/testdata/basic
   ```

2. Run the MassRename command (without applying changes):
   ```bash
   haskell-language-server-wrapper mass-rename src
   ```

3. To apply the changes, set the `APPLY=1` environment variable:
   ```bash
   APPLY=1 haskell-language-server-wrapper mass-rename src
   ```

4. Compare the results with expected output:
   ```bash
   diff -r src/ expected/
   ```

## Test Data

The test project uses **Cabal** (not Stack) for simplicity and to avoid hardcoded LTS versions.

### Input Files (src/)

- **Types1.hs**: Defines `Restaurant` and `FulfillmentMethod` datatypes with `_` prefixed fields
- **Types2.hs**: Defines `Account` datatype with `_` prefixed fields
- **Use.hs**: Demonstrates various usage patterns for the fields
- **UseSelector.hs**: Tests field selector functions

### Expected Output (expected/)

Contains the same files after the `_` prefix has been stripped from all field names.

## Adding New Test Cases

To add a new test case:

1. Create a new directory under `testdata/`
2. Add input Haskell files in `<testname>/src/`
3. Add expected output files in `<testname>/expected/`
4. Add necessary build files (hie.yaml, .cabal)
5. Update Main.hs to include the new test case

## Known Limitations

The current automated test suite only verifies file structure. A full integration test that:
- Sets up an IdeState
- Runs the MassRename command programmatically
- Compares output with golden files

is planned but not yet implemented due to the complexity of setting up the IDE infrastructure in tests.
