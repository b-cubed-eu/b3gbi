# Track Plan - Include original cell IDs in map functions (#93)

## Phase 1: Reduce NA Clutter and Verify Cell IDs [x]
- [x] Task: Update `print.indicator_map` to filter out rows with NA `diversity_val`.
- [x] Task: Verify `cellCode` is preserved in `calc_map` functions.
- [x] Task: Handle missing `cellCode` gracefully in `print`/`plot`.

## Phase 2: Verification [x]
- [x] Task: Update unit tests to verify the presence of `cellCode` and absence of NA clutter in printed output.
- [x] Task: Run full test suite.
- [x] Task: Verify code coverage (>80%).