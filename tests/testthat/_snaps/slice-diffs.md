# Error when `comparison` isn't a comparison

    Code
      slice_diffs(example_df_a, disp)
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `comparison = disp`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      slice_diffs(example_df_a, example_df_b, disp)
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `comparison = example_df_b`
      i `comparison` must be the output of `versus::compare()`

# Error when supplied table doesn't contain cols in `comparison`

    Code
      slice_diffs(tibble(x = 1), comp, column = drat)
    Condition
      Error in `slice_diffs()`:
      ! `table` is missing some columns from `comparison`
      column `car` is not present in `table`

# Error when `by` columns in `table` aren't compatible with `comparison`

    Code
      slice_diffs(bad_table, comp, column = mpg)
    Condition
      Error in `slice_diffs()`:
      ! `by` columns in `table` must be compatible with those in `comparison`
      `vs` class in `table`: <character>
      `vs` class in `comparison`: <numeric>

