# weave_diffs_long works

    Code
      out
    Output
      # A tibble: 4 x 10
        table car          mpg   cyl  disp    hp  drat wt       vs    am
        <chr> <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl>
      1 a     Merc 240D   24.4     4  147.    62  3.69 3.19      1     0
      2 b     Merc 240D   26.4     4  147.    62  3.69 3.19      1     0
      3 a     Duster 360  14.3     8  360    245  3.21 3.57      0     0
      4 b     Duster 360  16.3     8  360    245  3.21 3.57      0     0

# weave_diffs_long works with multi-variable `by`

    Code
      out
    Output
      # A tibble: 4 x 13
        table car       vs  gear   mpg   cyl  disp    hp  drat wt     qsec    am  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>
      1 a     Duste~     0     3  14.3     8  360    245  3.21 3.57   15.8     0     4
      2 b     Duste~     0     3  16.3     8  360    245  3.21 3.57   15.8     0     4
      3 a     Merc ~     1     4  24.4     4  147.    62  3.69 3.19   20       0     2
      4 b     Merc ~     1     4  26.4     4  147.    62  3.69 3.19   20       0     2

# Error when `comparison` isn't a comparison

    Code
      weave_diffs_long(example_df_a, example_df_b, disp)
    Condition
      Error in `weave_diffs_long()`:
      ! Problem with argument `comparison = disp`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      weave_diffs_long(example_df_a, example_df_b, 1, disp)
    Condition
      Error in `weave_diffs_long()`:
      ! Problem with argument `comparison = 1`
      i `comparison` must be the output of `versus::compare()`

# Error when supplied table doesn't contain cols in `comparison`

    Code
      weave_diffs_long(test_df_a, tibble(car = 1), comp, column = drat)
    Condition
      Error in `weave_diffs_long()`:
      ! `table_b` is missing some columns from `comparison`
      column `mpg` is not present in `table_b`

