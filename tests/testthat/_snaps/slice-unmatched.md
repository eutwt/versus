# slice_unmatched_both works

    Code
      out
    Output
      # A tibble: 5 x 13
        table car      mpg   cyl  disp    hp  drat wt     qsec    vs    am  gear  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
      1 a     Mazda~  21       6  160    110  3.9  2.62   16.5     0     1     4     4
      2 a     extra~  21       6  160    110  3.9  2.62   16.5     0     1     4     4
      3 b     Merc ~  17.8     6  168.   123  3.92 3.44   18.9     1     0     4     4
      4 b     Merc ~  16.4     8  276.   180  3.07 4.07   17.4     0     0     3     3
      5 b     extra~  21       6  160    110  3.9  2.875  17.0     0     1     4     4

# slice_unmatched_both works with multi-variable `by`

    Code
      out
    Output
      # A tibble: 5 x 13
        table car       vs  gear   mpg   cyl  disp    hp  drat wt     qsec    am  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>
      1 a     Mazda~     0     4  21       6  160    110  3.9  2.62   16.5     1     4
      2 a     extra~     0     4  21       6  160    110  3.9  2.62   16.5     1     4
      3 b     Merc ~     1     4  17.8     6  168.   123  3.92 3.44   18.9     0     4
      4 b     Merc ~     0     3  16.4     8  276.   180  3.07 4.07   17.4     0     3
      5 b     extra~     0     4  21       6  160    110  3.9  2.875  17.0     1     4

# unmatched() errors when `comparison` isn't a comparison

    Code
      slice_unmatched(example_df_a, disp)
    Condition
      Error in `slice_unmatched()`:
      ! Problem with argument `comparison = disp`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      slice_unmatched(example_df_a, example_df_b)
    Condition
      Error in `slice_unmatched()`:
      ! Problem with argument `comparison = example_df_b`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      slice_unmatched_both(example_df_a, example_df_b, disp)
    Condition
      Error in `slice_unmatched_both()`:
      ! Problem with argument `comparison = disp`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      slice_unmatched_both(example_df_a, example_df_b, 1)
    Condition
      Error in `slice_unmatched_both()`:
      ! Problem with argument `comparison = 1`
      i `comparison` must be the output of `versus::compare()`

# unmatched() errors when supplied table doesn't contain cols in `comparison`

    Code
      slice_unmatched(tibble(x = 1), comp)
    Condition
      Error in `slice_unmatched()`:
      ! `table` is missing some columns from `comparison`
      column `car` is not present in `table`

---

    Code
      slice_unmatched_both(test_df_a, tibble(x = 1), comp)
    Condition
      Error in `slice_unmatched_both()`:
      ! `table_b` is missing some columns from `comparison`
      column `car` is not present in `table_b`

# unmatched() Errors when `by` columns in `table` aren't compatible with `comparison`

    Code
      slice_unmatched(bad_table, comp)
    Condition
      Error in `slice_unmatched()`:
      ! `by` columns in `table` must be compatible with those in `comparison`
      `vs` class in `table`: <character>
      `vs` class in `comparison`: <numeric>

