# slice_unmatched_both works

    Code
      slice_unmatched_both(comp)
    Message
      i Columns converted to character: wt
    Output
      # A tibble: 5 x 13
        table car      mpg   cyl  disp    hp  drat wt     qsec    vs    am  gear  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
      1 a     Mazda~  21       6  160    110  3.9  2.62   16.5     0     1     4     4
      2 a     extra~  21       6  160    110  3.9  2.62   16.5     0     1     4     4
      3 b     Merc ~  17.8     6  168.   123  3.92 3.44   18.9     1     0     4     4
      4 b     Merc ~  16.4     8  276.   180  3.07 4.07   17.4     0     0     3     3
      5 b     extra~  21       6  160    110  3.9  2.875  17.0     0     1     4     4

# slice_unmatched_both works with custom table_id

    Code
      slice_unmatched_both(comp)
    Message
      i Columns converted to character: wt
    Output
      # A tibble: 5 x 13
        table car      mpg   cyl  disp    hp  drat wt     qsec    vs    am  gear  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
      1 x     Mazda~  21       6  160    110  3.9  2.62   16.5     0     1     4     4
      2 x     extra~  21       6  160    110  3.9  2.62   16.5     0     1     4     4
      3 y     Merc ~  17.8     6  168.   123  3.92 3.44   18.9     1     0     4     4
      4 y     Merc ~  16.4     8  276.   180  3.07 4.07   17.4     0     0     3     3
      5 y     extra~  21       6  160    110  3.9  2.875  17.0     0     1     4     4

# slice_unmatched errors when `table` isn't expected

    Code
      slice_unmatched(comp, "a")
    Condition
      Error in `slice_unmatched()`:
      ! Problem with argument `table = "a"`
      i `table` must be either "x" or "y"

# unmatched() errors when `comparison` isn't a comparison

    Code
      slice_unmatched(example_df_a, disp)
    Condition
      Error in `slice_unmatched()`:
      ! Problem with argument `comparison = example_df_a`
      i `comparison` must be the output of `versus::compare()`

