# slice_diffs() works

    Code
      slice_diffs(comp, "a", disp)
    Output
      # A tibble: 2 x 9
        car              mpg   cyl  disp    hp  drat    wt    vs    am
        <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
      1 Datsun 710      22.8    NA   109    93  3.85  2.32     1     1
      2 Hornet 4 Drive  21.4     6   259   110  3.08  3.22     1     0

---

    Code
      slice_diffs(comp, "a", c(mpg, disp))
    Output
      # A tibble: 4 x 9
        car              mpg   cyl  disp    hp  drat    wt    vs    am
        <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
      1 Duster 360      14.3     8  360    245  3.21  3.57     0     0
      2 Merc 240D       24.4     4  147.    62  3.69  3.19     1     0
      3 Datsun 710      22.8    NA  109     93  3.85  2.32     1     1
      4 Hornet 4 Drive  21.4     6  259    110  3.08  3.22     1     0

# Error when `comparison` isn't a comparison

    Code
      slice_diffs(example_df_a, "a", disp)
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `comparison = example_df_a`
      i `comparison` must be the output of `versus::compare()`

# Error when `table` isn't 'a' or 'b'

    Code
      slice_diffs(comp, a, disp)
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `table = a`
      i `table` must be a single character value: "a" or "b"

---

    Code
      slice_diffs(comp, disp)
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `table = disp`
      i `table` must be a single character value: "a" or "b"

---

    Code
      slice_diffs(comp, "z")
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `table = "z"`
      i `table` must be either "a" or "b"

---

    Code
      slice_diffs(comp)
    Condition
      Error in `slice_diffs()`:
      ! `table` is absent but must be supplied.

# Error on slice_diffs() with empty selection

    Code
      slice_diffs(comp, "a", where(is.factor))
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `column = where(is.factor)`:
      * Must select at least one item.

# Error on value_diffs when column doesn't exist

    Code
      slice_diffs(comp, "a", bear)
    Condition
      Error in `slice_diffs()`:
      ! Problem with argument `column = bear`:
      * Must select columns from `comparison$intersection`
      i column `bear` is not part of the supplied comparison

