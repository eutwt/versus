# weave_diffs_long works

    Code
      weave_diffs_long(comp, mpg)
    Message
      i Columns converted to character: wt
    Output
      # A tibble: 4 x 10
        table car          mpg   cyl  disp    hp  drat wt       vs    am
        <chr> <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl>
      1 a     Merc 240D   24.4     4  147.    62  3.69 3.19      1     0
      2 b     Merc 240D   26.4     4  147.    62  3.69 3.19      1     0
      3 a     Duster 360  14.3     8  360    245  3.21 3.57      0     0
      4 b     Duster 360  16.3     8  360    245  3.21 3.57      0     0

---

    Code
      weave_diffs_long(comp, c(mpg, wt))
    Message
      i Columns converted to character: wt
    Output
      # A tibble: 4 x 10
        table car          mpg   cyl  disp    hp  drat wt       vs    am
        <chr> <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl>
      1 a     Merc 240D   24.4     4  147.    62  3.69 3.19      1     0
      2 b     Merc 240D   26.4     4  147.    62  3.69 3.19      1     0
      3 a     Duster 360  14.3     8  360    245  3.21 3.57      0     0
      4 b     Duster 360  16.3     8  360    245  3.21 3.57      0     0

---

    Code
      weave_diffs_long(comp, c(mpg, disp))
    Message
      i Columns converted to character: wt
    Output
      # A tibble: 8 x 10
        table car              mpg   cyl  disp    hp  drat wt       vs    am
        <chr> <chr>          <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl>
      1 a     Merc 240D       24.4     4  147.    62  3.69 3.19      1     0
      2 b     Merc 240D       26.4     4  147.    62  3.69 3.19      1     0
      3 a     Duster 360      14.3     8  360    245  3.21 3.57      0     0
      4 b     Duster 360      16.3     8  360    245  3.21 3.57      0     0
      5 a     Hornet 4 Drive  21.4     6  259    110  3.08 3.215     1     0
      6 b     Hornet 4 Drive  21.4    NA  258    110  3.08 3.215     1     0
      7 a     Datsun 710      22.8    NA  109     93  3.85 2.32      1     1
      8 b     Datsun 710      22.8    NA  108     93  3.85 2.32      1     1

# weave_diffs_wide works

    Code
      weave_diffs_wide(comp, mpg)
    Output
      # A tibble: 2 x 10
        car        mpg_a mpg_b   cyl  disp    hp  drat    wt    vs    am
        <chr>      <dbl> <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
      1 Duster 360  14.3  16.3     8  360    245  3.21  3.57     0     0
      2 Merc 240D   24.4  26.4     4  147.    62  3.69  3.19     1     0

---

    Code
      weave_diffs_wide(comp, c(mpg, wt))
    Output
      # A tibble: 2 x 10
        car        mpg_a mpg_b   cyl  disp    hp  drat    wt    vs    am
        <chr>      <dbl> <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
      1 Duster 360  14.3  16.3     8  360    245  3.21  3.57     0     0
      2 Merc 240D   24.4  26.4     4  147.    62  3.69  3.19     1     0

---

    Code
      weave_diffs_wide(comp, c(mpg, disp))
    Output
      # A tibble: 4 x 11
        car            mpg_a mpg_b   cyl disp_a disp_b    hp  drat    wt    vs    am
        <chr>          <dbl> <dbl> <int>  <dbl>  <dbl> <int> <dbl> <dbl> <int> <int>
      1 Duster 360      14.3  16.3     8   360    360    245  3.21  3.57     0     0
      2 Merc 240D       24.4  26.4     4   147.   147.    62  3.69  3.19     1     0
      3 Datsun 710      22.8  22.8    NA   109    108     93  3.85  2.32     1     1
      4 Hornet 4 Drive  21.4  21.4     6   259    258    110  3.08  3.22     1     0

# Error when `comparison` isn't a comparison

    Code
      weave_diffs_long(example_df_a, disp)
    Condition
      Error in `weave_diffs_long()`:
      ! Problem with argument `comparison = example_df_a`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      weave_diffs_wide(example_df_a, disp)
    Condition
      Error in `weave_diffs_wide()`:
      ! Problem with argument `comparison = example_df_a`
      i `comparison` must be the output of `versus::compare()`

# Error when `column` isn't a comparison

    Code
      weave_diffs_long(example_df_a, disp)
    Condition
      Error in `weave_diffs_long()`:
      ! Problem with argument `comparison = example_df_a`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      weave_diffs_wide(example_df_a, disp)
    Condition
      Error in `weave_diffs_wide()`:
      ! Problem with argument `comparison = example_df_a`
      i `comparison` must be the output of `versus::compare()`

# Error on weave_diffs() with empty selection

    Code
      weave_diffs_long(comp, where(is.factor))
    Condition
      Error in `weave_diffs_long()`:
      ! Problem with argument `column = where(is.factor)`:
      * Must select at least one item.

---

    Code
      weave_diffs_wide(comp, where(is.factor))
    Condition
      Error in `weave_diffs_wide()`:
      ! Problem with argument `column = where(is.factor)`:
      * Must select at least one item.

# Error on value_diffs when column doesn't exist

    Code
      weave_diffs_long(comp, bear)
    Condition
      Error in `weave_diffs_long()`:
      ! Problem with argument `column = bear`:
      * Must select columns from `comparison$intersection`
      i column `bear` is not part of the supplied comparison

---

    Code
      weave_diffs_wide(comp, bear)
    Condition
      Error in `weave_diffs_wide()`:
      ! Problem with argument `column = bear`:
      * Must select columns from `comparison$intersection`
      i column `bear` is not part of the supplied comparison

