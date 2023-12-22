# value_diffs with a single column works

    Code
      value_diffs(comp, mpg)
    Output
      # A tibble: 2 x 5
        mpg_a mpg_b car        row_a row_b
        <dbl> <dbl> <chr>      <int> <int>
      1  14.3  16.3 Duster 360     7     6
      2  24.4  26.4 Merc 240D      8     7

# value_diffs works when the supplied columns have no diffs 

    Code
      value_diffs(comp, hp)
    Output
      # A tibble: 0 x 5
      # i 5 variables: hp_a <dbl>, hp_b <dbl>, car <chr>, row_a <int>, row_b <int>

---

    Code
      value_diffs_stacked(comp, c(hp, drat))
    Output
      # A tibble: 0 x 4
      # i 4 variables: column <chr>, val_a <chr>, val_b <chr>, car <chr>

# Error when `comparison` isn't a comparison

    Code
      value_diffs(mtcars, mpg)
    Condition
      Error in `value_diffs()`:
      ! Problem with argument `comparison = mtcars`
      i `comparison` must be the output of `versus::compare()`

---

    Code
      value_diffs_stacked(mtcars, c(mpg, disp))
    Condition
      Error in `value_diffs_stacked()`:
      ! Problem with argument `comparison = mtcars`
      i `comparison` must be the output of `versus::compare()`

# Error on value_diffs with empty selection

    Code
      value_diffs(comp, where(is.factor))
    Condition
      Error in `value_diffs()`:
      ! Problem with argument `column = where(is.factor)`:
      * Must select at least one item.

---

    Code
      value_diffs_stacked(comp, where(is.factor))
    Condition
      Error in `value_diffs_stacked()`:
      ! Problem with argument `column = where(is.factor)`:
      * Must select at least one item.

# Error on value_diffs when column doesn't exist

    Code
      value_diffs(comp, bear)
    Condition
      Error in `value_diffs()`:
      ! Problem with argument `column = bear`:
      * Must select columns from `comparison$intersection`
      i column `bear` is not part of the supplied comparison

---

    Code
      value_diffs_stacked(comp, c(bear, mpg))
    Condition
      Error in `value_diffs_stacked()`:
      ! Problem with argument `column = c(bear, mpg)`:
      * Must select columns from `comparison$intersection`
      i column `bear` is not part of the supplied comparison

# Error on value_diffs with multiple columns

    Code
      value_diffs(comp, c(mpg, disp))
    Condition
      Error in `value_diffs()`:
      ! Must select only one column.
      i Columns selected: mpg, disp
      i For multiple columns, use `value_diffs_stacked()`

# value_diffs_stacked works

    Code
      value_diffs_stacked(comp, c(mpg, disp))
    Output
      # A tibble: 4 x 6
        column val_a val_b car            row_a row_b
        <chr>  <dbl> <dbl> <chr>          <int> <int>
      1 mpg     14.3  16.3 Duster 360         7     6
      2 mpg     24.4  26.4 Merc 240D          8     7
      3 disp   109   108   Datsun 710         3     2
      4 disp   259   258   Hornet 4 Drive     4     3

---

    Code
      value_diffs_stacked(comp, where(is.numeric))
    Output
      # A tibble: 5 x 6
        column val_a val_b car            row_a row_b
        <chr>  <dbl> <dbl> <chr>          <int> <int>
      1 mpg     14.3  16.3 Duster 360         7     6
      2 mpg     24.4  26.4 Merc 240D          8     7
      3 cyl      6    NA   Hornet 4 Drive     4     3
      4 disp   109   108   Datsun 710         3     2
      5 disp   259   258   Hornet 4 Drive     4     3

# value_diffs_all() works

    Code
      value_diffs_all(comp)
    Output
      # A tibble: 5 x 6
        column val_a val_b car            row_a row_b
        <chr>  <dbl> <dbl> <chr>          <int> <int>
      1 mpg     14.3  16.3 Duster 360         7     6
      2 mpg     24.4  26.4 Merc 240D          8     7
      3 cyl      6    NA   Hornet 4 Drive     4     3
      4 disp   109   108   Datsun 710         3     2
      5 disp   259   258   Hornet 4 Drive     4     3

# value_diffs_all coerces to char on incompatible ptypes

    Code
      as_tibble(value_diffs_all(comp))
    Message
      i values converted to character
    Output
      # A tibble: 5 x 6
        column val_a val_b car            row_a row_b
        <chr>  <chr> <chr> <chr>          <int> <int>
      1 mpg    14.3  16.3  Duster 360         7     6
      2 mpg    24.4  26.4  Merc 240D          8     7
      3 cyl    6     <NA>  Hornet 4 Drive     4     3
      4 disp   109   108   Datsun 710         3     2
      5 disp   259   258   Hornet 4 Drive     4     3

