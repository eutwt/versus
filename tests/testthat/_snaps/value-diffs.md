# value_diffs with a single column works

    Code
      value_diffs(comp, mpg)
    Output
      # A tibble: 2 x 3
        mpg_a mpg_b car       
        <dbl> <dbl> <chr>     
      1  14.3  16.3 Duster 360
      2  24.4  26.4 Merc 240D 

# value_diffs works when the supplied columns have no diffs 

    Code
      value_diffs(comp, hp)
    Output
      # A tibble: 0 x 3
      # i 3 variables: hp_a <dbl>, hp_b <dbl>, car <chr>

---

    Code
      value_diffs_stacked(comp, c(hp, drat))
    Output
      # A tibble: 0 x 4
      # i 4 variables: column <chr>, val_a <dbl>, val_b <dbl>, car <chr>

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

# Error on value_diffs() with multiple columns

    Code
      value_diffs(comp, c(mpg, disp))
    Condition
      Error in `value_diffs()`:
      ! Must select only one column.
      i Columns selected: mpg, disp
      i For multiple columns, use `value_diffs_stacked()`

# value_diffs_stacked() works

    Code
      value_diffs_stacked(comp, c(mpg, disp))
    Output
      # A tibble: 4 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 disp   109   108   Datsun 710    
      4 disp   259   258   Hornet 4 Drive

---

    Code
      value_diffs_stacked(comp, where(is.numeric))
    Output
      # A tibble: 5 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 cyl      6    NA   Hornet 4 Drive
      4 disp   109   108   Datsun 710    
      5 disp   259   258   Hornet 4 Drive

# value_diffs_stacked() works without column

    Code
      value_diffs_stacked(comp)
    Output
      # A tibble: 5 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 cyl      6    NA   Hornet 4 Drive
      4 disp   109   108   Datsun 710    
      5 disp   259   258   Hornet 4 Drive

# value_diffs_stacked() coerces to char on incompatible ptypes

    Code
      value_diffs_stacked(comp)
    Message
      i Columns converted to character: val_a
    Output
      # A tibble: 5 x 4
        column val_a val_b car           
        <chr>  <chr> <dbl> <chr>         
      1 mpg    14.3   16.3 Duster 360    
      2 mpg    24.4   26.4 Merc 240D     
      3 cyl    6      NA   Hornet 4 Drive
      4 disp   109   108   Datsun 710    
      5 disp   259   258   Hornet 4 Drive

