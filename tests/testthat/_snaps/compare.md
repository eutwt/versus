# Error on input with duplicates

    Code
      compare(mtcars, mtcars, by = c(disp, cyl))
    Error <rlang_error>
      `table_a` must be unique on `by` vars (`disp`, `cyl`)

# Error on empty `by`

    Code
      compare(a, b, by = where(is.character))
    Error <rlang_error>
      x Issue with `table_a`
      i No columns matching supplied `by`

# Error when `by` columns don't match

    Code
      compare(a, b, by = where(is.numeric))
    Error <rlang_error>
      x Column names of `by` variables must be the same in both data frames
      i table_a names: x
      i table_b names: g

# Error when columns in `by` aren't present

    Code
      compare(a, b, by = x)
    Error <rlang_error>
      Issue with `table_b`
      Can't subset columns that don't exist.
      x Column `x` doesn't exist.

# Error on named `by`

    Code
      compare(a, b, by = c(y = x))
    Error <rlang_error>
      Can't rename variables in this context.

# Error on different classes with coerce = FALSE

    Code
      compare(test_df_a, test_df_b, by = car, coerce = FALSE)
    Error <rlang_error>
      x coerce = FALSE but some columns classes do not match
      i wt

# example comparison

    Code
      comp
    Output
      $tables
      # A tibble: 2 x 4
        table   expr       ncol  nrow
        <chr>   <chr>     <int> <int>
      1 table_a test_df_a    13    11
      2 table_b test_df_b    12    12
      
      $by
      # A tibble: 1 x 3
        column class_a   class_b  
        <chr>  <chr>     <chr>    
      1 car    character character
      
      $summ
      # A tibble: 11 x 5
         column n_diffs class_a class_b   value_diffs       
         <chr>    <int> <chr>   <chr>     <list>            
       1 mpg          2 numeric numeric   <dckplyr_ [2 x 3]>
       2 cyl          0 numeric numeric   <dckplyr_ [0 x 3]>
       3 disp         2 numeric numeric   <dckplyr_ [2 x 3]>
       4 hp           0 numeric numeric   <dckplyr_ [0 x 3]>
       5 drat         0 numeric numeric   <dckplyr_ [0 x 3]>
       6 wt           0 numeric character <dckplyr_ [0 x 3]>
       7 qsec         0 numeric numeric   <dckplyr_ [0 x 3]>
       8 vs           0 numeric numeric   <dckplyr_ [0 x 3]>
       9 am           0 numeric numeric   <dckplyr_ [0 x 3]>
      10 gear         0 numeric numeric   <dckplyr_ [0 x 3]>
      11 carb         0 numeric numeric   <dckplyr_ [0 x 3]>
      
      $unmatched_cols
      # A tibble: 1 x 2
        table column    
        <chr> <chr>     
      1 a     extracol_a
      
      $unmatched_rows
        table        car
      1     a  Mazda RX4
      2     a    extra_a
      3     b  Merc 280C
      4     b Merc 450SE
      5     b    extra_b
      

---

    Code
      all_value_diffs(comp)
    Output
        column val_a val_b            car
      1    mpg  14.3  16.3     Duster 360
      2    mpg  24.4  26.4      Merc 240D
      3   disp   109   108     Datsun 710
      4   disp   259   258 Hornet 4 Drive

# example comparison with allow_bothNA = FALSE

    Code
      comp
    Output
      $tables
      # A tibble: 2 x 4
        table   expr       ncol  nrow
        <chr>   <chr>     <int> <int>
      1 table_a test_df_a    13    11
      2 table_b test_df_b    12    12
      
      $by
      # A tibble: 1 x 3
        column class_a   class_b  
        <chr>  <chr>     <chr>    
      1 car    character character
      
      $summ
      # A tibble: 11 x 5
         column n_diffs class_a class_b   value_diffs       
         <chr>    <int> <chr>   <chr>     <list>            
       1 mpg          2 numeric numeric   <dckplyr_ [2 x 3]>
       2 cyl          1 numeric numeric   <dckplyr_ [1 x 3]>
       3 disp         2 numeric numeric   <dckplyr_ [2 x 3]>
       4 hp           0 numeric numeric   <dckplyr_ [0 x 3]>
       5 drat         0 numeric numeric   <dckplyr_ [0 x 3]>
       6 wt           0 numeric character <dckplyr_ [0 x 3]>
       7 qsec         0 numeric numeric   <dckplyr_ [0 x 3]>
       8 vs           0 numeric numeric   <dckplyr_ [0 x 3]>
       9 am           0 numeric numeric   <dckplyr_ [0 x 3]>
      10 gear         0 numeric numeric   <dckplyr_ [0 x 3]>
      11 carb         0 numeric numeric   <dckplyr_ [0 x 3]>
      
      $unmatched_cols
      # A tibble: 1 x 2
        table column    
        <chr> <chr>     
      1 a     extracol_a
      
      $unmatched_rows
        table        car
      1     a  Mazda RX4
      2     a    extra_a
      3     b  Merc 280C
      4     b Merc 450SE
      5     b    extra_b
      

---

    Code
      all_value_diffs(comp)
    Output
        column val_a val_b            car
      1    mpg  14.3  16.3     Duster 360
      2    mpg  24.4  26.4      Merc 240D
      3    cyl  <NA>  <NA>     Datsun 710
      4   disp   109   108     Datsun 710
      5   disp   259   258 Hornet 4 Drive

