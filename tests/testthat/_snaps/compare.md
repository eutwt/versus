# Error on input with duplicates

    Code
      compare(with_dupe, without_dupe, by = c(x, y))
    Condition
      Error in `compare()`:
      ! `table_a` must be unique on `by` vars (`x`, `y`)

---

    Code
      compare(without_dupe, with_dupe, by = c(x, y))
    Condition
      Error in `compare()`:
      ! `table_b` must be unique on `by` vars (`x`, `y`)

# Error on empty `by`

    Code
      compare(a, b, by = where(is.character))
    Condition
      Error in `compare()`:
      ! Must select at least one column with `by`
      i No matching columns found in `table_a`

# Error when `by` columns don't match

    Code
      compare(a, b, by = where(is.numeric))
    Condition
      Error in `compare()`:
      ! Column names of `by` variables must be the same in both data frames
      i table_a names: x
      i table_b names: g

# Error when columns in `by` aren't present

    Code
      compare(a, b, by = x)
    Condition
      Error in `compare()`:
      ! Issue with `table_b`
      * Can't subset columns that don't exist.
      x Column `x` doesn't exist.

# Error on named `by`

    Code
      compare(a, b, by = c(y = x))
    Condition
      Error in `compare()`:
      ! Can't rename variables in this context.

# Error when `by` uses `join_by`

    Code
      compare(a, b, by = join_by(x))
    Condition
      Error in `compare()`:
      ! `join_by()` is not supported
      i provide `by` columns with tidy-select, as in `dplyr::across()`

# Error on different classes with coerce = FALSE

    Code
      compare(test_df_a, test_df_b, by = car, coerce = FALSE)
    Condition
      Error in `compare()`:
      ! coerce = FALSE but some columns classes do not match
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
       1 mpg          2 numeric numeric   <tibble [2 x 3]>
       2 cyl          1 numeric numeric   <tibble [1 x 3]>
       3 disp         2 numeric numeric   <tibble [2 x 3]>
       4 hp           0 numeric numeric   <tibble [0 x 3]>
       5 drat         0 numeric numeric   <tibble [0 x 3]>
       6 wt           0 numeric character <tibble [0 x 3]>
       7 qsec         0 numeric numeric   <tibble [0 x 3]>
       8 vs           0 numeric numeric   <tibble [0 x 3]>
       9 am           0 numeric numeric   <tibble [0 x 3]>
      10 gear         0 numeric numeric   <tibble [0 x 3]>
      11 carb         0 numeric numeric   <tibble [0 x 3]>
      
      $unmatched_cols
      # A tibble: 1 x 2
        table column    
        <chr> <chr>     
      1 a     extracol_a
      
      $unmatched_rows
      # A tibble: 5 x 2
        table car       
        <chr> <chr>     
      1 a     Mazda RX4 
      2 a     extra_a   
      3 b     Merc 280C 
      4 b     Merc 450SE
      5 b     extra_b   
      

---

    Code
      value_diffs_all(comp)
    Output
      # A tibble: 5 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 cyl      6    NA   Hornet 4 Drive
      4 disp   109   108   Datsun 710    
      5 disp   259   258   Hornet 4 Drive

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
       1 mpg          2 numeric numeric   <tibble [2 x 3]>
       2 cyl          1 numeric numeric   <tibble [1 x 3]>
       3 disp         2 numeric numeric   <tibble [2 x 3]>
       4 hp           0 numeric numeric   <tibble [0 x 3]>
       5 drat         0 numeric numeric   <tibble [0 x 3]>
       6 wt           0 numeric character <tibble [0 x 3]>
       7 qsec         0 numeric numeric   <tibble [0 x 3]>
       8 vs           0 numeric numeric   <tibble [0 x 3]>
       9 am           0 numeric numeric   <tibble [0 x 3]>
      10 gear         0 numeric numeric   <tibble [0 x 3]>
      11 carb         0 numeric numeric   <tibble [0 x 3]>
      
      $unmatched_cols
      # A tibble: 1 x 2
        table column    
        <chr> <chr>     
      1 a     extracol_a
      
      $unmatched_rows
      # A tibble: 5 x 2
        table car       
        <chr> <chr>     
      1 a     Mazda RX4 
      2 a     extra_a   
      3 b     Merc 280C 
      4 b     Merc 450SE
      5 b     extra_b   
      

---

    Code
      value_diffs_all(comp)
    Output
      # A tibble: 5 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 cyl     NA    NA   Datsun 710    
      4 disp   109   108   Datsun 710    
      5 disp   259   258   Hornet 4 Drive

