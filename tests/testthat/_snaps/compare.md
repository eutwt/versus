# Error on input with duplicates

    Code
      compare(with_dupe, without_dupe, by = c(x, y))
    Condition
      Error in `compare()`:
      ! `by` variables must uniquely identify rows
      i `table_a` has 2 rows with the same `by` values as row 1
      $ x: -2
      $ y: -3

---

    Code
      compare(without_dupe, with_dupe, by = c(x, y))
    Condition
      Error in `compare()`:
      ! `by` variables must uniquely identify rows
      i `table_b` has 2 rows with the same `by` values as row 1
      $ x: -2
      $ y: -3

---

    Code
      compare(a, b, by = all_of(names(mtcars)))
    Condition
      Error in `compare()`:
      ! `by` variables must uniquely identify rows
      i `table_b` has 2 rows with the same `by` values as row 2
      $ mpg: 22.8
      $ cyl: 4
      $ disp: 108
      i 8 more: hp, drat, wt, qsec, vs, am, gear, carb

# Error when `by` columns are incompatible

    Code
      compare(test_df_a, test_df_b, by = c(car, wt, mpg))
    Condition
      Error in `compare()`:
      ! `by` columns must be compatible
      `table_a$wt` <numeric>
      `table_b$wt` <character>

# Error on dupes when there are lots of `by` columns

    Code
      compare(without_dupe, with_dupe, by = all_of(letters))
    Condition
      Error in `compare()`:
      ! `by` variables must uniquely identify rows
      i `table_b` has 2 rows with the same `by` values as row 1
      $ a: 1
      $ b: 2
      $ c: 3
      i 23 more: d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, ...

# Error on dupes when there is a `by` column with a long name

    Code
      compare(with_dupe, without_dupe, by = 1:6)
    Condition
      Error in `compare()`:
      ! `by` variables must uniquely identify rows
      i `table_a` has 2 rows with the same `by` values as row 1
      $ a: 1
      $ b: 2
      $ c: 3
      i 3 more: azbzczdzezfzgzhzizjzkzlzmznzozpzqzrzsztzuzvzwzx...

# Error on dupes when there is a `by` value with a large print width

    Code
      compare(with_dupe, without_dupe, by = a)
    Condition
      Error in `compare()`:
      ! `by` variables must uniquely identify rows
      i `table_a` has 2 rows with the same `by` values as row 1
      $ a: "azbzczdzezfzgzhzizjzkzlzmznzozpzqzrzsztzuzvzwz...

# Error on non data frame input

    Code
      compare(example_df_a, non_df, by = car)
    Condition
      Error in `compare()`:
      ! `table_b` must be a data frame
      i class(table_b): <POSIXct/POSIXt>

---

    Code
      compare(non_df, example_df_b, by = car)
    Condition
      Error in `compare()`:
      ! `table_a` must be a data frame
      i class(table_a): <POSIXct/POSIXt>

# Error on input with duplicated names

    Code
      compare(one, two, by = mpg)
    Condition
      Error in `compare()`:
      ! Problem with `table_b`
      * Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.

---

    Code
      compare(one, two, by = "mpg")
    Condition
      Error in `compare()`:
      ! Problem with `table_b`
      * Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.

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
      ! Problem with `table_b`:
      * Can't subset columns that don't exist.
      x Column `x` doesn't exist.

# Error on named `by`

    Code
      compare(a, b, by = c(y = x))
    Condition
      Error in `compare()`:
      ! Problem with `by`:
      * Can't rename variables in this context.

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
      ! `coerce = FALSE` but some column classes do not match
      i table_a: wt <numeric>
      i table_b: wt <character>

# example comparison

    Code
      comp
    Output
      $tables
      # A tibble: 2 x 4
        table   expr       nrow  ncol
        <chr>   <chr>     <int> <int>
      1 table_a test_df_a    11    13
      2 table_b test_df_b    12    12
      
      $by
      # A tibble: 1 x 3
        column class_a   class_b  
        <chr>  <chr>     <chr>    
      1 car    character character
      
      $intersection
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
        table   expr       nrow  ncol
        <chr>   <chr>     <int> <int>
      1 table_a test_df_a    11    13
      2 table_b test_df_b    12    12
      
      $by
      # A tibble: 1 x 3
        column class_a   class_b  
        <chr>  <chr>     <chr>    
      1 car    character character
      
      $intersection
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

# compare() works when the tables only have one column

    Code
      compare(a, b, by = car)
    Output
      $tables
      # A tibble: 2 x 4
        table   expr   nrow  ncol
        <chr>   <chr> <int> <int>
      1 table_a a         4     1
      2 table_b b         4     1
      
      $by
      # A tibble: 1 x 3
        column class_a class_b
        <chr>  <chr>   <chr>  
      1 car    integer integer
      
      $intersection
      # A tibble: 0 x 5
      # i 5 variables: column <chr>, n_diffs <int>, class_a <chr>, class_b <chr>,
      #   value_diffs <list>
      
      $unmatched_cols
      # A tibble: 0 x 2
      # i 2 variables: table <chr>, column <chr>
      
      $unmatched_rows
      # A tibble: 2 x 2
        table   car
        <chr> <int>
      1 a         1
      2 b         5
      

# compare() works when no rows are common

    Code
      compare(a, b, by = car)
    Output
      $tables
      # A tibble: 2 x 4
        table   expr   nrow  ncol
        <chr>   <chr> <int> <int>
      1 table_a a         2     1
      2 table_b b         2     1
      
      $by
      # A tibble: 1 x 3
        column class_a class_b
        <chr>  <chr>   <chr>  
      1 car    integer integer
      
      $intersection
      # A tibble: 0 x 5
      # i 5 variables: column <chr>, n_diffs <int>, class_a <chr>, class_b <chr>,
      #   value_diffs <list>
      
      $unmatched_cols
      # A tibble: 0 x 2
      # i 2 variables: table <chr>, column <chr>
      
      $unmatched_rows
      # A tibble: 4 x 2
        table   car
        <chr> <int>
      1 a         1
      2 a         2
      3 b         5
      4 b         6
      

