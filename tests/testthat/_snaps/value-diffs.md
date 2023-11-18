# value_diffs with a single column works

    Code
      value_diffs(comp, mpg)
    Output
      # A tibble: 2 x 3
        mpg_a mpg_b car       
      * <dbl> <dbl> <chr>     
      1  14.3  16.3 Duster 360
      2  24.4  26.4 Merc 240D 

# value_diffs with multiple columns errors

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
      # A tibble: 4 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 disp   109   108   Datsun 710    
      4 disp   259   258   Hornet 4 Drive

# value_diffs_all() works

    Code
      value_diffs_all(comp)
    Output
      # A tibble: 4 x 4
        column val_a val_b car           
        <chr>  <dbl> <dbl> <chr>         
      1 mpg     14.3  16.3 Duster 360    
      2 mpg     24.4  26.4 Merc 240D     
      3 disp   109   108   Datsun 710    
      4 disp   259   258   Hornet 4 Drive

# value_diffs_all coerces to char on incompatible ptypes

    Code
      as_tibble(value_diffs_all(comp))
    Message
      i values converted to character
    Output
      # A tibble: 4 x 4
        column val_a val_b car           
        <chr>  <chr> <chr> <chr>         
      1 mpg    14.3  16.3  Duster 360    
      2 mpg    24.4  26.4  Merc 240D     
      3 disp   109   108   Datsun 710    
      4 disp   259   258   Hornet 4 Drive

