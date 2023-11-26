# slice_diffs_both works

    Code
      as_tibble(out)
    Output
      # A tibble: 4 x 13
        table car      mpg   cyl  disp    hp  drat wt     qsec    vs    am  gear  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
      1 a     Duste~  14.3     8  360    245  3.21 3.57   15.8     0     0     3     4
      2 b     Duste~  16.3     8  360    245  3.21 3.57   15.8     0     0     3     4
      3 a     Merc ~  24.4     4  147.    62  3.69 3.19   20       1     0     4     2
      4 b     Merc ~  26.4     4  147.    62  3.69 3.19   20       1     0     4     2

# slice_diffs_both works with multi-variable `by`

    Code
      as_tibble(out)
    Output
      # A tibble: 4 x 13
        table car       vs  gear   mpg   cyl  disp    hp  drat wt     qsec    am  carb
        <chr> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr> <dbl> <dbl> <dbl>
      1 a     Duste~     0     3  14.3     8  360    245  3.21 3.57   15.8     0     4
      2 b     Duste~     0     3  16.3     8  360    245  3.21 3.57   15.8     0     4
      3 a     Merc ~     1     4  24.4     4  147.    62  3.69 3.19   20       0     2
      4 b     Merc ~     1     4  26.4     4  147.    62  3.69 3.19   20       0     2

