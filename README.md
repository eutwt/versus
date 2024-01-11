
<!-- README.md is generated from README.Rmd. Please edit that file -->

# versus <img src="man/figures/logo.png" id="logo" align="right" width="17%" height="17%"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/eutwt/versus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eutwt/versus/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/versus)](https://CRAN.R-project.org/package=versus)
[![Codecov test
coverage](https://codecov.io/gh/eutwt/versus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/eutwt/versus?branch=main)

<!-- badges: end -->

A toolset for interactively exploring the differences between two data
frames.

## Installation

``` r
install.packages("versus")

# Or install the development version from GitHub with
# pak::pak("eutwt/versus")
```

## Example

The two data frames below are used as an example to demonstrate
functionality

``` r
library(versus)

example_df_a
#> # A tibble: 9 × 9
#>   car              mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Duster 360      14.3     8  360    245  3.21  3.57     0     0
#> 2 Mazda RX4 Wag   21       6  160    110  3.9   2.88     0     1
#> 3 Merc 230        22.8     4  141.    95  3.92  3.15     1     0
#> 4 Datsun 710      22.8    NA  109     93  3.85  2.32     1     1
#> 5 Merc 240D       24.4     4  147.    62  3.69  3.19     1     0
#> 6 Hornet 4 Drive  21.4     6  259    110  3.08  3.22     1     0
#> 7 Mazda RX4       21       6  160    110  3.9   2.62     0     1
#> 8 Valiant         18.1     6  225    105  2.76  3.46     1     0
#> 9 Merc 280        19.2     6  168.   123  3.92  3.44     1     0
example_df_b
#> # A tibble: 10 × 9
#>    car               wt   mpg    hp   cyl  disp  carb  drat    vs
#>    <chr>          <dbl> <dbl> <int> <int> <dbl> <int> <dbl> <int>
#>  1 Merc 240D       3.19  26.4    62     4  147.     2  3.69     1
#>  2 Valiant         3.46  18.1   105     6  225      1  2.76     1
#>  3 Duster 360      3.57  16.3   245     8  360      4  3.21     0
#>  4 Datsun 710      2.32  22.8    93    NA  108      1  3.85     1
#>  5 Merc 280C       3.44  17.8   123     6  168.     4  3.92     1
#>  6 Merc 280        3.44  19.2   123     6  168.     4  3.92     1
#>  7 Hornet 4 Drive  3.22  21.4   110     6  258      1  3.08     1
#>  8 Merc 450SE      4.07  16.4   180     8  276.     3  3.07     0
#>  9 Merc 230        3.15  22.8    95     4  141.     2  3.92     1
#> 10 Mazda RX4 Wag   2.88  21     110     6  160      4  3.9      0
```

Use `compare()` to create a comparison of two tables.

A comparison contains:

- `compare()$intersection`: columns in both tables and rows with
  differing values
- `compare()$unmatched_cols`: columns in only one table
- `compare()$unmatched_rows`: rows in only one table

``` r
comparison <- compare(example_df_a, example_df_b, by = car)
comparison
#> $tables
#> # A tibble: 2 × 4
#>   table   expr          nrow  ncol
#>   <chr>   <chr>        <int> <int>
#> 1 table_a example_df_a     9     9
#> 2 table_b example_df_b    10     9
#> 
#> $by
#> # A tibble: 1 × 3
#>   column class_a   class_b  
#>   <chr>  <chr>     <chr>    
#> 1 car    character character
#> 
#> $intersection
#> # A tibble: 7 × 5
#>   column n_diffs class_a class_b diff_rows       
#>   <chr>    <int> <chr>   <chr>   <list>          
#> 1 mpg          2 numeric numeric <tibble [2 × 2]>
#> 2 cyl          0 integer integer <tibble [0 × 2]>
#> 3 disp         2 numeric numeric <tibble [2 × 2]>
#> 4 hp           0 integer integer <tibble [0 × 2]>
#> 5 drat         0 numeric numeric <tibble [0 × 2]>
#> 6 wt           0 numeric numeric <tibble [0 × 2]>
#> 7 vs           0 integer integer <tibble [0 × 2]>
#> 
#> $unmatched_cols
#> # A tibble: 2 × 2
#>   table column
#>   <chr> <chr> 
#> 1 a     am    
#> 2 b     carb  
#> 
#> $unmatched_rows
#> # A tibble: 3 × 3
#>   table car          row
#>   <chr> <chr>      <int>
#> 1 a     Mazda RX4      7
#> 2 b     Merc 280C      5
#> 3 b     Merc 450SE     8
```

Use `value_diffs()` to see the values that are different.

``` r
comparison |>
  value_diffs(disp)
#> # A tibble: 2 × 3
#>   disp_a disp_b car           
#>    <dbl>  <dbl> <chr>         
#> 1    109    108 Datsun 710    
#> 2    259    258 Hornet 4 Drive
comparison |>
  value_diffs_stacked(c(mpg, disp))
#> # A tibble: 4 × 4
#>   column val_a val_b car           
#>   <chr>  <dbl> <dbl> <chr>         
#> 1 mpg     14.3  16.3 Duster 360    
#> 2 mpg     24.4  26.4 Merc 240D     
#> 3 disp   109   108   Datsun 710    
#> 4 disp   259   258   Hornet 4 Drive
```

Use `weave_diffs_*()` to see the differing values in context.

``` r
comparison |>
  weave_diffs_wide(disp)
#> # A tibble: 2 × 9
#>   car              mpg   cyl disp_a disp_b    hp  drat    wt    vs
#>   <chr>          <dbl> <int>  <dbl>  <dbl> <int> <dbl> <dbl> <int>
#> 1 Datsun 710      22.8    NA    109    108    93  3.85  2.32     1
#> 2 Hornet 4 Drive  21.4     6    259    258   110  3.08  3.22     1
comparison |>
  weave_diffs_wide(c(mpg, disp))
#> # A tibble: 4 × 10
#>   car            mpg_a mpg_b   cyl disp_a disp_b    hp  drat    wt    vs
#>   <chr>          <dbl> <dbl> <int>  <dbl>  <dbl> <int> <dbl> <dbl> <int>
#> 1 Duster 360      14.3  16.3     8   360    360    245  3.21  3.57     0
#> 2 Merc 240D       24.4  26.4     4   147.   147.    62  3.69  3.19     1
#> 3 Datsun 710      22.8  22.8    NA   109    108     93  3.85  2.32     1
#> 4 Hornet 4 Drive  21.4  21.4     6   259    258    110  3.08  3.22     1
comparison |>
  weave_diffs_long(disp)
#> # A tibble: 4 × 9
#>   table car              mpg   cyl  disp    hp  drat    wt    vs
#>   <chr> <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int>
#> 1 a     Datsun 710      22.8    NA   109    93  3.85  2.32     1
#> 2 b     Datsun 710      22.8    NA   108    93  3.85  2.32     1
#> 3 a     Hornet 4 Drive  21.4     6   259   110  3.08  3.22     1
#> 4 b     Hornet 4 Drive  21.4     6   258   110  3.08  3.22     1
```

Use `slice_diffs()` to get the rows with differing values from one
table.

``` r
comparison |>
  slice_diffs("a", mpg)
#> # A tibble: 2 × 9
#>   car          mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>      <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Duster 360  14.3     8  360    245  3.21  3.57     0     0
#> 2 Merc 240D   24.4     4  147.    62  3.69  3.19     1     0
```

Use `slice_unmatched()` to get the rows unmatched rows from one or both
tables.

``` r
comparison |>
  slice_unmatched("a")
#> # A tibble: 1 × 9
#>   car         mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>     <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Mazda RX4    21     6   160   110   3.9  2.62     0     1
comparison |>
  slice_unmatched_both()
#> # A tibble: 3 × 9
#>   table car          mpg   cyl  disp    hp  drat    wt    vs
#>   <chr> <chr>      <dbl> <int> <dbl> <int> <dbl> <dbl> <int>
#> 1 a     Mazda RX4   21       6  160    110  3.9   2.62     0
#> 2 b     Merc 280C   17.8     6  168.   123  3.92  3.44     1
#> 3 b     Merc 450SE  16.4     8  276.   180  3.07  4.07     0
```

Use `summary()` to see what kind of differences were found

``` r
summary(comparison)
#> # A tibble: 4 × 2
#>   difference     found
#>   <chr>          <lgl>
#> 1 value_diffs    TRUE 
#> 2 unmatched_cols TRUE 
#> 3 unmatched_rows TRUE 
#> 4 class_diffs    FALSE
```
