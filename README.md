
<!-- README.md is generated from README.Rmd. Please edit that file -->

# versus <img id="logo" src="man/figures/logo.png" align="right" width="17%" height="17%" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/eutwt/versus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eutwt/versus/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/versus)](https://CRAN.R-project.org/package=versus)
[![Codecov test
coverage](https://codecov.io/gh/eutwt/versus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/eutwt/versus?branch=main)
<!-- badges: end -->

## Overview

A toolset for interactively exploring the differences between two data
frames.

## Installation

``` r
install.packages("versus")

# Or install the development version from GitHub with
# pak::pak("eutwt/versus")
```

## Example

We will use the two data frames below as an example to demonstrate
functionality

``` r
library(versus)

example_df_a
#>              car  mpg cyl  disp  hp drat    wt vs am
#> 1     Duster 360 14.3   8 360.0 245 3.21 3.570  0  0
#> 2  Mazda RX4 Wag 21.0   6 160.0 110 3.90 2.875  0  1
#> 3       Merc 230 22.8   4 140.8  95 3.92 3.150  1  0
#> 4     Datsun 710 22.8  NA 109.0  93 3.85 2.320  1  1
#> 5      Merc 240D 24.4   4 146.7  62 3.69 3.190  1  0
#> 6 Hornet 4 Drive 21.4   6 259.0 110 3.08 3.215  1  0
#> 7      Mazda RX4 21.0   6 160.0 110 3.90 2.620  0  1
#> 8        Valiant 18.1   6 225.0 105 2.76 3.460  1  0
#> 9       Merc 280 19.2   6 167.6 123 3.92 3.440  1  0
example_df_b
#>               car    wt  mpg  hp cyl  disp carb drat vs
#> 1       Merc 240D 3.190 26.4  62   4 146.7    2 3.69  1
#> 2         Valiant 3.460 18.1 105   6 225.0    1 2.76  1
#> 3      Duster 360 3.570 16.3 245   8 360.0    4 3.21  0
#> 4      Datsun 710 2.320 22.8  93  NA 108.0    1 3.85  1
#> 5       Merc 280C 3.440 17.8 123   6 167.6    4 3.92  1
#> 6        Merc 280 3.440 19.2 123   6 167.6    4 3.92  1
#> 7  Hornet 4 Drive 3.215 21.4 110   6 258.0    1 3.08  1
#> 8      Merc 450SE 4.070 16.4 180   8 275.8    3 3.07  0
#> 9        Merc 230 3.150 22.8  95   4 140.8    2 3.92  1
#> 10  Mazda RX4 Wag 2.875 21.0 110   6 160.0    4 3.90  0
```

Use `compare()` to see

- The number of differing values in each column -
  `compare()$intersection`
- Which columns are in only one table - `compare()$unmatched_cols`
- Which rows are in only one table - `compare()$unmatched_rows`

``` r
comp <- compare(example_df_a, example_df_b, by = car)
comp
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
#>   column n_diffs class_a class_b value_diffs     
#>   <chr>    <int> <chr>   <chr>   <list>          
#> 1 mpg          2 numeric numeric <tibble [2 × 3]>
#> 2 cyl          0 integer integer <tibble [0 × 3]>
#> 3 disp         2 numeric numeric <tibble [2 × 3]>
#> 4 hp           0 integer integer <tibble [0 × 3]>
#> 5 drat         0 numeric numeric <tibble [0 × 3]>
#> 6 wt           0 numeric numeric <tibble [0 × 3]>
#> 7 vs           0 integer integer <tibble [0 × 3]>
#> 
#> $unmatched_cols
#> # A tibble: 2 × 2
#>   table column
#>   <chr> <chr> 
#> 1 a     am    
#> 2 b     carb  
#> 
#> $unmatched_rows
#> # A tibble: 3 × 2
#>   table car       
#>   <chr> <chr>     
#> 1 a     Mazda RX4 
#> 2 b     Merc 280C 
#> 3 b     Merc 450SE
```

Use `summary()` to see what kind of differences were found

``` r
summary(comp)
#> # A tibble: 4 × 2
#>   difference     found
#>   <chr>          <lgl>
#> 1 value_diffs    TRUE 
#> 2 unmatched_cols TRUE 
#> 3 unmatched_rows TRUE 
#> 4 class_diffs    FALSE
```

Use `value_diffs()` to see the values that are different.

``` r
value_diffs(comp, disp)
#> # A tibble: 2 × 3
#>   disp_a disp_b car           
#>    <dbl>  <dbl> <chr>         
#> 1    109    108 Datsun 710    
#> 2    259    258 Hornet 4 Drive
value_diffs(comp, mpg)
#> # A tibble: 2 × 3
#>   mpg_a mpg_b car       
#>   <dbl> <dbl> <chr>     
#> 1  14.3  16.3 Duster 360
#> 2  24.4  26.4 Merc 240D
```

Use `value_diffs_all()` to combine all `value_diffs()` output into one
table

``` r
value_diffs_all(comp)
#> # A tibble: 4 × 4
#>   column val_a val_b car           
#>   <chr>  <dbl> <dbl> <chr>         
#> 1 mpg     14.3  16.3 Duster 360    
#> 2 mpg     24.4  26.4 Merc 240D     
#> 3 disp   109   108   Datsun 710    
#> 4 disp   259   258   Hornet 4 Drive
```
