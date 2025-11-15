# Get the differing values from a comparison

Get the differing values from a comparison

## Usage

``` r
value_diffs(comparison, column)

value_diffs_stacked(comparison, column = everything())
```

## Arguments

- comparison:

  The output of
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)

- column:

  \<[`tidy-select`](https://eutwt.github.io/versus/reference/versus_tidy_select.md)\>.
  The output will show the differing values for the provided columns.

## Value

- `value_diffs()`: A data frame with one row for each element of `col`
  found to be unequal between the input tables ( `table_a` and `table_b`
  from the original
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)
  output). The output table has the column specified by `column` from
  each of the input tables, plus the `by` columns.

- `value_diffs_stacked()`: A data frame containing the `value_diffs()`
  outputs for the specified columns combined row-wise using
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  If
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  is not possible due to incompatible types, values are converted to
  character first.

## Examples

``` r
comp <- compare(example_df_a, example_df_b, by = car)
value_diffs(comp, disp)
#> # A tibble: 2 × 3
#>   disp_a disp_b car           
#>    <dbl>  <dbl> <chr>         
#> 1    109    108 Datsun 710    
#> 2    259    258 Hornet 4 Drive
value_diffs_stacked(comp, c(disp, mpg))
#> # A tibble: 4 × 4
#>   column val_a val_b car           
#>   <chr>  <dbl> <dbl> <chr>         
#> 1 mpg     14.3  16.3 Duster 360    
#> 2 mpg     24.4  26.4 Merc 240D     
#> 3 disp   109   108   Datsun 710    
#> 4 disp   259   258   Hornet 4 Drive
```
