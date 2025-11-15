# Get rows with differing values

Get rows with differing values

## Usage

``` r
slice_diffs(comparison, table, column = everything())
```

## Arguments

- comparison:

  The output of
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)

- table:

  A string matching one of the identifiers supplied via `table_id` when
  calling
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)
  (defaults are `"a"` and `"b"`). Within the comparison, these
  identifiers are stored in `comparison$tables$table`.

- column:

  \<[`tidy-select`](https://eutwt.github.io/versus/reference/versus_tidy_select.md)\>.
  A row will be in the output if the comparison shows differing values
  for any columns matching this argument

## Value

The input table is filtered to the rows for which `comparison` shows
differing values for one of the columns selected by `column`

## Examples

``` r
comp <- compare(example_df_a, example_df_b, by = car)
comp |> slice_diffs("a", mpg)
#> # A tibble: 2 × 9
#>   car          mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>      <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Duster 360  14.3     8  360    245  3.21  3.57     0     0
#> 2 Merc 240D   24.4     4  147.    62  3.69  3.19     1     0
comp |> slice_diffs("b", mpg)
#> # A tibble: 2 × 9
#>   car           wt   mpg    hp   cyl  disp  carb  drat    vs
#>   <chr>      <dbl> <dbl> <int> <int> <dbl> <int> <dbl> <int>
#> 1 Duster 360  3.57  16.3   245     8  360      4  3.21     0
#> 2 Merc 240D   3.19  26.4    62     4  147.     2  3.69     1
comp |> slice_diffs("a", c(mpg, disp))
#> # A tibble: 4 × 9
#>   car              mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Duster 360      14.3     8  360    245  3.21  3.57     0     0
#> 2 Merc 240D       24.4     4  147.    62  3.69  3.19     1     0
#> 3 Datsun 710      22.8    NA  109     93  3.85  2.32     1     1
#> 4 Hornet 4 Drive  21.4     6  259    110  3.08  3.22     1     0

comp <- compare(example_df_a, example_df_b, by = car, table_id = c("old", "new"))
comp |> slice_diffs("old", mpg)
#> # A tibble: 2 × 9
#>   car          mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>      <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Duster 360  14.3     8  360    245  3.21  3.57     0     0
#> 2 Merc 240D   24.4     4  147.    62  3.69  3.19     1     0
```
