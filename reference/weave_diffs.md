# Get differences in context

Get differences in context

## Usage

``` r
weave_diffs_long(comparison, column = everything())

weave_diffs_wide(comparison, column = everything(), suffix = NULL)
```

## Arguments

- comparison:

  The output of
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)

- column:

  \<[`tidy-select`](https://eutwt.github.io/versus/reference/versus_tidy_select.md)\>.
  A row will be in the output if the comparison shows differing values
  for any columns matching this argument

- suffix:

  A character vector of length 2 providing suffixes appended to the
  renamed columns in `weave_diffs_wide()`. Set to `NULL` (the default)
  to use `paste0("_", table_id)`. The first suffix is applied to values
  from `table_a`, the second to values from `table_b`.

## Value

- `weave_diffs_wide()`:

  The input `table_a` filtered to rows where differing values exist for
  one of the columns selected by `column`. The selected columns with
  differences will be in the result twice, one for each input table.

- `weave_diffs_long()`:

  Input tables are filtered to rows where differing values exist for one
  of the columns selected by `column`. These two sets of rows (one for
  each input table) are interleaved row-wise.

## Examples

``` r
comp <- compare(example_df_a, example_df_b, by = car)
comp |> weave_diffs_wide(disp)
#> # A tibble: 2 × 9
#>   car              mpg   cyl disp_a disp_b    hp  drat    wt    vs
#>   <chr>          <dbl> <int>  <dbl>  <dbl> <int> <dbl> <dbl> <int>
#> 1 Datsun 710      22.8    NA    109    108    93  3.85  2.32     1
#> 2 Hornet 4 Drive  21.4     6    259    258   110  3.08  3.22     1
comp |> weave_diffs_wide(c(mpg, disp))
#> # A tibble: 4 × 10
#>   car            mpg_a mpg_b   cyl disp_a disp_b    hp  drat    wt    vs
#>   <chr>          <dbl> <dbl> <int>  <dbl>  <dbl> <int> <dbl> <dbl> <int>
#> 1 Duster 360      14.3  16.3     8   360    360    245  3.21  3.57     0
#> 2 Merc 240D       24.4  26.4     4   147.   147.    62  3.69  3.19     1
#> 3 Datsun 710      22.8  22.8    NA   109    108     93  3.85  2.32     1
#> 4 Hornet 4 Drive  21.4  21.4     6   259    258    110  3.08  3.22     1
comp |> weave_diffs_wide(c(mpg, disp), suffix = c("", "_new"))
#> # A tibble: 4 × 10
#>   car              mpg mpg_new   cyl  disp disp_new    hp  drat    wt    vs
#>   <chr>          <dbl>   <dbl> <int> <dbl>    <dbl> <int> <dbl> <dbl> <int>
#> 1 Duster 360      14.3    16.3     8  360      360    245  3.21  3.57     0
#> 2 Merc 240D       24.4    26.4     4  147.     147.    62  3.69  3.19     1
#> 3 Datsun 710      22.8    22.8    NA  109      108     93  3.85  2.32     1
#> 4 Hornet 4 Drive  21.4    21.4     6  259      258    110  3.08  3.22     1
comp |> weave_diffs_long(disp)
#> # A tibble: 4 × 9
#>   table car              mpg   cyl  disp    hp  drat    wt    vs
#>   <chr> <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int>
#> 1 a     Datsun 710      22.8    NA   109    93  3.85  2.32     1
#> 2 b     Datsun 710      22.8    NA   108    93  3.85  2.32     1
#> 3 a     Hornet 4 Drive  21.4     6   259   110  3.08  3.22     1
#> 4 b     Hornet 4 Drive  21.4     6   258   110  3.08  3.22     1
comp |> weave_diffs_long(c(mpg, disp))
#> # A tibble: 8 × 9
#>   table car              mpg   cyl  disp    hp  drat    wt    vs
#>   <chr> <chr>          <dbl> <int> <dbl> <int> <dbl> <dbl> <int>
#> 1 a     Duster 360      14.3     8  360    245  3.21  3.57     0
#> 2 b     Duster 360      16.3     8  360    245  3.21  3.57     0
#> 3 a     Merc 240D       24.4     4  147.    62  3.69  3.19     1
#> 4 b     Merc 240D       26.4     4  147.    62  3.69  3.19     1
#> 5 a     Datsun 710      22.8    NA  109     93  3.85  2.32     1
#> 6 b     Datsun 710      22.8    NA  108     93  3.85  2.32     1
#> 7 a     Hornet 4 Drive  21.4     6  259    110  3.08  3.22     1
#> 8 b     Hornet 4 Drive  21.4     6  258    110  3.08  3.22     1
```
