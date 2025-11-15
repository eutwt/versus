# Get rows in only one table

Get rows in only one table

## Usage

``` r
slice_unmatched(comparison, table)

slice_unmatched_both(comparison)
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

## Value

- `slice_unmatched()`:

  The table identified by `table` is filtered to the rows `comparison`
  shows as not appearing in the other table

- `slice_unmatched_both()`:

  The output of `slice_unmatched()` for both input tables row-stacked
  with a column `table` indicating which table the row is from. The
  output contains only columns present in both tables.

## Examples

``` r
comp <- compare(example_df_a, example_df_b, by = car)
comp |> slice_unmatched("a")
#> # A tibble: 1 × 9
#>   car         mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>     <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Mazda RX4    21     6   160   110   3.9  2.62     0     1
comp |> slice_unmatched("b")
#> # A tibble: 2 × 9
#>   car           wt   mpg    hp   cyl  disp  carb  drat    vs
#>   <chr>      <dbl> <dbl> <int> <int> <dbl> <int> <dbl> <int>
#> 1 Merc 280C   3.44  17.8   123     6  168.     4  3.92     1
#> 2 Merc 450SE  4.07  16.4   180     8  276.     3  3.07     0

# slice_unmatched(comp, "a") output is the same as
example_df_a |> dplyr::anti_join(example_df_b, by = comp$by$column)
#> # A tibble: 1 × 9
#>   car         mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>     <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Mazda RX4    21     6   160   110   3.9  2.62     0     1

comp <- compare(example_df_a, example_df_b, by = car, table_id = c("old", "new"))
comp |> slice_unmatched("old")
#> # A tibble: 1 × 9
#>   car         mpg   cyl  disp    hp  drat    wt    vs    am
#>   <chr>     <dbl> <int> <dbl> <int> <dbl> <dbl> <int> <int>
#> 1 Mazda RX4    21     6   160   110   3.9  2.62     0     1

comp |> slice_unmatched_both()
#> # A tibble: 3 × 9
#>   table car          mpg   cyl  disp    hp  drat    wt    vs
#>   <chr> <chr>      <dbl> <int> <dbl> <int> <dbl> <dbl> <int>
#> 1 old   Mazda RX4   21       6  160    110  3.9   2.62     0
#> 2 new   Merc 280C   17.8     6  168.   123  3.92  3.44     1
#> 3 new   Merc 450SE  16.4     8  276.   180  3.07  4.07     0
```
