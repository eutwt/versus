# Compare two data frames

`compare()` creates a representation of the differences between two
tables, along with a shallow copy of the tables. This output is used as
the `comparison` argument when exploring the differences further with
other versus functions e.g. `slice_*()` and `weave_*()`.

## Usage

``` r
compare(
  table_a,
  table_b,
  by,
  allow_both_NA = TRUE,
  coerce = TRUE,
  table_id = c("a", "b")
)
```

## Arguments

- table_a:

  A data frame

- table_b:

  A data frame

- by:

  \<[`tidy-select`](https://eutwt.github.io/versus/reference/versus_tidy_select.md)\>.
  Selection of columns to use when matching rows between `table_a` and
  `table_b`. Both data frames must be unique on `by`.

- allow_both_NA:

  Logical. If `TRUE` a missing value in both data frames is considered
  as equal

- coerce:

  Logical. If `FALSE` and columns from the input tables have differing
  classes, the function throws an error.

- table_id:

  A character vector of length 2 providing custom identifiers for
  `table_a` and `table_b` respectively. These identifiers are used in
  the output instead of the default "a" and "b".

## Value

- `compare()`:

  A list of data frames having the following elements:

  tables

  :   A data frame with one row per input table showing the number of
      rows and columns in each.

  by

  :   A data frame with one row per `by` column showing the class of the
      column in each of the input tables.

  intersection

  :   A data frame with one row per column common to `table_a` and
      `table_b` and columns "n_diffs" showing the number of values which
      are different between the two tables, "class_a"/"class_b" the
      class of the column in each table, and "value_diffs" a (nested)
      data frame showing the the row indices with differing values

  unmatched_cols

  :   A data frame with one row per column which is in one input table
      but not the other and columns "table": which table the column
      appears in, "column": the name of the column, and "class": the
      class of the column.

  unmatched_rows

  :   A data frame which, for each row present in one input table but
      not the other, contains the column "table" showing which table the
      row appears in and the `by` columns for that row.

## data.table inputs

If the input is a data.table, you may want `compare()` to make a deep
copy instead of a shallow copy so that future changes to the table don't
affect the comparison. To achieve this, you can set
`options(versus.copy_data_table = TRUE)`.

## Examples

``` r
compare(example_df_a, example_df_b, by = car)
#> $tables
#> # A tibble: 2 × 4
#>   table expr          nrow  ncol
#>   <chr> <chr>        <int> <int>
#> 1 a     example_df_a     9     9
#> 2 b     example_df_b    10     9
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
#> 
```
