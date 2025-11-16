# Changelog

## versus 0.3.1

CRAN release: 2025-11-15

- New `table_id` argument in
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)
  allows custom table identifiers. By default, outputs identify tables
  as “a” and “b”. You can now provide meaningful names
  e.g. `compare(..., table_id = c("original", "updated"))`, which are
  reflected in the output of
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md) and
  related functions. [@elipousson](https://github.com/elipousson)

- In the output of
  [`compare()`](https://eutwt.github.io/versus/reference/compare.md)
  with no `table_id` specified, the table summary `compare()$tables`
  uses “a” and “b” as identifiers for the `table` column rather than
  “table_a” and “table_b” as used in prior versions. This is for
  consistency with the case when custom ids are provided using the new
  `table_id` argument, so that `compare()$tables$table` always matches
  the `table_id` argument.

- [`weave_diffs_wide()`](https://eutwt.github.io/versus/reference/weave_diffs.md)
  gains a `suffix` argument so column names in the wide output can use
  custom suffixes instead of `_{table_id}`. The default keeps the
  behaviour of prior versions.
  [@elipousson](https://github.com/elipousson)

## versus 0.3.0

CRAN release: 2024-01-11

## versus 0.3

- The [`compare()`](https://eutwt.github.io/versus/reference/compare.md)
  function now saves a shallow copy of the input tables. This means
  functions using a comparison,
  e.g. [`slice_diffs()`](https://eutwt.github.io/versus/reference/slice_diffs.md),
  now only need the `comparison` argument, rather than requiring the
  original input tables to be re-supplied.

- New functions
  [`weave_diffs_wide()`](https://eutwt.github.io/versus/reference/weave_diffs.md)
  an
  [`weave_diffs_long()`](https://eutwt.github.io/versus/reference/weave_diffs.md)
  are added, which show the differing values in context with other
  columns from the input tables.

- A new function
  [`slice_unmatched()`](https://eutwt.github.io/versus/reference/slice_unmatched.md)
  is added, used to get the rows from input tables a comparison shows as
  not existing in the other table

## versus 0.2.1

CRAN release: 2023-12-11

- Patch to avoid error when input is data.table

## versus 0.2.0

CRAN release: 2023-12-10

- A new function `slice_diffs_both()` is added, used to get the
  differences from both input tables in a single output with the rows
  interleaved

- The error message for duplicated `by` values now shows the row which
  is duplicated

- Comparisons are now faster

## versus 0.1.0

CRAN release: 2023-11-12

- Initial CRAN submission.
