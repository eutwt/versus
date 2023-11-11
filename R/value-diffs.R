#' Show the differing values from a comparison

#' @param comparison The output of a \code{versus::compare()} call
#' @param column <[`tidy-select`][versus_tidy_select]>. The output will show the differing values
#' for the provided columns.
#'
#' @return
#' \item{\code{value_diffs()}}{A data frame with one row for each element
#' of \code{col} found to be unequal between the input tables (
#' \code{table_a} and \code{table_b} from the original \code{compare()} call)
#' The output table has columns "val_a"/"val_b": the value of \code{col} in the
#' input tables, and the \code{by} columns for the identified rows in the
#' input tables.}
#'
#' \item{\code{value_diffs_stacked()}, \code{value_diffs_all()}}{A data frame of
#' the \code{value_diffs()} output for the specified columns (or all columns if
#' \code{value_diffs_all()})
#' combined row-wise into a single table. To facilitate this combination into a
#' single table, the "val_a" and "val_b" columns are coerced to character.}

#' @rdname value-diffs
#' @export
value_diffs <- function(comparison, column) {
  column <- enquo(column)
  column_char <- get_cols_from_comparison(comparison, column)
  if (length(column_char) != 1) {
    cols_selected <- char_vec_display(column_char, 30)
    abort(c("Must select only one column.",
      i = glue("Columns selected: {cols_selected}"),
      i = "For multiple columns, use `value_diffs_stacked()`"
    ))
  }
  comparison$summ %>%
    filter(column == column_char) %>%
    pull(value_diffs) %>%
    `[[`(1)
}

#' @rdname value-diffs
#' @export
value_diffs_stacked <- function(comparison, column) {
  column <- enquo(column)

  conform <- function(value_diffs, col_name) {
    value_diffs %>%
      rename_with(\(x) replace(x, seq(2), paste0("val_", c("a", "b")))) %>%
      mutate(across(seq(2), as.character)) %>%
      mutate(column = col_name, .before = 1)
  }
  stack_value_diffs(comparison, column, pre_stack_fun = conform)
}

#' @rdname value-diffs
#' @export
value_diffs_all <- function(comparison) {
  value_diffs_stacked(comparison, everything())
}
