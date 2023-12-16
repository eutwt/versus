#' Show rows which have differing values in a comparison
#'
#' @param table A data frame. One of \code{table_a} or \code{table_b} used to
#' create \code{comparison}
#' @param table_a The data frame\code{table_a} used to create \code{comparison}
#' @param table_b The data frame\code{table_b} used to create \code{comparison}
#' @param column <[`tidy-select`][versus_tidy_select]>. A row will be in the output of slice functions if
#' the comparison shows differing values for any columns matching this argument
#' @param comparison The output of \code{compare()}
#'
#' @return
#' \item{\code{slice_diffs()}}{The input \code{table} is filtered to only the rows for which
#' \code{comparison} shows differing values for one of the columns in the supplied
#' \code{column} argument}
#' \item{\code{slice_diffs_both()}}{The output of \code{slice_diffs()} for both input
#' tables with the rows interleaved and a column `table` indicating which table the row
#' is from. The output contains only columns present in both tables.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' example_df_a |> slice_diffs(comp, c(disp, mpg))
#' slice_diffs_both(example_df_a, example_df_b, comp, column = disp)

#' @rdname slice_diffs
#' @export
slice_diffs <- function(table, comparison, column = everything()) {
  column <- enquo(column)
  validate_comparison(enquo(comparison))
  assert_has_columns(table, comparison$by$column)
  assert_same_types(table, table_init(comparison, cols = "by"))

  select_by_vars <- function(value_diffs, col_name) {
    fsubset(value_diffs, j = comparison$by$column)
  }
  by_vals_with_diffs <- stack_value_diffs(
    comparison,
    column,
    pre_stack_fun = select_by_vars
  )
  assert_same_types(table, by_vals_with_diffs)

  join(
    table,
    by_vals_with_diffs,
    on = comparison$by$column,
    how = "semi",
    verbose = FALSE,
    overid = 2
  ) %>%
    as_tibble()
}

#' @rdname slice_diffs
#' @export
slice_diffs_both <- function(table_a, table_b, comparison, column = everything()) {
  validate_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)

  slice_diffs_for_interleave <- function(df, name) {
    df %>%
      fsubset(j = required_columns) %>%
      slice_diffs(comparison, {{ column }}) %>%
      roworderv(comparison$by$column) %>%
      mutate(table = name, .before = 1)
  }

  diffs <- list("a" = table_a, "b" = table_b) %>%
    imap(\(x, nm) slice_diffs_for_interleave(x, nm))

  # if the column types are incompatible, convert them to character first
  is_incompatible <- !is_ptype_compatible(diffs$a, diffs$b)
  if (any(is_incompatible)) {
    incompatible_cols <- names(is_incompatible)[is_incompatible]
    cols_char <- dottize(incompatible_cols, 30)
    cli_alert_info("Columns converted to character: {cols_char}")

    diffs <- diffs %>%
      map(\(x) mutate(x, across(all_of(incompatible_cols), as.character)))
  }

  vec_interleave(!!!diffs)
}
