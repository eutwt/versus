#' Show rows which have differing values in a comparison
#'
#' @param table A data frame. One of \code{table_a} or \code{table_b} used to
#' create \code{comparison}
#' @param column <[`tidy-select`][versus_tidy_select]>. A row will be in the output if
#' the comparison shows differing values for any columns matching this argument
#' @param comparison The output of \code{compare()}
#'
#' @return
#' The input \code{table} is filtered to the rows for which
#' \code{comparison} shows differing values for one of the columns in the supplied
#' \code{column} argument
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' example_df_a |> slice_diffs(comp, c(disp, mpg))

#' @rdname slice_diffs
#' @export
slice_diffs <- function(table, comparison, column = everything()) {
  check_required(table)
  column <- enquo(column)
  assert_is_comparison(enquo(comparison))
  slice_diffs_impl(table, comparison, column)
}

slice_diffs_impl <- function(table, comparison, column, call = caller_env()) {
  assert_has_columns(table, comparison$by$column, call = call)
  assert_ptype_compatible(table, table_init(comparison, cols = "by"), call = call)

  select_by_vars <- function(value_diffs, col_name) {
    fsubset(value_diffs, j = comparison$by$column)
  }
  by_vals_with_diffs <- stack_value_diffs(
    comparison,
    column,
    pre_stack_fun = select_by_vars,
    call = call
  )

  out <- join(
    table,
    by_vals_with_diffs,
    on = comparison$by$column,
    how = "semi",
    verbose = FALSE,
    overid = 2
  )
  as_tibble(out)
}
