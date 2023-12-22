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
slice_diffs <- function(comparison, table, column = everything()) {
  assert_is_comparison(enquo(comparison))
  validate_table_arg(enquo(table))
  column <- enquo(column)
  slice_diffs_impl(comparison, table, column)
}

slice_diffs_impl <- function(comparison, table, column, j, call = caller_env()) {
  select_row <- function(value_diffs, col_name) {
    fsubset(value_diffs, j = paste0("row_", table))
  }
  rows <- stack_value_diffs(
    comparison,
    column,
    pre_stack_fun = select_row,
    call = call
  ) %>%
    pull(1) %>%
    funique()

  out <- fsubset(comparison$input$value[[table]], rows, j)
  as_tibble(out)
}
