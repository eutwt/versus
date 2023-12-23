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
  assert_table_is_a_or_b(enquo(table))
  slice_diffs_impl(comparison, table, enquo(column))
}

slice_diffs_impl <- function(comparison, table, column, j, call = caller_env()) {
  diff_cols <- identify_diff_cols(comparison, column)
  rows <- fsubset(comparison$intersection, diff_cols, "diff_rows")[[1]] %>%
    lapply(fsubset, j = paste0("row_", table)) %>%
    bind_rows() %>%
    distinct() %>%
    pull(1)

  out <- fsubset(comparison$input$value[[table]], rows, j)
  as_tibble(out)
}
