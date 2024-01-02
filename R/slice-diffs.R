#' Get rows with differing values
#'
#' @param comparison The output of \code{compare()}
#' @param table One of \code{"a"} or \code{"b"} indicating which of the tables used to
#' create \code{comparison} should be sliced
#' @param column <[`tidy-select`][versus_tidy_select]>. A row will be in the output if
#' the comparison shows differing values for any columns matching this argument
#'
#' @return
#' The input table is filtered to the rows for which \code{comparison}
#' shows differing values for one of the columns selected by \code{column}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' comp |> slice_diffs("a", mpg)
#' comp |> slice_diffs("b", mpg)
#' comp |> slice_diffs("a", c(mpg, disp))

#' @rdname slice_diffs
#' @export
slice_diffs <- function(comparison, table, column = everything()) {
  assert_is_comparison(enquo(comparison))
  assert_table_is_a_or_b(enquo(table))
  diff_cols <- identify_diff_cols(comparison, enquo(column))
  out <- slice_diffs_impl(comparison, table, diff_cols)
  new_tbl_versus(out, diff_cols)
}

slice_diffs_impl <- function(comparison, table, diff_cols, j, call = caller_env()) {
  if (is_empty(diff_cols)) {
    out <- fsubset(comparison$input$value[[table]], integer(0), j)
    return(as_tibble(out))
  }
  rows <- fsubset(comparison$intersection, diff_cols, "diff_rows")[[1]] %>%
    lapply(fsubset, j = paste0("row_", table)) %>%
    bind_rows() %>%
    distinct() %>%
    pull(1)

  fsubset(comparison$input$value[[table]], rows, j)
}
