#' Combine `slice_diffs()` output from both tables
#'
#' @inheritParams slice_diffs
#' @param table_a The data frame\code{table_a} used to create \code{comparison}
#' @param table_b The data frame\code{table_b} used to create \code{comparison}
#'
#' @return
#' \item{\code{weave_diffs_row()}}{The output of \code{slice_diffs()} for both input
#' tables weaved together row-wise with a column `table` indicating which table the row
#' is from. The output contains only columns present in both tables.}
#' \item{\code{weave_diffs_col()}}{The output of \code{slice_diffs()} for \code{table_a}
#' with columns from \code{table_b} added for columns with differing values. The
#' output contains only columns present in both tables.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' weave_diffs_col(example_df_a, example_df_b, comp, column = disp)
#' weave_diffs_row(example_df_a, example_df_b, comp, column = disp)

#' @rdname weave_diffs
#' @export
weave_diffs_row <- function(comparison, column = everything()) {
  assert_is_comparison(enquo(comparison))
  call <- current_env()
  column <- enquo(column)

  out_cols <- with(comparison, c(by$column, intersection$column))
  diff <- comparison$input$value %>%
    Map(f = \(x, table) {
      slice_diffs_impl(comparison, table, column, j = out_cols, call = call) %>%
        mutate(table = nm, .before = 1)
    }, ., names(.)) %>%
    ensure_ptype_compatible()

  vec_interleave(!!!diff)
}

#' @rdname weave_diffs
#' @export
weave_diffs_col <- function(comparison, column = everything()) {
  assert_is_comparison(enquo(comparison))
  column <- enquo(column)

  out_cols <- with(comparison, c(by$column, intersection$column))
  diff_cols <- names(identify_diff_cols(comparison, column))
  slice_a <- slice_diffs_impl(comparison, "a", column, j = out_cols)
  slice_b <- slice_diffs_impl(comparison, "b", column, j = diff_cols)

  reduce(.init = slice_a, diff_cols, \(x, col) {
    x %>%
      mutate(!!glue("{col}_b") := slice_b[[col]], .after = !!sym(col)) %>%
      rename(!!glue("{col}_a") := !!sym(col))
  })
}
