#' Get differences in context
#'
#' @inheritParams slice_diffs
#'
#' @return
#' \item{\code{weave_diffs_wide()}}{The input \code{table_a} filtered to rows where
#' differing values exist for one of the columns selected by \code{column}.
#' The selected columns with differences will be in the result twice, one for
#' each input table.}
#'
#' \item{\code{weave_diffs_long()}}{Input tables are filtered to rows where
#' differing values exist for one of the columns selected by \code{column}.
#' These two sets of rows (one for each input table) are interleaved row-wise.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' comp |> weave_diffs_wide(disp)
#' comp |> weave_diffs_wide(c(mpg, disp))
#' comp |> weave_diffs_long(disp)
#' comp |> weave_diffs_long(c(mpg, disp))

#' @rdname weave_diffs
#' @export
weave_diffs_long <- function(comparison, column = everything()) {
  assert_is_comparison(enquo(comparison))
  call <- current_env()
  column <- enquo(column)

  out_cols <- with(comparison, c(by$column, intersection$column))
  diff <- comparison$input$value %>%
    Map(f = \(x, table) {
      slice_diffs_impl(comparison, table, column, j = out_cols, call = call) %>%
        mutate(table = .env$table, .before = 1)
    }, ., names(.)) %>%
    ensure_ptype_compatible()

  vec_interleave(!!!diff)
}

#' @rdname weave_diffs
#' @export
weave_diffs_wide <- function(comparison, column = everything()) {
  assert_is_comparison(enquo(comparison))
  column <- enquo(column)

  out_cols <- with(comparison, c(by$column, intersection$column))
  diff_cols <- names(identify_diff_cols(comparison, column))
  slice_a <- slice_diffs_impl(comparison, "a", column, j = out_cols)
  slice_b <- slice_diffs_impl(comparison, "b", column, j = diff_cols)

  reduce(.init = slice_a, diff_cols, \(x, col) {
    x %>%
      mutate("{col}_b" := slice_b[[col]], .after = !!sym(col)) %>%
      rename("{col}_a" := !!sym(col))
  })
}
