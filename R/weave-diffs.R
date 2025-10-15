#' Get differences in context
#'
#' @inheritParams slice_diffs
#' @param suffix A character vector of length 2 providing suffixes appended to
#'   the renamed columns in `weave_diffs_wide()`. Set to `NULL` (the default) to
#'   use `paste0("_", table_id)`. The first suffix is applied to values from
#'   `table_a`, the second to values from `table_b`.
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
#' comp |> weave_diffs_wide(c(mpg, disp), suffix = c("", "_new"))
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
weave_diffs_wide <- function(comparison, column = everything(), suffix = NULL) {
  assert_is_comparison(enquo(comparison))
  column <- enquo(column)
  table_id <- comparison$tables$table
  suffix <- clean_suffix(suffix, table_id)

  out_cols <- with(comparison, c(by$column, intersection$column))
  diff_cols <- names(identify_diff_cols(comparison, column))
  slice_a <- slice_diffs_impl(comparison, table_id[1], column, j = out_cols)
  slice_b <- slice_diffs_impl(comparison, table_id[2], column, j = diff_cols)

  reduce(.init = slice_a, diff_cols, \(x, col) {
    col_first <- paste0(col, suffix[1])
    col_second <- paste0(col, suffix[2])
    x %>%
      rename("{col_first}" := !!sym(col)) %>%
      mutate("{col_second}" := slice_b[[col]], .after = !!sym(col_first))
  })
}

clean_suffix <- function(suffix, table_id, call = caller_env()) {
  if (is.null(suffix)) {
    return(paste0("_", table_id))
  }
  if (!is_character(suffix, n = 2)) {
    message <- c(
      "{.arg suffix} must be NULL or a character vector of length 2",
      i = "{.arg suffix} is {.obj_type_friendly {suffix}} of length {length(suffix)}"
    )
    cli_abort(message, call = call)
  }
  attributes(suffix) <- NULL
  if (anyNA(suffix)) {
    cli_abort("{.arg suffix} must not contain missing values.", call = call)
  }
  if (identical(suffix[1], suffix[2])) {
    cli_abort("{.arg suffix} entries must be distinct.", call = call)
  }
  suffix
}
