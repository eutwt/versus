#' Combine `slice_diffs()` output from both tables
#'
#' @inheritParams slice_diffs
#' @param table_a The data frame\code{table_a} used to create \code{comparison}
#' @param table_b The data frame\code{table_b} used to create \code{comparison}
#'
#' @return
#' \item{\code{weave_diffs_long()}}{The output of \code{slice_diffs()} for both input
#' tables weaved together row-wise with a column `table` indicating which table the row
#' is from. The output contains only columns present in both tables.}
#' \item{\code{weave_diffs_wide()}}{The output of \code{slice_diffs()} for \code{table_a}
#' with columns from \code{table_b} added for columns with differing values. The
#' output contains only columns present in both tables.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' weave_diffs_wide(example_df_a, example_df_b, comp, column = disp)
#' weave_diffs_long(example_df_a, example_df_b, comp, column = disp)

#' @rdname weave_diffs
#' @export
weave_diffs_long <- function(table_a, table_b, comparison, column = everything()) {
  check_required(table_a)
  check_required(table_b)
  column <- enquo(column)
  assert_is_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)
  call <- current_env()

  diff <- list("a" = table_a, "b" = table_b) %>%
    Map(f = \(x, nm) {
      fsubset(x, j = required_columns) %>%
        slice_diffs_impl(comparison, column, name = nm, call = call) %>%
        mutate(table = nm, .before = 1)
    }, ., names(.)) %>%
    ensure_ptype_compatible()

  matches <- locate_matches(diff$a, diff$b, by = comparison$by$column)
  diff$b <- fsubset(diff$b, matches$haystack$common)
  vec_interleave(!!!diff)
}

#' @rdname weave_diffs
#' @export
weave_diffs_wide <- function(table_a, table_b, comparison, column = everything()) {
  check_required(table_a)
  check_required(table_b)
  column <- enquo(column)
  assert_is_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)

  slice_a <- table_a %>%
    fsubset(j = required_columns) %>%
    slice_diffs_impl(comparison = comparison, column = column, name = "a")
  diff_cols <- names(identify_value_diffs(comparison, column))
  if (is_empty(diff_cols)) {
    return(slice_a)
  }
  slice_b <- table_b %>%
    fsubset(j = c(comparison$by$column, diff_cols)) %>%
    slice_diffs_impl(comparison, column = column, name = "b")
  matches <- locate_matches(slice_a, slice_b, by = comparison$by$column)
  slice_b <- fsubset(slice_b, matches$haystack$common, j = diff_cols)

  purrr::reduce(.init = slice_a, diff_cols, \(x, col) {
    x %>%
      mutate(!!glue("{col}_b") := slice_b[[col]], .after = !!sym(col)) %>%
      rename(!!glue("{col}_a") := !!sym(col))
  })
}
