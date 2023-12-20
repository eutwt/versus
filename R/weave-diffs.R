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
#' weave_diffs_wide(example_df_a, example_df_b, comp)
#' weave_diffs_long(example_df_a, example_df_b, comp)

#' @rdname weave_diffs
#' @export
weave_diffs_long <- function(table_a, table_b, comparison, column = everything()) {
  column <- enquo(column)
  assert_is_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)

  diffs <- list("a" = table_a, "b" = table_b) %>%
    map(slice_diffs_for_weave,
      j = required_columns,
      column = column,
      comparison = comparison
    ) %>%
    imap(\(x, nm) mutate(x, table = nm, .before = 1)) %>%
    ensure_ptype_compatible()

  vec_interleave(!!!diffs)
}

#' @rdname weave_diffs
#' @export
weave_diffs_wide <- function(table_a, table_b, comparison, column = everything()) {
  column <- enquo(column)
  assert_is_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)

  slice_a <- table_a %>%
    slice_diffs_for_weave(j = required_columns, column = column, comparison = comparison)
  if (nrow(slice_a) == 0) {
    return(slice_a)
  }
  diff_cols <- names(identify_value_diffs(comparison, column))
  slice_b <- slice_diffs_for_weave(
    table_b,
    j = c(comparison$by$column, diff_cols),
    column = column,
    comparison = comparison
  )
  for (col in diff_cols) {
    slice_a <- slice_a %>%
      mutate(!!paste0(col, "_b") := slice_b[[col]], .after = all_of(col)) %>%
      rename_with(\(x) paste0(x, "_a"), .cols = all_of(col))
  }
  slice_a
}

# Helpers -------------------

slice_diffs_for_weave <- function(df, j, column, comparison) {
  df %>%
    fsubset(j = j) %>%
    slice_diffs_impl(comparison, column) %>%
    roworderv(comparison$by$column)
}
