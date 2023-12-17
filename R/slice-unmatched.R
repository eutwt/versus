#' Show rows in only one table
#'
#' @inheritParams slice_diffs
#'
#' @return
#' \item{\code{slice_unmatched()}}{The input \code{table} is filtered to only the rows
#' \code{comparison} shows as only appearing in \code{table}}
#' \item{\code{slice_unmatched_both()}}{The output of \code{slice_unmatched()} for both input
#' tables row-stacked with a column `table` indicating which table the row
#' is from. The output contains only columns present in both tables.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' example_df_a |> slice_unmatched(comp)
#' # slice_unmatched() output is the same as
#' example_df_a |> dplyr::anti_join(example_df_b, by = comp$by$column)
#' slice_unmatched_both(example_df_a, example_df_b, comp)

#' @rdname slice_unmatched
#' @export
slice_unmatched <- function(table, comparison) {
  validate_comparison(enquo(comparison))
  assert_has_columns(table, comparison$by$column)
  assert_ptype_compatible(table, table_init(comparison, cols = "by"))

  out <- join(
    table,
    comparison$unmatched_rows,
    on = comparison$by$column,
    how = "semi",
    verbose = FALSE,
    overid = 2
  )
  as_tibble(out)
}

#' @rdname slice_unmatched
#' @export
slice_unmatched_both <- function(table_a, table_b, comparison) {
  validate_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)

  unmatched <- list("a" = table_a, "b" = table_b) %>%
    map(slice_unmatched, comparison) %>%
    map(fsubset, j = required_columns)

  # if the column types are incompatible, convert them to character first
  is_incompatible <- !is_ptype_compatible(unmatched$a, unmatched$b)
  if (any(is_incompatible)) {
    incompatible_cols <- names(is_incompatible)[is_incompatible]
    cols_char <- dottize(incompatible_cols, 30)
    cli_alert_info("Columns converted to character: {cols_char}")

    unmatched <- unmatched %>%
      map(\(x) mutate(x, across(all_of(incompatible_cols), as.character)))
  }

  bind_rows(unmatched, .id = "table")
}
