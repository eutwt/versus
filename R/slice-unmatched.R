#' Show rows in only one table
#'
#' @inheritParams slice_diffs
#' @param table_a The data frame\code{table_a} used to create \code{comparison}
#' @param table_b The data frame\code{table_b} used to create \code{comparison}
#'
#' @return
#' \item{\code{slice_unmatched()}}{The input \code{table} is filtered to the rows
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
  assert_is_comparison(enquo(comparison))
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
  assert_is_comparison(enquo(comparison))
  required_columns <- with(comparison, c(by$column, intersection$column))
  assert_has_columns(table_a, required_columns)
  assert_has_columns(table_b, required_columns)

  unmatched <- list("a" = table_a, "b" = table_b) %>%
    map(fsubset, j = required_columns) %>%
    map(slice_unmatched, comparison) %>%
    ensure_ptype_compatible()

  bind_rows(unmatched, .id = "table")
}
