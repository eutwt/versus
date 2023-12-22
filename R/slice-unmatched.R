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
slice_unmatched <- function(comparison, table) {
  validate_table_arg(enquo(table))
  assert_is_comparison(enquo(comparison))
  slice_unmatched_impl(comparison, table)
}

#' @rdname slice_unmatched
#' @export
slice_unmatched_both <- function(comparison) {
  assert_is_comparison(enquo(comparison))

  out_cols <- with(comparison, c(by$column, intersection$column))
  unmatched <- c(a = "a", b = "b") %>%
    map(\(table) slice_unmatched_impl(comparison, table, j = out_cols)) %>%
    ensure_ptype_compatible()

  bind_rows(unmatched, .id = "table")
}


# Helpers ---------

slice_unmatched_impl <- function(comparison, table, j) {
  rows <- comparison$unmatched_rows %>%
    fsubset(comparison$unmatched_rows$table == table, "row") %>%
    pull(1)

  out <- fsubset(comparison$input$value[[table]], rows, j)
  as_tibble(out)
}
