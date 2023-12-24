#' Get rows in only one table
#'
#' @inheritParams slice_diffs
#'
#' @return
#' \item{\code{slice_unmatched()}}{The table identified by \code{table} is filtered
#' to the rows \code{comparison} shows as not appearing in the other table}
#' \item{\code{slice_unmatched_both()}}{The output of \code{slice_unmatched()} for both input
#' tables row-stacked with a column `table` indicating which table the row
#' is from. The output contains only columns present in both tables.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' comp |> slice_unmatched("a")
#' comp |> slice_unmatched("b")
#'
#' # slice_unmatched(comp, "a") output is the same as
#' example_df_a |> dplyr::anti_join(example_df_b, by = comp$by$column)
#'
#' comp |> slice_unmatched_both()

#' @rdname slice_unmatched
#' @export
slice_unmatched <- function(comparison, table) {
  assert_is_comparison(enquo(comparison))
  assert_table_is_a_or_b(enquo(table))
  slice_unmatched_impl(comparison, table)
}

#' @rdname slice_unmatched
#' @export
slice_unmatched_both <- function(comparison) {
  assert_is_comparison(enquo(comparison))

  out_cols <- with(comparison, c(by$column, intersection$column))

  c(a = "a", b = "b") %>%
    map(slice_unmatched_impl, comparison = comparison, j = out_cols) %>%
    ensure_ptype_compatible() %>%
    bind_rows(.id = "table")
}


# Helpers ---------

slice_unmatched_impl <- function(comparison, table, j) {
  rows <- comparison$unmatched_rows %>%
    fsubset(comparison$unmatched_rows$table == table, "row") %>%
    pull(1)

  out <- fsubset(comparison$input$value[[table]], rows, j)
  as_tibble(out)
}
