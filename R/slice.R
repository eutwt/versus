#' Show rows which have differing values in a comparison
#'
#' @param table A data frame. One of \code{table_a} or \code{table_b} used to
#' create \code{comparison}
#' @param column <[`tidy-select`][versus_tidy_select]>. A row will be in the output of slice functions if
#' the comparison shows differing values for any columns matching this argument
#' @param comparison The output of a \code{versus::compare()} call
#'
#' @return
#' A data frame. The input \code{table} is filtered to only the rows for which
#' \code{comparison} shows differing values for one of the columns in the supplied
#' \code{column} argument
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' example_df_a |> slice_diffs(comp, disp)
#' example_df_a |> slice_diffs(comp, c(disp, mpg))

#' @rdname slice_diffs
#' @export
slice_diffs <- function(table, comparison, column) {
  column <- enquo(column)
  select_by_vars <- function(value_diffs, col_name) {
    value_diffs %>%
      select(all_of(comparison$by$column))
  }
  indices_having_diffs <- stack_value_diffs(
    comparison,
    column,
    pre_stack_fun = select_by_vars
  )

  if (nrow(indices_having_diffs) == 0) {
    out <- table %>% slice(c())
  } else {
    out <- table %>%
      semi_join(indices_having_diffs, by = comparison$by$column)
  }
  out
}
