#' Get the differing values from a comparison
#'
#' @param comparison The output of \code{compare()}
#' @param column <[`tidy-select`][versus_tidy_select]>. The output will show the differing values
#' for the provided columns.
#'
#' @return
#' \item{\code{value_diffs()}}{A data frame with one row for each element
#' of \code{col} found to be unequal between the input tables (
#' \code{table_a} and \code{table_b} from the original \code{compare()} output)
#' The output table has the column specified by \code{column} from each of the
#' input tables, plus the \code{by} columns. }
#'
#' \item{\code{value_diffs_stacked()}, \code{value_diffs_all()}}{A data frame containing
#' the \code{value_diffs()} outputs for the specified columns combined row-wise
#' using \code{dplyr::bind_rows()}. If \code{dplyr::bind_rows()} is not possible
#' due to incompatible types, values are converted to character first.
#' \code{value_diffs_all()} is the same as \code{value_diffs_stacked()} with
#' \code{column = everything()}}
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' value_diffs(comp, disp)
#' value_diffs_stacked(comp, c(disp, mpg))

#' @rdname value-diffs
#' @export
value_diffs <- function(comparison, column) {
  assert_is_comparison(enquo(comparison))
  column <- enquo(column)
  column_loc <- get_cols_from_comparison(comparison, column)
  assert_is_single_column(column_loc)

  diff_rows <- fsubset(comparison$intersection, column_loc, "diff_rows")[[1]][[1]]
  col <- names(column_loc)
  a <- comparison$input$value$a %>%
    fsubset(diff_rows$row_a, col) %>%
    rename("{col}_a" := !!sym(col))
  b <- comparison$input$value$b %>%
    fsubset(diff_rows$row_b, c(col, comparison$by$column)) %>%
    rename("{col}_b" := !!sym(col))
  tibble(a, b)
}

#' @rdname value-diffs
#' @export
value_diffs_stacked <- function(comparison, column = everything()) {
  assert_is_comparison(enquo(comparison))
  column <- enquo(column)

  get_value_diff_for_stack <- function(comparison, col_name) {
    value_diffs(comparison, all_of(col_name)) %>%
      frename(c("val_a", "val_b"), cols = 1:2) %>%
      mutate(column = .env$col_name, .before = 1)
  }

  diff_cols <- identify_diff_cols(comparison, column) %0%
    get_cols_from_comparison(comparison, column)

  names(diff_cols) %>%
    lapply(get_value_diff_for_stack, comparison = comparison) %>%
    ensure_ptype_compatible() %>%
    bind_rows()
}

# Helpers -------------------

assert_is_single_column <- function(column_loc, call = caller_env()) {
  if (length(column_loc) == 1) {
    return(invisible())
  }
  cols_selected <- dottize(names(column_loc), 30)
  message <- c(
    "Must select only one column.",
    i = "Columns selected: {cols_selected}",
    i = "For multiple columns, use `value_diffs_stacked()`"
  )
  cli_abort(message, call = call)
}
