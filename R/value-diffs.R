#' Show the differing values from a comparison

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
  column <- enquo(column)
  validate_comparison(enquo(comparison))
  column_loc <- get_cols_from_comparison(comparison, column)
  if (length(column_loc) != 1) {
    cols_selected <- dottize(names(column_loc), 30)
    cli_abort(c("Must select only one column.",
      i = "Columns selected: {cols_selected}",
      i = "For multiple columns, use `value_diffs_stacked()`"
    ))
  }
  fsubset(comparison$intersection, column_loc, "value_diffs")[[1]][[1]]
}

#' @rdname value-diffs
#' @export
value_diffs_stacked <- function(comparison, column) {
  column <- enquo(column)
  validate_comparison(enquo(comparison))

  conform <- function(value_diffs, col_name) {
    out <- value_diffs %>%
      rename_with(\(x) replace(x, seq(2), paste0("val_", c("a", "b")))) %>%
      mutate(column = .env$col_name, .before = 1)
  }
  conform_with_coerce <- function(...) {
    conform(...) %>% mutate(across(c(val_a, val_b), as.character))
  }

  try_fetch(
    stack_value_diffs(comparison, column, pre_stack_fun = conform),
    vctrs_error_ptype2 = \(e) {
      # if we can't bind_rows() due to incompatible ptypes, convert to character first
      inform(c(i = "values converted to character"))
      stack_value_diffs(comparison, column, pre_stack_fun = conform_with_coerce)
    }
  )
}

#' @rdname value-diffs
#' @export
value_diffs_all <- function(comparison) {
  value_diffs_stacked(comparison, everything())
}

# Helpers -------------------

stack_value_diffs <- function(comparison, column, pre_stack_fun, call = caller_env()) {
  column_locs <- get_cols_from_comparison(comparison, column, call = call)
  is_selected <- seq(nrow(comparison$intersection)) %in% column_locs
  has_value_diffs <- comparison$intersection$n_diffs > 0
  to_stack <- which(is_selected & has_value_diffs) %0% which.max(is_selected)

  Map(
    pre_stack_fun,
    comparison$intersection$value_diffs[to_stack],
    comparison$intersection$column[to_stack]
  ) %>%
    bind_rows()
}
