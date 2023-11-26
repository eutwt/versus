#' Show rows which have differing values in a comparison
#'
#' @param table A data frame. One of \code{table_a} or \code{table_b} used to
#' create \code{comparison}
#' @param table_a The data frame\code{table_a} used to create \code{comparison}
#' @param table_b The data frame\code{table_b} used to create \code{comparison}
#' @param column <[`tidy-select`][versus_tidy_select]>. A row will be in the output of slice functions if
#' the comparison shows differing values for any columns matching this argument
#' @param comparison The output of \code{compare()}
#'
#' @return
#' \item{\code{slice_diffs()}}{The input \code{table} is filtered to only the rows for which
#' \code{comparison} shows differing values for one of the columns in the supplied
#' \code{column} argument}
#' \item{\code{slice_diffs_both()}}{The output of \code{slice_diffs()} for both input
#' tables with the rows interleaved and a column `table` indicating which table the row
#' is from. The output contains only those columns present in both tables.}
#'
#' @examples
#' comp <- compare(example_df_a, example_df_b, by = car)
#' example_df_a |> slice_diffs(comp, disp)
#' example_df_a |> slice_diffs(comp, c(disp, mpg))
#' slice_diffs_both(example_df_a, example_df_b, comp, column = disp)

#' @rdname slice_diffs
#' @export
slice_diffs <- function(table, comparison, column = everything()) {
  column <- enquo(column)
  validate_comparison(enquo(comparison))
  validate_slice_diff_columns(table, comparison, type = "single")

  select_by_vars <- function(value_diffs, col_name) {
    fsubset(value_diffs, j = comparison$by$column)
  }
  by_vals_with_diffs <- stack_value_diffs(
    comparison,
    column,
    pre_stack_fun = select_by_vars
  )
  validate_slice_diff_types(table, by_vals_with_diffs)

  join(
    table,
    by_vals_with_diffs,
    on = comparison$by$column,
    how = "semi",
    verbose = FALSE,
    overid = 2
  ) %>%
    as_tibble()
}

#' @rdname slice_diffs
#' @export
slice_diffs_both <- function(table_a, table_b, comparison, column = everything()) {
  validate_comparison(enquo(comparison))
  validate_slice_diff_columns(table_a, comparison, type = "both")
  validate_slice_diff_columns(table_b, comparison, type = "both")

  output_cols <- c(comparison$by$column, comparison$intersection$column)
  slice_diffs_for_interleave <- function(df, name) {
    df %>%
      fsubset(j = output_cols) %>%
      slice_diffs(comparison, {{ column }}) %>%
      roworderv(comparison$by$column) %>%
      mutate(table = name, .before = 1)
  }
  diffs_a <- slice_diffs_for_interleave(table_a, "a")
  diffs_b <- slice_diffs_for_interleave(table_b, "b")

  # if the column types are incompatible, convert them to character first
  is_incompatible <- !is_ptype_compatible(diffs_a, diffs_b)
  if (any(is_incompatible)) {
    incompatible_cols <- names(is_incompatible)[is_incompatible]
    cols_char <- dottize(incompatible_cols, 30)
    cli_alert_info("Columns converted to character: {cols_char}")

    diffs_a <- diffs_a %>%
      mutate(across(all_of(incompatible_cols), as.character))
    diffs_b <- diffs_b %>%
      mutate(across(all_of(incompatible_cols), as.character))
  }

  vec_interleave(diffs_a, diffs_b)
}


# Helpers ------------

validate_slice_diff_columns <- function(table, comparison, type, call = caller_env()) {
  arg_name <- deparse(substitute(table))
  required_cols <- switch(type,
    single = comparison$by$column,
    both = c(comparison$by$column, comparison$intersection$column),
    cli_abort("Internal error")
  )
  not_present <- !(required_cols %in% names(table))
  if (any(not_present)) {
    missing_col <- required_cols[which.max(not_present)]
    message <- c(
      "`{arg_name}` is missing some columns from `comparison`",
      "column `{missing_col}` is not present in `{arg_name}`"
    )
    cli_abort(message, call = call)
  }
}

validate_slice_diff_types <- function(table, by_vals_with_diffs, call = caller_env()) {
  # collapse::join silently coerces join-key variables
  # don't want that, so check compatibility before join
  incompatible <- !is_ptype_compatible(
    fsubset(table, j = names(by_vals_with_diffs)),
    by_vals_with_diffs
  )
  if (any(incompatible)) {
    col <- names(by_vals_with_diffs)[which.max(incompatible)]
    class_table <- class(table[[col]])
    class_comparison <- class(by_vals_with_diffs[[col]])
    message <- c(
      "`by` columns in `table` must be compatible with those in `comparison`",
      "`{col}` class in `table`: {.cls {class_table}}",
      "`{col}` class in `comparison`: {.cls {class_comparison}}"
    )
    cli_abort(message, call = call)
  }
}
