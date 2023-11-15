#' Show rows which have differing values in a comparison
#'
#' @param table A data frame. One of \code{table_a} or \code{table_b} used to
#' create \code{comparison}
#' @param table_a The data frame\code{table_a} used to create \code{comparison}
#' @param table_b The data frame\code{table_b} used to create \code{comparison}
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

#' @rdname slice_diffs
#' @export
slice_diffs_both <- function(table_a, table_b, comparison, column) {
  output_cols <- c(comparison$by$column, comparison$summ$column)
  slice_diffs_for_interleave <- function(df, name) {
    df %>%
      select(all_of(output_cols)) %>%
      slice_diffs(comparison, {{ column }}) %>%
      arrange(comparison$by$column) %>%
      mutate(table = name, .before = 1)
  }
  diffs_a <- slice_diffs_for_interleave(table_a, "a")
  diffs_b <- slice_diffs_for_interleave(table_b, "b")

  # if the column-types are incompatible, convert them to character first
  is_incompatible <- map2_lgl(diffs_a, diffs_b, \(col_a, col_b) {
    cnd <- catch_cnd(vec_ptype_common(col_a, col_b))
    inherits(cnd, "vctrs_error_ptype2")
  })
  if (any(is_incompatible)) {
    incompatible_cols <- names(is_incompatible)[is_incompatible]
    cols_char <- char_vec_display(incompatible_cols, 30)
    inform(c(i = glue("Columns converted to character: {cols_char}")))

    diffs_a <- diffs_a %>%
      mutate(across(all_of(incompatible_cols), as.character))
    diffs_b <- diffs_b %>%
      mutate(across(all_of(incompatible_cols), as.character))
  }

  vec_interleave(diffs_a, diffs_b)
}
