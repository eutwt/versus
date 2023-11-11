get_by_vars <- function(by_quo, table_a, table_b, call = caller_env()) {
  cols_a <- try_select(
    eval_select(by_quo, table_a, allow_rename = FALSE, allow_empty = FALSE),
    "table_a", call
  )
  cols_b <- try_select(
    eval_select(by_quo, table_b, allow_rename = FALSE, allow_empty = FALSE),
    "table_b", call
  )
  if (!identical(names(cols_a), names(cols_b))) {
    msg <- "Column names of `by` variables must be the same in both data frames"
    a_names <- char_vec_display(names(cols_a), 50)
    b_names <- char_vec_display(names(cols_b), 50)
    msg <- c(
      msg,
      i = glue("table_a names: {a_names}"),
      i = glue("table_b names: {b_names}")
    )
    abort(msg, call = call)
  }
  names(cols_a)
}

try_select <- function(eval_select_call, arg_name, call) {
  cnd <- catch_cnd(eval_select_call)
  if (!is_null(cnd)) {
    abort_on_by_select_error(cnd, arg_name, call)
  }

  eval_select_call
}

abort_on_by_select_error <- function(cnd, arg_name, call) {
  cnd_msg <- cnd_message(cnd)
  if (inherits(cnd, "vctrs_error_subscript_oob")) {
    cnd_msg <- c(glue("Issue with `{arg_name}`"), cnd_msg)
  } else if (inherits(cnd, "vctrs_error_subscript_type") && grepl("join_by", cnd_msg)) {
    cnd_msg <- c("`join_by()` is not supported",
      i = "provide `by` columns with tidy-select, as in `dplyr::across()`"
    )
  } else if (grepl("Must select at least one item", cnd_msg)) {
    cnd_msg <- c("Must select at least one column with `by`",
      i = glue("No matching columns found in `{arg_name}`")
    )
  }
  abort(cnd_msg, call = call)
}
