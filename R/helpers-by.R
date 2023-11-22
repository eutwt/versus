get_by_vars <- function(by_quo, table_a, table_b, call = caller_env()) {
  cols_a <- try_fetch(
    eval_select(by_quo, table_a, allow_rename = FALSE, allow_empty = FALSE),
    error = rethrow_by_select_error("table_a", call)
  )
  cols_b <- try_fetch(
    eval_select(by_quo, table_b, allow_rename = FALSE, allow_empty = FALSE),
    error = rethrow_by_select_error("table_b", call)
  )
  if (!identical(names(cols_a), names(cols_b))) {
    msg <- "Column names of `by` variables must be the same in both data frames"
    a_names <- dottize(names(cols_a), 50)
    b_names <- dottize(names(cols_b), 50)
    msg <- c(
      msg,
      i = "table_a names: {a_names}",
      i = "table_b names: {b_names}"
    )
    cli_abort(msg, call = call)
  }
  names(cols_a)
}

rethrow_by_select_error <- function(arg_name, call) {
  function(cnd) {
    cnd_msg <- cnd_message(cnd)
    if (inherits(cnd, "vctrs_error_subscript_oob")) {
      cnd_msg <- c(glue("Problem with `{arg_name}`:"), cnd_msg)
    } else if (inherits(cnd, "vctrs_error_subscript_type") && grepl("join_by", cnd_msg)) {
      cnd_msg <- c(
        "`join_by()` is not supported",
        i = "provide `by` columns with tidy-select, as in `dplyr::across()`"
      )
    } else if (grepl("Must select at least one item", cnd_msg)) {
      cnd_msg <- c(
        "Must select at least one column with `by`",
        i = glue("No matching columns found in `{arg_name}`")
      )
    } else {
      cnd_msg <- c("Problem with `by`:", cnd_msg)
    }
    abort(cnd_msg, call = call)
  }
}
