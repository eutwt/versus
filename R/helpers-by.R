
get_by_vars <- function(by_quo, table_a, table_b, call = caller_env()) {
  cols_a <- try_select(eval_select(by_quo, table_a), 'table_a', call)
  cols_b <- try_select(eval_select(by_quo, table_b), 'table_b', call)
  if (!identical(names(cols_a), names(cols_b))) {
    msg <- "Column names of `by` variables must be the same in both data frames"
    a_names <- shorten(glue_collapse(names(cols_a), ', '), 50)
    b_names <- shorten(glue_collapse(names(cols_b), ', '), 50)
    msg <- c(x = msg,
             i = glue("table_a names: {a_names}"),
             i = glue("table_b names: {b_names}"))
    abort(msg, call = call)
  }
  names(cols_a)
}

try_select <- function(eval_select_call, arg_name, call) {
  cnd <- catch_cnd(eval_select_call)
  if (!is_null(cnd)) {
    cnd_msg <- cnd_message(cnd)
    new_cnd_msg <- glue("Issue with `{arg_name}`\n{cnd_msg}")
    abort(new_cnd_msg, call = call)
  }
  cols <- eval_select_call
  if (is_empty(cols)) {
    msg <- c(
      x = glue("Issue with `{arg_name}`"),
      i = "No columns matching supplied `by`")
    abort(msg, call = call)
  }
  cols
}
