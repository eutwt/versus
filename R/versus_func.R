
versus <- function(table_a, table_b, by, allow_bothNA = TRUE, coerce = TRUE) {
  check_required(by)
  by <- enquo(by)
  table_a_chr <- as.character(substitute(table_a))
  table_b_chr <- as.character(substitute(table_b))
  by_vars <- get_by_vars(by_quo = by, table_a = table_a, table_b = table_b)
  table_summ <-
    tibble(
      table = c("table_a", "table_b"),
      expr = c(table_a_chr, table_b_chr),
      ncol = c(ncol(table_a), ncol(table_b)),
      nrow = c(nrow(table_a), nrow(table_b)))
  assert_unique(table_a, by_vars)
  assert_unique(table_b, by_vars)
  table_summ
}

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

