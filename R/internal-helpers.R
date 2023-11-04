
shorten <- function(x, max_char = 10) {
  if_else(
    nchar(x) > max_char,
    paste0(substr(x, 1, max_char - 3), '...'),
    x)
}

assert_unique <- function(table, cols, call = caller_env()) {
  any_dupe <- vec_duplicate_any(select(table, all_of(cols)))
  if (!any_dupe) {
    return(0)
  }
  table_name <- substitute(table)
  cols_char <- shorten(glue_collapse(glue("`{cols}`"), ', '), 30)
  abort(glue("`{table_name}` must be unique on `by` vars ({cols_char})"),
        call = call)
}
