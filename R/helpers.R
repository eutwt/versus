
shorten <- function(x, max_char = 10) {
  if_else(
    nchar(x) > max_char,
    paste0(substr(x, 1, max_char - 3), '...'),
    x)
}

char_vec_display <- function(vec, max_char = 10) {
  shorten(glue_collapse(vec, ', '), max_char)
}

assert_unique <- function(table, cols, call = caller_env()) {
  any_dupe <- vec_duplicate_any(select(table, all_of(cols)))
  if (!any_dupe) {
    return(TRUE)
  }
  table_name <- substitute(table)
  cols_char <- char_vec_display(glue("`{cols}`"), 30)
  abort(glue("`{table_name}` must be unique on `by` vars ({cols_char})"),
        call = call)
}

contents <- function(table) {
  table %>%
    lapply(\(x) glue_collapse(class(x), ', ')) %>%
    bind_cols %>%
    unlist %>%
    tibble(column = names(.), class = .)
}
