get_cols_from_comparison <- function(comparison, column) {
  # simulate a data frame with the same classes as table_a to eval_select from
  fake_table_a <- comparison$summ$class_a %>%
    strsplit(", ") %>%
    lapply(\(x) `class<-`(list(), x)) %>%
    setNames(comparison$summ$column)
  names(eval_select(column, fake_table_a))
}

shorten <- function(x, max_char = 10) {
  if_else(
    nchar(x) > max_char,
    paste0(substr(x, 1, max_char - 3), "..."),
    x
  )
}

char_vec_display <- function(vec, max_char = 10) {
  shorten(glue_collapse(vec, ", "), max_char)
}

assert_unique <- function(table, cols, call = caller_env()) {
  any_dupe <- vec_duplicate_any(select(table, all_of(cols)))
  if (!any_dupe) {
    return(TRUE)
  }
  table_name <- substitute(table)
  cols_char <- char_vec_display(glue("`{cols}`"), 30)
  abort(glue("`{table_name}` must be unique on `by` vars ({cols_char})"),
    call = call
  )
}

contents <- function(table) {
  table %>%
    lapply(\(x) glue_collapse(class(x), ", ")) %>%
    bind_cols() %>%
    unlist() %>%
    tibble(column = names(.), class = .)
}

stack_value_diffs <- function(comparison, column, pre_stack_fun) {
  column_char <- get_cols_from_comparison(comparison, column)
  has_value_diffs <- comparison$summ$n_diffs > 0
  to_stack <- has_value_diffs & comparison$summ$column %in% column_char

  Map(
    pre_stack_fun,
    comparison$summ$value_diffs[to_stack],
    comparison$summ$column[to_stack]
  ) %>%
    bind_rows()
}

test_df_a <- mtcars %>%
  rownames_to_column("car") %>%
  mutate(
    disp = replace(disp, 3:4, disp[3:4] + 1),
    cyl = replace(cyl, 3, NA),
    extracol_a = 1
  ) %>%
  head(10) %>%
  bind_rows(., head(., 1) %>% mutate(car = "extra_a"))

test_df_b <- mtcars %>%
  rownames_to_column("car") %>%
  mutate(
    mpg = replace(mpg, 7:8, mpg[7:8] + 2),
    cyl = replace(cyl, 3, NA),
    wt = as.character(wt)
  ) %>%
  filter(row_number() %in% seq(2, 12)) %>%
  bind_rows(., head(., 1) %>% mutate(car = "extra_b"))

utils::globalVariables(c(
  ".",
  "class_a",
  "class_b",
  "column",
  "common",
  "unmatched",
  "versus_in_a",
  "versus_in_b",
  "val_a",
  "val_b"
))
