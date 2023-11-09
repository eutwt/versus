
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

simulate_df <- function(col_names, classes) {
  empty_lists <- replicate(length(col_names), list())
  fake_df <- Map(`class<-`, empty_lists, classes) %>%
    setNames(col_names) %>%
    as.data.frame
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
  "versus_in_b"
))
