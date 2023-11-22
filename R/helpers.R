fsubset <- function(x, i, j) {
  check <- function(i) {
    length(i) == 1 && i == 0
  }
  ss(x, i, j, check = check(i))
}

get_cols_from_comparison <- function(comparison, column) {
  # simulate a data frame with the same classes as table_a to eval_select from
  template_a <- with(attr(comparison, "template"), setNames(a, column))
  names(eval_select(column, template_a))
}

shorten <- function(x, max_char = 10) {
  x <- as.character(x)
  if_else(
    nchar(x) > max_char,
    paste0(substr(x, 1, max_char - 3), "..."),
    x
  )
}

dottize <- function(vec, max_size = 20) {
  if (is_empty(vec)) {
    return(character(0))
  }
  if (nchar(vec[1]) > max_size) {
    return(shorten(vec[1], max_size))
  }
  shorten_vec <- function(vec, max_size) {
    # get printed size when printed with ", " between each element and ...
    print_size <- cumsum(nchar(vec)) + 2 * (seq_along(vec) - 1) + 4
    for (i in seq2(2, length(vec))) {
      if (print_size[i] > max_size) {
        return(c(head(vec, i - 1), "..."))
      }
    }
    return(vec)
  }

  vec %>%
    as.character() %>%
    shorten_vec(max_size) %>%
    glue_collapse(", ")
}

abort_glimpse <- function(df, max_lines = 3, width = 50) {
  # print the first line of a df in ~glimpse() form, boxed by [max_lines x width]
  tdf <- tibble(
    var = names(df),
    val = shorten(map_chr(fsubset(df, 1), format_glimpse), width)
  )
  if (nrow(tdf) < max_lines) {
    out <- with(tdf, glue("$ {var}: {val}"))
    return(out)
  }
  first <- head(tdf, max_lines) %>%
    with(glue("$ {var}: {val}"))
  n_more <- nrow(tdf) - max_lines
  more <- paste0(
    glue("{symbol$info} {n_more} more: "),
    dottize(tail(tdf$var, n_more), width)
  )
  c(first, more)
}
contents <- function(table) {
  tibble(
    column = names(table),
    class = map_chr(table, \(x) paste(class(x), collapse = ", ")),
    template = as.list(vec_init(table))
  )
}

stack_value_diffs <- function(comparison, column, pre_stack_fun) {
  column_char <- get_cols_from_comparison(comparison, column)
  has_value_diffs <- comparison$intersection$n_diffs > 0
  to_stack <- has_value_diffs & comparison$intersection$column %in% column_char

  Map(
    pre_stack_fun,
    comparison$intersection$value_diffs[to_stack],
    comparison$intersection$column[to_stack]
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
    cyl = replace(cyl, 3:4, NA),
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
