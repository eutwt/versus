
versus <- function(table_a, table_b, by, allow_bothNA = TRUE, coerce = TRUE) {
  check_required(by)
  by <- enquo(by)
  table_a_chr <- as.character(substitute(table_a))
  table_b_chr <- as.character(substitute(table_b))

  by_vars <- get_by_vars(by_quo = by, table_a = table_a, table_b = table_b)
  assert_unique(table_a, by_vars)
  assert_unique(table_b, by_vars)

  table_summ <-
    tibble(
      table = c("table_a", "table_b"),
      expr = c(table_a_chr, table_b_chr),
      ncol = c(ncol(table_a), ncol(table_b)),
      nrow = c(nrow(table_a), nrow(table_b)))

  cols <- join_split(contents(table_a), contents(table_b), by = 'column') %>%
    with(list(by = common %>% filter(column %in% by_vars),
              compare = common %>% filter(!column %in% by_vars),
              unmatched = unmatched))

  if (!coerce) {
    diff_class <- cols$compare %>%
      filter(class_a != class_b)
    if (nrow(diff_class) > 0) {
      msg <- c(x = "coerce = FALSE but some columns classes do not match",
               i = char_vec_display(diff_class$column, 50))
      abort(msg)
    }
  }

  data <- join_split(table_a, table_b, by = by_vars)

  if (!nrow(data$common)) {
    abort("No rows found in common. Check data and `by` argument.")
  }

  cols$compare <- cols$compare %>%
    mutate(value_diffs = column %>%
             lapply(col_value_diffs, data = data$common, by = by_vars)) %>%
    mutate(n_diffs = sapply(value_diffs, nrow), .after = column)

  list(
    tables = table_summ,
    by = cols$by,
    summ = cols$compare,
    unmatched_cols = cols$unmatched,
    unmatched_rows = data$unmatched)
}

value_diffs <- function(comparison, column) {
  column <- as.character(substitute(column))
  compared_cols <- comparison$summ$column
  if (!column %in% compared_cols) {
    cols_adist_ordered <- c(adist(column, compared_cols)) %>%
      setNames(compared_cols) %>%
      sort %>%
      names
    col_list_str <- char_vec_display(cols_adist_ordered, 30)
    msg <- c(
      x = glue("Column `{column}` is not part of the supplied comparison"),
      i = paste0("comparison includes: ", col_list_str))
    abort(msg)
  }
  comparison$summ %>%
    filter(column == .env$column) %>%
    pull(value_diffs) %>%
    `[[`(1)
}

all_value_diffs <- function(comparison) {
  conform <- function(value_diffs, col_name) {
    names(value_diffs)[seq(2)] <- paste0('val_', c('a', 'b'))
    value_diffs %>%
      mutate(across(seq(2), as.character)) %>%
      mutate(column = col_name, .before = 1)
  }
  Map(conform, comparison$summ$value_diffs, comparison$summ$column) %>%
    bind_rows
}

# Helpers ---------

join_split <- function(table_a, table_b, by) {
  # full_join output split into common and unmatched
  data <- full_join(
    table_a %>% mutate(versus_in_a = TRUE),
    table_b %>% mutate(versus_in_b = TRUE),
    by = by, suffix = c('_a', '_b')) %>%
    mutate(across(starts_with('versus_in'), \(x) coalesce(x, FALSE)),
           common = versus_in_a & versus_in_b)
  common <- data %>%
    filter(common) %>%
    select(-starts_with('versus_in'), -common)
  unmatched <- data %>%
    filter(!common) %>%
    mutate(table = if_else(versus_in_a, 'a', 'b'), .before = 1) %>%
    select(table, all_of(by))
  list(common = common, unmatched = unmatched)
}

col_value_diffs <- function(data, col, by) {
  col_a <- sym(paste0(col, '_a'))
  col_b <- sym(paste0(col, '_b'))
  data %>%
    filter(!!col_a != !!col_b) %>%
    select(!!col_a, !!col_b, all_of(by))
}


