
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

# Helpers ---------

merge_split <- function(table_a, table_b, by) {
  table_a$versus_in_a <- TRUE
  table_b$versus_in_b <- TRUE
  data <- full_join(table_a, table_b, by = by) %>%
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
