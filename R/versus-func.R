
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

