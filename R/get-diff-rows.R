get_diff_rows <- function(col, table_a, table_b, matches, allow_both_NA) {
  col_a <- fsubset(table_a, matches$common$a, col)[[1]]
  col_b <- fsubset(table_b, matches$common$b, col)[[1]]
  matches$common %>%
    fsubset(not_equal(col_a, col_b, allow_both_NA)) %>%
    frename(c("row_a", "row_b"))
}

not_equal <- function(col_a, col_b, allow_both_NA) {
  classes <- unique(c(class(col_a), class(col_b)))
  is_simple_class <- 
    identical(classes, "character") ||
    identical(classes, "numeric") ||
    identical(classes, "Date") ||
    identical(classes, "integer") ||
    identical(classes, c("POSIXct", "POSIXt"))
  if (is_simple_class && !is_empty(col_a)) {
    return(col_a %!=% col_b)
  }
  is_not_equal(col_a, col_b, allow_both_NA)
}

is_not_equal <- function(col_a, col_b, allow_both_NA) {
  neq <- col_a != col_b
  if (allow_both_NA) {
    out <- fcoalesce(neq, is.na(col_a) != is.na(col_b))
  } else {
    out <- fcoalesce(neq, is.na(col_a) | is.na(col_b))
  }
  out
}


