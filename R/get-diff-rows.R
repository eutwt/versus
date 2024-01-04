get_diff_rows <- function(col, table_a, table_b, matches, allow_both_NA) {
  col_a <- fsubset(table_a, matches$common$a, col)[[1]]
  col_b <- fsubset(table_b, matches$common$b, col)[[1]]
  matches$common %>%
    fsubset(not_equal(col_a, col_b, allow_both_NA)) %>%
    frename(c("row_a", "row_b"))
}

not_equal <- function(col_a, col_b, allow_both_NA) {
  if (allow_both_NA && is_simple_class(col_a, col_b) && !is_empty(col_a)) {
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

is_simple_class <- function(col_a, col_b) {
  class_a <- class(col_a)
  if (!identical(class_a, class(col_b))) {
    return(FALSE)
  }
  simple_classes <- list(
    "logical",
    "character",
    "numeric",
    "Date",
    "integer",
    c("POSIXct", "POSIXt")
  )
  for (cls in simple_classes) {
    if (identical(class_a, cls)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
