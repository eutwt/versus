get_diff_rows <- function(col, table_a, table_b, matches, allow_both_NA) {
  if (can_use_cpp(col, table_a, table_b, matches, allow_both_NA)) {
    cpp_get_diff_rows(
      table_a[[col]],
      table_b[[col]],
      matches$common$a,
      matches$common$b
    )
  } else {
    r_get_diff_rows(col, table_a, table_b, matches, allow_both_NA)
  }
}

can_use_cpp <- function(col, table_a, table_b, matches, allow_both_NA) {
  if (!allow_both_NA) {
    return(FALSE)
  }
  class_a <- class(table_a[[col]])
  class_b <- class(table_b[[col]])
  if (!identical(class_a, class_b)) {
    return(FALSE)
  }
  for (cpp_class in c("integer", "numeric", "Date")) {
    if (identical(class_a, cpp_class)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

cpp_get_diff_rows <- function(vec_a, vec_b, idx_a, idx_b) {
  class_a <- class(vec_a)
  if (identical(class_a, "numeric") || identical(class_a, "Date")) {
    return(get_diff_rows_dbl(vec_a, vec_b, idx_a, idx_b))
  }
  if (identical(class_a, "integer")) {
    return(get_diff_rows_int(vec_a, vec_b, idx_a, idx_b))
  }
}

r_get_diff_rows <- function(col, table_a, table_b, matches, allow_both_NA) {
  col_a <- fsubset(table_a, matches$common$a, col)[[1]]
  col_b <- fsubset(table_b, matches$common$b, col)[[1]]
  matches$common %>%
    fsubset(not_equal(col_a, col_b, allow_both_NA)) %>%
    rename_with(\(x) paste0("row_", x))
}

not_equal <- function(col_a, col_b, allow_both_NA) {
  neq <- col_a != col_b
  if (allow_both_NA) {
    out <- fcoalesce(neq, is.na(col_a) != is.na(col_b))
  } else {
    out <- fcoalesce(neq, is.na(col_a) | is.na(col_b))
  }
  out
}
