get_diff_rows <- function(col, table_a, table_b, matches, allow_both_NA) {
  if (can_use_cpp(col, table_a, table_b, matches, allow_both_NA)) {
    cpp_get_diff_rows(
      table_a[[col]],
      table_b[[col]],
      matches$needles$common,
      matches$haystack$common
    )
  } else {
    r_get_diff_rows(col, table_a, table_b, matches, allow_both_NA)
  }
}

can_use_cpp <- function(col, table_a, table_b, matches, allow_both_NA) {
  if (is_null(matches$needles$common) || !allow_both_NA) {
    return(FALSE)
  }
  cpp_classes <- c("integer", "numeric")
  col_class <- unique(c(class(table_a[[col]]), class(table_b[[col]])))
  length(col_class) == 1 && col_class %in% cpp_classes
}

cpp_get_diff_rows <- function(vec_a, vec_b, idx_a, idx_b) {
  UseMethod("cpp_get_diff_rows")
}

cpp_get_diff_rows.numeric <- function(vec_a, vec_b, idx_a, idx_b) {
  get_diff_rows_dbl(vec_a, vec_b, idx_a, idx_b)
}

cpp_get_diff_rows.integer <- function(vec_a, vec_b, idx_a, idx_b) {
  get_diff_rows_int(vec_a, vec_b, idx_a, idx_b)
}

r_get_diff_rows <- function(col, table_a, table_b, matches, allow_both_NA) {
  col_a <- fsubset(table_a, matches$needles$common, col)[[1]]
  col_b <- fsubset(table_b, matches$haystack$common, col)[[1]]
  not_equal <- which(not_equal(col_a, col_b, allow_both_NA))
  tibble(
    row_a = matches$needles$common[not_equal] %||% integer(0),
    row_b = matches$haystack$common[not_equal] %||% integer(0)
  )
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
