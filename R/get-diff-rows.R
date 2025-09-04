get_diff_rows <- function(
  col,
  table_a,
  table_b,
  matches,
  allow_both_NA = TRUE,
  ...,
  .f = NULL
) {
  col_a <- fsubset(table_a, matches$common$a, col)[[1]]
  col_b <- fsubset(table_b, matches$common$b, col)[[1]]
  if (is.null(.f)) {
    diff_rows <- not_equal(col_a, col_b, allow_both_NA)
  } else {
    diff_rows <- .f(col_a, col_b, ...)
  }
  matches$common %>%
    fsubset(diff_rows) %>%
    frename(c("row_a", "row_b"))
}

#' Test two vectors for not equal values
#' @param col_a,col_b Two equal length vectors, typically column values.
#' @param allow_both_NA If `TRUE` (default), an `NA` value in `col_a` and `col_b` is treated as equal.
#' @keywords internal
#' @export
not_equal <- function(col_a, col_b, allow_both_NA = TRUE) {
  if (allow_both_NA && is_simple_class(col_a, col_b) && !is_empty(col_a)) {
    return(col_a %!=% col_b)
  }
  seq_along(col_a)[is_not_equal(col_a, col_b, allow_both_NA)]
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
