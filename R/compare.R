#' Compare two data frames
#'
#' @param table_a A data frame
#' @param table_b A data frame
#' @param by <[`tidy-select`][versus_tidy_select]>. Selection of columns to use when matching rows between
#' \code{.data_a} and \code{.data_b}. Both data frames must be unique on \code{by}.
#' @param allow_both_NA Logical. If \code{TRUE} a missing value in both data frames is
#' considered as equal
#' @param coerce Logical. If \code{FALSE} only columns with the same class are compared.
#' @param use_duckplyr Logical. Run \code{as_duckplyr_df()} on input tables before
#' performing comparison. This is useful when the tables are large because it makes
#' the comparison faster. If TRUE, the outputs will also be duckplyr_df objects.
#'
#' @return
#' \describe{
#' \item{\code{compare()}}{A list of data frames having the following elements:
#' \describe{
#'   \item{tables}{
#'     A data frame with one row per input table showing the number of rows
#'     and columns in each.
#'   }
#'   \item{by}{
#'     A data frame with one row per \code{by} column showing the class
#'     of the column in each of the input tables.
#'  }
#'  \item{summ}{
#'    A data frame with one row per column common to \code{table_a} and
#'    \code{table_b} and columns "n_diffs" showing the number of values which
#'    are different between the two tables, "class_a"/"class_b" the class of the
#'    column in each table, and "value_diffs" a (nested) data frame showing
#'    the the values in each table which are unequal and the \code{by} columns
#'  }
#'  \item{unmatched_cols}{
#'    A data frame with one row per column which is in one input table but
#'    not the other and columns "table": which table the column appears in,
#'    "column": the name of the column, and "class": the class of the
#'    column.
#'  }
#'  \item{unmatched_rows}{
#'    A data frame which, for each row present in one input table but not
#'    the other, contains the column "table" showing which table the row appears
#'    in and the \code{by} columns for that row.
#'  }
#' }
#' }
#' }
#' @examples
#' compare(example_df_a, example_df_b, by = car)

#' @rdname compare
#' @export
compare <- function(table_a, table_b, by, allow_both_NA = TRUE, coerce = TRUE,
                    use_duckplyr = FALSE) {
  check_required(by)
  by <- enquo(by)
  table_a_chr <- as_label(enexpr(table_a))
  table_b_chr <- as_label(enexpr(table_b))
  by_vars <- get_by_vars(by_quo = by, table_a = table_a, table_b = table_b)

  table_summ <-
    tibble(
      table = c("table_a", "table_b"),
      expr = c(table_a_chr, table_b_chr),
      ncol = c(ncol(table_a), ncol(table_b)),
      nrow = c(nrow(table_a), nrow(table_b))
    )

  cols <- join_split(contents(table_a), contents(table_b), by = "column") %>%
    with(list(
      by = common %>% filter(column %in% by_vars),
      compare = common %>% filter(!column %in% by_vars),
      unmatched = unmatched
    ))

  abort_differing_class(cols, coerce)

  matches <- tryCatch(
    locate_matches(table_a, table_b, by = by_vars),
    vctrs_error_matches_relationship_one_to_one =
      abort_duplicates(by_vars)
  )

  unmatched_rows <- get_unmatched_rows(
    table_a,
    table_b,
    by = by_vars,
    matches = matches
  )
  cols$compare$value_diffs <- cols$compare$column %>%
    lapply(get_value_diffs,
      table_a = table_a,
      table_b = table_b,
      by = by_vars,
      matches = matches,
      allow_both_NA = allow_both_NA
    )

  cols$compare <- cols$compare %>%
    mutate(n_diffs = sapply(value_diffs, nrow), .after = column)

  list(
    tables = table_summ,
    by = cols$by,
    summ = cols$compare,
    unmatched_cols = cols$unmatched,
    unmatched_rows = unmatched_rows
  )
}

# Helpers ---------

sss <- function(x, i, j) {
  check <- function(i) {
    length(i) == 1 && i == 0
  }
  ss(x, i, j, check = check(i))
}

locate_matches <- function(table_a, table_b, by) {
  matches <- vec_locate_matches(
    table_a[by],
    table_b[by],
    relationship = "one-to-one",
    no_match = -1L,
    remaining = -2L
  )
  match_group <- fcase(
    matches$haystack == -1, "a",
    matches$needles == -2, "b",
    default = "common"
  )
  out <- split(matches, qF(match_group))
  if (!"a" %in% names(out)) {
    out$a <- tibble(needles = 0, haystack = 0)
  }
  if (!"b" %in% names(out)) {
    out$b <- tibble(needles = 0, haystack = 0)
  }
  if (!"common" %in% names(out)) {
    abort("nothing in common")
  }
  out
}

get_unmatched_rows <- function(table_a, table_b, by, matches) {
  unmatched <- list(
    a = sss(table_a, matches$a$needles, by),
    b = sss(table_b, matches$b$haystack, by)
  )
  as_tibble(rowbind(unmatched, idcol = "table", id.factor = FALSE))
}

get_common_rows <- function(table_a, table_b, by, matches) {
  common_cols <- setdiff(intersect(names(table_a), names(table_b)), by)

  by_a <- sss(table_a, matches$common$needles, by)
  common_a <- sss(table_a, matches$common$needles, common_cols)
  common_b <- sss(table_b, matches$common$needles, common_cols)

  add_vars(
    by_a,
    frename(common_a, \(nm) paste0(nm, "_a")),
    frename(common_b, \(nm) paste0(nm, "_b"))
  )
}

join_split <- function(table_a, table_b, by, matches) {
  matches <- locate_matches(table_a, table_b, by)
  unmatched <- get_unmatched_rows(table_a, table_b, by, matches)
  common <- get_common_rows(table_a, table_b, by, matches)
  list(common = common, unmatched = unmatched)
}

get_value_diffs <- function(col, table_a, table_b, by, matches, allow_both_NA) {
  col_a <- sss(table_a, matches$common$needles, col)[[1]]
  col_b <- sss(table_b, matches$common$haystack, col)[[1]]
  is_not_equal <- not_equal(col_a, col_b, allow_both_NA)

  if (any(is_not_equal)) {
    vals <- tibble(a = col_a[is_not_equal], b = col_b[is_not_equal]) %>%
      frename(paste0(col, c("_a", "_b")))
    by_cols <- sss(table_a, matches$common$needles[is_not_equal], by)
  } else {
    vals_a <- sss(table_a, 0, col)
    vals_b <- sss(table_b, 0, col)
    vals <- add_vars(vals_a, vals_b) %>%
      frename(paste0(col, c("_a", "_b")))
    by_cols <- sss(table_a, 0, by)
  }
  as_tibble(add_vars(vals, by_cols))
}

not_equal <- function(col_a, col_b, allow_both_NA) {
  neq <- col_a != col_b
  if (allow_both_NA) {
    out <- fcoalesce(neq, is.na(col_a) != is.na(col_b))
  } else {
    out <- fcoalesce(neq, is.na(col_a), is.na(col_b))
  }
  out
}

abort_duplicates <- function(by) {
  function(e) {
    cnd_msg <- cnd_message(e)
    table_name <- if_else(grepl("at most 1 value from `needles`", cnd_msg),
      "table_a",
      "table_b"
    )
    cols_char <- char_vec_display(glue("`{by}`"), 30)
    abort(
      glue("`{table_name}` must be unique on `by` vars ({cols_char})"),
      call = expr(compare())
    )
  }
}

abort_differing_class <- function(cols, coerce, call = caller_env()) {
  if (!coerce) {
    diff_class <- cols$compare %>%
      filter(class_a != class_b)
    if (nrow(diff_class) > 0) {
      abort(c(
        "coerce = FALSE but some columns classes do not match",
        i = char_vec_display(diff_class$column, 50)
      ), call = call)
    }
  }
}
