#' Compare two data frames
#'
#' @param table_a A data frame
#' @param table_b A data frame
#' @param by <[`tidy-select`][versus_tidy_select]>. Selection of columns to use when matching rows between
#' \code{.data_a} and \code{.data_b}. Both data frames must be unique on \code{by}.
#' @param allow_both_NA Logical. If \code{TRUE} a missing value in both data frames is
#' considered as equal
#' @param coerce Logical. If \code{FALSE} only columns with the same class are compared.
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
#'  \item{intersection}{
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
compare <- function(table_a, table_b, by, allow_both_NA = TRUE, coerce = TRUE) {
  check_required(by)
  by <- enquo(by)
  table_a_chr <- as_label(enexpr(table_a))
  table_b_chr <- as_label(enexpr(table_b))

  ensure_data_frame(table_a)
  ensure_data_frame(table_b)
  ensure_well_named(table_a, table_b)

  by_vars <- get_by_vars(by_quo = by, table_a = table_a, table_b = table_b)

  table_summ <- tibble(
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

  matches <- try_fetch(
    locate_matches(table_a, table_b, by = by_vars),
    vctrs_error_matches_relationship_one_to_one =
      abort_duplicates(table_a, table_b, by = by_vars)
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
    mutate(n_diffs = map_int(value_diffs, nrow), .after = column)

  list(
    tables = table_summ,
    by = cols$by,
    intersection = cols$compare,
    unmatched_cols = cols$unmatched,
    unmatched_rows = unmatched_rows
  )
}

# Helpers ---------

fsubset <- function(x, i, j) {
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
    a = fsubset(table_a, matches$a$needles, by),
    b = fsubset(table_b, matches$b$haystack, by)
  )
  as_tibble(rowbind(unmatched, idcol = "table", id.factor = FALSE))
}

get_common_rows <- function(table_a, table_b, by, matches) {
  common_cols <- setdiff(intersect(names(table_a), names(table_b)), by)

  by_a <- fsubset(table_a, matches$common$needles, by)
  common_a <- fsubset(table_a, matches$common$needles, common_cols)
  common_b <- fsubset(table_b, matches$common$haystack, common_cols)

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
  col_a <- fsubset(table_a, matches$common$needles, col)[[1]]
  col_b <- fsubset(table_b, matches$common$haystack, col)[[1]]
  is_not_equal <- not_equal(col_a, col_b, allow_both_NA)

  if (any(is_not_equal)) {
    vals <- tibble(a = col_a[is_not_equal], b = col_b[is_not_equal]) %>%
      frename(paste0(col, c("_a", "_b")))
    by_cols <- fsubset(table_a, matches$common$needles[is_not_equal], by)
  } else {
    vals_a <- fsubset(table_a, 0, col)
    vals_b <- fsubset(table_b, 0, col)
    vals <- add_vars(vals_a, vals_b) %>%
      frename(paste0(col, c("_a", "_b")))
    by_cols <- fsubset(table_a, 0, by)
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

abort_duplicates <- function(table_a, table_b, by) {
  call <- caller_env()
  function(e) {
    tbl <- if_else(e$which == "haystack", "table_a", "table_b")

    cols_char <- char_vec_display(glue("`{by}`"), 20)
    top_msg <- glue("`{tbl}` must be unique on `by` vars ({cols_char})")

    if (tbl == "table_a") {
      tbl_row <- fsubset(table_b, e$i, by)
    } else {
      tbl_row <- fsubset(table_a, e$i, by)
    }
    tbl_char <- capture.output(as_tibble(tbl_row))[-1]
    info <- c(i = glue("The row shown below is duplicated."), tbl_char)

    abort(message = top_msg, body = info, call = call)
  }
}

abort_differing_class <- function(cols, coerce, call = caller_env()) {
  if (coerce) {
    return(invisible())
  }
  diff_class <- cols$compare %>%
    filter(class_a != class_b)
  if (nrow(diff_class) > 0) {
    message <- c(
      "coerce = FALSE but some column classes do not match",
      i = char_vec_display(diff_class$column, 50)
    )
    abort(message, call = call)
  }
}

ensure_well_named <- function(table_a, table_b, call = caller_env()) {
  try_fetch(
    vec_as_names(names(table_a), repair = "check_unique"),
    error = rethrow_with_arg_name("table_a", call)
  )
  try_fetch(
    vec_as_names(names(table_b), repair = "check_unique"),
    error = rethrow_with_arg_name("table_b", call)
  )
}

ensure_data_frame <- function(x, call = caller_env()) {
  arg_name <- deparse(substitute(x))
  if (is.data.frame(x)) {
    return(TRUE)
  }
  class_display <- char_vec_display(glue('"{class(x)}"'), 40)
  message <- c(
    glue("`{arg_name}` must be a data frame"),
    i = glue("class({arg_name}): {class_display}")
  )
  abort(message, call = call)
}

rethrow_with_arg_name <- function(arg_name, call) {
  function(cnd) {
    cnd_msg <- cnd_message(cnd)
    message <- c(glue("Issue with `{arg_name}`"), cnd_msg)
    abort(message, call = call)
  }
}
