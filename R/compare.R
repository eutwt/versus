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
  table_a_chr <- as_label(enquo(table_a))
  table_b_chr <- as_label(enquo(table_b))

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

  tbl_contents <- get_contents(table_a, table_b, by_vars)

  abort_differing_class(tbl_contents, coerce)

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
  tbl_contents$compare$value_diffs <- tbl_contents$compare$column %>%
    lapply(get_value_diffs,
      table_a = table_a,
      table_b = table_b,
      by = by_vars,
      matches = matches,
      allow_both_NA = allow_both_NA
    )

  tbl_contents$compare <- tbl_contents$compare %>%
    mutate(n_diffs = map_int(value_diffs, nrow), .after = column)

  out <- list(
    tables = table_summ,
    by = tbl_contents$by,
    intersection = tbl_contents$compare,
    unmatched_cols = tbl_contents$unmatched_cols,
    unmatched_rows = unmatched_rows
  )
  attr(out, "template") <- tbl_contents$template
  structure(out, class = "vs_compare")
}

# Methods -----------

#' @export
print.vs_compare <- function(x, ...) {
  attr(x, "template") <- NULL
  class(x) <- "list"
  print(x)
}

#' @export
summary.vs_compare <- function(object, ...) {
  out_vec <- c(
    value_diffs = sum(object$intersection$n_diffs) > 0,
    unmatched_cols = nrow(object$unmatched_cols) > 0,
    unmatched_rows = nrow(object$unmatched_rows) > 0,
    class_diffs =
      !all(with(attr(object, "classes"), map2_lgl(a, b, identical)))
  )
  out <- tibble(
    difference = names(out_vec),
    found = unname(out_vec)
  )
  out
}

# Helpers ---------

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

converge <- function(table_a, table_b, by, matches) {
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
  intersection <- converge(table_a, table_b, by, matches)
  unmatched_rows <- get_unmatched_rows(table_a, table_b, by, matches)
  list(intersection = intersection, unmatched_rows = unmatched_rows)
}

get_contents <- function(table_a, table_b, by) {
  tbl_contents <- join_split(contents(table_a), contents(table_b), by = "column")

  by <- tbl_contents$intersection %>%
    select(-starts_with("template")) %>%
    filter(column %in% by)

  compare <- tbl_contents$intersection %>%
    select(-starts_with("template")) %>%
    filter(!column %in% by)

  template <- tbl_contents$intersection %>%
    select(column, starts_with("template")) %>%
    filter(!column %in% by) %>%
    rename_with(\(x) sub("template_", "", x))

  unmatched_cols <- tbl_contents$unmatched_rows

  list(by = by, compare = compare, template = template, unmatched_cols = unmatched_cols)
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

# Error handling -------------

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

abort_differing_class <- function(contents, coerce, call = caller_env()) {
  if (coerce) {
    return(invisible())
  }

  same_class <- map2_lgl(contents$template$a, contents$template$b, identical)
  if (all(same_class)) {
    return(invisible())
  }

  columns <- contents$compare$column[!same_class]
  message <- c(
    "coerce = FALSE but some column classes do not match",
    i = char_vec_display(columns, 50)
  )
  abort(message, call = call)
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
