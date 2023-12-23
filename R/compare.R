#' Compare two data frames
#'
#' @param table_a A data frame
#' @param table_b A data frame
#' @param by <[`tidy-select`][versus_tidy_select]>. Selection of columns to use when matching rows between
#' \code{.data_a} and \code{.data_b}. Both data frames must be unique on \code{by}.
#' @param allow_both_NA Logical. If \code{TRUE} a missing value in both data frames is
#' considered as equal
#' @param coerce Logical. If \code{FALSE} and columns from the input tables have
#' differing classes, the function throws an error.
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
  table_chr <- names(enquos(table_a, table_b, .named = TRUE))
  validate_tables(table_a, table_b, coerce = coerce)

  by_vars <- get_by_vars(by_quo = by, table_a = table_a, table_b = table_b)

  table_summ <- tibble(
    table = c("table_a", "table_b"),
    expr = table_chr,
    nrow = c(nrow(table_a), nrow(table_b)),
    ncol = c(ncol(table_a), ncol(table_b))
  )

  tbl_contents <- get_contents(table_a, table_b, by_vars)

  matches <- withCallingHandlers(
    locate_matches(table_a, table_b, by = by_vars),
    vctrs_error_matches_relationship_one_to_one =
      rethrow_match_relationship(table_a, table_b, by = by_vars),
    vctrs_error_ptype2 =
      rethrow_incompatible_by_vars(table_a, table_b, by = by_vars)
  )

  unmatched_rows <- get_unmatched_rows(
    table_a,
    table_b,
    by = by_vars,
    matches = matches
  )

  tbl_contents$compare$diff_rows <- tbl_contents$compare$column %>%
    lapply(get_diff_rows,
      table_a = table_a,
      table_b = table_b,
      by = by_vars,
      matches = matches,
      allow_both_NA = allow_both_NA
    )

  tbl_contents$compare <- tbl_contents$compare %>%
    mutate(n_diffs = map_int(diff_rows, nrow), .after = column)

  out <- list(
    tables = table_summ,
    by = tbl_contents$by,
    intersection = tbl_contents$compare,
    unmatched_cols = tbl_contents$unmatched_cols,
    unmatched_rows = unmatched_rows,
    input = store_tables(table_a, table_b)
  )
  structure(out, class = "vs_compare")
}

# Methods -----------

#' @export
print.vs_compare <- function(x, ...) {
  local({ # need local() for Rmd
    class(x) <- "list"
    print(x[setdiff(names(x), "input")])
  })
  invisible(x)
}


#' @export
summary.vs_compare <- function(object, ...) {
  out_vec <- c(
    value_diffs = sum(object$intersection$n_diffs) > 0,
    unmatched_cols = nrow(object$unmatched_cols) > 0,
    unmatched_rows = nrow(object$unmatched_rows) > 0,
    class_diffs = object$input$value %>%
      lapply(fsubset, j = object$intersection$column) %>%
      lapply(lapply, class) %>%
      pmap_lgl(Negate(identical)) %>%
      any()
  )
  enframe(out_vec, name = "difference", value = "found")
}

# Helpers ---------

locate_matches <- function(table_a, table_b, by) {
  matches <- vec_locate_matches(
    fsubset(table_a, j = by),
    fsubset(table_b, j = by),
    relationship = "one-to-one",
    no_match = -1L,
    remaining = -2L
  )
  match_group <- fcase(
    matches$haystack == -1, "a",
    matches$needles == -2, "b",
    default = "common"
  )
  out <- lapply(matches, gsplit, match_group, use.g.names = TRUE)
  out$haystack$a <- NULL
  out$needles$b <- NULL
  out
}

get_unmatched_rows <- function(table_a, table_b, by, matches) {
  unmatched <- list(
    a = fsubset(table_a, matches$needles$a, by),
    b = fsubset(table_b, matches$haystack$b, by)
  )
  unmatched %>%
    bind_rows(.id = "table") %>%
    mutate(row = with(matches, c(needles$a, haystack$b)) %||% integer(0)) %>%
    as_tibble()
}

converge <- function(table_a, table_b, by, matches) {
  common_cols <- setdiff(intersect(names(table_a), names(table_b)), by)

  by_a <- fsubset(table_a, matches$needles$common, by)
  common_a <- fsubset(table_a, matches$needles$common, common_cols)
  common_b <- fsubset(table_b, matches$haystack$common, common_cols)

  add_vars(
    by_a,
    frename(common_a, \(nm) paste0(nm, "_a")),
    frename(common_b, \(nm) paste0(nm, "_b"))
  )
}

join_split <- function(table_a, table_b, by) {
  matches <- locate_matches(table_a, table_b, by)
  intersection <- converge(table_a, table_b, by, matches)
  unmatched_rows <- get_unmatched_rows(table_a, table_b, by, matches)
  list(intersection = intersection, unmatched_rows = unmatched_rows)
}

get_contents <- function(table_a, table_b, by) {
  tbl_contents <- join_split(contents(table_a), contents(table_b), by = "column")
  out <- list()

  out$by <- tbl_contents$intersection %>%
    filter(column %in% by)

  out$compare <- tbl_contents$intersection %>%
    filter(!column %in% by)

  out$unmatched_cols <- tbl_contents$unmatched_rows %>%
    select(-row)

  out
}

get_diff_rows <- function(col, table_a, table_b, by, matches, allow_both_NA) {
  col_a <- fsubset(table_a, matches$needles$common, col)[[1]]
  col_b <- fsubset(table_b, matches$haystack$common, col)[[1]]
  not_equal <- which(not_equal(col_a, col_b, allow_both_NA))
  tibble(
    row_a = matches$needles$common[not_equal],
    row_b = matches$haystack$common[not_equal]
  )
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

store_tables <- function(table_a, table_b) {
  env <- new_environment()
  env$value <- list(a = table_a, b = table_b) %>%
    map_if(\(x) inherits(x, "data.table"), compose(as_tibble, copy))
  lockEnvironment(env, bindings = TRUE)
  env
}

# Error handling -------------

rethrow_match_relationship <- function(table_a, table_b, by) {
  call <- caller_env()
  function(e) {
    tbl <- if_else(e$which == "haystack", "table_a", "table_b")
    top_msg <- "`by` variables must uniquely identify rows"

    if (tbl == "table_a") {
      tbl_row <- fsubset(table_b, e$i, by)
      row_num <- vec_locate_matches(tbl_row, fsubset(table_a, j = by))$haystack
    } else {
      tbl_row <- fsubset(table_a, e$i, by)
      row_num <- vec_locate_matches(tbl_row, fsubset(table_b, j = by))$haystack
    }
    n_rows <- length(row_num)
    info <- c(i = "`{tbl}` has {n_rows} rows with the same `by` values as row {row_num[1]}")

    cli_abort(c(top_msg, info, itemize_row(tbl_row)), call = call)
  }
}

validate_tables <- function(table_a, table_b, coerce, call = caller_env()) {
  assert_data_frame(table_a, call = call)
  assert_data_frame(table_b, call = call)
  assert_unique_names(table_a, call = call)
  assert_unique_names(table_b, call = call)
  if (!coerce) {
    assert_same_class(table_a, table_b, call = call)
  }
}

assert_unique_names <- function(table, call = caller_env()) {
  arg_name <- deparse(substitute(table))
  withCallingHandlers(
    vec_as_names(names(table), repair = "check_unique"),
    error = function(e) {
      msg <- strsplit(cnd_message(e), "\n")[[1]]
      cli_abort(c("Problem with `{arg_name}`: {msg[1]}", msg[-1]), call = call)
    }
  )
}

assert_data_frame <- function(table, call = caller_env()) {
  arg_name <- deparse(substitute(table))
  if (is.data.frame(table)) {
    return(invisible())
  }
  message <- c(
    "`{arg_name}` must be a data frame",
    i = "class({arg_name}): {.cls {class(table)}}"
  )
  cli_abort(message, call = call)
}

assert_same_class <- function(table_a, table_b, call = caller_env()) {
  common_cols <- intersect(names(table_a), names(table_b))
  for (col in common_cols) {
    a <- table_a[[col]][0]
    b <- table_b[[col]][0]
    if (identical(a, b)) {
      next
    }
    message <- c(
      "`coerce = FALSE` but some column classes do not match",
      i = "table_a: {col} {.cls {class(a)}}",
      i = "table_b: {col} {.cls {class(b)}}"
    )
    cli_abort(message, call = call)
  }
}

rethrow_incompatible_by_vars <- function(table_a, table_b, by) {
  call <- caller_env()
  function(e) {
    compatible <- is_ptype_compatible(
      fsubset(table_a, j = by),
      fsubset(table_b, j = by)
    )
    bad_column <- by[which.max(!compatible)]

    class_a <- class(table_a[[bad_column]])
    class_b <- class(table_b[[bad_column]])
    message <- c(
      "`by` columns must be compatible",
      "`table_a${bad_column}` {.cls {class_a}}",
      "`table_b${bad_column}` {.cls {class_b}}"
    )
    cli_abort(message, call = call)
  }
}
