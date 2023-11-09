#' Compare two data frames
#'
#' @param table_a A data frame
#' @param table_b A data frame
#' @param by tidy-select. Selection of columns to use when matching rows between
#' \code{.data_a} and \code{.data_b}. Both data frames must be unique on \code{by}.
#' @param allow_both_NA Logical. If \code{TRUE} a missing value in both data frames is
#' considered as equal
#' @param comparison The output of a \code{versus::compare()} call
#' @param coerce Logical. If \code{FALSE} only columns with the same class are compared.
#' @param column tidy-select. A single column
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
#'    the the values in each table which are unequal, and the \code{by} columns
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
#' \item{\code{value_diffs()}}{A data frame with one row for each element
#' of \code{col} found to be unequal between the input tables (
#' \code{table_a} and \code{table_b} from the original \code{compare()} call)
#' The output table has columns "val_a"/"val_b": the value of \code{col} in the
#' input tables, and the \code{by} columns for the identified rows in the
#' input tables.}
#'
#' \item{\code{all_value_diffs()}}{A data frame of the \code{value_diffs()}
#' output for all columns having at least one value difference, combined row-wise
#' into a single table. To facilitate this combination into a single table, the
#' "val_a" and "val_b" columns are coerced to character.}
#' }

#' @rdname compare
#' @export
compare <- function(table_a, table_b, by, allow_both_NA = TRUE, coerce = TRUE,
                    use_duckplyr = FALSE) {
  check_required(by)
  by <- enquo(by)
  table_a_chr <- as.character(substitute(table_a))
  table_b_chr <- as.character(substitute(table_b))
  if (use_duckplyr) {
    if (!requireNamespace("duckplyr", quietly = TRUE)) {
      abort("Please install duckplyr to use this feature")
    }
    rownames(table_a) <- NULL
    rownames(table_b) <- NULL
    table_a <- duckplyr::as_duckplyr_df(table_a)
    table_b <- duckplyr::as_duckplyr_df(table_b)
  }

  by_vars <- get_by_vars(by_quo = by, table_a = table_a, table_b = table_b)
  assert_unique(table_a, by_vars)
  assert_unique(table_b, by_vars)

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

  if (!coerce) {
    diff_class <- cols$compare %>%
      filter(class_a != class_b)
    if (nrow(diff_class) > 0) {
      msg <- c(
        "coerce = FALSE but some columns classes do not match",
        i = char_vec_display(diff_class$column, 50)
      )
      abort(msg)
    }
  }

  data <- join_split(table_a, table_b, by = by_vars)

  if (!nrow(data$common)) {
    abort("No rows found in common. Check data and `by` argument.")
  }

  cols$compare$value_diffs <- cols$compare$column %>%
    lapply(col_value_diffs,
      data = data$common,
      by = by_vars,
      allow_both_NA = allow_both_NA
    )

  cols$compare <- cols$compare %>%
    mutate(n_diffs = sapply(value_diffs, nrow), .after = column)

  list(
    tables = table_summ,
    by = cols$by,
    summ = cols$compare,
    unmatched_cols = cols$unmatched,
    unmatched_rows = data$unmatched
  )
}

#' @rdname compare
#' @export
value_diffs <- function(comparison, column) {
  column <- enquo(column)
  column_char <- get_cols_from_comparison(comparison, column)
  if (length(column_char) != 1) {
    cols_selected <- char_vec_display(column_char, 30)
    abort(c("Must select only one column.",
            i = glue("Columns selected: {cols_selected}"),
            i = "For multiple columns, use `value_diffs_stacked()`"))
  }
  comparison$summ %>%
    filter(column == column_char) %>%
    pull(value_diffs) %>%
    `[[`(1)
}

#' @rdname compare
#' @export
value_diffs_stacked <- function(comparison, column) {
  column <- enquo(column)
  column_char <- get_cols_from_comparison(comparison, column)
  has_value_diffs <- comparison$summ$n_diffs > 0
  to_stack <- has_value_diffs & comparison$summ$column %in% column_char

  conform <- function(value_diffs, col_name) {
    names(value_diffs)[seq(2)] <- paste0("val_", c("a", "b"))
    value_diffs %>%
      mutate(across(seq(2), as.character)) %>%
      mutate(column = col_name, .before = 1)
  }

  Map(conform,
      comparison$summ$value_diffs[to_stack],
      comparison$summ$column[to_stack]) %>%
    bind_rows()
}

#' @rdname compare
#' @export
all_value_diffs <- function(comparison) {
  value_diffs_stacked(comparison, everything())
}

# Helpers ---------

get_cols_from_comparison <- function(comparison, column) {
  # simulate a data frame with the same classes as table_a to eval_select from
  cols_class_list <- strsplit(comparison$summ$class_a, ", ")
  fake_df <- simulate_df(comparison$summ$column, cols_class_list)
  names(eval_select(column, fake_df))
}

join_split <- function(table_a, table_b, by) {
  # full_join output split into common and unmatched
  data <- full_join(
    table_a %>% mutate(versus_in_a = TRUE),
    table_b %>% mutate(versus_in_b = TRUE),
    by = by, suffix = c("_a", "_b")
  ) %>%
    mutate(across(starts_with("versus_in"), \(x) coalesce(x, FALSE)),
      common = versus_in_a & versus_in_b
    )
  common <- data %>%
    filter(common) %>%
    select(-starts_with("versus_in"), -common)
  unmatched <- data %>%
    filter(!common) %>%
    mutate(table = if_else(versus_in_a, "a", "b"), .before = 1) %>%
    select(table, all_of(by))
  list(common = common, unmatched = unmatched)
}

col_value_diffs <- function(data, col, by, allow_both_NA = TRUE) {
  col_a <- sym(paste0(col, "_a"))
  col_b <- sym(paste0(col, "_b"))
  if (allow_both_NA) {
    filter_expr <- expr(!!col_a != !!col_b)
  } else {
    filter_expr <-
      expr(coalesce(!!col_a != !!col_b, is.na(!!col_a), is.na(!!col_b)))
  }
  data %>%
    filter(!!filter_expr) %>%
    select(!!col_a, !!col_b, all_of(by))
}
