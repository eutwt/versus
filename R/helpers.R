fsubset <- function(x, i, j) {
  if (!missing(i) && is_null(i)) {
    i <- 0
  }
  is_zero <- function(i) {
    length(i) == 1 && i == 0
  }
  ss(x, i, j, check = is_zero(i))
}

lock <- function(value) {
  env <- new.env()
  env$value <- value
  lockEnvironment(env, bindings = TRUE)
  return(env)
}

validate_table_arg <- function(table, call = caller_env()) {
  if (identical(table, quo())) {
    cli_abort("`table` is absent but must be supplied.", call = call)
  }
  table_expr <- quo_squash(table)
  table_chr <- shorten(deparse(table_expr), 30)
  top_msg <- "Problem with argument `table = {table_chr}`"
  if (!is_character(table_expr)) {
    info <- '`table` must be a single character value: "a" or "b"'
    cli_abort(c(top_msg, i = info), call = call)
  }
  if (!(identical(table_expr, "a") | identical(table_expr, "b"))) {
    info <- '`table` must be either "a" or "b"'
    cli_abort(c(top_msg, i = info), call = call)
  }
}

assert_is_comparison <- function(comparison_quo, call = caller_env()) {
  comparison_label <- shorten(as_label(comparison_quo), 40)
  message <- c(
    "Problem with argument `comparison = {comparison_label}`",
    i = "`comparison` must be the output of `versus::compare()`"
  )

  comparison_class <- try_fetch(
    class(eval_tidy(comparison_quo)),
    error = \(e) cli_abort(message, call = call)
  )

  if (identical(comparison_class, "vs_compare")) {
    return(invisible())
  }
  cli_abort(message, call = call)
}

is_ptype_compatible <- function(tbl_a, tbl_b) {
  incompatible <- map2_lgl(tbl_a, tbl_b, \(col_a, col_b) {
    cnd <- catch_cnd(vec_ptype_common(col_a, col_b))
    inherits(cnd, "vctrs_error_ptype2")
  })
  !incompatible
}

table_init <- function(comparison, tbl = c("a", "b"), cols = c("intersection", "by")) {
  # simulate a data frame with the same classes as table_[tbl]
  tbl <- arg_match(tbl)
  cols <- arg_match(cols)

  if (cols == "intersection") {
    value_diff_column <- if_else(tbl == "a", 1, 2)
    comparison$intersection %>%
      with(setNames(value_diffs, column)) %>%
      lapply(\(x) x[[value_diff_column]][0])
  } else if (cols == "by") {
    fsubset(comparison$unmatched_rows, 0, comparison$by$column)
  }
}

get_cols_from_comparison <- function(
    comparison,
    column,
    allow_empty = FALSE,
    call = caller_env()) {
  template_a <- table_init(comparison, tbl = "a", cols = "intersection")

  rethrow_oob <- function(e) {
    column_arg <- shorten(glue("column = {as_label(column)}"), 50)
    message <- c(
      "Must select columns from `comparison$intersection`",
      i = "column `{e$i}` is not part of the supplied comparison"
    )
    cli_abort(message, call = call)
  }
  rethrow_default <- function(e) {
    column_arg <- shorten(glue("column = {as_label(column)}"), 50)
    top_message <- glue("Problem with argument `{column_arg}`:")
    abort(message = c(top_message, cnd_message(e)), call = call)
  }
  try_fetch(
    eval_select(column, template_a, allow_empty = allow_empty),
    vctrs_error_subscript_oob = rethrow_oob,
    error = rethrow_default
  )
}

shorten <- function(x, max_char = 10) {
  stopifnot(is.character(x))
  is_long <- nchar(x) > max_char
  if (!any(is_long)) {
    return(x)
  }
  x[is_long] <- paste0(substr(x[is_long], 1, max_char - 3), "...")
  x
}

dottize <- function(vec, max_size = 20) {
  vec <- as.character(vec)
  if (is_empty(vec)) {
    return(vec)
  }
  if (nchar(vec[1]) > max_size) {
    return(shorten(vec[1], max_size))
  }
  shorten_vec <- function(vec, max_size) {
    # get printed size when printed with ", " between each element and ...
    print_size <- cumsum(nchar(vec)) + 2 * (seq_along(vec) - 1) + 4
    for (i in seq2(2, length(vec))) {
      if (print_size[i] > max_size) {
        return(c(head(vec, i - 1), "..."))
      }
    }
    return(vec)
  }

  vec %>%
    shorten_vec(max_size) %>%
    glue_collapse(", ")
}

itemize_row <- function(df, max_lines = 3, width = 50) {
  # similar to `capture.output(glimpse(df[1,]))`, boxed by max_lines x width
  tdf <- fsubset(df, 1) %>%
    map_chr(format_glimpse) %>%
    shorten(width) %>%
    enframe()
  if (nrow(tdf) <= max_lines) {
    out <- glue_data(tdf, "$ {name}: {value}")
    return(out)
  }
  first <- head(tdf, max_lines) %>%
    glue_data("$ {name}: {value}")
  n_more <- nrow(tdf) - max_lines
  more <- paste0(
    glue("{symbol$info} {n_more} more: "),
    dottize(tail(tdf$name, n_more), width)
  )
  c(first, more)
}

contents <- function(table) {
  out_vec <- map_chr(table, \(x) paste(class(x), collapse = ", "))
  enframe(out_vec, name = "column", value = "class")
}

# slice_() helpers ------------

ensure_ptype_compatible <- function(slice_list) {
  # if the column types are incompatible, convert them to character first
  col_compatible <- is_ptype_compatible(slice_list$a, slice_list$b)
  if (all(col_compatible)) {
    return(slice_list)
  }
  incompatible_cols <- names(col_compatible)[!col_compatible]
  cols_char <- dottize(incompatible_cols, 30)
  cli_alert_info("Columns converted to character: {cols_char}")

  slice_list %>%
    map(\(x) mutate(x, across(all_of(incompatible_cols), as.character)))
}

# test objects ------------

test_df_a <- mtcars %>%
  rownames_to_column("car") %>%
  mutate(
    disp = replace(disp, 3:4, disp[3:4] + 1),
    cyl = replace(cyl, 3, NA),
    extracol_a = 1
  ) %>%
  head(10) %>%
  bind_rows(., head(., 1) %>% mutate(car = "extra_a"))

test_df_b <- mtcars %>%
  rownames_to_column("car") %>%
  mutate(
    mpg = replace(mpg, 7:8, mpg[7:8] + 2),
    cyl = replace(cyl, 3:4, NA),
    wt = as.character(wt)
  ) %>%
  filter(row_number() %in% seq(2, 12)) %>%
  bind_rows(., head(., 1) %>% mutate(car = "extra_b"))

utils::globalVariables(c(
  ".",
  "class_a",
  "class_b",
  "column",
  "common",
  "unmatched",
  "versus_in_a",
  "versus_in_b",
  "val_a",
  "val_b"
))
