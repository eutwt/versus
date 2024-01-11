fsubset <- function(x, i, j, check = FALSE) {
  ss(x, i, j, check = check)
}

assert_table_is_a_or_b <- function(table, call = caller_env()) {
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

  comparison_class <- withCallingHandlers(
    class(eval_tidy(comparison_quo)),
    error = \(e) cli_abort(message, call = call)
  )

  if (identical(comparison_class, "vs_comparison")) {
    return(invisible())
  }
  cli_abort(message, call = call)
}

is_ptype_compatible <- function(...) {
  incompatible <- pmap_lgl(list(...), \(...) {
    cnd <- catch_cnd(vec_ptype_common(...))
    inherits(cnd, "vctrs_error_ptype2")
  })
  !incompatible
}

table_init <- function(comparison, cols = c("intersection", "by"), tbl = c("a", "b")) {
  # simulate a data frame with the same classes as table_[tbl]
  cols <- arg_match(cols)
  tbl <- arg_match(tbl)
  fsubset(comparison$input$value[[tbl]], integer(0), comparison[[cols]]$column)
}

get_cols_from_comparison <- function(
    comparison,
    column,
    allow_empty = FALSE,
    call = caller_env()) {
  rethrow_oob <- function(e) {
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

  template <- table_init(comparison, "intersection")
  withCallingHandlers(
    eval_select(column, template, allow_empty = allow_empty),
    vctrs_error_subscript_oob = rethrow_oob,
    error = rethrow_default
  )
}

identify_diff_cols <- function(comparison, column, call = caller_env()) {
  selected_cols <- get_cols_from_comparison(comparison, column, call = call)
  is_selected <- seq_len(nrow(comparison$intersection)) %in% selected_cols
  has_value_diffs <- comparison$intersection$n_diffs > 0
  out <- which(is_selected & has_value_diffs)
  setNames(out, comparison$intersection$column[out])
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
  tdf <- vec_slice(df, 1) %>%
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
  col_compatible <- exec(is_ptype_compatible, !!!slice_list)
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
  "diff_rows",
  "unmatched",
  "versus_in_a",
  "versus_in_b",
  "val_a",
  "val_b"
))
