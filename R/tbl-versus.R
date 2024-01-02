#' print tables with differing values

#' @rdname tbl_versus
#' @export
print.tbl_versus <- function(x, ...) {
  if (is_null(attr(x, "diff_cols"))) {
    return(NextMethod())
  }
  focus_cols <- get_focus_cols(x)
  if (is_empty(focus_cols)) {
    return(NextMethod())
  }
  attr(x, "diff_cols") <- get_diff_cols(x)
  attr(x, "pillar_focus") <- focus_cols
  writeLines(format(x))
  attr(x, "pillar_focus") <- NULL
  invisible(x)
}

#' @rdname tbl_versus
#' @export
tbl_format_header.tbl_versus <- function(x, ...) {
  default_header <- NextMethod()
  i_focus_header <- grep('Focus columns', default_header)
  diff_cols <- attr(x, "diff_cols") %>% 
    dottize(console_width() - 20)
  diff_cols_header <- glue("# Diff columns: {diff_cols}") %>% 
    style_subtle()
  replace(default_header, i_focus_header, diff_cols_header)
}

new_tbl_versus <- function(x, diff_cols, wide = FALSE) {
  out <- as_tibble(x)
  if (is.integer(diff_cols)) {
    diff_cols <- names(diff_cols)
  }
  attr(out, "diff_cols") <- diff_cols
  attr(out, "wide") <- wide
  class(out) <- c("tbl_versus", class(out))
  out
}

get_focus_cols <- function(tvs) {
  diff_cols <- attr(tvs, "diff_cols")
  if (!attr(tvs, "wide")) {
    return(intersect(diff_cols, names(tvs)))
  }
  vctrs::vec_expand_grid(col = diff_cols, tbl = c("a", "b")) %>% 
    glue_data("{col}_{tbl}") %>% 
    intersect(names(tvs))
}

get_diff_cols <- function(tvs) {
  diff_cols <- attr(tvs, "diff_cols")
  if (!attr(tvs, "wide")) {
    return(intersect(diff_cols, names(tvs)))
  }
  vctrs::vec_expand_grid(col = diff_cols, tbl = c("a", "b")) %>% 
    filter(glue("{col}_{tbl}") %in% names(tvs)) %>% 
    pull(col) %>% 
    unique()
}


