#' print tables with differing values
#' @keywords internal 

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
  attr(x, "pillar_focus") <- focus_cols
  writeLines(format(x))
  attr(x, "pillar_focus") <- NULL
  invisible(x)
}

#' @rdname tbl_versus
#' @export
tbl_format_header.tbl_versus <- function(x, ...) {
  default_header <- NextMethod()
  default_header[!grepl('Focus columns', default_header)]
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

