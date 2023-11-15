#' @description Compare two tables
#' @keywords internal
#'
#' @importFrom stats setNames
#' @importFrom utils adist
#' @import rlang
#' @import dplyr
#' @import tidyselect
#' @import glue
#' @importFrom vctrs vec_duplicate_any
#' @importFrom vctrs vec_interleave vec_ptype_common
#' @importFrom purrr map_lgl map2_lgl
#' @importFrom tibble tibble rownames_to_column
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  options(duckdb.materialize_message = FALSE)
}
