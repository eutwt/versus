#' @description Compare two tables
#' @keywords internal
#'
#' @importFrom stats setNames
#' @import rlang
#' @import dplyr
#' @import tidyselect
#' @import glue
#' @importFrom utils capture.output
#' @importFrom vctrs vec_locate_matches
#' @importFrom vctrs vec_interleave vec_ptype_common
#' @importFrom purrr map_int map_lgl map2_lgl
#' @importFrom tibble tibble rownames_to_column
#' @importFrom collapse ss qF rowbind add_vars frename
#' @importFrom data.table fcase fcoalesce
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  options(duckdb.materialize_message = FALSE)
}
