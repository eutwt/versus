#' @description Compare two tables
#' @keywords internal
#'
#' @importFrom stats setNames
#' @importFrom utils adist
#' @import rlang
#' @import dplyr
#' @import duckplyr
#' @import duckplyr
#' @import tidyselect
#' @import glue
#' @importFrom vctrs vec_duplicate_any
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  options(duckdb.materialize_message = FALSE)
}
