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
  old_option <- getOption("duckdb.materialize_message")
  assign(".duckdb_materialize_old_option", old_option, envir = .GlobalEnv)
  options(duckdb.materialize_message = FALSE)
}

.onUnload <- function(libname) {
  if (exists(".duckdb_materialize_old_option", envir = .GlobalEnv)) {
    options(duckdb.materialize_message = get(".duckdb_materialize_old_option",
                                             envir = .GlobalEnv))
    rm(.duckdb_materialize_old_option, envir = .GlobalEnv)
  }
}
