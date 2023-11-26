#' @description Compare two tables
#' @keywords internal
#'
#' @importFrom stats setNames
#' @import rlang
#' @import cli
#' @import dplyr
#' @import tidyselect
#' @importFrom pillar format_glimpse
#' @importFrom glue glue glue_collapse
#' @importFrom utils capture.output head tail
#' @importFrom cli cli_abort
#' @importFrom vctrs vec_locate_matches vec_as_names vec_init
#' @importFrom vctrs vec_interleave vec_ptype_common %0%
#' @importFrom purrr map_int map_lgl map2_lgl map_chr
#' @importFrom tibble tibble rownames_to_column
#' @importFrom collapse ss qF rowbind add_vars frename gsplit join
#' @importFrom data.table fcase fcoalesce
"_PACKAGE"
