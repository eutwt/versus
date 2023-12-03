#' @description Compare two tables
#' @keywords internal
#'
#' @importFrom stats setNames
#' @import rlang
#' @import cli
#' @import dplyr
#' @importFrom tidyselect eval_select
#' @importFrom pillar format_glimpse
#' @importFrom glue glue glue_collapse glue_data
#' @importFrom utils capture.output head tail
#' @importFrom vctrs vec_locate_matches vec_as_names
#' @importFrom vctrs vec_interleave vec_ptype_common %0%
#' @importFrom purrr map_int map_lgl map2_lgl map_chr
#' @importFrom tibble tibble rownames_to_column enframe
#' @importFrom collapse ss rowbind add_vars frename gsplit join roworderv
#' @importFrom data.table fcase fcoalesce
"_PACKAGE"
