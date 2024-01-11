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
#' @importFrom vctrs vec_locate_matches vec_as_names vec_slice
#' @importFrom vctrs vec_interleave vec_ptype_common %0%
#' @importFrom purrr map imap map_int map_lgl map2_lgl map_chr reduce map_if
#' @importFrom purrr pmap pmap_lgl compose
#' @importFrom tibble tibble rownames_to_column enframe
#' @importFrom collapse ss add_vars frename whichNA %!=%
#' @importFrom data.table fcoalesce copy
"_PACKAGE"
