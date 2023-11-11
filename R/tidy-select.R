#' Argument type: tidy-select
#'
#' @description
#' This page describes the `<tidy-select>` argument modifier which
#' indicates that the argument uses **tidy selection**, a sub-type of
#' tidy evaluation. If you've never heard of tidy evaluation before,
#' start with the practical introduction in
#' <https://r4ds.hadley.nz/functions.html#data-frame-functions> then
#' then read more about the underlying theory in
#' <https://rlang.r-lib.org/reference/topic-data-mask.html>.
#'
#' # Overview of selection features
#'
#' ```{r, child = "man/rmd/overview.Rmd"}
#' ```
#'
#' # Key techniques
#'
#' *   If you want the user to supply a tidyselect specification in a
#'     function argument, you need to tunnel the selection through the function
#'     argument. This is done by embracing the function argument `{{ }}`,
#'     e.g `unnest(df, {{ vars }})`.
#'
#' *   If you have a character vector of column names, use `all_of()`
#'     or `any_of()`, depending on whether or not you want unknown variable
#'     names to cause an error, e.g `unnest(df, all_of(vars))`,
#'     `unnest(df, !any_of(vars))`.
#'
#' *   To suppress `R CMD check` `NOTE`s about unknown variables use `"var"`
#'     instead of `var`:
#'
#'    ```R
#'    # has NOTE
#'    df %>% select(x, y, z)
#'
#'    # no NOTE
#'    df %>% select("x", "y", "z")
#'    ```
#'
#' @keywords internal
#' @name versus_tidy_select
NULL
