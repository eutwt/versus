% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice-diffs.R
\name{slice_diffs}
\alias{slice_diffs}
\title{Get rows with differing values}
\usage{
slice_diffs(comparison, table, column = everything())
}
\arguments{
\item{comparison}{The output of \code{compare()}}

\item{table}{One of \code{"a"} or \code{"b"} indicating which of the tables used to
create \code{comparison} should be sliced}

\item{column}{<\code{\link[=versus_tidy_select]{tidy-select}}>. A row will be in the output if
the comparison shows differing values for any columns matching this argument}
}
\value{
The input table is filtered to the rows for which \code{comparison}
shows differing values for one of the columns selected by \code{column}
}
\description{
Get rows with differing values
}
\examples{
comp <- compare(example_df_a, example_df_b, by = car)
comp |> slice_diffs("a", mpg)
comp |> slice_diffs("b", mpg)
comp |> slice_diffs("a", c(mpg, disp))
}
