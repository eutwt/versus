% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare}
\alias{compare}
\title{Compare two data frames}
\usage{
compare(table_a, table_b, by, allow_both_NA = TRUE, coerce = TRUE)
}
\arguments{
\item{table_a}{A data frame}

\item{table_b}{A data frame}

\item{by}{<\code{\link[=versus_tidy_select]{tidy-select}}>. Selection of columns to use when matching rows between
\code{.data_a} and \code{.data_b}. Both data frames must be unique on \code{by}.}

\item{allow_both_NA}{Logical. If \code{TRUE} a missing value in both data frames is
considered as equal}

\item{coerce}{Logical. If \code{FALSE} and columns from the input tables have
differing classes, the function throws an error.}
}
\value{
\describe{
\item{\code{compare()}}{A list of data frames having the following elements:
\describe{
\item{tables}{
A data frame with one row per input table showing the number of rows
and columns in each.
}
\item{by}{
A data frame with one row per \code{by} column showing the class
of the column in each of the input tables.
}
\item{intersection}{
A data frame with one row per column common to \code{table_a} and
\code{table_b} and columns "n_diffs" showing the number of values which
are different between the two tables, "class_a"/"class_b" the class of the
column in each table, and "value_diffs" a (nested) data frame showing
the the row indices with differing values
}
\item{unmatched_cols}{
A data frame with one row per column which is in one input table but
not the other and columns "table": which table the column appears in,
"column": the name of the column, and "class": the class of the
column.
}
\item{unmatched_rows}{
A data frame which, for each row present in one input table but not
the other, contains the column "table" showing which table the row appears
in and the \code{by} columns for that row.
}
}
}
}
}
\description{
\code{compare()} creates a representation of the differences between two tables,
along with a shallow copy of the tables. This output is used
as the \code{comparison} argument when exploring the differences further with other
versus functions e.g. \verb{slice_*()} and \verb{weave_*()}.
}
\section{data.table inputs}{

If the input is a data.table, you may want \code{compare()} to make a deep copy instead
of a shallow copy so that future changes to the table don't affect the comparison.
To achieve this, you can set \code{options(versus.copy_data_table = TRUE)}.
}

\examples{
compare(example_df_a, example_df_b, by = car)

}
