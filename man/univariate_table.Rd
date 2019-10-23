\name{univariate_table}
\alias{univariate_table}
\title{Create a custom univariate summary for a dataset}
\description{
Produces a formatted table of univariate summary statistics with options allowing for stratification by 1 or more variables, computing of custom summary/association statistics, custom string templates for results, etc.
}
\usage{
univariate_table(
    data,
    strata = NULL,
    associations = NULL,
    numeric_summary = c(Summary = "median (q1, q3)"),
    categorical_summary = c(Summary = "count (percent\%)"),
    other_summary = c(Summary = "unique"),
    all_summary = NULL,
    evaluate = FALSE,
    add_n = FALSE,
    order = NULL,
    labels = NULL,
    levels = NULL,
    format = c("html", "latex", "markdown", "pandoc", "none"),
    variableName = "Variable",
    levelName = "Level",
    na_string = "(missing)",
    strata_sep = "/",
    summary_strata_sep = "_",
    fill_blanks = "",
    caption = "",
    ...
)
}
\arguments{
\item{data}{A \code{data.frame} to summarise.}
\item{strata}{A \code{formula} specifying one or more stratification variables. LHS variables go to rows, RHS variables go to columns. Defaults to \code{NULL}.}
\item{associations}{A named \code{list} of functions to evaluate with column strata and each variable. Defaults to \code{NULL}.}
\item{numeric_summary}{A (preferably named) character vector containing string templates of how results for numeric data should be presented. See details for a list of what is available by default. Defaults to \code{c(Summary = "median (q1, q3)")}.}
\item{categorical_summary}{A (preferably named) character vector containing string templates of how results for categorical data should be presented. See details for a list of what is available by default. Defaults to \code{c(Summary = "count (percent\%)")}.}
\item{other_summary}{A (preferably named) character vector containing string templates of how results for non-numeric and non-categorical data should be presented. See details for a list of what is available by default. Defaults to \code{c(Summary = "unique")}.}
\item{all_summary}{A (preferably named) character vector containing string templates of additional results for all variables should be presented. See details for a list of what is available by default. Defaults to \code{NULL}.}
\item{evaluate}{Should the results of the string templates be evaluated as an \code{R} expression after filled with their values? See \code{"\link{absorb}"} for details.  Defaults to \code{FALSE}.}
\item{add_n}{Should the sample size for each stratfication level be added to the result? Defaults to \code{FALSE}.}
\item{order}{Character vector of 1 or more variables to reorder the result by from top to bottom. If \code{NULL} (default), the result is sorted according to \code{names(data)}.}
\item{labels}{Named character vector for re-labeling variables in the result. Defaults to \code{NULL}.}
\item{levels}{Named \code{list} of character vectors for re-labeling factor levels in the result. Defaults to \code{NULL}.}
\item{format}{The format that the result should be rendered as. Must be one of \code{c("html", "latex", "markdown", "pandoc", "none")}. Defaults to \code{"html"}.}
\item{variableName}{Header for the variable column in the result. Defaults to \code{"Variable"}.}
\item{levelName}{Header for the factor level column in the result. Defaults to \code{"Level"}.}
\item{na_string}{String for \code{NA} factor levels in the result. Defaults to \code{"(missing)"}.}
\item{strata_sep}{Delimiter to separate stratification levels by in the result. Defaults to \code{"/"}.}
\item{summary_strata_sep}{Delimiter to separate summary column names with the strata groups. Defaults to \code{"_"}.}
\item{fill_blanks}{String to fill in blank spaces in the result. Defaults to \code{""}.}
\item{caption}{Caption for resulting table passed to \code{knitr::kable}. Defaults to \code{NULL}.}
\item{...}{Additional arguments to pass to \code{"\link{descriptives}"}.}
}

\value{
A table of summary statistics according to the specified \code{format}. A \code{tibble} is returned if \code{format = "none"}.
}

\details{
The following statistics are available by default for each data type:

Numeric: "min", "max", "median", "iqr", "mean", "sd", "q1", "q3"

Categorical: "count", "percent"

All variables: "length", "missing", "available", "class", "unique"

These strings are typed explicitly in the \code{._summary} arguments and serve as placeholders for where the actual value will appear. Custom functions can be entered in a named \code{list}, where the names are what provide access to the values in string templates. See \code{"\link{descriptives}"} and \code{"\link{absorb}"}.

The names of the \code{._summary} arguments are what become the column headers in the result. If unnamed, an arbitrary name (i.e. "VX") will appear in the column header.

}

\author{Alex Zajichek}
\examples{
require(tidyverse)
    
#1) Default summary
heart_disease \%>\%
    univariate_table()

#2) Stratified summary
heart_disease \%>\%
    univariate_table(
        strata = ~Sex,
        add_n = TRUE
    )
    
}


