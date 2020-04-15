\name{univariate_table}
\alias{univariate_table}
\title{Create a custom descriptive table for a dataset}
\description{
Produces a formatted table of univariate summary statistics with options allowing for stratification by one or more variables, computing of custom summary/association statistics, custom string templates for results, etc.
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
    sep = "_",
    variableName = "Variable",
    levelName = "Level",
    fill_blanks = "",
    format = c("html", "latex", "markdown", "pandoc", "none"),
    caption = NULL,
    ...
)
}
\arguments{
\item{data}{A \code{base::data.frame}.}
\item{strata}{An additive \code{stats::formula} specifying stratification columns. Columns on the left side go down the rows, and columns on the right side go across the columns. Defaults to \code{NULL}.}
\item{associations}{A named \code{base::list} of functions to evaluate with column strata and each variable. Defaults to \code{NULL}. See \code{univarite_associations}.}
\item{numeric_summary}{A (preferably named) character vector containing string templates of how results for numeric data should be presented. See details for a list of what is available by default. Defaults to \code{c(Summary = "median (q1, q3)")}.}
\item{categorical_summary}{A (preferably named) character vector containing string templates of how results for categorical data should be presented. See details for a list of what is available by default. Defaults to \code{c(Summary = "count (percent\%)")}.}
\item{other_summary}{A (preferably named) character vector containing string templates of how results for non-numeric and non-categorical data should be presented. See details for a list of what is available by default. Defaults to \code{c(Summary = "unique")}.}
\item{all_summary}{A (preferably named) character vector containing string templates of additional results for all variables should be presented. See details for a list of what is available by default. Defaults to \code{NULL}.}
\item{evaluate}{Should the results of the string templates be evaluated as an \code{R} expression after filled with their values? See \code{"\link{absorb}"} for details.  Defaults to \code{FALSE}.}
\item{add_n}{Should the sample size for each stratfication level be added to the result? Defaults to \code{FALSE}.}
\item{sep}{Delimiter to separate summary columns. Defaults to \code{"_"}.}
\item{variableName}{Header for the variable column in the result. Defaults to \code{"Variable"}.}
\item{levelName}{Header for the factor level column in the result. Defaults to \code{"Level"}.}
\item{fill_blanks}{String to fill in blank spaces in the result. Defaults to \code{""}.}
\item{format}{The format that the result should be rendered as. Must be one of \code{c("html", "latex", "markdown", "pandoc", "none")}. Defaults to \code{"html"}.}
\item{caption}{Caption for resulting table passed to \code{knitr::kable}. Defaults to \code{NULL}.}
\item{...}{Additional arguments to pass to \code{"\link{descriptives}"}.}
}

\value{
A table of summary statistics according to the specified \code{format}. A \code{tibble} is returned if \code{format = "none"}.
}

\author{Alex Zajichek}
\examples{

#Set format
format <- "pandoc"

#Default summary
heart_disease \%>\%
    univariate_table(
      format = format
    )

#Stratified summary
heart_disease \%>\%
    univariate_table(
        strata = ~Sex,
        add_n = TRUE,
        format = format
    )

#Row strata with custom summaries with
heart_disease \%>\%
    univariate_table(
        strata = HeartDisease~1,
        numeric_summary = c(Mean = "mean", Median = "median"),
        categorical_summary = c(`Count (\%)` = "count (percent\%)"),
        categorical_types = c("factor", "logical"),
        add_n = TRUE,
        format = format
    )
    
}


