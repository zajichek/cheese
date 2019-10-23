\name{typly}
\alias{typly}
\title{Apply function(s) to elements conforming to specified type(s)}
\description{
    Evaluates a function or a list of function on all elements of \code{list} or \code{data.frame} that inherit at least one of the allowable types specified by the user. An option is available to evaluate the function(s) on all elements that do not match.
}
\usage{
typly(
    data,
    types,
    f,
    negated = FALSE,
    keep = FALSE,
    ...
)
}
\arguments{
\item{data}{A \code{data.frame} or \code{list}.}
\item{types}{A character vector of allowable data types identifying columns in which function(s) should be applied.}
\item{f}{A \code{function} or \code{list} of \code{function}s.}
\item{negated}{Should the function(s) be applied to columns that don't match any \code{types}? Defaults to \code{FALSE}.}
\item{keep}{Should the non-matching columns be kept as is? Defaults to \code{FALSE}.}
\item{...}{Additional arguments to be passed to \code{f}.}
}
\value{
A \code{list} with the result(s) of \code{f} for each applicable column.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

heart_disease \%>\%
    
    #Compute means and medians on numeric data
    typly(
        c("numeric", "logical"),
        list(
            mean = mean,
            median = median
        ),
        keep = TRUE,
        na.rm = TRUE
    ) \%>\%
    
    #Compute table
    typly(
        "factor",
        table,
        keep = TRUE
    )
}