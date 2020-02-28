\name{typly}
\alias{typly}
\title{Evaluate a function on columns conforming to one or more (or no) specified types}
\usage{
typly(
    data,
    f,
    types,
    negated = FALSE,
    ...
)
}
\arguments{
\item{data}{A data frame.}
\item{f}{A \code{function}.}
\item{types}{A character vector of classes to check against \code{object}.}
\item{negated}{Should the function be applied to columns that don't match any \code{types}? Defaults to \code{FALSE}.}
\item{...}{Additional arguments to be passed to \code{f}.}
}
\value{
A \code{list} with the result of \code{f} for each column.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

heart_disease \%>\%
    
    #Compute means on numeric or logical data
    typly(
        f = mean,
        types = c("numeric", "logical"),
        na.rm = TRUE
    ) 
}