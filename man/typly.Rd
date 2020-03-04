\name{typly}
\alias{typly}
\title{Evaluate a function on columns conforming to one or more (or no) specified types}
\description{
Apply a function to columns in a \code{base::data.frame} that inherit one of the specified types.
}
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
\item{data}{A \code{base::data.frame}.}
\item{f}{A \code{function}.}
\item{types}{A \code{base::character} vector of classes to test against.}
\item{negated}{Should the function be applied to columns that don't match any \code{types}? Defaults to \code{FALSE}.}
\item{...}{Additional arguments to be passed to \code{f}.}
}
\value{
A \code{base::list}
}
\author{Alex Zajichek}
\examples{
heart_disease \%>\%
    
    #Compute means on numeric or logical data
    typly(
        f = mean,
        types = c("numeric", "logical"),
        na.rm = TRUE
    ) 
}