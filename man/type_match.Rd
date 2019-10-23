\name{type_match}
\alias{type_match}
\title{Check if an object is at least one (or none) of the specified types}
\description{
This is a utility function to assess whether an object inherits at least one (or none) of the user-specified classes.
}
\usage{
type_match(
    object,
    types,
    negated = FALSE
)
}
\arguments{
\item{object}{Any \code{R} object.}
\item{types}{A character vector of classes to check against \code{object}.}
\item{negated}{Should \code{TRUE} be returned if \code{object} inherits no \code{types}? Defaults to \code{FALSE}.}
}
\value{
A \code{logical} indicator
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Default
heart_disease \%>\%
    map_lgl(
        type_match,
        types = "numeric"
    )

#2) Use negation
heart_disease \%>\%
    map_lgl(
        type_match,
        types = "numeric",
        negated = TRUE
    )

#3) Multiple types
heart_disease \%>\%
    map_lgl(
        type_match,
        types = c("numeric", "factor")
    )

#4) Check other objects
heart_disease \%>\%
    glm(Sex ~ Age, data = ., family = "binomial") \%>\%
    type_match(
        types = "glm"
    )
}