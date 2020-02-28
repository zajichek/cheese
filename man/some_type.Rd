\name{some_type}
\alias{some_type}
\title{Check if an object conforms to one or more of the specified types}
\usage{
some_type(
    object,
    types
)
}
\arguments{
\item{object}{Any \code{R} object.}
\item{types}{A character vector of classes to check against \code{object}.}
}
\value{
A \code{logical} indicator
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

heart_disease \%>\%
    map_lgl(
        some_type,
        types = c("numeric", "logical")
    )

}