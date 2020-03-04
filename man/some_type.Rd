\name{some_type}
\alias{some_type}
\title{Is an object one of the specified types?}
\description{
Check if an object inherits one (or more) of a vector classes.
}
\usage{
some_type(
    object,
    types
)
}
\arguments{
\item{object}{Any \code{R} object.}
\item{types}{A \code{base::character} vector of classes to test against.}
}
\value{
A \code{base::logical} indicator
}
\author{Alex Zajichek}
\examples{
#Columns of a data frame
heart_disease \%>\%
    purrr::map_lgl(
        some_type,
        types = c("numeric", "logical")
    )

}