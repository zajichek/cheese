\name{muddle}
\alias{muddle}
\title{Randomly permute some or all columns of a data frame}

\usage{
muddle(
    data,
    at,
    ...
)
}
\arguments{
\item{data}{A data frame.}
\item{at}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers}. Defaults to all columns.}
\item{...}{Additional arguments passed to \code{base::sample}}
}.
\value{
A \code{tibble::tibble}
}
\author{Alex Zajichek}
\examples{

require(tidyverse)

#Set a seed
set.seed(123)

#Default permutes all columns
heart_disease \%>\%
  muddle

#Permute select columns
heart_disease \%>\%
  muddle(
    at = c(Age, Sex)
  )

#Using a select helper
heart_disease \%>\%
  muddle(
    at = matches("^S")
  )

#Pass other arguments
heart_disease \%>\%
  muddle(
    size = 5,
    replace = TRUE
  )

}  
