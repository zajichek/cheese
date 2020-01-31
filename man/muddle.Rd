\name{muddle}
\alias{muddle}
\title{Randomly permute some or all columns of a data frame}

\usage{
muddle(
    data,
    at = names(data),
    ...
)
}
\arguments{
\item{data}{A \code{data.frame}.}
\item{at}{Columns or positions to permute, or a \code{tidyselect::vars_select} helper. Defaults to all columns.}
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
    at = c("Age", "Sex")
  )

#Pass other arguments
heart_disease \%>\%
  muddle(
    size = 5,
    replace = TRUE
  )

}  
