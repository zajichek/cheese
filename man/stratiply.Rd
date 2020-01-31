\name{stratiply}
\alias{stratiply}
\title{Stratify a data frame and apply a function}
\usage{
stratiply(
    data,
    by,
    f,
    ...
)
}
\arguments{
\item{data}{A \code{data.frame}.}
\item{by}{Columns or positions, or a \code{tidyselect::vars_select} helper.}
\item{f}{A function that takes a \code{data.frame} as an argument.}
\item{...}{Additional arguments passed to \code{f}.}
}
\value{
A list with the result of \code{f} at the leaves.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

heart_disease \%>\%
    stratiply(
        by = c("Sex", "HeartDisease"),
        f = head
    )

#Select helper
heart_disease \%>\%
    stratiply(
        by = starts_with("S"),
        f = head
    )
    
#Custom function
heart_disease \%>\%
  stratiply(
        by = "Sex",
        f = glm,
        formula = HeartDisease ~ .,
        family = "binomial"
  ) 
}