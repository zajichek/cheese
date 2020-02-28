\name{stratiply}
\alias{stratiply}
\title{Stratify a data frame and apply a function}
\usage{
stratiply(
    data,
    f,
    by,
    ...
)
}
\arguments{
\item{data}{A data frame.}
\item{f}{A function that takes a \code{data.frame} as an argument.}
\item{by}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers}. Defaults to all columns.}
\item{...}{Additional arguments passed to \code{f}.}
}
\value{
A list with the result of \code{f} at the leaves.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#Unquoted selection
heart_disease \%>\%
    stratiply(
        head,
        Sex
    )

#Select helper
heart_disease \%>\%
    stratiply(
        f = head,
        by = starts_with("S")
    )
    
#Use additional arguments for the function
heart_disease \%>\%
  stratiply(
        f = glm,
        by = Sex,
        formula = HeartDisease ~ .,
        family = "binomial"
  )

#Use mixed selections to split by desired columns
heart_disease \%>\%
  stratiply(
        f = glm,
        by = c(Sex, is.logical),
        formula = HeartDisease ~ Age,
        family = "binomial"
  ) 
  
}