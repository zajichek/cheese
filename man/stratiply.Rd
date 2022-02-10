\name{stratiply}
\alias{stratiply}
\title{Stratify a data frame and apply a function}
\description{
Split a \code{\link{data.frame}} by any number of columns and apply a function to subset.
}
\usage{
stratiply(
    data,
    f,
    by,
    ...
)
}
\arguments{
\item{data}{A \code{\link{data.frame}}.}
\item{f}{A function that takes a \code{\link{data.frame}} as an argument.}
\item{by}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers}}
\item{...}{Additional arguments passed to \code{f}.}
}
\value{
A \code{\link{list}}
}
\author{Alex Zajichek}
\examples{
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
        by = c(Sex, where(is.logical)),
        formula = HeartDisease ~ Age,
        family = "binomial"
  ) 
  
}