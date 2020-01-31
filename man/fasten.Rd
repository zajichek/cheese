\name{fasten}
\alias{fasten}
\title{Bind lists of data frames back together}
\description{
Take a list with data frames at the leaves and bind them back together (row-wise).
}
\usage{
fasten(
    list,
    into = NULL,
    depth = 0
)
}
\arguments{
\item{list}{A \code{list} with data frames at the leaves.}
\item{into}{A character vector of resulting column names. Defaults to \code{NULL}.}
\item{depth}{Depth to bind the list to. Defaults to 0.}
}
\details{
For the \code{into} argument, use empty strings \code{""} to omit column creation when rows are binded. Use positive integers for the \code{depth} to move from the root and negative integers to move from the leaves. The maximum (minimum) depth will be used for integers larger (smaller) than such. The leaves of the input \code{list} should be at the same depth.
}
\value{
\itemize{
\item If the (implied) depth is equal to zero, a \code{tibble::tibble}.
\item Otherwise, a \code{list} with \code{tibble::tibble} objects at the leaves.
}
}
\author{Alex Zajichek}
\examples{

require(tidyverse)

#Make a divided data frame
list <-
  heart_disease \%>\%
  divide(
    Sex,
    HeartDisease,
    ChestPain
  )

#Bind without creating names
list \%>\% 
  fasten

#Bind with names
list \%>\% 
  fasten(
    into = c("Sex", "HeartDisease", "ChestPain")
  )

#Only retain "Sex"
list \%>\%
  fasten(
    into = "Sex"
  )

#Only retain "HeartDisease"
list \%>\%
  fasten(
    into = c("", "HeartDisease")
  )

#Bind up to Sex
list \%>\%
  fasten(
    into = c("HeartDisease", "ChestPain"),
    depth = 1
  )

#Same thing, but start at the leaves
list \%>\%
  fasten(
    into = c("HeartDisease", "ChestPain"),
    depth = -2
  )

#Too large of depth returns original list
list \%>\%
  fasten(
    depth = 100
  )

#Too small of depth goes to 0
list \%>\%
  fasten(
    depth = -100
  )


}  