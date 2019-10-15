\name{divide}
\alias{divide}
\title{Divide a data frame into a list}
\description{
Separate a \code{data.frame} into a list of any depth by one or more stratification columns whose levels become the names.
}
\usage{
divide(
    data,
    ...,
    depth = Inf,
    remove = TRUE,
    drop = TRUE,
    sep = "|"
)
}
\arguments{
\item{data}{Any \code{data.frame}.}
\item{...}{Selection of columns to split by. See \code{?dplyr::select} for details.}
\item{depth}{Depth to split the list to. Defaults to \code{Inf}. See details for more information.}
\item{remove}{Should the stratfication columns be removed? Defaults to \code{TRUE}.}
\item{drop}{Should unused combinations of stratification variables be dropped? Defaults to \code{TRUE}.}
\item{sep}{String to separate values of each stratification variable by. Defaults to "|". Only used when the number of stratification columns exceeds the desired depth.}
}
\details{
For the \code{depth}, use positive integers to move from the root and negative integers to move from the leaves. The maximum depth will be used for integers larger than such.
}
\value{
A \code{list} with \code{tibble::tibble} objects at the leaves.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Unquoted
heart_disease \%>\%
    divide(
        Sex
    )

#2) Using select helpers
heart_disease \%>\%
    divide(
        matches("^S")
    )

#3) Reduce the depth
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        depth = 1
    ) \%>\%
    length() #Each combinations
    
#4) Keep columns in result
heart_disease \%>\%
    divide(
        Sex,
        remove = FALSE
    )

#5) Move from leaves
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        ChestPain,
        depth = -1
    ) \%>\%
    str()
}  
