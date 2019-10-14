\name{divide}
\alias{divide}
\title{Divide a data frame into a list of any depth by any number of variables.}
\description{
Separates a data frame by one or more stratification variables into a list of arbitrary depth whose leaves are data frames and names are the strata.
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
\item{sep}{String to separate values of each stratification variable by. Defaults to \code{"|"}. Only used when the number of stratification columns exceeds the desired depth.}
}
\details{
For the \code{depth}, use positive integers to move from the root and negative integers to move from the leaves. 
}
\value{
A \code{list} with \code{tibble} objects at the leaves.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Unquoted
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease
    )

#2) Using select helpers
heart_disease \%>\%
    divide(
        matches("^(S|H)")
    )

#3) Reduce the depth
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        depth = 1
    )
    
#4) Keep columns in result
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        remove = FALSE
    )

#5) Move from leaves
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        ChestPain,
        depth = -1
    )
}  
