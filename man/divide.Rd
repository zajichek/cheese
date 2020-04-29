\name{divide}
\alias{divide}
\title{Divide a data frame into a list}
\description{
Separate a \code{\link{data.frame}} into a \code{\link{list}} of any depth by one or more stratification columns whose levels become the names.
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
\item{data}{Any \code{\link{data.frame}}.}
\item{...}{Selection of columns to split by. See \code{dplyr::select} for details.}
\item{depth}{Depth to split to. Defaults to \code{Inf}. See details for more information.}
\item{remove}{Should the stratfication columns be removed? Defaults to \code{TRUE}.}
\item{drop}{Should unused combinations of stratification variables be dropped? Defaults to \code{TRUE}.}
\item{sep}{String to separate values of each stratification variable by. Defaults to \code{"|"}. Only used when the number of stratification columns exceeds the desired depth.}
}
\details{
For the \code{depth}, use positive integers to move from the root and negative integers to move from the leaves. The maximum (minimum) depth will be used for integers larger (smaller) than such.
}
\value{
A \code{\link{list}}
}
\author{Alex Zajichek}
\examples{
#Unquoted selection
heart_disease \%>\%
    divide(
        Sex
    )

#Using select helpers
heart_disease \%>\%
    divide(
        matches("^S")
    )

#Reduced depth
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        depth = 1
    )
    
#Keep columns in result; change delimiter in names
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        depth = 1,
        remove = FALSE,
        sep = ","
    )

#Move inward from maximum depth
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        ChestPain,
        depth = -1
    )

#No depth returns original data (and warning)
heart_disease \%>\%
    divide(
        Sex,
        depth = 0
    )
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        depth = -5
    )

#Larger than maximum depth returns maximum depth (default)
heart_disease \%>\%
    divide(
        Sex,
        depth = 100
    )

}  
