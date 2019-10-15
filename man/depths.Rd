\name{depths}
\alias{depths}
\title{Traverse a list of arbitrary depth to find elements that satisfy a predicate}
\description{
Finds positions of a list structure that satisfy a binary function. The depth of the list can be arbitrary and uneven. See details for more information.
}
\usage{
depths(
    list,
    predicate,
    bare = FALSE,
    ...
)
}
\arguments{
\item{list}{Any \code{list}, \code{data.frame}, or atomic vector.}
\item{predicate}{Any binary \code{function}.}
\item{bare}{Should algorithm only continue for bare lists? Defaults to FALSE.}
\item{...}{Additional arguments to pass to \code{predicate}.}
}
\details{
The input is recursively evaluated to indicate whether each element satisfies \code{predicate}, and only proceeds where the next element \code{rlang::is_list} when argument \code{bare} is \code{FALSE}, and \code{rlang::is_bare_list} when it is \code{TRUE}. The result uses "\{" and "\}" to indicate the level of the tree, and elements within a level are separated by ",".
}
\value{
A character string indicating the positions and elements that satisfy \code{predicate}.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

heart_disease \%>\%
    divide(
        Sex,
        HeartDisease
    ) \%>\%
    depths(is.data.frame)
    
heart_disease \%>\%
    divide(
        Sex,
        HeartDisease,
        depth = 1
    ) \%>\%
    depths(is.data.frame)
}  
