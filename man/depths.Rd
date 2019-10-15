\name{depths}
\alias{depths}
\title{Traverse a list of arbitrary depth to find elements that satisfy a predicate.}
\description{
Finds paths and positions of a list structure that satisfy a binary function. The depth of the list can be arbitrary and uneven. See details for more information.
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
The input is recursively evaluated to indicate whether each element satisfies \code{predicate}, and only proceeds where the next element \code{rlang::is_list} when argument \code{bare} is \code{FALSE}, and \code{rlang::is_bare_list} when it is \code{TRUE}. The result uses "\{" and "\}" to indicate the level of the tree, "," to separate element indices within a level, and the sign to indicate whether the element satisfied \code{predicate} (+ = yes, - = no).
}
\value{
A character string indicating the path, positions and elements that satisfy \code{predicate}.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#Find positions of divided data frames
heart_disease \%>\%
  divide(
    Sex,
    HeartDisease
  ) \%>\%
  depths(
    predicate = is.data.frame
  )

heart_disease \%>\%
  divide(
    Sex,
    HeartDisease
  ) \%>\%
  depths(
    predicate = is.data.frame,
    bare = TRUE
  )

heart_disease \%>\%
  divide(
    Sex,
    HeartDisease,
    depth = 1
  ) \%>\%
  depths(
    predicate = is.data.frame,
    bare = TRUE
  )

#Make uneven list
my_list <-
  list(
    heart_disease,
    list(
      heart_disease
    ),
    1:10
  )

my_list \%>\%
  depths(
    is.data.frame,
    bare = TRUE
  )

}  
