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
    bare = TRUE,
    ...
)
}
\arguments{
\item{list}{A \code{list}, \code{data.frame}, or atomic vector.}
\item{predicate}{A binary \code{function}.}
\item{bare}{Should algorithm only continue for bare lists? Defaults to TRUE.}
\item{...}{Additional arguments to pass to \code{predicate}.}
}
\details{
The input is recursively evaluated to indicate whether each element satisfies \code{predicate}, and only proceeds where the next element \code{rlang::is_list} when argument \code{bare} is \code{FALSE}, and \code{rlang::is_bare_list} when it is \code{TRUE}.
}
\value{
A character string indicating the path, positions and elements that satisfy \code{predicate}. Brackets \{\} are used to indicate the level of the tree, commas to separate element-indices within a level, and the sign of the index to indicate whether the element satisfied \code{predicate} (- = yes, + = no).
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#Find positions of divided data frames
heart_disease \%>\%

  #Divide the frame into a list
  divide(
    Sex,
    HeartDisease,
    ChestPain
  ) \%>\%
  
  #Show paths to the data frames
  depths(
    predicate = is.data.frame
  )
  
#Shallower list
heart_disease \%>\%
  divide(
    Sex,
    HeartDisease,
    ChestPain,
    depth = 1
  ) \%>\%
  depths(
    predicate = is.data.frame
  )

#Allow for non-bare lists to be traversed
heart_disease \%>\%
  divide(
    Sex
  ) \%>\%
  depths(
    predicate = is.data.frame,
    bare = FALSE
  )

#Make uneven list with diverse objects
my_list <-
  list(
    heart_disease,
    list(
      heart_disease
    ),
    1:10,
    list(
      heart_disease$Age,
      list(
        heart_disease
      )
    ),
    glm(
      formula = HeartDisease ~ .,
      data = heart_disease,
      family = "binomial"
    )
  )

#Find the data frames
my_list \%>\%
  depths(
    predicate = is.data.frame
  )

#Go deeper by relaxing bare list argument
my_list \%>\%
  depths(
    predicate = is.data.frame,
    bare = FALSE
  )

}  
