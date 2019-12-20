\name{pick}
\alias{pick}
\title{Select a subset of a list}

\usage{
pick(
    list,
    ...
)
}
\arguments{
\item{list}{Any \code{list}.}
\item{...}{Selection of elements. See \code{?dplyr::select} for details.}
}
\value{
A subset of the input \code{list} at the specified elements.
}
\author{Alex Zajichek}
\examples{

require(tidyverse)

list <-
  heart_disease \%>\%
  divide(
    Sex,
    HeartDisease,
    ChestPain
  )

length(list)

#Get Male sublist
list \%>\%
  pick(Male)

#Get heart disease subset with each sex
list \%>\%
  map(
    pick,
    Yes
  )

#Make same list with single depth
list2 <-
  heart_disease \%>\%
  divide(
    Sex,
    HeartDisease,
    ChestPain,
    depth = 1
  )

length(list2)

#Use select helpers
list2 \%>\%
  pick(
    matches("angina$")
  )

#Multiple methods
list2 \%>\%
  pick(
    matches("angina$"),
    starts_with("Male")
  )


}  
