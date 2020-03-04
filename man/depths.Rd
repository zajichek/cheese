\name{depths}
\alias{depths}
\alias{depths_string}
\title{Find the elements in a list structure that satisfy a predicate}
\description{
Traverse a list of structure to find the depths and positions of its elements that satisfy a predicate.
}
\usage{
depths(
    list,
    predicate,
    bare = TRUE,
    ...
)
depths_string(
    list,
    predicate,
    bare = TRUE,
    ...
)
}
\arguments{
\item{list}{A \code{base::list}, \code{base::data.frame}, or \code{base::vector}.}
\item{predicate}{A \code{function} that evaluates to \code{TRUE} or \code{FALSE}.}
\item{bare}{Should algorithm only continue for bare lists? Defaults to TRUE. See \code{rlang::`bare-type-predicates`}}
\item{...}{Additional arguments to pass to \code{predicate}.}
}
\details{
The input is recursively evaluated to find elements that satisfy \code{predicate}, and only proceeds where \code{rlang::is_list} when argument \code{bare} is \code{FALSE}, and \code{rlang::is_bare_list} when it is \code{TRUE}.
}
\value{
\itemize{
\item \code{depths()} returns a \code{base::integer} vector indicating the levels that contain elements satisfying the predicate.

\item \code{depths_string()} returns a \code{base::character} representation of the traversal. Brackets \{\} are used to indicate the level of the tree, commas to separate element-indices within a level, and the sign of the index to indicate whether the element satisfied \code{predicate} (- = yes, + = no).

}
}
\author{Alex Zajichek}
\examples{
#Find depths of data frames
df1 <-
  heart_disease \%>\%
  
    #Divide the frame into a list
    divide(
      Sex,
      HeartDisease,
      ChestPain
    )

df1 \%>\%
  
  #Get depths as an integer
  depths(
    predicate = is.data.frame
  )

df1 \%>\%

  #Get full structure
  depths_string(
    predicate = is.data.frame
  )

#Shallower list
df2 <-
  heart_disease \%>\%
    divide(
      Sex,
      HeartDisease,
      ChestPain,
      depth = 1
    ) 

df2 \%>\%
  depths(
    predicate = is.data.frame
  )

df2 \%>\%
  depths_string(
    predicate = is.data.frame
  )

#Allow for non-bare lists to be traversed
df1 \%>\%
  depths(
    predicate = is.factor,
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

my_list \%>\%
  depths_string(
    predicate = is.data.frame
  )

#Go deeper by relaxing bare list argument
my_list \%>\%
  depths_string(
    predicate = is.data.frame,
    bare = FALSE
  )

}  
