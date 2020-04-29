\name{univariate_associations}
\alias{univariate_associations}
\title{Compute association statistics between columns of a data frame}
\description{
Evaluate a \code{\link{list}} of scalar functions on any number of "response" columns by any number of "predictor" columns
}
\usage{
univariate_associations(
    data,
    f,
    responses,
    predictors
)
}
\arguments{
\item{data}{A \code{\link{data.frame}}.}
\item{f}{A function or a \code{\link{list}} of functions (preferably named) that take a vector as input in the first two arguments and return a scalar.}
\item{responses}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} to be evaluated as the first argument. See the \code{left} argument in \code{\link{dish}}.}
\item{predictors}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} to be evaluated as the second argument. See the \code{right} argument in \code{\link{dish}}.}
}
 
\value{
A \code{tibble::tibble} with the response/predictor columns down the rows and the results of the \code{f} across the columns. The names of the result columns will be the names provided in \code{f}.
}

\author{Alex Zajichek}
\examples{
#Make a list of functions to evaluate
f <-
  list(
    
    #Compute a univariate p-value
    `P-value` =
      function(y, x) {
        if(some_type(x, c("factor", "character"))) {
          
          p <- fisher.test(factor(y), factor(x), simulate.p.value = TRUE)$p.value
          
        } else {
          
          p <- kruskal.test(x, factor(y))$p.value
          
        }
        
        ifelse(p < 0.001, "<0.001", as.character(round(p, 2)))
        
      },
    
    #Compute difference in AIC model between null model and one predictor model
    `AIC Difference` =
      function(y, x) {
        
        glm(factor(y)~1, family = "binomial")$aic -
          glm(factor(y)~x, family = "binomial")$aic
        
      }
  )

#Choose a couple binary outcomes
heart_disease \%>\% 
  univariate_associations(
    f = f,
    responses = c(ExerciseInducedAngina, HeartDisease)
  )

#Use a subset of predictors
heart_disease \%>\% 
  univariate_associations(
    f = f,
    responses = c(ExerciseInducedAngina, HeartDisease),
    predictors = c(Age, BP)
  )

#Numeric predictors only
heart_disease \%>\% 
  univariate_associations(
    f = f,
    responses = c(ExerciseInducedAngina, HeartDisease),
    predictors = is.numeric
  )

}