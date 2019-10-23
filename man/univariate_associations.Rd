\name{univariate_associations}
\alias{univariate_associations}
\title{Compute association metrics between any number of variables}
\description{
Evaluates a list of scalar functions on any number of "responses" for each individual variable and gathers results.
}
\usage{
univariate_associations(
    data,
    f,
    responses = NULL,
    predictors = NULL
)
}
\arguments{
\item{data}{Any \code{data.frame}.}
\item{f}{A \code{function} or a \code{list} of functions (prefereably named) that take a vector as input in the first two arguments and return an \code{atomic} scalar.}
\item{responses}{Response or outcome variables. See \code{"\link{dish}"}.}
\item{predictors}{Predictors or covariates. See \code{"\link{dish}"}.}
}

\value{
A \code{tibble} with the variables in the rows and the results of the functions in the columns. The names of the columns will be the names provided in \code{f}.
}

\author{Alex Zajichek}
\examples{
require(tidyverse)

#Make a list of functions
f <-
    list(
        
        #Compute a univariate p-value
        `P-value` =
            
            function(x, y) {
                
                if(type_match(y, c("factor", "character"))) {
                    
                    p <- fisher.test(factor(x), factor(y), simulate.p.value = TRUE)$p.value
                    
                    
                } else {
                    
                    p <- kruskal.test(y, factor(x))$p.value
                    
                }
                if_else(
                    p < 0.001, "<0.001", as.character(round(p, 2))
                )
            },
        
        #Compute difference in AIC model between null model and one predictor model
        `AIC Difference` =
            function(x, y) {
                
                glm(factor(x)~1, family = "binomial")$aic - 
                glm(factor(x)~y, family = "binomial")$aic
                
            }
    )

#1) Apply functions to Sex/HeartDisease by all other variables
heart_disease \%>\%
    univariate_associations(
        f = f,
        responses = c("Sex", "HeartDisease")
    )

#2) Only use two variables on RHS
heart_disease \%>\%
    univariate_associations(
        f = f,
        responses = c("Sex", "HeartDisease"),
        predictors = c("Age", "ChestPain")
    )

#3) Use all combinations
heart_disease \%>\%
    univariate_associations(
        f = f
    )

}