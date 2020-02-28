\name{dish}
\alias{dish}
\title{Evaluate a two-argument function with combinations of columns}
\usage{
dish(
    data,
    f,
    left,
    right,
    each_left = TRUE,
    each_right = TRUE,
    ...
)
}
\arguments{
\item{data}{A \code{data.frame}.}
\item{f}{A \code{function} that takes a \code{vector} and/or \code{data.frame} in the first two arguments.}    
\item{left}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} to be evaluated in the first argument of \code{f}}
\item{right}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} to be evaluated in the second argument of \code{f}}
\item{each_left}{Should each \code{left} variable be indivdually evaluated in \code{f}? Defaults to \code{TRUE}. If \code{FALSE}, \code{left} is entered into \code{f} as a \code{data.frame}.}
\item{each_right}{Should each \code{right} variable be separately evaluated in \code{f}? Defaults to \code{TRUE}. If \code{FALSE}, \code{right} is entered into \code{f} as a \code{data.frame}.}
\item{...}{Additional arguments to be passed to \code{f}.}
}
\value{
A \code{list} 
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#All variables on both sides
heart_disease \%>\%
    select_if(
        is.numeric
    ) \%>\%
    dish(
        f = cor
    )

#Simple regression of each numeric variable on each other variable
heart_disease \%>\%
    dish(
        f =
            function(y, x) {
                mod <- lm(y ~ x)
                tibble(
                    Parameter = names(mod$coef),
                    Estimate = mod$coef
                )
            },
        left = is.numeric
    ) \%>\%
    
    #Bind rows together
    fasten(
        into = c("Outcome", "Predictor")
    )

#Multiple regression of each numeric variable on all others simultaneously
heart_disease \%>\%
    dish(
        f =
            function(y, x) {
                mod <- lm(y ~ ., data = x)
                tibble(
                    Parameter = names(mod$coef),
                    Estimate = mod$coef
                )
            },
        left = is.numeric,
        each_right = FALSE
    ) \%>\%
    
    #Bind rows together
    fasten(
        into = "Outcome"
    )

}