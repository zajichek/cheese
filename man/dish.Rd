\name{dish}
\alias{dish}
\title{Dish out a function to combinations of variables}
\description{
Evaluates a two argument function on subsets of a data frame by evaluating each combination of columns or subsets.
}
\usage{
dish(
    data,
    f,
    left = NULL,
    right = NULL,
    each_left = TRUE,
    each_right = TRUE,
    bind = FALSE,
    ...
)
}
\arguments{
\item{data}{A \code{data.frame}.}
\item{f}{Any \code{function} that takes a \code{vector} and/or \code{data.frame} in the first two arguments.}    
\item{left}{Variables to be used in the first argument of \code{f}. If \code{NULL} (default), all variables except those in \code{right} are used. Has \code{tidyselect::vars_select} capabilities.}
\item{right}{Variables to be used in the second argument of \code{f}. If \code{NULL} (default), all variables except those in \code{left} are used. Has \code{tidyselect::vars_select} capabilities.}
\item{each_left}{Should each \code{left} variable be separately evaluated in \code{f}? Defaults to \code{TRUE}. If \code{FALSE}, \code{left} is entered into \code{f} as a \code{data.frame}.}
\item{each_right}{Should each \code{right} variable be separately evaluated in \code{f}? Defaults to \code{TRUE}. If \code{FALSE}, \code{right} is entered into \code{f} as a \code{data.frame}.}
\item{bind}{Should results be binded into a single \code{data.frame}? Defaults to \code{FALSE}.}
\item{...}{Additional arguments to be passed to \code{f}.}
}
\value{
A \code{list} (if \code{bind = FALSE}) or a \code{tibble} (if \code{bind = TRUE}) with the results of \code{f} evaluated on data subsets.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Default uses every variable on both sides
heart_disease \%>\%
    select_if(
        is.numeric
    ) \%>\%
    dish(
        f = cor
    )

#2) Simple regression of Age and BP on each variable
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
        left = c("Age", "BP"),
        bind = TRUE
    )

#3) Multiple regression with Age, BP on all variables simultaneously
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
        left = c("Age", "BP"),
        each_right = FALSE,
        bind = TRUE
    )

}