\name{absorb}
\alias{absorb}
\title{Absorb values into a string containing keys}
\description{
Populate string templates containing keys with their values. The keys are interpreted as regular expressions. Results can optionally be evaluated as \code{R} expressions.
}
\usage{
absorb(
    key, 
    value, 
    text, 
    sep = "_",
    trace = FALSE,
    evaluate = FALSE
)
}
\arguments{
    \item{key}{A vector that can be coerced to type \code{base::character}.}
    \item{value}{A vector with the same length as \code{key}.}
    \item{text}{A (optionally named) \code{base::character} vector containing patterns.}
    \item{sep}{Delimiter to separate values by in the placeholder for duplicate patterns. Defaults to \code{"_"}}
    \item{trace}{Should the recursion results be printed to the console each iteration? Defaults to \code{FALSE}.}
    \item{evaluate}{Should the result(s) be evaluated as \code{R} expressions? Defaults to \code{FALSE}.}
}
\details{
The inputs are iterated in sequential order to replace each pattern with its corresponding value. It is possible that a subsequent pattern could match with a prior result, and hence be replaced more than once. If duplicate keys exist, the placeholder will be filled with a collapsed string of all the values for that key.
}
\value{
\itemize{
\item If \code{evaluate = FALSE} (default), a \code{base::character} vector the same length as \code{text} with all matching patterns replaced by their value.
\item Otherwise, a \code{base::list} with the same length as \code{text}.
}
}
\author{Alex Zajichek}
\examples{
#Simple example
absorb(
    key = c("mean", "sd", "var"),
    value = c("10", "2", "4"),
    text = 
        c("MEAN: mean, SD: sd",
          "VAR: var = sd^2",
          MEAN = "mean"
        )
)

#Evaluating results
absorb(
    key = c("mean", "mean", "sd", "var"),
    value = c("10", "20", "2", "4"),
    text = c("(mean)/2", "sd^2"),
    sep = "+",
    trace = TRUE,
    evaluate = TRUE
) \%>\%
    rlang::flatten_dbl()

}