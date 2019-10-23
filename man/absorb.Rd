\name{absorb}
\alias{absorb}
\title{Absorb values of a key-value pair into an arbitrary character string containing keys}
\description{
Populates user-specified string templates containing keys as placeholders, with the values. The keys are interpreted as regular expressions. Results can optionally be evaluated as \code{R} expressions.
}
\usage{
absorb(
    key, 
    value, 
    text, 
    sep = "|",
    print = FALSE,
    evaluate = FALSE
)
}
\arguments{
    \item{key}{Vector of keys that can be coerced to type \code{character}.}
    \item{value}{Vector of values with positions corresponding to the \code{key}.}
    \item{text}{Vector of character strings containing sequences of characters and keys/patterns where values should be filled.}
    \item{sep}{Character to separate values by in the placeholder in the event of duplicate keys (patterns). Defaults to \code{"|"}}
    \item{print}{Should the recursion results be printed to the console each iteration? Defaults to \code{FALSE}.}
    \item{evaluate}{Should the resulting strings be evaluated as \code{R} expressions? Defaults to \code{FALSE}.}
}
\details{
The algorithm iterates the provided \code{value} vector (in sequential order) and recursivley replaces substrings where there is a matching pattern of a \code{key}. Thus, it is possible that a subsequent key could match with a previous value, and hence be replaced more than once. If duplicate keys exist, the placeholder will be filled with a collapsed string of all the values for that key.
}
\value{
    If \code{evaluate = FALSE} (default), a character vector the same length as \code{text} with all matching patterns replaced by their value. If \code{evaluate = TRUE}, a list with the same length as \code{text} is returned with the result of the evaluation of each string.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Simple example
absorb(
    key = c("mean", "sd", "var"),
    value = c("10", "2", "4"),
    text = 
        c("MEAN: mean, SD: sd",
          "VAR: var = sd^2",
          MEAN = "mean"
        )
)

#2) Evaluating results
absorb(
    key = c("mean", "mean", "sd", "var"),
    value = c("10", "20", "2", "4"),
    text = c("(mean)/2", "sd^2"),
    sep = "+",
    evaluate = TRUE
) \%>\%
    flatten_dbl()


}