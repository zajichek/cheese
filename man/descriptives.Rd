\name{descriptives}
\alias{descriptives}
\title{Compute descriptive statistics on columns of a data frame.}
\description{
Computes a number of common descriptive statistics for different types of data. The user can specify an unlimited number of additional functions to compute and the types of data that each set (including the default) of functions will be applied to. 
}
\usage{
descriptives(
    data,
    f_all = NULL,
    f_numeric = NULL,
    numeric_types = "numeric",
    f_categorical = NULL,
    categorical_types = "factor",
    f_other = NULL,
    na.rm = TRUE,
    useNA = c("ifany", "no", "always"),
    round = 2
)
}
\arguments{
\item{data}{A \code{data.frame}. Could also be a \code{list}.}
\item{f_all}{Functions to apply to all columns. Should return a scalar. See "Details" for information computed by default.}
\item{f_numeric}{Functions to apply to columns conforming to \code{numeric_types}. Should return a scalar. See "Details" for information computed by default.}
\item{numeric_types}{Character vector of data types that should be evaluated with \code{f_numeric}.}
\item{f_categorical}{Functions to apply to columns conforming to \code{categorical_types}. Should return a named vector where the names correpond to the levels. See "Details" for information computed by default.}
\item{categorical_types}{Character vector of data types that should be evaluated with \code{f_categorical}.}
\item{f_other}{Functions to apply to remaining columns.}
\item{na.rm}{Logical argument supplied to \code{f_numeric}. Defaults to \code{TRUE}.}
\item{useNA}{Supplied to \code{f_categorical}. See \code{?base::table} for details. Defaults to \code{"ifany"}.}
\item{round}{Digit to round numeric data. Defaults to \code{2}.}
}

\value{
A long \code{tibble} with columns \code{.variable} (for the variable name), \code{.key} (for the statistic or attribute), \code{.value} (for numeric results), \code{.label} (for non-numeric results), and \code{.combo} (convenient combination of .value and .label coerced to a \code{character} vector). If categorical variables exist, additional columns \code{.level} (for the factor levels) and \code{.order} (to retain order of the levels).
}
        
\details{
The \code{min, max, median, iqr, mean, sd} are automatically computed for numeric data and \code{table, prop.table*100} for categorical data. The sample size, number of missing values, number of nonmissing values, the number of unique values, the \code{class} are automatically computed on all columns. 
}

\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Default
heart_disease \%>\%
    descriptives()

#2) Allow logicals as categorical
heart_disease \%>\%
    descriptives(
        categorical_types = c("logical", "factor")
    )

#3) Only apply "other" functions to numeric types
heart_disease \%>\%
    descriptives(
        numeric_types = NULL
    )

#4) Compute a custom function
heart_disease \%>\%
    descriptives(
        f_numeric = 
            list(
                cv = function(x, na.rm) sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
            )
    )

}