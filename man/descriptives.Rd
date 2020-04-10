\name{descriptives}
\alias{descriptives}
\title{Compute descriptive statistics on columns of a data frame}
\description{
The user can specify an unlimited number of functions to evaluate and the types of data that each set of functions will be applied to (including the default; see "Details").
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
    useNA = c("ifany", "no", "always"),
    round = 2,
    na_string = "(missing)"
)
}
\arguments{
\item{data}{A \code{base::data.frame}.}
\item{f_all}{A \code{base::list} of functions to evaluate on all columns.}
\item{f_numeric}{A \code{base::list} of functions to evaluate on \code{numeric_types} columns.}
\item{numeric_types}{Character vector of data types that should be evaluated by \code{f_numeric}.}
\item{f_categorical}{A \code{base::list} of functions to evaluate on \code{categorical_types} columns.}
\item{categorical_types}{Character vector of data types that should be evaluated by \code{f_categorical}.}
\item{f_other}{A \code{base::list} of functions to evaluate on remaining columns.}
\item{useNA}{See \code{base::table} for details. Defaults to \code{"ifany"}.}
\item{round}{Digit to round numeric data. Defaults to \code{2}.}
\item{na_string}{String to fill in \code{NA} names.}
}
 
\details{
The \code{min, max, median, iqr, mean, sd} are automatically computed for numeric data and \code{table, prop.table*100} for categorical data. The sample size, number of missing values, number of nonmissing values, the number of unique values, the \code{class} are automatically computed on all columns. 
}

\value{
A \code{tibble::tibble} with the following columns:
\itemize{
\item \code{column}: Position/index of the input dataset
\item \code{column_name}: Label of the column
\item \code{type}: Set of column types function was applied to
\item \code{key}: Name of function that was evaluated
\item \code{value_order}: Position of the value within the function result
\item \code{value_name}: Label extracted from the result with \code{base::names}
\item \code{value_num}: Numeric results
\item \code{value_chr}: Non-numeric results
\item \code{value_combo}: Output combined with rounding applied to numeric values
}
}

\author{Alex Zajichek}
\examples{
#Default
heart_disease \%>\%
    descriptives()

#Allow logicals as categorical
heart_disease \%>\%
    descriptives(
        categorical_types = c("logical", "factor")
    )

#Only apply "other" functions to numeric types
heart_disease \%>\%
    descriptives(
        numeric_types = NULL
    )

#Compute a custom function
heart_disease \%>\%
    descriptives(
        f_numeric = 
            list(
                cv = function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
            )
    )

}