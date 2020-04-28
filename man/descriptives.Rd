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
\item{data}{A \code{\link{data.frame}}.}
\item{f_all}{A \code{\link{list}} of functions to evaluate on all columns.}
\item{f_numeric}{A \code{\link{list}} of functions to evaluate on \code{numeric_types} columns.}
\item{numeric_types}{Character vector of data types that should be evaluated by \code{f_numeric}.}
\item{f_categorical}{A \code{\link{list}} of functions to evaluate on \code{categorical_types} columns.}
\item{categorical_types}{Character vector of data types that should be evaluated by \code{f_categorical}.}
\item{f_other}{A \code{\link{list}} of functions to evaluate on remaining columns.}
\item{useNA}{See \code{\link{table}} for details. Defaults to \code{"ifany"}.}
\item{round}{Digit to round numeric data. Defaults to \code{2}.}
\item{na_string}{String to fill in \code{NA} names.}
}
 
\details{
The following \code{fun_key}'s are available by default for the specified types:
\itemize{
\item ALL: \code{\link{length}, missing, available, \link{class}, \link{unique}}
\item Numeric: \code{\link{mean}, \link{sd}, \link{min}, q1, \link{median}, q3, \link{max}, iqr, range}
\item Categorical: \code{count, proportion, percent}
}
}

\value{
A \code{\link{tibble::tibble}} with the following columns:
\itemize{
\item \code{fun_eval}: Column types function was applied to
\item \code{fun_key}: Name of function that was evaluated
\item \code{col_ind}: Index from input dataset
\item \code{col_lab}: Label of the column
\item \code{val_ind}: Index of the value within the function result
\item \code{val_lab}: Label extracted from the result with \code{\link{names}}
\item \code{val_dbl}: Numeric result
\item \code{val_chr}: Non-numeric result
\item \code{val_cbn}: Combination of (rounded) numeric and non-numeric values
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
    ) \%>\%
    
    #Extract info from the column
    dplyr::filter(
        col_lab == "BloodSugar"
    ) 

#Nothing treated as numeric
heart_disease \%>\%
    descriptives(
        numeric_types = NULL
    )

#Evaluate a custom function
heart_disease \%>\%
    descriptives(
        f_numeric = 
            list(
                cv = function(x) sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
            )
    ) \%>\%
    
    #Extract info from the custom function
    dplyr::filter(
        fun_key == "cv"
    ) 

}