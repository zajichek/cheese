\name{stretch}
\alias{stretch}
\title{Span keys and values across the columns}
\description{
Pivot one or more values across the columns by one or more keys
}
\usage{
stretch(
    data,
    key,
    value,
    sep = "_"
)
}
\arguments{
\item{data}{A \code{\link{data.frame}}.}
\item{key}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} whose values will become the column name(s).}
\item{value}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} whose values will be spread across the columns.}
\item{sep}{String to separate keys/values by in the resulting column names. Defaults to \code{"_"}. Only used when there are more than one keys/values.}
}
\details{
In the case of multiple \code{value}'s, the labels are always appended to the end of the resulting columns.
}
\value{
A \code{tibble::tibble}
}
\author{Alex Zajichek}
\examples{

#Make a summary table
set.seed(123)
data <- 
  heart_disease \%>\%
  dplyr::group_by(
    Sex,
    BloodSugar,
    HeartDisease
  ) \%>\%
  dplyr::summarise(
    Mean = mean(Age),
    SD = sd(Age),
    .groups = "drop"
  ) \%>\%
  dplyr::mutate(
    Random =
      rbinom(nrow(.), size = 1, prob = .5) \%>\%
      factor
  )

data \%>\%
  stretch(
    key = c(BloodSugar, HeartDisease),
    value = c(Mean, SD, Random)
  )

data \%>\%
  stretch(
    key = where(is.factor),
    value = where(is.numeric)
  )

data \%>\%
  stretch(
    key = c(where(is.factor), where(is.logical)),
    value = where(is.numeric)
  )

}