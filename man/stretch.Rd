\name{stretch}
\alias{stretch}
\title{Span keys and values across the columns}
\description{
Select one or more key columns to be spread across the columns for one or value columns
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
\item{data}{A \code{base::data.frame}.}
\item{key}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} whose values will become the column name(s).}
\item{value}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers} whose values will be spread across the columns.}
\item{sep}{String to separate keys/values by in the resulting column names. Defaults to "_". Only used when there are more than one keys or more than one values.}
}
\value{
A \code{tibble::tibble} 
}
\author{Alex Zajichek}
\examples{

data <- 
  heart_disease \%>\%
  dplyr::group_by(
    Sex,
    BloodSugar,
    HeartDisease
  ) \%>\%
  dplyr::summarise(
    Mean = mean(Age),
    SD = sd(Age)
  ) \%>\%
  dplyr::ungroup() \%>\%
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

}