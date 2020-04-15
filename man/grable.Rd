\name{grable}
\alias{grable}
\title{Make a \code{kable} with a hierarchical header}
\description{
Create a \code{knitr::kable} with a multi-layered (graded) header.
}
\usage{
grable(
    data,
    at,
    sep = "_",
    reverse = FALSE,
    format = c("html", "latex"),
    caption = NULL,
    ...
)
}
\arguments{
\item{data}{A \code{base::data.frame}.}
\item{at}{A vector of quoted/unquoted columns, positions, and/or \code{tidyselect::select_helpers}. Defaults to all columns.}
\item{sep}{String to separate the columns. Defaults to "_".}
\item{reverse}{Should the layers be added in the opposite direction? Defaults to \code{FALSE}.}
\item{format}{Format for rendering the table. Must be "html" (default) or "latex".}
\item{caption}{Optional caption for the table}
\item{...}{Arguments to pass to \code{kableExtra::kable_styling}}
}
\value{
A \code{knitr::kable}
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
    SD = sd(Age)
  ) \%>\%
  dplyr::ungroup() \%>\%
  dplyr::mutate(
    Random =
      rbinom(nrow(.), size = 1, prob = .5) \%>\%
      factor
  ) \%>\%
  stretch(
    key = HeartDisease,
    value = c(Mean, SD, Random)
  )

data \%>\%
  grable(
    at =
      c(
        -Sex,
        -BloodSugar
      )
  )

data \%>\%
  grable(
    at =
      c(
        -Sex,
        -BloodSugar
      ),
    format = "latex"
  )

}