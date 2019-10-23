\name{stretch}
\alias{stretch}
\title{Stretch one or variables over many columns by one or more keys}
\description{
Provides similar functionality to \code{tidyr::spread} or \code{reshape2::dcast} but allows for an unlimited number of variables to be spanned across the columns. 
}
\usage{
stretch(
    data,
    keys,
    keep = NULL,
    send = NULL,
    join = dplyr::full_join,
    .sep = "_",
    extract_keys_as_header = FALSE,
    keep_keys_in_header = TRUE,
    ...
)
}
\arguments{
\item{data}{A \code{data.frame} object.}
\item{keys}{A vector of column names or positions, or a \code{tidyselect::vars_select} helper.}
\item{keep}{Variables to remain as their own columns. If \code{NULL} (default), all variables except those in \code{send} are chosen, excluding the \code{keys}. Has \code{tidyselect::vars_select} capabilities.}
\item{send}{Variables to send into new columns for each key. If \code{NULL} (default), all variables except those in \code{keep} are chosen, excluding the \code{keys}. Has \code{tidyselect::vars_select} capabilities.}
\item{join}{Function that joins on \code{keep} across the \code{keys}. Defaults to \code{dplyr::full_join}. See \code{?dplyr::join} for choices.}
\item{.sep}{Character to separate and identify \code{keys} values over the columns. Defaults to "_".}
\item{extract_keys_as_header}{Should the \code{keys} labels be returned as a separate character vector? Defaults to \code{FALSE}. Has no effect when there is only 1 \code{send} column.}
\item{keep_keys_in_header}{If \code{extract_keys_as_header = TRUE}, should the keys be left in the result columns? Defaults to \code{TRUE}. Useful to set to \code{FALSE} if a call to \code{knitr::kable} follows.} 
\item{...}{Additional arguments to be passed to \code{join}.}
}
\value{
A wide \code{data.frame} with variables spread over the columns. If \code{extract_keys_as_header = TRUE}, the result is a two-element \code{list} with the transformed data in 1 element and the top-level header in the other. If only 1 \code{send} variable exists, the original names are removed.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#Make data frame with multiple summary columns 
temp_summary <-
    
    heart_disease \%>\%
    group_by(
        Sex,
        HeartDisease,
        BloodSugar
    ) \%>\%
    summarise(
        Mean = mean(Age, na.rm = TRUE),
        SD = sd(Age, na.rm = TRUE),
        Median = median(Age, na.rm = TRUE)
    ) \%>\%
    ungroup() 

#1) Span summaries for each combination of Sex and BloodSugar
temp_summary \%>\%
    stretch(
        keys = c("Sex", "BloodSugar"),
        keep = "HeartDisease"
    )

#2) If "HeartDisease" wasn't fully crossed, use different joining to get only matching groups
temp_summary \%>\%
    stretch(
        keys = c("Sex", "BloodSugar"),
        keep = "HeartDisease",
        join = inner_join
    )

#3) Only send two of the summaries
temp_summary \%>\%
    stretch(
        keys = c("Sex", "BloodSugar"),
        keep = "HeartDisease",
        send = c("Mean", "Median")
    )

#4) Clean HTML table with keys spanned over columns
result <- 
    temp_summary \%>\%
    stretch(
        keys = c("Sex", "BloodSugar"),
        keep = "HeartDisease",
        extract_keys_as_header = TRUE,
        keep_keys_in_header = FALSE
    )
result$.result \%>\%
    knitr::kable(format = "html") \%>\%
    kableExtra::kable_styling() \%>\%
    kableExtra::add_header_above(
        kableExtra::auto_index(result$.header)
    )
    
}