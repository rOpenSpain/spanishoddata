% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal-utils.R
\name{spod_is_data_version_overlaps}
\alias{spod_is_data_version_overlaps}
\title{Check if specified dates span both data versions}
\usage{
spod_is_data_version_overlaps(dates)
}
\arguments{
\item{dates}{A \code{Dates} vector of dates to check.}
}
\value{
\code{TRUE} if the dates span both data versions, \code{FALSE} otherwise.
}
\description{
This function checks if the specified dates or date ranges span both v1 and v2 data versions.
}
\keyword{internal}
