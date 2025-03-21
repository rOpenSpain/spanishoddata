% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/available-data.R
\name{spod_available_data}
\alias{spod_available_data}
\title{Get available data list}
\usage{
spod_available_data(
  ver = 2,
  check_local_files = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir()
)
}
\arguments{
\item{ver}{Integer. Can be 1 or 2. The version of the data to use. v1 spans 2020-2021, v2 covers 2022 and onwards.}

\item{check_local_files}{Whether to check if the local files exist. Defaults to \code{FALSE}.}

\item{quiet}{A \code{logical} value indicating whether to suppress messages. Default is \code{FALSE}.}

\item{data_dir}{The directory where the data is stored. Defaults to the value returned by \code{spod_get_data_dir()}.}
}
\value{
A tibble with links, release dates of files in the data, dates of data coverage, local paths to files, and the download status.
\describe{
\item{target_url}{\code{character}. The URL link to the data file.}
\item{pub_ts}{\code{POSIXct}. The timestamp of when the file was published.}
\item{file_extension}{\code{character}. The file extension of the data file (e.g., 'tar', 'gz').}
\item{data_ym}{\code{Date}. The year and month of the data coverage, if available.}
\item{data_ymd}{\code{Date}. The specific date of the data coverage, if available.}
\item{local_path}{\code{character}. The local file path where the data is stored.}
\item{downloaded}{\code{logical}. Indicator of whether the data file has been downloaded locally. This is only available if \code{check_local_files} is \code{TRUE}.}
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Get a table with links to available data files for the specified data version. Optionally check (see arguments) if certain files have already been downloaded into the cache directory specified with SPANISH_OD_DATA_DIR environment variable (set by \link{spod_set_data_dir}) or a custom path specified with \code{data_dir} argument.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{

# Set data dir for file downloads
spod_set_data_dir(tempdir())

# Get available data list for v1 (2020-2021) data
spod_available_data(ver = 1)

# Get available data list for v2 (2022 onwards) data
spod_available_data(ver = 2)

# Get available data list for v2 (2022 onwards) data
# while also checking for local files that are already downloaded
spod_available_data(ver = 2, check_local_files = TRUE)
}
\dontshow{\}) # examplesIf}
}
