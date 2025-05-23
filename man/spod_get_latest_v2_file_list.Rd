% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/available-data.R
\name{spod_get_latest_v2_file_list}
\alias{spod_get_latest_v2_file_list}
\title{Get latest file list from the XML for MITMA open mobility data v2 (2022 onwards)}
\usage{
spod_get_latest_v2_file_list(
  data_dir = spod_get_data_dir(),
  xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
)
}
\arguments{
\item{data_dir}{The directory where the data is stored. Defaults to the value returned by \code{spod_get_data_dir()}.}

\item{xml_url}{The URL of the XML file to download. Defaults to "https://movilidad-opendata.mitma.es/RSS.xml".}
}
\value{
The path to the downloaded XML file.
}
\description{
Get latest file list from the XML for MITMA open mobility data v2 (2022 onwards)
}
\keyword{internal}
