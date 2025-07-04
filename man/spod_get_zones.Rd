% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-zones.R
\name{spod_get_zones}
\alias{spod_get_zones}
\title{Get zones}
\usage{
spod_get_zones(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  ver = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE
)
}
\arguments{
\item{zones}{The zones for which to download the data. Can be \code{"districts"} (or \code{"dist"}, \code{"distr"}, or the original Spanish \code{"distritos"}) or \code{"municipalities"} (or \code{"muni"}, \code{"municip"}, or the original Spanish \code{"municipios"}) for both data versions. Additionaly, these can be \code{"large_urban_areas"} (or \code{"lua"}, or the original Spanish \code{"grandes_areas_urbanas"}, or \code{"gau"}) for v2 data (2022 onwards).}

\item{ver}{Integer. Can be 1 or 2. The version of the data to use. v1 spans 2020-2021, v2 covers 2022 and onwards. See more details in codebooks with \code{\link[=spod_codebook]{spod_codebook()}}.}

\item{data_dir}{The directory where the data is stored. Defaults to the value returned by \code{spod_get_data_dir()} which returns the value of the environment variable \code{SPANISH_OD_DATA_DIR} or a temporary directory if the variable is not set. To set the data directory, use \link{spod_set_data_dir}.}

\item{quiet}{A \code{logical} value indicating whether to suppress messages. Default is \code{FALSE}.}
}
\value{
An \code{sf} object (Simple Feature collection).

The columns for v1 (2020-2021) data include:
\describe{
\item{id}{A character vector containing the unique identifier for each district, assigned by the data provider. This \code{id} matches the \code{id_origin}, \code{id_destination}, and \code{id} in district-level origin-destination and number of trips data.}
\item{census_districts}{A string with semicolon-separated identifiers of census districts classified by the Spanish Statistical Office (INE) that are spatially bound within the polygons for each \code{id}.}
\item{municipalities_mitma}{A string with semicolon-separated municipality identifiers (as assigned by the data provider) corresponding to each district \code{id}.}
\item{municipalities}{A string with semicolon-separated municipality identifiers classified by the Spanish Statistical Office (INE) corresponding to each \code{id}.}
\item{district_names_in_v2/municipality_names_in_v2}{A string with semicolon-separated district names (from the v2 version of this data) corresponding to each district \code{id} in v1.}
\item{district_ids_in_v2/municipality_ids_in_v2}{A string with semicolon-separated district identifiers (from the v2 version of this data) corresponding to each district \code{id} in v1.}
\item{geometry}{A \code{MULTIPOLYGON} column containing the spatial geometry of each district, stored as an sf object. The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
}

The columns for v2 (2022 onwards) data include:
\describe{
\item{id}{A character vector containing the unique identifier for each zone, assigned by the data provider.}
\item{name}{A character vector with the name of each district.}
\item{population}{A numeric vector representing the population of each district (as of 2022).}
\item{census_sections}{A string with semicolon-separated identifiers of census sections corresponding to each district.}
\item{census_districts}{A string with semicolon-separated identifiers of census districts as classified by the Spanish Statistical Office (INE) corresponding to each district.}
\item{municipalities}{A string with semicolon-separated identifiers of municipalities classified by the Spanish Statistical Office (INE) corresponding to each district.}
\item{municipalities_mitma}{A string with semicolon-separated identifiers of municipalities, as assigned by the data provider, that correspond to each district.}
\item{luas_mitma}{A string with semicolon-separated identifiers of LUAs (Local Urban Areas) from the provider, associated with each district.}
\item{district_ids_in_v1/municipality_ids_in_v1}{A string with semicolon-separated district identifiers from v1 data corresponding to each district in v2. If no match exists, it is marked as \code{NA}.}
\item{geometry}{A \code{MULTIPOLYGON} column containing the spatial geometry of each district, stored as an sf object. The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Get spatial zones for the specified data version. Supports both v1 (2020-2021) and v2 (2022 onwards) data.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
# get polygons for municipalities for the v2 data
municip_v2 <- spod_get_zones(zones = "municipalities", ver = 2)

# get polygons for the districts for the v1 data
distr_v1 <- spod_get_zones(zones = "districts", ver = 1)
}
\dontshow{\}) # examplesIf}
}
