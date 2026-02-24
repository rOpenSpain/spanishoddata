#' Get daily trip counts per origin-destionation municipality from 2022 onward
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' **WARNING: this function may stop working at any time, as the API may change**. This function provides a quick way to get daily aggregated (no hourly data) trip counts per origin-destination municipality from v2 data (2022 onward). Compared to \code{\link[=spod_get]{spod_get()}}, which downloads large CSV files, this function downloads the data directly from the GraphQL API. An interactive web map with this data is available at [https://mapas-movilidad.transportes.gob.es/](https://mapas-movilidad.transportes.gob.es/). No data aggregation is performed on your computer (unlike in \code{\link[=spod_get]{spod_get()}}), so you do not need to worry about memory usage and do not have to use a powerful computer with multiple CPU cores just to get this simple data. Only about 1 MB of data is downloaded for a single day. The limitation of this function is that it can only retrieve data for a single day at a time and only with total number of trips and total km travelled. So it is not possible to get any of the extra variables available in the full dataset via \code{\link[=spod_get]{spod_get()}}.
#'
#' For detailed data descriptions, see package vignettes using [`spod_codebook(ver = 1)`][spod_codebook] and [`spod_codebook(ver = 2)`][spod_codebook] and official methodology documents in **References** section.
#'
#' @template references
#'
#' @param date A character or Date object specifying the date for which to retrieve the data. If date is a character, the date must be in "YYYY-MM-DD" or "YYYYMMDD" format.
#' @param min_trips A numeric value specifying the minimum number of journeys per origin-destination pair to retrieve. Defaults to 100 to reduce the amount of data returned. Can be set to 0 to retrieve all data.
#' @param distances A character vector specifying the distances to retrieve. Valid values are "500m-2km", "2-10km", "10-50km", and "50+km". Defaults to `c("500m-2km", "2-10km", "10-50km", "50+km")`. The resulting data will not have number of trips per category of distance. Therefore, if you want to retrieve the number of trips per distance category, you need to make 4 separate calls to this function or use \code{\link[=spod_get]{spod_get()}} instead to get the full data from source CSV files.
#' @param id_origin A character vector specifying the origin municipalities to retrieve. If not provided, all origin municipalities will be included. Valid municipality IDs can be found in the dataset returned by `spod_get_zones(zones = "muni", ver = 2)`.
#' @param id_destination A character vector specifying the target municipalities to retrieve. If not provided, all target municipalities will be included. Valid municipality IDs can be found in the dataset returned by `spod_get_zones(zones = "muni", ver = 2)`.
#' @return A `tibble` containing the flows for the specified date, minimum number of journeys, distances and origin-destination pairs if specified. The columns are:
#' \describe{
#'   \item{date}{The date of the trips.}
#'   \item{id_origin}{The origin municipality ID.}
#'   \item{id_destination}{The target municipality ID.}
#'   \item{n_trips}{The number of trips between the origin and target municipality.}
#'   \item{trips_total_length_km}{The total length of trips in kilometers.}
#' }
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examplesIf interactive()
#' \donttest{
#' od_1000 <- spod_quick_get_od(
#'   date = "2022-01-01",
#'   min_trips = 1000
#' )
#' }
spod_quick_get_od <- function(
  date = NA,
  min_trips = 100,
  distances = c("500m-2km", "2-10km", "10-50km", "50+km"),
  id_origin = NA,
  id_destination = NA
) {
  # Validate inputs
  checkmate::assert_integerish(
    min_trips,
    lower = 0,
    null.ok = FALSE,
    max.len = 1
  )

  # Mapping user-friendly distances to GraphQL expected values
  distance_mapping <- c(
    "500m-2km" = "D_05_2",
    "2-10km" = "D_2_10",
    "10-50km" = "D_10_50",
    "50+km" = "D_50"
  )
  graphql_distances <- unname(distance_mapping[distances])
  if (any(is.na(graphql_distances))) {
    stop(
      "Invalid distance value. Allowed values are: ",
      paste(names(distance_mapping), collapse = ", ")
    )
  }

  checkmate::assert_character(id_origin, null.ok = TRUE)
  checkmate::assert_character(id_destination, null.ok = TRUE)

  # Convert the date into YYYYMMDD format
  if (is.character(date)) {
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      date <- as.Date(date)
    } else if (nchar(date) == 8 && grepl("^\\d{8}$", date)) {
      date <- as.Date(date, format = "%Y%m%d")
    } else {
      stop(
        "Invalid date format. Use 'YYYY-MM-DD', 'YYYYMMDD', or a Date object."
      )
    }
  }

  if (inherits(date, "Date")) {
    date_fmt <- format(date, "%Y%m%d")
  } else {
    stop(
      "Invalid date input. Must be a character in 'YYYY-MM-DD'/'YYYYMMDD' format or a Date object."
    )
  }

  muni_ref <- readRDS(
    system.file("extdata", "muni_v2_ref.rds", package = "spanishoddata")
  )
  validate_muni_ids <- function(ids, muni_ref) {
    if (is.null(ids) || length(ids) == 0 || all(is.na(ids))) {
      return(TRUE)
    }
    invalid <- setdiff(ids, muni_ref$id)
    if (length(invalid) > 0) {
      stop(
        "Invalid municipality IDs: ",
        paste(invalid, collapse = ", "),
        ". Use `spod_get_zones(zones='muni', ver=2)` for valid IDs."
      )
    }
    TRUE
  }
  if (!all(is.na(id_origin))) {
    validate_muni_ids(id_origin, muni_ref)
  }
  if (!all(is.na(id_destination))) {
    validate_muni_ids(id_destination, muni_ref)
  }

  # Check date is within API-supported range
  valid_dates <- spod_graphql_valid_dates_memoised()
  if (!lubridate::ymd(date_fmt) %in% valid_dates) {
    stop(
      "Invalid date. Must be within valid range: ",
      paste(spod_convert_dates_to_ranges(valid_dates), collapse = ", ")
    )
  }

  # build the GraphQL query
  full_query <- paste0(
    "query ($journeyDate:String!,$distances:[JourneysDistancesBasic!],",
    "$originMunicipality:[String!],$destinationMunicipality:[String!],",
    "$minJourneys:Float!){",
    "journeys_municipality_find_by_criteria_basic(criteria:{",
    "journeyDate:$journeyDate,",
    "distances:$distances,",
    "originMunicipality:$originMunicipality,",
    "destinationMunicipality:$destinationMunicipality,",
    "minJourneys:$minJourneys",
    "}){origin destination journeys journeysKm}}"
  )

  vars_list <- list(
    journeyDate = date_fmt,
    distances = graphql_distances,
    minJourneys = min_trips
  )

  if (!all(is.na(id_origin))) {
    vars_list$originMunicipality <- id_origin
  }

  if (!all(is.na(id_destination))) {
    vars_list$destinationMunicipality <- id_destination
  }

  # assemble final payload
  graphql_query <- list(
    query = full_query,
    variables = vars_list
  )

  # Query the API
  od <- spod_query_od_memoised(
    date_fmt = date_fmt,
    graphql_distances = graphql_distances,
    id_origin = id_origin,
    id_destination = id_destination,
    min_trips = min_trips,
    graphql_query = graphql_query
  )

  return(od)
}

#' Internal function to query the GraphQL API for origin-destination data
#' @inheritParams spod_quick_get_od
#' @return A `tibble` containing the flows for the specified date, minimum number of journeys, distances and origin-destination pairs if specified.
#' @keywords internal
spod_query_od_raw <- function(
  date_fmt,
  graphql_distances,
  id_origin,
  id_destination,
  min_trips,
  graphql_query
) {
  # in-memory session token
  session_token_and_signature <- spod_session_token_and_signature()
  x_session_token <- session_token_and_signature$x_session_token
  x_signature <- session_token_and_signature$x_signature

  graphql_endpoint <- getOption("spanishoddata.graphql_api_endpoint")
  req <- httr2::request(graphql_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("spanishoddata.user_agent"),
      "X-Session-Token" = x_session_token,
      "X-Signature" = x_signature
    ) |>
    httr2::req_body_json(graphql_query) |>
    httr2::req_progress(type = "down")
  # req |> httr2::req_dry_run()
  resp <- req |>
    spod_httr2_req_perform() |>
    spod_httr2_resp_body_json(simplifyVector = TRUE)

  # Handle empty data
  if (length(resp$data[[1]]) == 0) {
    stop(
      "No data for ",
      date_fmt,
      ". The server reports the date as valid but returns no records."
    )
  }

  # Tidy and return
  od <- tibble::as_tibble(resp$data[[1]]) |>
    dplyr::select(
      id_origin = "origin",
      id_destination = "destination",
      n_trips = "journeys",
      trips_total_length_km = "journeysKm"
    ) |>
    dplyr::mutate(date = lubridate::ymd(date_fmt)) |>
    dplyr::relocate("date", .before = id_origin)

  return(od)
}

#' Cache the spod_query_od_raw function to avoid repeated requests
#' @importFrom memoise memoise
#' @keywords internal
spod_query_od_memoised <- memoise::memoise(spod_query_od_raw)

#' Get the municipalities geometries
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function fetches the municipalities (for now this is the  only option) geometries from the mapas-movilidad website and returns a `sf` object with the municipalities geometries. This is intended for use with the flows data retrieved by the \code{\link[=spod_quick_get_od]{spod_quick_get_od()}} function. An interactive web map with this data is available at [https://mapas-movilidad.transportes.gob.es/](https://mapas-movilidad.transportes.gob.es/). These municipality geometries only include Spanish municipalities (and not the NUTS3 regions in Portugal and France) and do not contain extra columns that you can get with the \code{\link[=spod_get_zones]{spod_get_zones()}} function. The function caches the retrieved geometries in memory of the current R session to reduce the number of requests to the mapas-movilidad website.
#'
#' For detailed zone definitions and methodology, see
#' \insertRef{mitma_methodology_2020_v3}{spanishoddata} for v1 data and
#' \insertRef{mitms_methodology_2022_v8}{spanishoddata} for v2 data.
#'
#' @references
#' \insertRef{mitms_mobility_web}{spanishoddata}
#'
#' @param zones A character string specifying the zones to retrieve. Valid values are "municipalities", "muni", "municip", and "municipios". Defaults to "municipalities".
#' @return A `sf` object with the municipalities geometries to match with the data retrieved with \code{\link[=spod_quick_get_od]{spod_quick_get_od()}}.
#'
#' @export
#' @examplesIf interactive()
#' \donttest{
#' municipalities_sf <- spod_quick_get_zones()
#' }
#'
spod_quick_get_zones <- function(
  zones = "municipalities"
) {
  # Validate inputs
  checkmate::assert_choice(
    zones,
    choices = c(
      "municipalities",
      "muni",
      "municip",
      "municipios"
    )
  )
  zones <- spod_zone_names_en2es(zones)
  if (zones != "municipios") {
    stop("Only municipalities are available for now.")
  }
  municipalities_sf <- spod_fetch_municipalities_json_memoised()
  return(municipalities_sf)
}

#' Cache the municipalities geometries from the mapas-movilidad website
#' @importFrom memoise memoise
#' @keywords internal
spod_fetch_municipalities_json_memoised <- memoise::memoise(
  function() {
    municip_geometries_url <- "https://mapas-movilidad.transportes.gob.es/api/static/data/municipios60.json"
    municipalities_sf <- spod_sf_st_read(municip_geometries_url, quiet = TRUE) |>
      dplyr::rename(id = "ID") |>
      dplyr::mutate(
        population = as.numeric(dplyr::na_if(.data$population, "NA"))
      ) |>
      dplyr::select("id", "name", "population")
    return(municipalities_sf)
  }
)


#' Internal wrappers for httr2 and sf calls to enable mocking
#' @keywords internal
spod_httr2_req_perform <- function(req) {
  httr2::req_perform(req)
}

#' @keywords internal
spod_httr2_resp_body_string <- function(resp) {
  httr2::resp_body_string(resp)
}

#' @keywords internal
spod_httr2_resp_body_json <- function(resp, ...) {
  httr2::resp_body_json(resp, ...)
}

#' @keywords internal
spod_sf_st_read <- function(dsn, ...) {
  sf::st_read(dsn, ...)
}


#' Get the HMAC secret from the mapas-movilidad website
#' @param base_url The base URL of the mapas-movilidad website
#' @return Character vector with the HMAC secret.
#' @keywords internal
spod_get_hmac_secret <- function(
  base_url = "https://mapas-movilidad.transportes.gob.es"
) {
  # Fetch the homepage HTML
  homepage <- httr2::request(base_url) |>
    spod_httr2_req_perform() |>
    spod_httr2_resp_body_string()

  # Parse and find the inline <script> that mentions import_meta_env
  doc <- xml2::read_html(homepage)
  scripts <- xml2::xml_find_all(doc, "//script[not(@src)]")
  inline <- scripts[stringr::str_detect(
    xml2::xml_text(scripts),
    "import_meta_env"
  )]
  if (length(inline) != 1) {
    stop("Could not uniquely locate the import_meta_env script block.")
  }
  txt <- xml2::xml_text(inline)

  # Extract the real HMAC_SECRET from that block
  secret <- stringr::str_match(txt, '"HMAC_SECRET"\\s*:\\s*"([^"]+)"')[, 2]

  if (is.na(secret) || nchar(secret) < 10) {
    stop("Failed to parse HMAC_SECRET from the page.")
  }
  secret
}

#' Cache the HMAC secret to avoid repeated requests
#' @importFrom memoise memoise
#' @keywords internal
spod_get_hmac_secret_memoised <- memoise::memoise(spod_get_hmac_secret)
