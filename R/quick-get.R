#' Get daily trip counts per origin-destionation municipality from 2022 onward
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' **WARNING: this function may stop working at any time, as the API may change**. This function provides a quick way to get daily aggregated (no hourly data) trip counts per origin-destination municipality from v2 data (2022 onward). Compared to \link[spanishoddata]{spod_get}, which downloads large CSV files, this function downloads the data directly from the GraphQL API. An interactive web map with this data is available at [https://mapas-movilidad.transportes.gob.es/](https://mapas-movilidad.transportes.gob.es/) No data aggregation is performed on your computer (unlike in \link[spanishoddata]{spod_get}), so you do not need to worry about memory usage and do not have to use a powerful computer with multiple CPU cores just to get this simple data. Only about 1 MB of data is downloaded for a single day. The limitation of this function is that it can only retrieve data for a single day at a time and only with total number of trips and total km travelled. So it is not possible to get any of the extra variables available in the full dataset via \link[spanishoddata]{spod_get}.
#'
#' @param date A character or Date object specifying the date for which to retrieve the data. If date is a character, the date must be in "YYYY-MM-DD" or "YYYYMMDD" format.
#' @param min_trips A numeric value specifying the minimum number of journeys per origin-destination pair to retrieve. Defaults to 100 to reduce the amount of data returned. Can be set to 0 to retrieve all data.
#' @param distances A character vector specifying the distances to retrieve. Valid values are "500m-2km", "2-10km", "10-50km", and "50+km". Defaults to `c("500m-2km", "2-10km", "10-50km", "50+km")`. The resulting data will not have number of trips per category of distance. Therefore, if you want to retrieve the number of trips per distance category, you need to make 4 separate calls to this function or use `spod_get()` instead to get the full data from source CSV files.
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

  # Helper to convert date vectors into consecutive ranges
  convert_to_ranges <- function(dates) {
    dates <- as.Date(dates)
    ranges <- tibble::tibble(date = dates) |>
      dplyr::arrange(date) |>
      dplyr::mutate(
        diff = c(0, diff(date)),
        group = cumsum(diff != 1)
      ) |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(
        start = first(date),
        end = last(date),
        .groups = "drop"
      ) |>
      dplyr::mutate(range = paste(start, "to", end)) |>
      dplyr::pull(range)
    return(ranges)
  }

  # Validate municipality IDs
  muni_ref <- readRDS(
    system.file("extdata", "muni_v2_ref.rds", package = "spanishoddata")
  )
  validate_muni_ids <- function(ids, muni_ref) {
    if (is.null(ids) || length(ids) == 0 || all(is.na(ids))) return(TRUE)
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
  if (!all(is.na(id_origin))) validate_muni_ids(id_origin, muni_ref)
  if (!all(is.na(id_destination))) validate_muni_ids(id_destination, muni_ref)

  # Check date is within API-supported range
  valid_dates <- spod_graphql_valid_dates()
  if (!lubridate::ymd(date_fmt) %in% valid_dates) {
    stop(
      "Invalid date. Must be within valid range: ",
      paste(convert_to_ranges(valid_dates), collapse = ", ")
    )
  }

  # Build criteria
  journeysMunCriteria <- list(
    date = date_fmt,
    min_journeys = min_trips,
    distances = graphql_distances
  )
  if (!all(is.na(id_origin))) journeysMunCriteria$origin_muni <- id_origin
  if (!all(is.na(id_destination)))
    journeysMunCriteria$target_muni <- id_destination

  # Build GraphQL payload
  graphql_query <- list(
    query = paste(
      c(
        "query ($journeysMunCriteria: JourneysMunCriteriaGqlInput!) {",
        "  find_journeys_mun_criteria(journeysMunCriteria: $journeysMunCriteria) {",
        "    journeys, journeys_km, origin_muni, target_muni",
        "  }",
        "}"
      ),
      collapse = " "
    ),
    variables = list(journeysMunCriteria = journeysMunCriteria)
  )

  # in-memory session token
  session_token <- getOption("spanishoddata.session_token")
  if (is.null(session_token)) {
    session_token <- uuid::UUIDgenerate()
    options(spanishoddata.session_token = session_token)
  }

  # get HMAC secret
  hmac_secret <- get_hmac_secret()

  # compute the raw HMAC-SHA256 bytes
  raw_sig <- digest::hmac(
    key = hmac_secret,
    object = session_token,
    algo = "sha256",
    serialize = FALSE,
    raw = TRUE # <= return raw bytes, not hex
  )

  # base64-encode those raw bytes
  signature_b64 <- openssl::base64_encode(raw_sig)
  signature_b64

  graphql_endpoint <- getOption("spanishoddata.graphql_api_endpoint")
  req <- httr2::request(graphql_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = getOption("spanishoddata.user_agent"),
      "X-Session-Token" = session_token,
      "X-Signature" = signature_b64
    ) |>
    httr2::req_body_json(graphql_query)
  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

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
      id_origin = .data$origin_muni,
      id_destination = .data$target_muni,
      n_trips = .data$journeys,
      trips_total_length_km = .data$journeys_km
    ) |>
    dplyr::mutate(date = lubridate::ymd(date_fmt)) |>
    dplyr::relocate(.data$date, .before = id_origin)

  return(od)
}


get_hmac_secret <- function(
  base_url = "https://mapas-movilidad.transportes.gob.es"
) {
  # 1) Fetch the homepage HTML
  homepage <- httr2::request(base_url) %>%
    httr2::req_perform() %>%
    httr2::resp_body_string()

  # 2) Parse and find the inline <script> that mentions import_meta_env
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

  # 3) Extract the real HMAC_SECRET from that block
  secret <- stringr::str_match(txt, '"HMAC_SECRET"\\s*:\\s*"([^"]+)"')[, 2]

  if (is.na(secret) || nchar(secret) < 10) {
    stop("Failed to parse HMAC_SECRET from the page.")
  }
  secret
}
