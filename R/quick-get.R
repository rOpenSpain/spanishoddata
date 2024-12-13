#' Get daily trip counts per origin-destionation municipality from 2022 onward
#' 
#' This function provides a quick way to get daily aggregated (no hourly data) trip counts per origin-destination municipality from v2 data (2022 onward). Compared to \link[spanishoddata]{spod_get}, which downloads large CSV files, this function downloads the data directly from the GraphQL API. No data aggregation is performed on your computer (unlike in \link[spanishoddata]{spod_get}), so you do not need to worry about memory usage and do not have to use a powerful computer with multiple CPU cores just to get this simple data. Only about 1 MB of data is downloaded for a single day. The limitation of this function is that it can only retrieve data for a single day at a time and only with total number of trips and total km travelled. So it is not possible to get any of the extra variables available in the full dataset via \link[spanishoddata]{spod_get}.
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
#' @examples
#' \dontrun{
#' od_1000 <- spod_quick_get_od(
#'   date = "2022-01-01",
#'   min_trips = 1000
#' )
#' }
#' 
#' 
spod_quick_get_od <- function(
  date = NA,
  min_trips = 100,
  distances = c("500m-2km", "2-10km", "10-50km", "50+km"),
  id_origin = NA,
  id_destination = NA
){
  # Validate inputs
  checkmate::assert_integerish(min_trips, lower = 0, null.ok = FALSE)
  checkmate::assert_subset(distances, choices = c("500m-2km", "2-10km", "10-50km", "50+km"))
  checkmate::assert_character(id_origin, null.ok = TRUE)
  checkmate::assert_character(id_destination, null.ok = TRUE)

  # Convert the date into YYYYMMDD format
  if (is.character(date)) {
    # Check for "YYYY-MM-DD" format
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      date <- as.Date(date) # Safe to convert
    }
    # Check for "YYYYMMDD" format
    else if (nchar(date) == 8 && grepl("^\\d{8}$", date)) {
      date <- as.Date(date, format = "%Y%m%d") # Safe to convert
    }
    else {
      # If neither format matches, stop with a clear error message
      stop("Invalid date format. Use 'YYYY-MM-DD', 'YYYYMMDD', or a Date object.")
    }
  }

  # Check if the input is already a Date object
  if (inherits(date, "Date")) {
    date <- format(date, "%Y%m%d") # Convert to YYYYMMDD format for GraphQL
  } else {
    # Catch any remaining invalid inputs
    stop("Invalid date input. Must be a character in 'YYYY-MM-DD'/'YYYYMMDD' format or a Date object.")
  }
  
  # convert valid dates to ranges
  convert_to_ranges <- function(dates) {
    dates <- as.Date(dates) # Ensure dates are in Date format
    ranges <- tibble::tibble(date = dates) |>
      dplyr::arrange(date) |> 
        dplyr::mutate(
        diff = c(0, diff(date)), # Calculate differences
        group = cumsum(diff != 1) # Create groups for consecutive ranges
      ) |>
      dplyr::group_by(.data$group) |>
      dplyr::summarise(
        start = dplyr::first(date),
        end = dplyr::last(date),
        .groups = "drop"
      )
  
    # Create a character vector of ranges
    range_strings <- ranges |>
      dplyr::mutate(range = paste(.data$start, "to", .data$end)) |>
      dplyr::pull(range)
  
  return(range_strings)
  }

  # check if date is within valid range
  valid_dates <- spod_get_valid_dates(ver = 2)
  is_valid_date <- lubridate::ymd(date) %in% valid_dates
  if (!is_valid_date) {
    stop(
      paste0("Invalid date. Must be within valid range: ",
        paste(convert_to_ranges(valid_dates), collapse = ", ")
      )
    )
  }

  # Mapping user-friendly distances to GraphQL expected values
  distance_mapping <- c(
    "500m-2km" = "D_05_2",
    "2-10km" = "D_2_10",
    "10-50km" = "D_10_50",
    "50+km" = "D_50"
  )
  
  # Municipalities checks
  muni_ref <- readRDS(
    system.file("extdata", "muni_v2_ref.rds", package = "spanishoddata")
  )

  validate_muni_ids <- function(muni_ids, muni_ref) {
    # Handle cases where muni_ids is NULL, empty, or all NA
    if (is.null(muni_ids) || length(muni_ids) == 0 || all(is.na(muni_ids))) {
      return(TRUE) # Nothing to validate
    }
    
    # Check which IDs are invalid
    invalid_ids <- setdiff(muni_ids, muni_ref$id)
    
    # If there are invalid IDs, return a message
    if (length(invalid_ids) > 0) {
      stop(
        "Invalid municipality IDs detected: ",
        paste(invalid_ids, collapse = ", "),
        ". Please provide valid municipality IDs. Use `spod_get_zones(zones = 'muni', ver = 2)` to get valid municipality IDs."
      )
    }
    
    # If all IDs are valid
    return(TRUE)
  }

  # Validate municipality IDs if provided
  if (!is.null(id_origin) && length(id_origin) > 0 && !all(is.na(id_origin))) {
    validate_muni_ids(id_origin, muni_ref)
  }
  if (!is.null(id_destination) && length(id_destination) > 0 && !all(is.na(id_destination))) {
    validate_muni_ids(id_destination, muni_ref)
  }
  
  
  # Validate min_trips
  if (!is.numeric(min_trips) || min_trips < 0) {
    stop("Invalid minimum number of trips. Must be a non-negative integer.")
  }

  # Translate user-friendly distances into GraphQL distances
  graphql_distances <- unname(distance_mapping[distances])
  
  if (any(is.na(graphql_distances))) {
    stop("Invalid distance value. Allowed values are: ", 
         paste(names(distance_mapping), collapse = ", "))
  }
  
  # Construct the `journeysMunCriteria` part of the query
  journeysMunCriteria <- list(
    date = date,
    min_journeys = min_trips
  )
  
  # Add distances if provided (default is all)
  journeysMunCriteria$distances <- graphql_distances
  
  # Include origin_muni and target_muni only if they are not NA
  if (!is.null(id_origin) && length(id_origin) > 0 && !all(is.na(id_origin))) {
    journeysMunCriteria$origin_muni <- id_origin
  }

  if (!is.null(id_destination) && length(id_destination) > 0 && !all(is.na(id_destination))) {
  journeysMunCriteria$target_muni <- id_destination
}
  
  if (length(id_origin) == 0) id_origin <- NULL
  if (length(id_destination) == 0) id_destination <- NULL
  
  # Define the GraphQL endpoint
  graphql_endpoint <- "https://mapas-movilidad.transportes.gob.es/api/graphql"
  
  # Construct the GraphQL query
  graphql_query <- list(
    query = paste(
      collapse = " ",
      c(
        "query ($journeysMunCriteria: JourneysMunCriteriaGqlInput!) {",
        "find_journeys_mun_criteria(journeysMunCriteria: $journeysMunCriteria) {",
        "journeys, journeys_km, origin_muni, target_muni",
        "}",
        "}"
      )
    ),
    variables = list(
      journeysMunCriteria = journeysMunCriteria
    )
  )

  # Send the POST request
  response <- httr2::request(graphql_endpoint) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "User-Agent" = "spanishoddata R package, https://github.com/rOpenSpain/spanishoddata/"
    ) |>
    httr2::req_body_json(graphql_query) |>
    httr2::req_perform()

  # Parse the response
  response_data <- httr2::resp_body_json(response, simplifyVector = TRUE)

  od <- tibble::as_tibble(response_data$data[[1]]) |> 
    dplyr::select(
      id_origin = .data$origin_muni,
      id_destination = .data$target_muni,
      n_trips = .data$journeys,
      trips_total_length_km = .data$journeys_km
    ) |> 
    dplyr::mutate(
      date = lubridate::ymd(date)
    ) |>
    dplyr::relocate(.data$date, .before = id_origin)
  
  return(od)
}
