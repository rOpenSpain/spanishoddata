# Description: Internal utility functions for the MITMA package

#' Process multiple date arguments
#' 
#' This function processes the date arguments provided to various functions in the package.
#' It checks if more than one date argument is provided and returns the appropriate dates as a Dates vector.
#' The function ensures that the dates are in ISO format (yyyy-mm-dd) or yyyymmdd format.
#' 
#' @param date_range A vector of dates in ISO format (yyyy-mm-dd) or yyyymmdd format.
#' @param dates_list A vector of dates in ISO format (yyyy-mm-dd) or yyyymmdd format.
#' @param date_regex A regular expression to match dates in the format yyyymmdd.
#' @return A Dates vector of dates.
#' @keywords internal
process_date_arguments <- function(
  date_range = NULL,
  dates_list = NULL,
  date_regex = NULL,
  ver = c(1, 2)
) {
  rlang:::check_number_whole(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }

  # perhaps this funciton should automatically detect the type of dates passed. This will dramatically simplify the API and allow the download and get functions to have just one argument for dates that will accept any format.

  # Helper function to check if dates are in ISO format or yyyymmdd format
  is_valid_date <- function(dates) {
    all(sapply(dates, function(date) {
      !is.na(lubridate::ymd(date)) || !is.na(as.Date(date, format = "%Y%m%d"))
    }))
  }
  
  
  # Check if more than one date argument is provided
  args_provided <- list(
    date_range = !is.null(date_range),
    dates_list = !is.null(dates_list),
    date_regex = !is.null(date_regex)
  )
  
  # Count how many date arguments are set
  args_set <- sum(unlist(args_provided))
  
  # Assert that only one argument is set
  assertthat::assert_that(args_set <= 1,
    msg = "Only one of the date arguments (date_range, dates_list, date_regex) should be set.")
  
  # Determine which date argument to use and return the appropriate dates
  if (!is.null(date_range)) {
    assertthat::assert_that(is_valid_date(date_range),
      msg = "date_range must be in ISO format (yyyy-mm-dd) or yyyymmdd format.")
    return(date_range)
    
  } else if (!is.null(dates_list)) {
    assertthat::assert_that(is_valid_date(dates_list),
      msg = "dates_list must be in ISO format (yyyy-mm-dd) or yyyymmdd format.")
    return(dates_list)
    
  } else if (!is.null(date_regex)) {
    dates <- expand_dates_from_regex(date_regex, ver = ver)
    return(dates)
  }
}

#' Function to expand dates from a regex
#' 
#' This function generates a sequence of dates from a regular expression pattern.
#' based on the provided regular expression.
#' 
#' @param date_regex A regular expression to match dates in the format yyyymmdd.
#' @param ver The version of the data to use. Defaults to "v1". Can be "v1" or "v2".
#' @return A character vector of dates matching the regex.
#' @keywords internal
expand_dates_from_regex <- function(date_regex,
  ver = c(1, 2)
) {

  # currently checks for date range for od data only. not all datasets may be available for all dates, so this function may need to be updated to check for the availability of the specific for the requested dates. spod_match_data_type() helper in the same file may be useful here.

  rlang:::check_number_whole(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  
  if(ver == 1) {
    available_data <- spod_available_data_v1(check_local_files = FALSE)
    date_range <- range(available_data[grepl("maestra1.*diarios", available_data$target_url),]$data_ymd, na.rm = TRUE)
  } else if(ver == 2) {
    available_data <- spod_get_metadata() # replace with spod_available_data_v2() when available, spod_get_metadata can become a wrapper with v1/v2 argument. Potentially we can even automaticaly detect the data version based on the time intervals that user requests, but this is a bit controversial, as the methodology behind v1 and v2 data generation is not the same and Nommon+MITMA do not recommend mixing those together and comparing absoloute numbers of trips.
    date_range <- range(available_data[grepl("viajes.*diarios", available_data$target_url),]$data_ymd, na.rm = TRUE)
  }
  start_date <- date_range[1]
  end_date <- date_range[2]

  all_dates <- seq.Date(start_date, end_date, by = "day")
  
  # Filter dates matching the regex
  matching_dates <- all_dates[grepl(date_regex, format(all_dates, "%Y%m%d"))]
  
  return(as.character(matching_dates))
}

spod_zone_names_en2es <- function(
  zones = c("districts", "dist", "distr",
    "municipalities", "muni", "municip")
) {
  zones <- tolower(zones)
  zones <- match.arg(zones)
  if(zones %in% c("districts", "dist", "distr")) {
    return("distritos")
  } else if(zones %in% c("municipalities", "muni", "municip")) {
    return("municipios")
  }
}

#' Match data types to folders
#' @param type The type of data to match. Can be "od", "origin-destination", "os", "overnight_stays", or "tpp", "trips_per_person".
#' @param ver The version of the data to use. Defaults to 1. Can be 1 or 2.
#' @keywords internal
spod_match_data_type <- function(
  type = c(
    "od", "origin-destination",
    "os", "overnight_stays",
    "tpp", "trips_per_person"),
  ver = c(1, 2)
){
  rlang:::check_number_whole(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }

  type <- tolower(type)
  type <- match.arg(type)

  if(ver == 1) {
    if (type %in% c("od", "origin-destination")) {
      return("maestra1")
    } else if(type %in% c("tpp", "trips_per_person")) {
      return("maestra2")
    }
  }

  if(ver == 2) {
    if (type %in% c("od", "origin-destination")) {
      return("viajes")
    } else if(type %in% c("os", "overnight_stays")) {
      return("pernoctaciones")
    } else if(type %in% c("tpp", "trips_per_person")) {
      return("personas")
    }
  }

  # need to add a warning here that the type is not recognized
  return(NULL)
}