# Description: Internal utility functions for the MITMA package

#' Process multiple date arguments
#' 
#' This function processes the date arguments provided to various functions in the package.
#' It checks if more than one date argument is provided and returns the appropriate dates as a POSIXct vector.
#' The function ensures that the dates are in ISO format (yyyy-mm-dd) or yyyymmdd format.
#' 
#' @param date_range A vector of dates in ISO format (yyyy-mm-dd) or yyyymmdd format.
#' @param dates_list A vector of dates in ISO format (yyyy-mm-dd) or yyyymmdd format.
#' @param date_regex A regular expression to match dates in the format yyyymmdd.
#' @return A POSIXct vector of dates.
#' @keywords internal
process_date_arguments <- function(date_range = NULL,
  dates_list = NULL,
  date_regex = NULL,
  data_ver = c("v1", "v2")
) {
  data_ver <- match.arg(data_ver)

  # Helper function to check if dates are in ISO format or yyyymmdd format
  is_valid_date <- function(dates) {
    all(sapply(dates, function(date) {
      !is.na(lubridate::ymd(date)) || !is.na(as.Date(date, format = "%Y%m%d"))
    }))
  }
  
  # Convert valid dates to POSIXct
  convert_to_posixct <- function(dates) {
    as.POSIXct(sapply(dates, function(date) {
      if (!is.na(lubridate::ymd(date))) {
        lubridate::ymd(date)
      } else {
        as.Date(date, format = "%Y%m%d")
      }
    }), tz = "UTC")
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
    return(convert_to_posixct(date_range))
    
  } else if (!is.null(dates_list)) {
    assertthat::assert_that(is_valid_date(dates_list),
      msg = "dates_list must be in ISO format (yyyy-mm-dd) or yyyymmdd format.")
    return(convert_to_posixct(dates_list))
    
  } else if (!is.null(date_regex)) {
    dates <- expand_dates_from_regex(date_regex, data_ver = data_ver)
    return(convert_to_posixct(dates))
  }
}

#' Function to expand dates from a regex
#' 
#' This function generates a sequence of dates from a regular expression pattern.
#' based on the provided regular expression.
#' 
#' @param date_regex A regular expression to match dates in the format yyyymmdd.
#' @param data_ver The version of the data to use. Defaults to "v1". Can be "v1" or "v2".
#' @return A character vector of dates matching the regex.
#' @keywords internal
expand_dates_from_regex <- function(date_regex,
  data_ver = c("v1", "v2")
) {
  data_ver <- match.arg(data_ver)
  
  if(data_ver == "v1") {
    available_data <- spod_available_data_v1()
    date_range <- range(available_data[grepl("maestra2.*diarios", available_data$target_url),]$data_ymd, na.rm = TRUE)
  } else if(data_ver == "v2") {
    available_data <- spod_get_metadata() # replace with spod_available_data_v2() when available
    date_range <- range(available_data[grepl("viajes.*diarios", available_data$target_url),]$data_ymd, na.rm = TRUE)
  }
  start_date <- date_range[1]
  end_date <- date_range[2]

  all_dates <- seq.Date(start_date, end_date, by = "day")
  
  # Filter dates matching the regex
  matching_dates <- all_dates[grepl(date_regex, format(all_dates, "%Y%m%d"))]
  
  return(as.character(matching_dates))
}
