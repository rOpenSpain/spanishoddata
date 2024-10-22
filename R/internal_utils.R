#' Convert multiple formates of date arguments to a sequence of dates
#'
#' This function processes the date arguments provided to various functions in the package. It can handle single dates and arbitratry sequences (vectors) of dates in ISO (YYYY-MM-DD) and YYYYMMDD format. It can also handle date ranges in the format 'YYYY-MM-DD_YYYY-MM-DD' (or 'YYYYMMDD_YYYYMMDD'), date ranges in named vec and regular expressions to match dates in the format `YYYYMMDD`.
#'
#' @param dates A `character` or `Date` vector of dates to process. Kindly keep in mind that v1 and v2 data follow different data collection methodologies and may not be directly comparable. Therefore, do not try to request data from both versions for the same date range. If you need to compare data from both versions, please refer to the respective codebooks and methodology documents. The v1 data covers the period from 2020-02-14 to 2021-05-09, and the v2 data covers the period from 2022-01-01 to the present until further notice. The true dates range is checked against the available data for each version on every function run.
#'
#' The possible values can be any of the following:
#' 
#'  * For the `spod_get()` and `spod_convert()` functions, the `dates` can be set to "cached_v1" or "cached_v2" to request data from cached (already previously downloaded) v1 (2020-2021) or v2 (2022 onwards) data. In this case, the function will identify and use all data files that have been downloaded and cached locally, (e.g. using an explicit run of `spod_download()`, or any data requests made using the `spod_get()` or `spod_convert()` functions).
#'
#'  * A single date in ISO (YYYY-MM-DD) or YYYYMMDD format. `character` or `Date` object.
#'
#' * A vector of dates in ISO (YYYY-MM-DD) or YYYYMMDD format. `character` or `Date` object. Can be any non-consecutive sequence of dates.
#'
#' * A date range
#'
#'   * eigher a `character` or `Date` object of length 2 with clearly named elements `start` and `end` in ISO (YYYY-MM-DD) or YYYYMMDD format. E.g. `c(start = "2020-02-15", end = "2020-02-17")`;
#'
#'   * or a `character` object of the form `YYYY-MM-DD_YYYY-MM-DD` or `YYYYMMDD_YYYYMMDD`. For example, `2020-02-15_2020-02-17` or `20200215_20200217`.
#'
#' * A regular expression to match dates in the format `YYYYMMDD`. `character` object. For example, `^202002` will match all dates in February 2020.
#'
#'
#' @return A character vector of dates in ISO format (YYYY-MM-DD).
#' @keywords internal
spod_dates_argument_to_dates_seq <- function(dates) {
  if (is.null(dates) || (!is.character(dates) && !inherits(dates, "Date"))) {
    stop("Invalid date input format. Please provide a character vector or Date object.")
  }
  if (length(dates) == 1 && dates %in% c("cached_v1", "cached_v2")) {
    return(dates)
  }

  range_regex <- "^\\d{4}(-\\d{2}){2}_\\d{4}(-\\d{2}){2}$|^\\d{8}_\\d{8}$"
  single_date_regex <- "^(\\d{4}-\\d{2}-\\d{2}|\\d{8})$"
  # If dates is a vector of length one
  # Check if is single date, date range, or regex pattern
  if (length(dates) == 1) {
    # Check if date range
    # match both YYYY-MM-DD_YYYY-MM-DD and YYYYMMDD_YYYYMMDD
    if (grepl(range_regex, dates)) {
      date_parts <- strsplit(dates, "_")[[1]]
      date_parts <- lubridate::ymd(date_parts)
      dates <- seq.Date(date_parts[1], date_parts[2], by = "day")

      # if dates does not match the date range pattern
      # check if it is just a single day in YYYY-MM-DD or YYYYMMDD format
    } else if (grepl(single_date_regex, dates)) {
      dates <- lubridate::ymd(dates)

      # assume it is a regex pattern
    } else {
      dates <- spod_expand_dates_from_regex(dates)
      # since spod_expand_dates_from_regex already uses the metadata to generate valid dates we can skip any checks that are required for other date formats and only check for datte overlap
      if (isFALSE(spod_is_data_version_overlaps(dates))) {
        return(dates)
      }
    }

    # If dates if a vector of multiple values
  } else if (length(dates) > 1) {
    # Check if it is of length 2, then it may be a date range
    if (length(dates) == 2 & !is.null(names(dates))) {
      # if the vector is named with 'start' and 'end', we can assume it is a date range
      if (all(names(dates) %in% c("start", "end"))) {
        date_parts <- lubridate::ymd(dates)
        names(date_parts) <- names(dates)
        # check if start is before end
        if (date_parts["start"] > date_parts["end"]) {
          stop("Start date must be before end date.")
        }
        dates <- seq.Date(date_parts["start"], date_parts["end"], by = "day")
      }
    } else {
      # this is apparantly a sequence of dates
      dates <- lubridate::ymd(dates)
    }
  }

  # now that we have a clean sequence of dates, we can check for overlaps between data versions
  if (isFALSE(spod_is_data_version_overlaps(dates)) &
    spod_infer_data_v_from_dates(dates) %in% c(1, 2)
  ) {
    return(dates)
  }
}



#' Check if specified dates span both data versions
#'
#' This function checks if the specified dates or date ranges span both v1 and v2 data versions.
#'
#' @param dates A Dates vector of dates to check.
#' @return TRUE if the dates span both data versions, FALSE otherwise.
#' @keywords internal
spod_is_data_version_overlaps <- function(dates) {
  all_dates_v1 <- spod_get_valid_dates(ver = 1)
  all_dates_v2 <- spod_get_valid_dates(ver = 2)

  if (any(dates %in% all_dates_v1) && any(dates %in% all_dates_v2)) {
    stop(paste0("Dates found in both v1 and v2 data. The v1 and v2 data sets may not be comparable. Please see the respective codebooks and methodology documents.\nThe valid dates range for v1 is: ", paste0(min(all_dates_v1), " to ", max(all_dates_v1)), " and for v2 is: ", paste0(min(all_dates_v2), " to ", max(all_dates_v2))))
  }
  return(FALSE)
}

spod_infer_data_v_from_dates <- function(dates) {
  # check if user is requesting to just get all cached data
  if (length(dates) == 1){
    if (as.character(dates) %in% c("cached_v1", "cached_v2")) {
      return(
        as.integer(stringr::str_extract(as.character(dates), "[0-9]$"))
      )
    }
  }
  cached_data_requested <- length(dates) == 1 &&
    all(as.character(dates) %in% c("cached_v1", "cached_v2"))
  
  # in case of overlap
  # will throw an error from the spod_is_data_version_overlaps
  if (spod_is_data_version_overlaps(dates)) {
    invisible(return(NULL))
  }

  # of no overlap, compare with date ranges
  v1_dates <- spod_get_valid_dates(ver = 1)
  v2_dates <- spod_get_valid_dates(ver = 2)

  if (all(dates %in% v1_dates)) {
    return(1)
  } else if (all(dates %in% v2_dates)) {
    return(2)
  } else {
    # if some dates did not match stop with a message showing which dates are missing
    missing_dates <- dates[!dates %in% c(v1_dates, v2_dates)]
    stop(paste0("Some dates do not match the available data. The valid dates range for v1 is: ", paste0(min(v1_dates), " to ", max(v1_dates)), " and for v2 is: ", paste0(min(v2_dates), " to ", max(v2_dates), ".\nMissing dates: ", paste0(missing_dates, collapse = ", "))))
  }
}

#' Function to expand dates from a regex
#'
#' This function generates a sequence of dates from a regular expression pattern.
#' based on the provided regular expression.
#'
#' @param date_regex A regular expression to match dates in the format yyyymmdd.
#' @return A character vector of dates matching the regex.
#' @keywords internal
spod_expand_dates_from_regex <- function(date_regex) {
  all_dates_v1 <- spod_get_valid_dates(ver = 1)
  all_dates_v2 <- spod_get_valid_dates(ver = 2)

  # Filter dates matching the regex for both versions
  matching_dates_v1 <- all_dates_v1[grepl(date_regex, format(all_dates_v1, "%Y%m%d"))]
  matching_dates_v2 <- all_dates_v2[grepl(date_regex, format(all_dates_v2, "%Y%m%d"))]

  # if both vectors are empty, throw an error
  if (length(matching_dates_v1) == 0 && length(matching_dates_v2) == 0) {
    stop(paste0(
      "No matching dates found in the available data.",
      "The valid dates range for v1 is: ", paste0(min(all_dates_v1), " to ", max(all_dates_v1)), " and for v2 is: ", paste0(min(all_dates_v2), " to ", max(all_dates_v2))
    ))
  }
  # If checks above have passed, we can combine the matching dates as only one contains dates and the other is empty
  matching_dates <- sort(c(matching_dates_v1, matching_dates_v2))

  return(matching_dates)
}

#' Get valid dates for the specified data version
#' 
#' @inheritParams spod_available_data
#' @return A vector of type `Date` with all possible valid dates for the specified data version (v1 for 2020-2021 and v2 for 2020 onwards).
#' @export
spod_get_valid_dates <- function(ver = NULL) {
  ver <- as.integer(ver) # todo: add type safety check
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }


  if (ver == 1) {
    # available_data <- spod_available_data_v1(check_local_files = FALSE, quiet = TRUE)
    # all_dates <- unique(available_data[grepl("maestra1.*diarios", available_data$target_url),]$data_ymd, na.rm = TRUE)
    # perahps it is worth hardcoding at lest the v1 data range as it is unlikely to change at this point
    all_dates <- seq.Date(from = as.Date("2020-02-14"), to = as.Date("2021-05-09"), by = "day")
  } else if (ver == 2) {
    available_data <- spod_available_data_v2(quiet = TRUE)
    all_dates <- unique(available_data[grepl("viajes.*diarios", available_data$target_url), ]$data_ymd, na.rm = TRUE)
  }
  all_dates <- sort(all_dates)
  return(all_dates)
}
# TODO: currently checks for date range for od data only. not all datasets may be available for all dates, so this function may need to be updated to check for the availability of the specific for the requested dates. spod_match_data_type() helper in the same file may be useful here.



spod_zone_names_en2es <- function(
    zones = c(
      "districts", "dist", "distr", "distritos",
      "municipalities", "muni", "municip", "municipios",
      "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
    )) {
  zones <- tolower(zones)
  zones <- match.arg(zones)
  if (zones %in% c("districts", "dist", "distr", "distritos")) {
    return("distritos")
  } else if (zones %in% c("municipalities", "muni", "municip", "municipios")) {
    return("municipios")
  } else if (zones %in% c("lua", "large_urban_areas", "gau", "grandes_areas_urbanas")) {
    return("gau")
  }
}

#' Match data types to folders
#' @inheritParams spod_available_data
#' @keywords internal
spod_match_data_type_for_local_folders <- function(
    type = c(
      "od", "origin-destination",
      "os", "overnight_stays",
      "nt", "number_of_trips"
    ),
    ver = c(1, 2)) {
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }

  type <- tolower(type)
  type <- match.arg(type)

  if (ver == 1) {
    if (type %in% c("od", "origin-destination")) {
      return("maestra1")
    } else if (type %in% c("nt", "number_of_trips")) {
      return("maestra2")
    }
  }

  if (ver == 2) {
    if (type %in% c("od", "origin-destination")) {
      return("viajes")
    } else if (type %in% c("os", "overnight_stays")) {
      return("pernoctaciones")
    } else if (type %in% c("nt", "number_of_trips")) {
      return("personas")
    }
  }

  # need to add a warning here that the type is not recognized
  return(NULL)
}


#' Match data types for normalisation
#' @param type The type of data to match. Can be "od", "origin-destination", "os", "overnight_stays", or "nt", "number_of_trips".
#' @keywords internal
spod_match_data_type <- function(
    type = c(
      "od", "origin-destination", "viajes",
      "os", "overnight_stays", "pernoctaciones",
      "nt", "number_of_trips", "personas"
    )
) {
  
  type <- tolower(type)
  type <- match.arg(type)

  if (type %in% c("od", "origin-destination", "viajes")) {
    return("od")
  } else if (type %in% c("os", "overnight_stays", "pernoctaciones")) {
    return("os")
  } else if (type %in% c("nt", "number_of_trips", "personas")) {
    return("nt")
  }

  # need to add a warning here that the type is not recognized
  return(NULL)
}

#' Get available RAM
#' @keywords internal
#' @return A `numeric` amount of available RAM in GB.
spod_available_ram <- function(){
  return(
    as.numeric(unclass(memuse::Sys.meminfo())[1][['totalram']])/1024/1024/1024
  )
}

#' Remove duplicate values in a semicolon-separated string
#' 
#' @description
#' Remove duplicate IDs in a semicolon-separated string in a selected column in a data frame
#' @param column A `character` vector column in a data frame to remove duplicates from.
#' 
#' @return A `character` vector with semicolon-separated unique IDs.
#' @keywords internal
spod_unique_separated_ids <- function(column) {
  purrr::map_chr(column, ~ {
    unique_ids <- unique(stringr::str_split(.x, ";\\s*")[[1]])  # Split by semicolon and remove duplicates
    stringr::str_c(unique_ids, collapse = "; ")  # Join them back with semicolons
  })
}
