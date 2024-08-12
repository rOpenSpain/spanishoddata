#' Get raw data cache subfolder name
#' 
#' Change subfolder name in the code of this function for raw data cache here to apply globally, as all functions in the package should use this function to get the raw data cache path.
#' @param ver Integer. The version of the data. Must be 1 or 2.
#' @return Character string with the subfolder name for the raw data cache.
#' @keywords internal
spod_subfolder_raw_data_cache <- function(ver = 1) {
  ver <- as.integer(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  base_subdir_name <- "raw_data_cache"
  return(paste0(base_subdir_name, "/v", ver, "/"))
}

#' Get clean data subfolder name
#' 
#' Change subfolder name in the code of this function for clean data cache here to apply globally, as all functions in the package should use this function to get the clean data cache path.
#' @param ver Integer. The version of the data. Must be 1 or 2.
#' @return Character string with the subfolder name for the clean data cache.
#' @keywords internal
spod_subfolder_clean_data_cache <- function(ver = 1) {
  ver <- as.integer(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  base_subdir_name <- "clean_data"
  return(paste0(base_subdir_name, "/v", ver, "/"))
}