#' Get metadata cache subfolder name
#'
#' Change subfolder name in the code of this function for metadata cache here to apply globally, as all functions in the package should use this function to get the metadata cache path.
#' @return A `character` string with the subfolder name for the raw data cache.
#' @keywords internal
spod_subfolder_metadata_cache <- function() {
  "metadata_cache"
}

#' Get raw data cache subfolder name
#'
#' Change subfolder name in the code of this function for raw data cache here to apply globally, as all functions in the package should use this function to get the raw data cache path.
#' @inheritParams spod_available_data
#' @return A `character` string with the subfolder name for the raw data cache.
#' @keywords internal
spod_subfolder_raw_data_cache <- function(ver = 1) {
  ver <- as.integer(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  base_subdir_name <- "raw_data_cache"
  return(paste0(base_subdir_name, "/v", ver))
}

#' Get clean data subfolder name
#'
#' Change subfolder name in the code of this function for clean data cache here to apply globally, as all functions in the package should use this function to get the clean data cache path.
#' @inheritParams spod_available_data
#' @return A `character` string with the subfolder name for the clean data cache.
#' @keywords internal
spod_subfolder_clean_data_cache <- function(ver = 1) {
  ver <- as.integer(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  base_subdir_name <- "clean_data"
  return(paste0(base_subdir_name, "/v", ver))
}

#' Get temporary directory for DuckDB intermediate spilling
#' 
#' @description
#' Get the The path to the temp folder for DuckDB for \href{https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling}{intermediate spilling} in case the set memory limit and/or physical memory of the computer is too low to perform the query.
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @return A `character` string with the path to the temp folder for `DuckDB` for \href{https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling}{intermediate spilling}.
#' @keywords internal
spod_get_temp_dir <- function(
  data_dir = spod_get_data_dir()
) {
  temp_dir <- fs::path(data_dir, "temp")
  if (!dir.exists(temp_dir)) {
    fs::dir_create(temp_dir)
  }
  return(temp_dir)
}
