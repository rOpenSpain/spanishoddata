# change subfolder name for raw data cache here to apply globally
spod_subfolder_raw_data_cache <- function(ver = 1) {
  rlang:::check_number_whole(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  base_subdir_name <- "raw_data_cache"
  return(paste0(base_subdir_name, "/v", ver, "/"))
}