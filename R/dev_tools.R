# This file is for internal functions that update some of the package internal packaged data. These functions are not intended neither to be used by the user nor to be used in any of the package functions.

#' Get files sizes for remote files of v1 and v2 data and save them into a csv.gz file in the inst/extdata folder.
#' @param ver The version of the data (1 or 2). Can be both. Defaults to 2, as v1 data is not being updated since 2021.
#' @return NULL
#' 
#' @keywords internal
spod_files_sizes <- function(ver = 2) {
  data_dir = spod_get_data_dir()
  
  if (any(ver %in% 1)){
    v1 <- spod_available_data(1)
    
    # takes about 1 minute
    future::plan(future::multisession, workers = 6)
    v1$remote_file_size_mb <- furrr::future_map_dbl(
      .x = v1$target_url,
      .f = ~ spod_get_file_size_from_url(x_url = .x),
      .progress = TRUE
    )
    future::plan(future::sequential)

    v1_url_file_sizes <- v1[, c("target_url", "remote_file_size_mb")]
    readr::write_csv(
      x = v1_url_file_sizes,
      file = "inst/extdata/url_file_sizes_v1.csv.gz"
    )
  }

  if (any(ver %in% 2)){
    v2 <- spod_available_data(2)

    # takes about 4 minutes
    future::plan(future::multisession, workers = 6)
    v2$remote_file_size_mb <- furrr::future_map_dbl(
      .x = v2$target_url,
      .f = ~ spod_get_file_size_from_url(x_url = .x),
      .progress = TRUE
    )
    future::plan(future::sequential)

    v2_url_file_sizes <- v2[, c("target_url", "remote_file_size_mb")]
    readr::write_csv(
      x = v2_url_file_sizes,
      file = "inst/extdata/url_file_sizes_v2.csv.gz"
    )
  }
  
}


#' Get file size from URL
#' @param x_url URL
#' @return File size in MB
#' @importFrom utils URLencode
#' @keywords internal
spod_get_file_size_from_url <- function(x_url){
  
  url <- utils::URLencode(x_url)
  headers <- curlGetHeaders(url)
  content_length_line <- grep("Content-Length", headers, value = TRUE)
  content_length_value <- sub("Content-Length:\\s*(\\d+).*", "\\1", content_length_line)
  
  # Convert bytes to MB (1 MB = 1024 * 1024 bytes)
  file_size_mb <- as.numeric(content_length_value) / (1024 * 1024)
  
  return(file_size_mb)
}
