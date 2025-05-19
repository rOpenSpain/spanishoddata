# This file is for internal functions that update some of the package internal packaged data. These functions are not intended neither to be used by the user nor to be used in any of the package functions.

#' Get files sizes for remote files of v1 and v2 data and save them into a csv.gz file in the inst/extdata folder.
#' @param ver The version of the data (1 or 2). Can be both. Defaults to 2, as v1 data is not being updated since 2021.
#' @return Nothing. Only saves a csv.gz file with up to date file sizes in the inst/extdata folder.
#'
#' @keywords internal
#'
spod_files_sizes <- function(ver = 2) {
  data_dir <- spod_get_data_dir()

  if (any(ver %in% 1)) {
    v1 <- spod_available_data(1)

    # takes about 1 minute
    # tictoc::tic()
    future::plan(future::multisession, workers = 8)
    v1$true_remote_file_size_bytes <- furrr::future_map_dbl(
      .x = v1$target_url,
      .f = ~ spod_get_file_size_from_url(x_url = .x),
      .progress = TRUE
    )
    future::plan(future::sequential)
    # tictoc::toc()

    v1 <- v1 |>
      dplyr::arrange(!!!dplyr::syms(c("data_ymd", "target_url"))) |>
      dplyr::select(-"local_path")

    saveRDS(v1, file.path("inst", "extdata", "available_data_v1.rds"))
  }

  if (any(ver %in% 2)) {
    v2 <- spod_available_data(2)
    if (all(v2$size_imputed == FALSE)) {
      stop("all file sizes are known")
    }
    v2_known_size <- v2[v2$size_imputed == FALSE, ]
    v2_unknown_size <- v2[v2$size_imputed == TRUE, ]

    # takes about 5 minutes on full data set, but less when only updating the previously uknown files
    future::plan(future::multisession, workers = 6)
    v2_unknown_size$remote_file_size_mb <- furrr::future_map_dbl(
      .x = v2_unknown_size$target_url,
      .f = ~ spod_get_file_size_from_url(x_url = .x),
      .progress = TRUE
    )
    future::plan(future::sequential)

    v2_combined <- dplyr::bind_rows(v2_known_size, v2_unknown_size)
    v2_url_file_sizes <- v2_combined[, c("target_url", "remote_file_size_mb")]
    readr::write_csv(
      x = v2_url_file_sizes,
      file = "inst/extdata/url_file_sizes_v2.txt.gz"
    )
  }
}


#' Get file size from URL
#' @param x_url URL
#' @return File size in MB
#' @importFrom utils URLencode
#' @keywords internal
spod_get_file_size_from_url <- function(x_url) {
  url <- utils::URLencode(x_url)
  headers <- curlGetHeaders(url)
  content_length_line <- grep("Content-Length", headers, value = TRUE)
  content_length_value <- sub(
    "Content-Length:\\s*(\\d+).*",
    "\\1",
    content_length_line
  ) |>
    as.numeric()

  return(content_length_value)
}

#' Get Etags for locally saved v1 data files and save them into a RDS file in the inst/extdata folder.
#' @return Nothing. Only saves a RDS file with up to date ETags in the inst/extdata folder.
#' @keywords internal
spod_store_etags <- function() {
  available_data <- spod_available_data(1, check_local_files = TRUE)
  available_data <- available_data |>
    dplyr::filter(downloaded == TRUE)
  local_etags <- available_data$local_path |>
    purrr::map_chr(~ spod_compute_s3_etag(.x), .progress = TRUE)
  available_data <- available_data |>
    dplyr::mutate(local_etag = local_etags) |>
    dplyr::as_tibble()
  return(available_data)
}
