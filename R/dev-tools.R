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

#' Check if local files are consistent with remote files
#' @param nthreads Numeric. Number of threads to use. Defaults to the number of available cores minus 1.
#' @inheritParams spod_check_files
#' @inherit spod_check_files
#' @keywords internal
spod_check_files_parallel <- function(
  type = c(
    "od",
    "origin-destination",
    "os",
    "overnight_stays",
    "nt",
    "number_of_trips"
  ),
  zones = c(
    "districts",
    "dist",
    "distr",
    "distritos",
    "municipalities",
    "muni",
    "municip",
    "municipios",
    "lua",
    "large_urban_areas",
    "gau",
    "grandes_areas_urbanas"
  ),
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  ignore_missing_dates = FALSE,
  nthreads = parallelly::availableCores() - 1
) {
  # Validate inputs
  checkmate::assert_choice(
    type,
    choices = c(
      "od",
      "origin-destination",
      "os",
      "overnight_stays",
      "nt",
      "number_of_trips"
    )
  )
  checkmate::assert_choice(
    zones,
    choices = c(
      "districts",
      "dist",
      "distr",
      "distritos",
      "municipalities",
      "muni",
      "municip",
      "municipios",
      "lua",
      "large_urban_areas",
      "gau",
      "grandes_areas_urbanas"
    )
  )
  checkmate::assert_directory_exists(data_dir, access = "r")
  checkmate::assert_flag(quiet)

  # normalise zones
  zones <- spod_zone_names_en2es(zones)

  # simple null check is enough here, as spod_dates_arugument_to_dates_seq will do additional checks anyway
  if (is.null(dates)) {
    message(
      "`dates` argument is undefined. Please set `dates='cached_v1'` or `dates='cached_v2'` to convert all data that was previously downloaded. Alternatively, specify at least one date between 2020-02-14 and 2021-05-09 (for v1 data) or between 2022-01-01 onwards (for v2). Any missing data will be downloaded before conversion. For more details on the dates argument, see ?spod_download."
    )
  }

  dates_to_use <- spod_dates_argument_to_dates_seq(dates = dates)

  # check version
  ver <- spod_infer_data_v_from_dates(
    dates = dates_to_use,
    ignore_missing_dates = ignore_missing_dates
  )

  if (isFALSE(quiet)) {
    message("Data version detected from dates: ", ver)
  }

  # convert english data type names to spanish words used in the default data paths
  type <- match.arg(type)
  type <- spod_match_data_type_for_local_folders(type = type, ver = ver)

  available_data <- spod_available_data(
    ver = ver,
    check_local_files = TRUE,
    data_dir = data_dir,
    quiet = quiet,
    use_s3 = TRUE
  )

  if (all(as.character(dates) %in% c("cached_v1", "cached_v2"))) {
    dates_to_use <- available_data |>
      dplyr::filter(.data$downloaded == TRUE & !is.na(.data$data_ymd)) |>
      dplyr::pull(.data$data_ymd)
    available_data <- available_data |>
      dplyr::filter(.data$downloaded == TRUE & !is.na(.data$data_ymd))
  }

  # match the available_data to type, zones, version and dates
  if (ver == 1) {
    requested_files <- available_data[
      # selecting districts files for v1 to avoid issues with municipalities # this is to address the bugs described in detail in:
      # http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
      # http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
      # the decision was to use distrcit data and aggregate it to replicate municipal data
      grepl(
        glue::glue("v{ver}.*{type}.*distritos"),
        available_data$local_path
      ) &
        available_data$data_ymd %in% dates_to_use,
    ]
  } else if (ver == 2) {
    requested_files <- available_data[
      grepl(glue::glue("v{ver}.*{zones}.*{type}"), available_data$local_path) &
        available_data$data_ymd %in% dates_to_use,
    ]
  }

  # if some requested files are missing issue a warning
  if (!all(requested_files$downloaded)) {
    warning(glue::glue(
      "Some files for the requested dates are missing. Make sure you have downloaded all files requested to be checked for consistency with `spod_download(type = {type}, zones = {zones}, dates = {dates})`. For now, `spod_check_files()` will only check the files that were previously downloaded and currently exist on disk.",
    ))
    requested_files <- requested_files |>
      dplyr::filter(.data$downloaded == TRUE)
  }

  # compute ETag for each file
  with(
    future::plan(future.mirai::mirai_multisession, workers = nthreads),
    local = TRUE
  )
  local_etags <- requested_files$local_path |>
    furrr::future_map_chr(
      ~ spod_compute_s3_etag(.x),
      .progress = TRUE,
      future.seed = TRUE
    )

  # compare ETags
  requested_files <- requested_files |>
    dplyr::mutate(
      local_etag = local_etags,
    ) |>
    dplyr::mutate(
      local_file_consistent = dplyr::if_else(
        condition = .data$local_etag == .data$etag,
        true = TRUE,
        false = FALSE,
        missing = FALSE
      )
    )

  # issue a warning if there are mismatches or inform that everything is ok
  if (isFALSE(quiet)) {
    if (!all(requested_files$local_file_consistent)) {
      warning(glue::glue(
        "Some files are inconsistent with their local copies. Please inspect the returned table. Run `spod_download(type = {type}, zones = {zones}, dates = {dates})` to download the files again."
      ))
    } else {
      message("All files are consistent with their local copies.")
    }
  }

  return(requested_files)
}
