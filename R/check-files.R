#' Check cached files consistency against checksums from S3
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' **WARNING: The checks may fail for May 2022 data and for some 2024 data, as the remote cheksums that are used for checking the file consistency are incorrect. We are working on solving this in future updates, for now, kindly rely on the built-in file size checks of \code{\link{spod_download}}, \code{\link{spod_spod_get}}, and \code{\link{spod_convert}}.** This function checks downloaded data files whether they are consistent with their checksums in Amazon S3 by computing ETag for each file. This involves computing MD5 for each part of the file and concatenating them and computing MD5 again on the resulting concatenated MD5s. This may take very long time if you check all files, so use with caution.
#' @inheritParams spod_get
#' @inheritParams spod_download
#' @inheritParams global_quiet_param
#' @param n_threads Numeric. Number of threads to use for file verificaiton. Defaults to 1. When set to 2 or more threads, uses `future.mirai` as a backend for parallelization, resulting in significant (~4x) speedup, unless disk read speed is a bottleneck.
#'
#' @return A tibble similar to the output of `spod_available_data`, but with an extra column `local_file_consistent`, where `TRUE` indicates that the file cheksum matches the expected checksums in Amazon S3. Note: some v1 (2020-2021) files were not stored correctly on S3 and their ETag checksums are incorrectly reported by Amazon S3, so their true file sizes and ETag checksums were cached inside the `spanishoddata` package.
#'
#' @export
#'
#' @examplesIf interactive()
#' \donttest{
#' spod_set_data_dir(tempdir())
#' spod_download(
#'  type = "number_of_trips",
#'  zones = "distr",
#'  dates = "2020-03-14"
#' )
#'
#' # now check the consistency
#' check_results <- spod_check_files(
#'   type = "number_of_trips",
#'   zones = "distr",
#'   dates = "2020-03-14"
#' )
#' all(check_results$local_file_consistent)
#' }
spod_check_files <- function(
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
  n_threads = 1
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
  checkmate::assertNumber(n_threads, lower = 1)

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
  matched_type <- spod_match_data_type_for_local_folders(type = type, ver = ver)

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
        glue::glue("v{ver}.*{matched_type}.*distritos"),
        available_data$local_path
      ) &
        available_data$data_ymd %in% dates_to_use,
    ]
  } else if (ver == 2) {
    requested_files <- available_data[
      grepl(
        glue::glue("v{ver}.*{zones}.*{matched_type}"),
        available_data$local_path
      ) &
        available_data$data_ymd %in% dates_to_use,
    ]
  }

  # if some requested files are missing issue a warning
  if (!all(requested_files$downloaded)) {
    dates_missing_downloads <- requested_files |>
      dplyr::filter(.data$downloaded == FALSE) |>
      dplyr::filter(.data$data_ymd %in% dates_to_use) |>
      dplyr::pull(.data$data_ymd)
    warning(glue::glue(
      'Some files for the requested dates are missing ({paste(spod_convert_dates_to_ranges(dates_missing_downloads), collapse = ", ")}). Make sure you have downloaded all files requested to be checked for consistency with `spod_download()`. For now, `spod_check_files()` will only check the files that were previously downloaded and currently exist on disk.',
    ))
    requested_files <- requested_files |>
      dplyr::filter(.data$downloaded == TRUE)
  }

  # compute ETag for each file
  if (n_threads == 1) {
    local_etags <- requested_files$local_path |>
      purrr::map_chr(~ spod_compute_s3_etag(.x), .progress = TRUE)
  } else if (n_threads > 1) {
    spod_assert_package(c("future", "furrr", "future.mirai"))
    with(
      future::plan(future.mirai::mirai_multisession, workers = n_threads),
      local = TRUE
    )
    local_etags <- requested_files$local_path |>
      furrr::future_map_chr(
        ~ spod_compute_s3_etag(.x),
        .progress = TRUE,
        future.seed = TRUE
      )
  }

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
      broken_dates <- requested_files |>
        dplyr::filter(.data$local_file_consistent == FALSE) |>
        dplyr::pull(.data$data_ymd)
      warning(glue::glue(
        'Some files are inconsistent with the reference data ({paste(spod_convert_dates_to_ranges(broken_dates), collapse = ", ")}). Please inspect the returned table by filtering the output table by "local_file_consistent == FALSE". To re-download the inconsistent files, use `spod_download()` and specify the missing dates.'
      ))
    } else {
      message("All checked files are consistent.")
    }
  }

  return(requested_files)
}

#' Compute ETag for a file
#' @param file_path Character. The path to the file.
#' @param part_size Numeric. The size of each part in bytes. Do not change, as this is a default for S3 Etag.
#' @return Character. The ETag for the file.
#' @keywords internal
spod_compute_s3_etag <- function(file_path, part_size = 8 * 1024^2) {
  con <- file(file_path, "rb")
  on.exit(close(con))

  # MD5 each part
  part_md5s <- list()
  repeat {
    buf <- readBin(con, "raw", n = part_size)
    if (length(buf) == 0) {
      break
    }
    part_md5s[[length(part_md5s) + 1]] <- digest::digest(
      buf,
      algo = "md5",
      serialize = FALSE,
      raw = TRUE
    )
  }

  # Single‐part fallback
  if (length(part_md5s) == 1) {
    return(digest::digest(file = file_path, algo = "md5", serialize = FALSE))
  }

  # Concatenate raw MD5s and MD5 again
  combined <- do.call(c, part_md5s)
  final_raw <- digest::digest(
    combined,
    algo = "md5",
    serialize = FALSE,
    raw = TRUE
  )
  raw_to_hex <- function(r) paste(sprintf("%02x", as.integer(r)), collapse = "")
  etag_hex <- raw_to_hex(final_raw)
  paste0(etag_hex, "-", length(part_md5s))
}
