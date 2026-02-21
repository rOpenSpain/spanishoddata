# internal wrappers to allow mocking of base/utils functions
spod_download_file <- function(...) utils::download.file(...)
spod_interactive <- function(...) base::interactive(...)
spod_readline <- function(...) base::readline(...)

#' Download the data files of specified type, zones, and dates
#' @title Download the data files of specified type, zones, and dates
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This function downloads the data files of the specified type, zones, dates and data version.
#'
#'
#' For detailed data descriptions, see package vignettes using [`spod_codebook(ver = 1)`][spod_codebook] and [`spod_codebook(ver = 2)`][spod_codebook] and official methodology documents in **References** section.
#'
#' @template references
#'
#' @param type The type of data to download. Can be `"origin-destination"` (or ust `"od"`), or `"number_of_trips"` (or just `"nt"`) for v1 data. For v2 data `"overnight_stays"` (or just `"os"`) is also available. More data types to be supported in the future. See codebooks for v1 and v2 data in vignettes with `spod_codebook(1)` and `spod_codebook(2)` (\link{spod_codebook}).
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`) for both data versions. Additionaly, these can be `"large_urban_areas"` (or `"lua"`, or the original Spanish `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).
#' @inheritParams spod_dates_argument_to_dates_seq
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()` which returns the value of the environment variable `SPANISH_OD_DATA_DIR` or a temporary directory if the variable is not set. To set the data directory, use \link{spod_set_data_dir}.
#' @param max_download_size_gb The maximum download size in gigabytes. Defaults to 1.
#' @param return_local_file_paths Logical. If `TRUE`, the function returns a character vector of the paths to the downloaded files. If `FALSE`, the function returns `NULL`.
#' @param ignore_missing_dates Logical. If `TRUE`, the function will not raise an error if the some of the specified dates are missing. Any dates that are missing will be skipped, however the data for any valid dates will be acquired. Defaults to `FALSE`.
#' @param check_local_files Logical. Whether to check the file size of local files against known remote file sizes on the Amazon S3 storage. Defaults to `TRUE`, which fetches the metadata from Amazon S3. This setting ensures your downloaded files are not broken, so it is recommended to keep it `TRUE`.
#' @inheritParams global_quiet_param
#'
#' @return Nothing. If `return_local_file_paths = TRUE`, a `character` vector of the paths to the downloaded files.
#'
#' @export
#' @examplesIf interactive()
#' \donttest{
#'
#' # Set data dir for file downloads
#' spod_set_data_dir(tempdir())
#'
#' # Download the number of trips on district level for the a date range in March 2020
#' spod_download(
#'   type = "number_of_trips", zones = "districts",
#'   dates = c(start = "2020-03-20", end = "2020-03-21")
#' )
#'
#' # Download the number of trips on district level for select dates in 2020 and 2021
#' spod_download(
#'   type = "number_of_trips", zones = "dist",
#'   dates = c("2020-03-20", "2020-03-24", "2021-03-20", "2021-03-24")
#' )
#'
#' # Download the number of trips on municipality level using regex for a date range in March 2020
#' # (the regex will capture the dates 2020-03-20 to 2020-03-24)
#' spod_download(
#'   type = "number_of_trips", zones = "municip",
#'   dates = "2020032[0-4]"
#' )
#' }

spod_download <- function(
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
  max_download_size_gb = 1, # 1GB
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  return_local_file_paths = FALSE,
  ignore_missing_dates = FALSE,
  check_local_files = TRUE
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
  checkmate::assert_number(max_download_size_gb, lower = 0.1)
  checkmate::assert_directory_exists(data_dir, access = "rw")
  checkmate::assert_flag(quiet)
  checkmate::assert_flag(return_local_file_paths)
  checkmate::assert_flag(ignore_missing_dates)

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
  # this leads to a second call to an internal spod_get_valid_dates() which in turn causes a second call to spod_available_data(). This results in reading xml files with metadata for the second time. This is not optimal and should be fixed.

  if (isFALSE(quiet)) {
    message("Data version detected from dates: ", ver)
  }

  # convert english data type names to spanish words used in the default data paths
  type <- match.arg(type)
  type <- spod_match_data_type_for_local_folders(type = type, ver = ver)

  # get the available  data list while checking for files already cached on disk
  # TODO: make requests faster by providing filtering prefix for Amazon S3 to only get the files we need?
  available_data <- spod_available_data(
    ver = ver,
    check_local_files = check_local_files,
    data_dir = data_dir,
    quiet = quiet,
    use_s3 = TRUE
  )

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

  # compare file sizes
  requested_files <- requested_files |>
    dplyr::mutate(
      complete_download = dplyr::if_else(
        condition = .data$file_size_bytes == as.numeric(.data$local_file_size),
        true = TRUE,
        false = FALSE,
        missing = FALSE
      )
    )

  files_to_download <- requested_files |>
    dplyr::filter(.data$complete_download == FALSE)

  # only download files if some are missing
  if (nrow(files_to_download) > 0) {
    total_size_to_download_gb <- round(
      sum(files_to_download$remote_file_size_mb / 1024, na.rm = TRUE),
      4
    )
    # warn if more than 1 GB is to be downloaded
    if (total_size_to_download_gb > max_download_size_gb) {
      message(glue::glue(
        "Approximately {round(total_size_to_download_gb, 2)} GB of data will be downloaded."
      ))
      # ask for confirmation
      response <- spod_readline(
        prompt = "Are you sure you would like to continue with this download? (yes/no) "
      )
      response <- tolower(response) %in% c("y", "yes", "Yes")
      if (!response) {
        message(glue::glue(
          "Exiting without downloading missing files by user request. Requested data download size is larger than {max_download_size_gb} GB. Please increase `max_download_size_gb` parameter when running the function again."
        ))
        return()
      }
    }

    if (isFALSE(quiet)) {
      message(glue::glue(
        "Downloading approximately {round(total_size_to_download_gb, 2)} GB of data."
      ))
    }

    # pre-generate target paths for the files to download
    # and create all directories in the path
    # fs::dir_create(
    #   unique(fs::path_dir(files_to_download$local_path)),
    #   recurse = TRUE
    # )

    # download the missing files
    # TODO: disable mass curl::multi_download due to multiple failures on some connections
    # downloaded_files <- curl::multi_download(
    #   urls = files_to_download$target_url,
    #   destfiles = files_to_download$local_path,
    #   progress = TRUE,
    #   resume = TRUE,
    #   multiplex = FALSE
    # )

    # use curl::multi_download in a loop on one file at a time with manual progress bar
    downloaded_files <- spod_download_in_batches(
      files_to_download
    )
    # downloaded_files <- spod_multi_download_with_progress(
    #   files_to_download
    # )

    # set download status for downloaded files as TRUE in requested_files
    # update the columns in requested_files to have new local file size, downloaded and download complete status columns
    requested_files <- requested_files |>
      dplyr::rows_update(
        downloaded_files |>
          dplyr::select(
            "local_path",
            "local_file_size",
            "downloaded",
            "complete_download"
          ),
        by = "local_path"
      )

    if (isFALSE(quiet)) {
      message("Retrieved data for requested dates.")
    }
  }

  if (return_local_file_paths) {
    return(requested_files$local_path)
  }
}



#' Download multiple files with progress bar in parallel
#'
#' @description
#' Download multiple files with a progress bar. Retries failed downloads up to 3 times. Downloads are in parallel and in batches to show progress. First 10 Mb of a file is downloaded to check the speed.
#'
#' @param files_to_download A data frame with columns `target_url`, `local_path` and `file_size_bytes`.
#' @param batch_size Numeric. Number of files to download at a time.
#' @param bar_width Numeric. Width of the progress bar.
#' @param chunk_size Numeric. Number of bytes to download at a time for speed test.
#' @param show_progress Logical. Whether to show the progress bar.
#' @param max_retries Integer. Maximum number of retries for failed downloads.
#' @param timeout Numeric. Timeout in seconds for each download.
#' @return A data frame with columns `target_url`, `local_path`, `file_size_bytes` and `local_file_size`.
#'
#' @keywords internal
#'
spod_download_in_batches <- function(
  files_to_download,
  batch_size = 5,
  bar_width = 20,
  chunk_size = 1024 * 1024,
  test_size = 10 * 1024 * 1024, # 10 MB test
  max_retries = 3L,
  timeout = 900,
  show_progress = spod_interactive() && !isTRUE(getOption("knitr.in.progress"))
) {
  # Check interactive context
  if (!interactive() || isTRUE(getOption("knitr.in.progress"))) {
    show_progress <- FALSE
  }

  original_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = original_timeout))

  # Sort and ensure directories exist
  files_to_download <- files_to_download[order(files_to_download$data_ymd), ]
  dirs <- unique(dirname(files_to_download$local_path))
  for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  # Totals for progress
  total_files <- nrow(files_to_download)
  total_expected_bytes <- sum(files_to_download$file_size_bytes, na.rm = TRUE)
  total_gb <- total_expected_bytes / 2^30

  # Ensure tracking columns
  if (!"downloaded" %in% names(files_to_download)) {
    files_to_download$downloaded <- FALSE
  }
  if (!"local_file_size" %in% names(files_to_download)) {
    files_to_download$local_file_size <- NA_integer_
  }
  if (!"complete_download" %in% names(files_to_download)) {
    files_to_download$complete_download <- FALSE
  }

  # Pre-skip already-completed files
  cum_bytes <- 0L
  files_counted <- 0L
  to_download <- logical(total_files)
  for (i in seq_len(total_files)) {
    expb <- files_to_download$file_size_bytes[i]
    dest <- files_to_download$local_path[i]
    if (isTRUE(files_to_download$complete_download[i])) {
      actual <- if (file.exists(dest)) file.info(dest)$size else NA_integer_
      if (!is.na(actual) && actual == expb) {
        cum_bytes <- cum_bytes + expb
        files_counted <- files_counted + 1L
        to_download[i] <- FALSE
        next
      } else {
        files_to_download$complete_download[i] <- FALSE
      }
    }
    if (
      isTRUE(files_to_download$downloaded[i]) &&
        !is.na(files_to_download$local_file_size[i]) &&
        files_to_download$local_file_size[i] == expb
    ) {
      files_to_download$complete_download[i] <- TRUE
      cum_bytes <- cum_bytes + expb
      files_counted <- files_counted + 1L
      to_download[i] <- FALSE
    } else if (file.exists(dest) && file.info(dest)$size == expb) {
      files_to_download$downloaded[i] <- TRUE
      files_to_download$complete_download[i] <- TRUE
      files_to_download$local_file_size[i] <- expb
      cum_bytes <- cum_bytes + expb
      files_counted <- files_counted + 1L
      to_download[i] <- FALSE
    } else {
      to_download[i] <- TRUE
    }
  }

  # ETA formatter
  format_eta <- function(eta) {
    if (is.na(eta) || eta <= 0 || !is.finite(eta)) {
      return("--")
    }
    if (eta > 3600) {
      sprintf("%.1fh", eta / 3600)
    } else if (eta > 60) {
      sprintf("%.0fm", eta / 60)
    } else {
      sprintf("%.0fs", eta)
    }
  }

  # Progress bar redraw
  redraw_bar <- function(bytes_done, speed_bytes = NULL) {
    pct <- bytes_done / total_expected_bytes
    nfill <- max(floor(pct * bar_width), 1L)
    bar <- if (nfill < bar_width) {
      paste0(strrep("=", nfill), ">", strrep(" ", bar_width - nfill - 1))
    } else {
      strrep("=", bar_width)
    }
    elapsed <- as.numeric(Sys.time() - start_time, "secs")
    speed_bps <- if (!is.null(speed_bytes)) {
      speed_bytes
    } else {
      (bytes_done / max(elapsed, 0.1))
    }
    speed_mb <- speed_bps / 2^20
    eta_secs <- if (speed_bps > 0) {
      (total_expected_bytes - bytes_done) / speed_bps
    } else {
      NA
    }
    eta <- format_eta(eta_secs)

    msg <- sprintf(
      "Downloading: [%s] %3.0f%% (%d/%d files, %.2f/%.2f GB, %.1f MB/s, ETA: %s)",
      bar,
      pct * 100,
      files_counted,
      total_files,
      bytes_done / 2^30,
      total_gb,
      speed_mb,
      eta
    )
    cat(sprintf("\r%-120s", msg))
    utils::flush.console()
  }

  # Speed test with smallest file, read up to test_size without saving
  if (show_progress) {
    start_time <- Sys.time()
    redraw_bar(cum_bytes)
    rem_idx <- which(to_download)
    if (length(rem_idx) > 0) {
      sizes <- files_to_download$file_size_bytes[rem_idx]
      first_i <- rem_idx[which.min(sizes)]
      url1 <- files_to_download$target_url[first_i]

      bytes_read <- 0L
      t0 <- Sys.time()
      con <- url(url1, "rb")
      repeat {
        to_read <- min(chunk_size, test_size - bytes_read)
        if (to_read <= 0) {
          break
        }
        chunk <- readBin(con, "raw", n = to_read)
        if (length(chunk) == 0) {
          break
        }
        bytes_read <- bytes_read + length(chunk)
      }
      close(con)
      t1 <- Sys.time()

      dt <- as.numeric(t1 - t0, "secs")
      bps <- if (dt > 0) bytes_read / dt else NA
      redraw_bar(cum_bytes, speed_bytes = bps)
    }
  }

  # Prepare batches of all to_download (including test file)
  rem <- which(to_download)
  idx_batches <- split(rem, ceiling(seq_along(rem) / batch_size))

  # Download batches in parallel via libcurl
  for (batch in idx_batches) {
    urls <- files_to_download$target_url[batch]
    dests <- files_to_download$local_path[batch]

    res <- spod_download_file(
      url = urls,
      destfile = dests,
      method = "libcurl",
      mode = "wb",
      quiet = TRUE
    )
    if (length(res) == 1L) {
      res <- rep(res, length(batch))
    }

    # Retry on size mismatch or initial failure
    for (k in seq_along(batch)) {
      i <- batch[k]
      expected <- files_to_download$file_size_bytes[i]
      dest <- dests[k]
      # if initial status not ok or size mismatch
      if (
        res[k] != 0L || !file.exists(dest) || file.info(dest)$size != expected
      ) {
        attempts <- 1L
        while (attempts < max_retries) {
          attempts <- attempts + 1L
          status2 <- spod_download_file(
            url = urls[k],
            destfile = dest,
            method = "libcurl",
            mode = "wb",
            quiet = TRUE
          )
          actual <- if (file.exists(dest)) file.info(dest)$size else NA_integer_
          if (status2 == 0L && identical(actual, expected)) {
            res[k] <- 0L
            break
          }
        }
      }
    }

    for (k in seq_along(batch)) {
      i <- batch[k]
      expected <- files_to_download$file_size_bytes[i]
      if (
        res[k] == 0L &&
          file.exists(dests[k]) &&
          file.info(dests[k])$size == expected
      ) {
        sz <- file.info(dests[k])$size
        files_to_download$downloaded[i] <- TRUE
        files_to_download$complete_download[i] <- TRUE
        files_to_download$local_file_size[i] <- sz
        cum_bytes <- cum_bytes + sz
      } else {
        warning(sprintf(
          "Failed to download %s after %d attempts (got %s bytes, expected %s)",
          urls[k],
          max_retries,
          if (file.exists(dests[k])) file.info(dests[k])$size else NA,
          files_to_download$file_size_bytes[i]
        ))
      }
      files_counted <- files_counted + 1L
      if (show_progress) redraw_bar(cum_bytes)
    }
  }

  if (show_progress) {
    cat("\nAll downloads complete.\n")
  }
  files_to_download
}
