#' Download the data files of specified type, zones, and dates
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This function downloads the data files of the specified type, zones, dates and data version.
#' @param type The type of data to download. Can be `"origin-destination"` (or ust `"od"`), or `"number_of_trips"` (or just `"nt"`) for v1 data. For v2 data `"overnight_stays"` (or just `"os"`) is also available. More data types to be supported in the future. See codebooks for v1 and v2 data in vignettes with `spod_codebook(1)` and `spod_codebook(2)` (\link{spod_codebook}).
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`) for both data versions. Additionaly, these can be `"large_urban_areas"` (or `"lua"`, or the original Spanish `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).
#' @inheritParams spod_dates_argument_to_dates_seq
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()` which returns the value of the environment variable `SPANISH_OD_DATA_DIR` or a temporary directory if the variable is not set. To set the data directory, use \link{spod_set_data_dir}.
#' @param max_download_size_gb The maximum download size in gigabytes. Defaults to 1.
#' @param return_local_file_paths Logical. If `TRUE`, the function returns a character vector of the paths to the downloaded files. If `FALSE`, the function returns `NULL`.
#' @param ignore_missing_dates Logical. If `TRUE`, the function will not raise an error if the some of the specified dates are missing. Any dates that are missing will be skipped, however the data for any valid dates will be acquired. Defaults to `FALSE`.
#' @param check_local_files Logical. Whether to check the file size of local files against knwown remote file sizes on the Amazon S3 storage. Defaults to `TRUE`, which fetches the metadata from Amazon S3. This setting ensures your downloaded files are not broken, so it is recommended to keep it `TRUE`.
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
#'
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
  available_data <- spod_available_data(
    ver = ver,
    check_local_files = TRUE,
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

  files_to_download <- requested_files[!requested_files$downloaded, ]

  # only download files if some are missing
  if (nrow(files_to_download) > 0) {
    total_size_to_download_gb <- round(
      sum(files_to_download$remote_file_size_mb / 1024, na.rm = TRUE),
      4
    )
    # warn if more than 1 GB is to be downloaded
    if (total_size_to_download_gb > max_download_size_gb) {
      message(glue::glue(
        "Approximately {total_size_to_download_gb} GB of data will be downloaded."
      ))
      # ask for confirmation
      response <- readline(
        prompt = "Are you sure you would like to continue with this download? (yes/no) "
      )
      response <- tolower(response) %in% c("y", "yes", "Yes")
      if (!response) {
        message(glue::glue(
          "Exiting without downloading missing files by user request."
        ))
        return()
      }
    }

    if (isFALSE(quiet)) {
      message(glue::glue(
        "Downloading approximately {total_size_to_download_gb} GB of data."
      ))
    }

    # pre-generate target paths for the files to download
    fs::dir_create(
      unique(fs::path_dir(files_to_download$local_path)),
      recurse = TRUE
    )

    # download the missing files
    downloaded_files <- curl::multi_download(
      urls = files_to_download$target_url,
      destfiles = files_to_download$local_path,
      progress = TRUE,
      resume = TRUE
    )

    # set download status for downloaded files as TRUE in requested_files
    requested_files$downloaded[
      requested_files$local_path %in% downloaded_files$destfile
    ] <- TRUE

    if (isFALSE(quiet)) {
      message("Retrieved data for requested dates.")
    }
  }

  if (return_local_file_paths) {
    return(requested_files$local_path)
  }
}
