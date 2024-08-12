#' Download the data files of specified type, zones, and dates
#'
#' This function downloads the data files of the specified type, zones, dates and data version.
#' @param type The type of data to download. Can be `"origin-destination"` (or ust `"od"`), or `"trips_per_person"` (or just `"tpp"`) for v1 data. For v2 data `"overnight_stays"` (or just `"os"`) is also available. More data types to be supported in the future. See respective codebooks for more information. **ADD CODEBOOKS! to the package**
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`). Additionaly, these can be `"large_urban_areas"` (or `"lau"`, or the original Spanish `"grandes_areas_urbanas"`, or `"gau"`) for v2 data.
#' @inheritParams spod_dates_argument_to_dates_seq
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()` which returns the value of the environment variable `SPANISH_OD_DATA_DIR` or a temporary directory if the variable is not set.
#' @param quiet Logical. If `TRUE`, the function does not print messages to the console. Defaults to `FALSE`.
#' @param return_output Logical. If `TRUE`, the function returns a character vector of the paths to the downloaded files. If `FALSE`, the function returns `NULL`.
#'
#' @return A character vector of the paths to the downloaded files. Unless `return_output = FALSE`, in which case the function returns `NULL`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Download the origin-destination on district level for the a date range in March 2020
#' spod_download_data(
#'   type = "od", zones = "districts",
#'   date_range = c("2020-03-20", "2020-03-24")
#' )
#'
#' # Download the origin-destination on district level for select dates in 2020 and 2021
#' spod_download_data(
#'   type = "od", zones = "dist",
#'   dates_list = c("2020-03-20", "2020-03-24", "2021-03-20", "2021-03-24")
#' )
#'
#' # Download the origin-destination on municipality level using regex for a date range in March 2020
#' # (the regex will capture the dates 2020-03-20 to 2020-03-24)
#' spod_download_data(
#'   type = "od", zones = "municip",
#'   date_regex = "2020032[0-4]"
#' )
#' }
spod_download_data <- function(
    type = c(
      "od", "origin-destination",
      "os", "overnight_stays",
      "tpp", "trips_per_person"
    ),
    zones = c(
      "districts", "dist", "distr", "distritos",
      "municipalities", "muni", "municip", "municipios",
      "lau", "large_urban_areas", "gau", "grandes_areas_urbanas"
    ), # implement "urban_areas" for v2 data
    dates = NULL,
    data_dir = spod_get_data_dir(),
    quiet = FALSE,
    return_output = TRUE) {
  # convert english zone names to spanish words used in the default data paths
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  dates_to_use <- spod_dates_argument_to_dates_seq(dates = dates)


  # check version
  # replace this argument with automatic version detection based on the dates requested?
  ver <- spod_infer_data_v_from_dates(dates_to_use) # this leads to a second call to an internal spod_get_valid_dates() which in turn causes a second call to spod_available_data_v1() or spod_get_metadata(). This results in reading thedates_to_use <- spod_dates_argument_to_dates_seq(dates = dates) xml files with metadata for the second time. This is not optimal and should be fixed.
  if (isFALSE(quiet)) {
    message("Data version detected from dates: ", ver)
  }

  # convert english data type names to spanish words used in the default data paths
  type <- match.arg(type)
  type <- spod_match_data_type(type = type, ver = ver)



  # get the available  data list while checking for files already cached on disk
  if (ver == 1) {
    metadata <- spod_available_data_v1(
      data_dir = data_dir,
      check_local_files = TRUE
    )
  } else if (ver == 2) {
    metadata <- spod_get_metadata(data_dir = data_dir)
    # replace with spod_available_data_v2() when available, spod_get_metadata can become a wrapper with v1/v2 argument. Potentially we can even automaticaly detect the data version based on the time intervals that user requests, but this is a bit controversial, as the methodology behind v1 and v2 data generation is not the same and Nommon+MITMA do not recommend mixing those together and comparing absoloute numbers of trips.
  }

  # match the metadata to type, zones, version and dates
  if (ver == 1) {
    requested_files <- metadata[
      grepl(glue::glue("v{ver}.*{type}.*{zones}"), metadata$local_path) &
        metadata$data_ymd %in% dates_to_use,
    ]
  } else if (ver == 2) {
    requested_files <- metadata[
      grepl(glue::glue("v{ver}.*{zones}.*{type}"), metadata$local_path) &
        metadata$data_ymd %in% dates_to_use,
    ]
  }

  files_to_download <- requested_files[!requested_files$downloaded, ]

  # only download files if some are missing
  if (nrow(files_to_download) > 0) {
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
    requested_files$downloaded[requested_files$local_path %in% downloaded_files$destfile] <- TRUE

    message("Retrieved data for requested dates: ", paste(dates_to_use, collapse = ", ")) # this may output too many dates, shoudl be fixed when we create a flexible date argument processing function. Keeping for now.
  }

  if (return_output) {
    return(requested_files$local_path)
  }
}
