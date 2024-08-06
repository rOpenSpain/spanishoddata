#' Get latest file list from the XML for MITMA open mobiltiy data v1 (2020-2021)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://opendata-movilidad.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @export
#' @examples
#' if (FALSE) {
#'   spod_get_latest_v1_file_list()
#' }
spod_get_latest_v1_file_list <- function(
    data_dir = spod_get_data_dir(),
    xml_url = "https://opendata-movilidad.mitma.es/RSS.xml") {
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_timestamp <- format(Sys.time(), format = "%Y-%m-%d", usetz = FALSE, tz = "UTC")
  current_filename <- glue::glue("{data_dir}/data_links_v1_{current_timestamp}.xml")

  message("Saving the file to: ", current_filename)
  xml_requested <- curl::curl_download(
    url = xml_url,
    destfile = current_filename,
    quiet = FALSE
  )
  return(current_filename)
}

#' Get the available v1 data list
#'
#' This function provides a table of the available data list of MITMA v1 (2020-2021), both remote and local.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @return A tibble with links, release dates of files in the data, dates of data coverage, local paths to files, and the download status.
#' \describe{
#'   \item{target_url}{\code{character}. The URL link to the data file.}
#'   \item{pub_ts}{\code{POSIXct}. The timestamp of when the file was published.}
#'   \item{file_extension}{\code{character}. The file extension of the data file (e.g., 'tar', 'gz').}
#'   \item{data_ym}{\code{Date}. The year and month of the data coverage, if available.}
#'   \item{data_ymd}{\code{Date}. The specific date of the data coverage, if available.}
#'   \item{local_path}{\code{character}. The local file path where the data is stored.}
#'   \item{downloaded}{\code{logical}. Indicator of whether the data file has been downloaded locally.}
#' }
#' @export
#' @examples
#' # Get the available v1 data list for the default data directory
#' if (FALSE) {
#'   metadata <- spod_available_data_v1()
#'   names(metadata)
#'   head(metadata)
#' }
spod_available_data_v1 <- function(data_dir = spod_get_data_dir(),
  # check_local_files (below) is FALSE by default to avoid excessive filesystem access, perhaps should be TRUE. Download functions use it to load the xml file, but we probably do not want the script to check all local cache directories every time we run a get data function. Perhaps it is better to offload this check to a separate function and have a csv file or some other way to keep track of the files that were downloaded and cached. An output of curl::multi_download() could be used for this purpose.
  check_local_files = FALSE
) {
  xml_files_list <- fs::dir_ls(data_dir, type = "file", regexp = "data_links_v1") |> sort()
  latest_data_links_xml_path <- utils::tail(xml_files_list, 1)

  # Check if the XML file is 1 day old or older from its name
  file_date <- stringr::str_extract(latest_data_links_xml_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")

  if (file_date < format(Sys.Date(), format = "%Y-%m-%d")) {
    message("File list xml is 1 day old or older, getting latest data links xml")
    latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
  } else {
    message("Using existing data links xml: ", latest_data_links_xml_path)
  }

  if (length(latest_data_links_xml_path) == 0) {
    message("Getting latest data links xml")
    latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
  }

  x_xml <- xml2::read_xml(latest_data_links_xml_path)

  files_table <- tibble::tibble(
    target_url = xml2::xml_find_all(x = x_xml, xpath = "//link") |> xml2::xml_text(),
    pub_date = xml2::xml_find_all(x = x_xml, xpath = "//pubDate") |> xml2::xml_text()
  )

  files_table$pub_ts <- lubridate::dmy_hms(files_table$pub_date)
  files_table$file_extension <- tools::file_ext(files_table$target_url)
  files_table <- files_table[files_table$file_extension != "", ]
  files_table$pub_date <- NULL

  files_table$data_ym <- lubridate::ym(stringr::str_extract(files_table$target_url, "[0-9]{4}-[0-9]{2}"))
  files_table$data_ymd <- lubridate::ymd(stringr::str_extract(files_table$target_url, "[0-9]{8}"))
  # order by pub_ts
  files_table <- files_table[order(files_table$pub_ts, decreasing = TRUE), ]
  files_table$local_path <- file.path(
    data_dir,
    stringr::str_replace(files_table$target_url, ".*mitma.es/", spod_subfolder_raw_data_cache(ver = 1))
  )

  files_table$local_path <- stringr::str_replace_all(files_table$local_path, "\\/\\/\\/|\\/\\/", "/")

  # change path for daily data files to be in hive-style format
  files_table$local_path <- gsub("([0-9]{4})-([0-9]{2})\\/[0-9]{6}([0-9]{2})_", "year=\\1\\/month=\\2\\/day=\\3\\/", files_table$local_path)

  # fix paths for files that are in '0000-referencia' folder
  files_table$local_path <- gsub("0000-referencia\\/([0-9]{4})([0-9]{2})([0-9]{2})_", "year=\\1\\/month=\\2\\/day=\\3\\/", files_table$local_path)
  
  # replace 2 digit month with 1 digit month
  files_table$local_path <- gsub("month=0([1-9])", "month=\\1", files_table$local_path)
  
  # replace 2 digit day with 1 digit day
  files_table$local_path <- gsub("day=0([1-9])", "day=\\1", files_table$local_path)

  # now check if any of local files exist
  files_table$downloaded <- fs::file_exists(files_table$local_path)

  return(files_table)
}

#' Retrieves the zones for v1 data
#'
#' This function retrieves the zones data from the specified data directory.
#' It can retrieve either "distritos" or "municipios" zones data.
#'
#' @param data_dir The directory where the data is stored.
#' @param type The type of zones data to retrieve ("distritos" or "municipios").
#' @return A spatial object containing the zones data.
#' @export
#' @examples
#' if (FALSE) {
#'   zones <- spod_get_zones()
#' }
spod_get_zones_v1 <- function(
  type = c("distritos", "municipios"),
  data_dir = spod_get_data_dir()
) {
  type <- match.arg(type)

  # check if shp files are already extracted
  expected_gpkg_path <- fs::path(data_dir, glue::glue("clean_data/v1//zones/{type}_mitma.gpkg"))
  if (fs::file_exists(expected_gpkg_path)) {
    message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
    return(sf::read_sf(expected_gpkg_path))
  }

  # if data is not available, download, extract, clean and save it to gpkg

  metadata <- spod_available_data_v1(data_dir, check_local_files = FALSE)
  regex <- glue::glue("zonificacion_{type}\\.")
  sel_zones <- stringr::str_detect(metadata$target_url, regex)
  metadata_zones <- metadata[sel_zones, ]
  dir_name <- fs::path_dir(metadata_zones$local_path[1])
  if (!fs::dir_exists(dir_name)) {
    fs::dir_create(dir_name, recurse = TRUE)
  }

  if (!fs::file_exists(metadata_zones$local_path)) {
    message("Downloading the file to: ", metadata_zones$local_path)
    downloaded_file <- curl::curl_download(metadata_zones$target_url, destfile = metadata_zones$local_path, mode = "wb", quiet = FALSE)
  } else {
    message("File already exists: ", metadata_zones$local_path)
    downloaded_file <- metadata_zones$local_path
  }

  message("Unzipping the file: ", downloaded_file)
  utils::unzip(downloaded_file,
    exdir = fs::path_dir(downloaded_file)
  )

  # remove artifacts (remove __MACOSX if exists)
  junk_path <- paste0(fs::path_dir(downloaded_file), "/__MACOSX")
  if (fs::dir_exists(junk_path)) fs::dir_delete(junk_path)

  zones_path <- fs::dir_ls(data_dir, glob = glue::glue("**{type}/*.shp"), recurse = TRUE)

  zones <- spod_clean_zones_v1(zones_path)
  fs::dir_create(fs::path_dir(expected_gpkg_path), recurse = TRUE)
  sf::st_write(zones, expected_gpkg_path, delete_dsn = TRUE, delete_layer = TRUE)

  return(zones)
}

#' Fixes common issues in the zones data and cleans up variable names
#' 
#' This function fixes any invalid geometries in the zones data and renames the "ID" column to "id".
#' 
#' @param zones_path The path to the zones spatial data file.
#' @return A spatial object of class `sf`.
#' @keywords internal
#' 
spod_clean_zones_v1 <- function(zones_path) {
  suppressWarnings({
    zones <- sf::read_sf(zones_path)
  })
  invalid_geometries <- !sf::st_is_valid(zones)
  if (sum(invalid_geometries) > 0) {
    fixed_zones <- sf::st_make_valid(zones[invalid_geometries, ])
    zones <- rbind(zones[!invalid_geometries, ], fixed_zones)
  }
  names(zones)[names(zones) == "ID"] <- "id"
  return(zones)
}

#' Retrieve the origin-destination v1 data (2020-2021)
#' 
#' This function retrieves the origin-destination data from the specified data directory.
#' 
spod_get_od_v1 <- function(
  date_range = c("2020-02-14", "2020-02-15"),
  dates_list = NULL,
  date_regex = NULL,
  zones = c("distritos", "municipios"),
  data_dir = spod_get_data_dir(),
  read_fun = duckdb::tbl_file
) {
  # Processing of the date arguments is performed in subsequent functions, because we would want the downloading functions to be able to use the same date arguments for flexibility. So `spod_download_tables()` will handle the date arguments.`
  
  zones <- match.arg(zones)
  
  # check the locally cached and online available data
  # and download missing files if any
  # get local paths of the files requested with date arguments
  metadata <- spod_download_od_v1(
    date_range = date_range,
    dates_list = dates_list,
    date_regex = date_regex,
    zones = zones
  )

  # read data from cached files
  
  
  
}



spod_download_od_v1 <- function(
  date_range = c("2020-02-14", "2020-02-15"),
  dates_list = NULL,
  date_regex = NULL,
  zones = c("districts", "dist", "distr",
    "municipalities", "muni", "municip"),
  data_dir = spod_get_data_dir()
) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)
  
  downloaded_files <- spod_download_data(
    date_range = date_range,
    dates_list = dates_list,
    date_regex = date_regex,
    subdir = glue::glue("v1/maestra2-mitma-{zones}/ficheros-diarios/"),
    data_dir = data_dir
  )

  return(downloaded_files)
}

#' Download the data files of specified type, zones, dates and data version
#' 
#' This function downloads the data files of the specified type, zones, dates and data version.
#' @param type The type of data to download. Can be "origin-destination" (or ust "od"), or "trips_per_person" (or just "tpp") for v1 data. For v2 data "overnight_stays" (or just "os") is also available. More data types to be supported in the future. See respective codebooks for more information. [ADD CODEBOOKS!]
#' @param zones The zones for which to download the data. Can be "districts" (or "dist", "distr") or "municipalities" (or "muni", "municip") for v1 data. Additionaly, these can be "urban_areas" (GAUs) for v2 data.
#' @param date_range A character vector of dates in ISO format (YYYY-MM-DD) to download the data for.
#' @param dates_list A character vector of dates in ISO format (YYYY-MM-DD) to download the data for. Defaults to NULL.
#' @param date_regex A regular expression to match the dates of the data to download. Defaults to NULL.
#' @param ver The version of the data to use. Defaults to 1. Can be 1 or 2.
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()` which returns the value of the environment variable `SPANISH_OD_DATA_DIR` or a temporary directory if the variable is not set.
#' 
#' @export 
#' @example 
#'\dontrun{
#' # Download the origin-destination on district level for the a date range in March 2020
#' spod_download_tables(type = "od", zones = "districts", date_range = c("2020-03-20", "2020-03-24"), ver = 1)
#' 
#' # Download the origin-destination on district level for select dates in 2020 and 2021
#' spod_download_tables(type = "od", zones = "districts", dates_list = c("2020-03-20", "2020-03-24", "2021-03-20", "2021-03-24"), ver = 1)
#' 
#' # Download the origin-destination on district level using regex for a date range in March 2020 (the regex will capture the dates 2020-03-20 to 2020-03-24)
#' spod_download_tables(type = "od", zones = "districts", date_regex = "2020032[0-4]", ver = 1)
#' }
spod_download_data <- function(
  type = c(
    "od", "origin-destination",
    "os", "overnight_stays",
    "tpp", "trips_per_person"),
  zones = c("districts", "dist", "distr",
    "municipalities", "muni", "municip"), # add "urban_areas" for v2 data
  date_range = NULL,
  dates_list = NULL,
  date_regex = NULL,
  ver = 1, # infer version from dates?
  data_dir = spod_get_data_dir()
) {
  # convert english data type names to spanish words used in the default data paths
  type <- match.arg(type)
  type <- spod_match_data_type(type = type, ver = ver)
  
  # convert english zone names to spanish words used in the default data paths
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)
  


  # this is where the date arguments are processed
  # for all the wrapper functions that call this worker function
  dates_to_use <- process_date_arguments(
    date_range = date_range,
    dates_list = dates_list,
    date_regex = date_regex,
    ver = ver
  )

  # check version
  # replace this argument with automatic version detection based on the dates requested?
  rlang:::check_number_whole(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  
  # get the available  data list while checking for files already cached on disk
  if( ver == 1) {
    metadata <- spod_available_data_v1(data_dir = data_dir,
      check_local_files = TRUE)
  } else if (ver == 2) {
    metadata <- spod_get_metadata(data_dir = data_dir)
    # replace with spod_available_data_v2() when available, spod_get_metadata can become a wrapper with v1/v2 argument. Potentially we can even automaticaly detect the data version based on the time intervals that user requests, but this is a bit controversial, as the methodology behind v1 and v2 data generation is not the same and Nommon+MITMA do not recommend mixing those together and comparing absoloute numbers of trips.
  }
  
  # match the metadata to type, zones, version and dates
  if(ver == 1){
    requested_files <- metadata[
      grepl(glue::glue("v{ver}.*{type}.*{zones}"), metadata$local_path) &
      metadata$data_ymd %in% dates_to_use,
    ]
  } else if(ver == 2){
    requested_files <- metadata[
      grepl(glue::glue("v{ver}.*{zones}.*{type}"), metadata$local_path) &
      metadata$data_ymd %in% dates_to_use,
    ]
  }

  files_to_download <- requested_files[!requested_files$downloaded, ]
  
  # pre-generate target paths for the files to download
  fs::dir_create(
    unique(fs::path_dir(files_to_download$local_path)),
    recurse = TRUE)

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

  return(requested_files$local_path)
}