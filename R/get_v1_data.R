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
#' @param check_local_files Whether to check if the local files exist. Defaults to `FALSE`.
#' @param quiet Whether to suppress messages. Defaults to `FALSE`.
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
  check_local_files = FALSE,
  quiet = FALSE
) {
  xml_files_list <- fs::dir_ls(data_dir, type = "file", regexp = "data_links_v1") |> sort()
  if(length(xml_files_list) == 0) {
    if(isFALSE(quiet)) message("No data links xml files found, getting latest data links xml")
    latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
  } else {
    latest_data_links_xml_path <- utils::tail(xml_files_list, 1)
  }

  # Check if the XML file is 1 day old or older from its name
  file_date <- stringr::str_extract(latest_data_links_xml_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")

  if (file_date < format(Sys.Date(), format = "%Y-%m-%d")) {
    if(isFALSE(quiet)) message("File list xml is 1 day old or older, getting latest data links xml")
    latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
  } else {
    if(isFALSE(quiet)) message("Using existing data links xml: ", latest_data_links_xml_path)
  }

  if (length(latest_data_links_xml_path) == 0) {
    if(isFALSE(quiet)) message("Getting latest data links xml")
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
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`) or `"municipalities"` (or `"muni"`, `"municip"`).
#' @return A spatial object containing the zones data.
#' @export
#' @examples
#' if (FALSE) {
#'   zones <- spod_get_zones()
#' }
spod_get_zones_v1 <- function(
  zones = c("districts", "dist", "distr",
    "municipalities", "muni", "municip"),
  data_dir = spod_get_data_dir()
) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if shp files are already extracted
  expected_gpkg_path <- fs::path(data_dir, glue::glue("clean_data/v1//zones/{zones}_mitma.gpkg"))
  if (fs::file_exists(expected_gpkg_path)) {
    message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
    return(sf::read_sf(expected_gpkg_path))
  }

  # if data is not available, download, extract, clean and save it to gpkg

  metadata <- spod_available_data_v1(data_dir, check_local_files = FALSE)
  regex <- glue::glue("zonificacion_{zones}\\.")
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

  zones_path <- fs::dir_ls(data_dir, glob = glue::glue("**{zones}/*.shp"), recurse = TRUE)

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
#' This function retrieves the v1 (2020-2021) origin-destination data from the specified data directory.
#' @param read_fun The function to read the data. Defaults to `duckdb::tbl_file`.
#' @inheritParams spod_download_data
#' @return A tibble with the origin-destination data.
spod_get_od <- function(
  zones = c("districts", "dist", "distr",
    "municipalities", "muni", "municip"), # add "urban_areas" for v2 data
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  read_fun = duckdb::tbl_file
) {
  # Processing of the date arguments is performed in `spod_download_data()`
  
  zones <- match.arg(zones)
  
  # use the spot_download_data() function to download any missing data
  downloaded_files <- spod_download_data(
    type = "od",
    zones = zones,
    dates = dates,
    data_dir = data_dir
  )


  # read data from cached files
  
  
  
}
