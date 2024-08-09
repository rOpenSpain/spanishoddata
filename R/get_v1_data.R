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
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`).
#' @param quiet Whether to suppress messages. Defaults to `FALSE`.
#' @return A spatial object containing the zones data.
#' @export
#' @examples
#' if (FALSE) {
#'   zones <- spod_get_zones()
#' }
spod_get_zones_v1 <- function(
  zones = c("districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios"),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if shp files are already extracted
  expected_gpkg_path <- fs::path(data_dir, glue::glue("clean_data/v1//zones/{zones}_mitma.gpkg"))
  if (fs::file_exists(expected_gpkg_path)) {
    if (isFALSE(quiet)) message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
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
    if (isFALSE(quiet)) message("Downloading the file to: ", metadata_zones$local_path)
    downloaded_file <- curl::curl_download(metadata_zones$target_url, destfile = metadata_zones$local_path, mode = "wb", quiet = FALSE)
  } else {
    if (isFALSE(quiet)) message("File already exists: ", metadata_zones$local_path)
    downloaded_file <- metadata_zones$local_path
  }

  if (isFALSE(quiet)) message("Unzipping the file: ", downloaded_file)
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


#' Load the origin-destination v1 data (2020-2021) for specified dates
#' 
#' This function retrieves the v1 (2020-2021) origin_destination_data for the specified dates. It checks if the requested data is already cached locally and downloads it if it is not. When all the requested data is cached, it creates a `DuckDB` connection to the cache data folder and provides an table
#' @inheritParams spod_download_data
#' @return A duckdb table connection object. It can be manupulated using `dplyr` verbs, or can be loaded into memory using `dplyr::collect()`. The structure of the object is as follows:
#' 
#' \describe{
#'   \item{full_date}{\code{Date}. The full date of the trip, including year, month, and day.}
#'   \item{id_origin}{\code{factor}. The identifier for the origin location of the trip, formatted as a code (e.g., '01001_AM').}
#'   \item{id_destination}{\code{factor}. The identifier for the destination location of the trip, formatted as a code (e.g., '01001_AM').}
#'   \item{activity_origin}{\code{factor}. The type of activity at the origin location (e.g., 'home', 'work').}
#'   \item{activity_destination}{\code{factor}. The type of activity at the destination location (e.g., 'home', 'other').}
#'   \item{residence_province}{\code{factor}. The province of residence for the individual making the trip (e.g. 'Cuenca', 'Girona').}
#'   \item{time_slot}{\code{integer}. The time slot during which the trip started, represented as an integer (e.g., 0, 1, 2).}
#'   \item{distance}{\code{factor}. The distance category of the trip, represented as a code (e.g., '002-005' for 2-5 km).}
#'   \item{n_trips}{\code{double}. The number of trips taken within the specified time slot and distance.}
#'   \item{trips_total_length_km}{\code{double}. The total length of all trips in kilometers for the specified time slot and distance.}
#'   \item{year}{\code{double}. The year of the trip.}
#'   \item{month}{\code{double}. The month of the trip.}
#'   \item{day}{\code{double}. The day of the trip.}
#' }
#' @export 
spod_get_od_v1 <- function(
  zones = c("districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios"),
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  # hardcode od as this is a wrapper to get origin-destiation data
  type <- "od"
  
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)
  
  dates <- spod_dates_argument_to_dates_seq(dates = dates)
  
  # use the spot_download_data() function to download any missing data
  spod_download_data(
    type = type,
    zones = zones,
    dates = dates,
    data_dir = data_dir,
    return_output = FALSE
  )

  # attach the od folder with predefined and cleaned up data types
  con <- spod_duckdb_od_v1(
    zones = zones,
    dates = dates,
    data_dir = data_dir
  )

  # DBI::dbListTables(con) # for debugging only
  # dplyr::tbl(con, "trips_view") |> dplyr::glimpse() # for debugging only
  # DBI::dbDisconnect(con) # for debugging only
  
  # speed comparison REMOVE a bit later AFTER TESTING
  # b1 <- bench::mark(iterations = 5, check = FALSE,
  #   hive_date = {dplyr::tbl(con, "trips") |>
  #     dplyr::distinct(full_date) |> 
  #     dplyr::collect()}, # this is prefiltered using custom SQL query using only the columns (year, month, day) that we know are constructed from the hive style partitioning
  #   full_date = {dplyr::tbl(con, "trips_view") |>
  #     dplyr::filter(full_date %in% dates) |>
  #     dplyr::distinct(full_date) |>
  #     dplyr::collect()} # this is causing DuckDB to scan ALL csv.gz files in the folder because it has to match the desired dates with full_date column
  # )
  # bench:::plot.bench_mark(b1, type = "violin") + ggpubr::theme_pubclean(base_size = 24)
  
  # perhaps let's not confuse the user with the duckdb connection, see help for the @return of the spod_duckdb_od_v1() function
  # return(con)

  # return the tbl conection for user friendly data manipulation
  # this may have an implication that there is no way for the user to properly disconnect the db connection, should think how this can be addressed
  # not a problem! can be done with:
  # DBI::dbDisconnect(od$src$con)
  
  return(dplyr::tbl(con, "od_filtered"))
}
