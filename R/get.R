#' Get latest file list from the XML for MITMA open mobiltiy data v2 (2022 onwards)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://movilidad-opendata.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @export
#' @examples
#' if (FALSE) {
#'   spod_get_latest_v2_file_list()
#' }
spod_get_latest_v2_file_list <- function(
    data_dir = spod_get_data_dir(),
    xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
) {
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue("{data_dir}/data_links_v2_{current_date}.xml")

  message("Saving the file to: ", current_filename)
  xml_requested <- curl::curl_download(url = xml_url, destfile = current_filename, quiet = FALSE)
  return(current_filename)
}

#' Get the data dictionary
#'
#' This function retrieves the data dictionary for the specified data directory.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param quiet Whether to suppress messages. Defaults to `FALSE`.
#' @return The data dictionary.
#' @export
#' @examples
#' # Get the data dictionary for the default data directory
#' if (FALSE) {
#'   metadata <- spod_available_data_v2()
#'   names(metadata)
#'   head(metadata)
#' }
spod_available_data_v2 <- function(
  data_dir = spod_get_data_dir(),
  check_local_files = FALSE,
  quiet = FALSE
) {
  xml_files_list <- fs::dir_ls(data_dir, type = "file", regexp = "data_links_v2") |> sort()
  if (length(xml_files_list) == 0) {
    if (isFALSE(quiet)) {
      message("No data links xml files found, getting latest v2 data links xml.")
    }
    latest_data_links_xml_path <- spod_get_latest_v2_file_list(data_dir = data_dir)
  } else {
    latest_data_links_xml_path <- utils::tail(xml_files_list, 1)
  }

  # Check if the XML file is 1 day old or older from its name
  file_date <- stringr::str_extract(latest_data_links_xml_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  
  if (file_date < format(Sys.Date(), format = "%Y-%m-%d")) {
    if (isFALSE(quiet)) {
      message("File list xml is 1 day old or older, getting latest data links xml")
    }
    latest_data_links_xml_path <- spod_get_latest_v2_file_list(data_dir = data_dir)
  } else {
    if (isFALSE(quiet)) {
      message("Using existing data links xml: ", latest_data_links_xml_path)
    }
  }

  if (length(latest_data_links_xml_path) == 0) {
    if (isFALSE(quiet)) {
      message("Getting latest data links xml")
    }
    latest_data_links_xml_path <- spod_get_latest_v2_file_list(data_dir = data_dir)
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
    stringr::str_replace(files_table$target_url, ".*mitma.es/",
      spod_subfolder_raw_data_cache(ver = 2))
  )
  files_table$local_path <- stringr::str_replace_all(files_table$local_path, "\\/\\/\\/|\\/\\/", "/")

  # change path for daily data files to be in hive-style format
  # TODO: check if this is needed for estudios completo and rutas
  files_table$local_path <- gsub("([0-9]{4})-([0-9]{2})\\/[0-9]{6}([0-9]{2})_", "year=\\1\\/month=\\2\\/day=\\3\\/", files_table$local_path)

  # replace 2 digit month with 1 digit month
  files_table$local_path <- gsub("month=0([1-9])", "month=\\1", files_table$local_path)

  # replace 2 digit day with 1 digit day
  files_table$local_path <- gsub("day=0([1-9])", "day=\\1", files_table$local_path)

  # now check if any of local files exist
  if( check_local_files == TRUE){
    files_table$downloaded <- fs::file_exists(files_table$local_path)
  }

  return(files_table)
}

#' Get the data directory
#'
#' This function retrieves the data directory from the environment variable SPANISH_OD_DATA_DIR.
#' If the environment variable is not set, it returns the temporary directory.
#' @param quiet Logical. If `TRUE`, the function does not print messages to the console. Defaults to `FALSE`.
#' @return The data directory.
#' @export
#' @keywords internal
spod_get_data_dir <- function(quiet = FALSE) {
  data_dir_env <- Sys.getenv("SPANISH_OD_DATA_DIR")
  if (data_dir_env == "") {
    if (isFALSE(quiet)) warning("Warning: SPANISH_OD_DATA_DIR is not set. Using the temporary directory, which is not recommended, as the data will be deleted when the session ends.\n\n To set the data directory, use `Sys.setenv(SPANISH_OD_DATA_DIR = '/path/to/data')` or set SPANISH_OD_DATA_DIR permanently in the environment by editing the `.Renviron` file locally for current project with `usethis::edit_r_environ('project')` or `file.edit('.Renviron')` or globally for all projects with `usethis::edit_r_environ('user')` or `file.edit('~/.Renviron')`.")
    data_dir_env <- tempdir() # if not set, use the temp directory
  }
  # check if dir exists and create it if it doesn't
  if (!fs::dir_exists(data_dir_env)) {
    fs::dir_create(data_dir_env)
  }
  return(fs::path_real(data_dir_env))
}

#' Retrieves the zones data
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
spod_get_zones_v2 <- function(
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lau", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if gpkg files are already saved and load them if available
  # expected_gpkg_path
  # TODO after done with the download part
  
  # if no existing gpkg found above, contunue here with download and data cleanup
  metadata <- spod_available_data_v2(data_dir, check_local_files = TRUE)
  zones_regex <- glue::glue("(zonificacion_{zones}\\.*)|(poblacion\\.csv)|(relacion_ine_zonificacionMitma\\.csv)")
  sel_zones <- stringr::str_detect(metadata$target_url, zones_regex)
  metadata_zones <- metadata[sel_distritos, ]
  metadata_zones <- metadata_zones[metadata_zones$downloaded == FALSE, ]
  dir_names <- unique(fs::path_dir(metadata_zones$local_path))
  if (any(!fs::dir_exists(dir_names))) {
    fs::dir_create(dir_names, recurse = TRUE)
  }
  if (isFALSE(quiet)) {
    message("Downloading missing zones data...")
    curl::multi_download(
      urls = metadata_zones$target_url,
      destfiles = metadata_zones$local_path,
      resume = TRUE,
      progress = TRUE
    )
  }
  
  sel_shp <- stringr::str_detect(
    metadata_zones$local_path,
    glue::glue("zonificacion_{zones}\\.shp$")
  )
  shp_file <- metadata_zones$local_path[sel_shp]
  suppressWarnings({
    return(sf::read_sf(shp_file))
  })
}

#' Retrieves the origin-destination data
#'
#' This function downloads data from URLs such as
#' https://movilidad-opendata.mitma.es/estudios_basicos/por-distritos/viajes/ficheros-diarios/2024-03/20240301_Viajes_distritos.csv.gz
#' if the file does not exist in the data directory.
#'
#' @param data_dir The directory where the data is stored.
#' @param subdir The subdirectory where the data is stored.
#' @param date_regex The regular expression to match the date of the data to download.
#' @param read_fun The function to read the data. Defaults to `duckdb::tbl_file`.
#' @return The local path of the downloaded file (`download_od`), or a data frame with the origin-destination data (`spod_get`).
#' @export
#' @examples
#' # Download the origin-destination data for the first two days of March 2024
#' if (FALSE) {
#'   od_20240301_20240302 <- spod_get(date_regex = "2024-03-0[1-2]")
#' }
spod_get <- function(
    data_dir = spod_get_data_dir(),
    subdir = "estudios_basicos/por-distritos/viajes/ficheros-diarios",
    date_regex = "2024030[1-2]",
    read_fun = duckdb::tbl_file) {
  file_paths <- download_od(data_dir = data_dir, subdir = subdir, date_regex = date_regex)
  if (identical(read_fun, readr::read_csv)) {
    return(purrr::map_dfr(file_paths, read_fun))
  }
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv)
  # file.exists(file_paths[1])
  # od1 = duckdb::tbl_file(con, file_paths[2])
  od_list <- purrr::map(file_paths, ~ duckdb::tbl_file(con, .))
}
download_od <- function(
    data_dir = spod_get_data_dir(),
    subdir = "estudios_basicos/por-distritos/viajes/ficheros-diarios",
    date_regex = "2024030[1-2]") {
  regex <- glue::glue("{subdir}*.+{date_regex}_Viajes_distritos.csv.gz")
  metadata <- spod_available_data_v2(data_dir)
  sel_od <- stringr::str_detect(metadata$target_url, regex)
  metadata_od <- metadata[sel_od, ]
  metadata_od[[1]]
  dir_name <- dirname(metadata_od$local_path[1])
  if (!fs::dir_exists(dir_name)) {
    fs::dir_create(dir_name)
  }
  for (i in 1:nrow(metadata_od)) {
    if (!fs::file_exists(metadata_od$local_path[i])) {
      message("Downloading ", metadata_od$target_url[i])
      curl::curl_download(url = metadata_od$target_url[i], destfile = metadata_od$local_path[i], quiet = FALSE)
    }
  }
  return(metadata_od$local_path)
}
