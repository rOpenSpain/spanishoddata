#' Get zones
#' 
#' @description
#' Get spatial zones for the specified data version. Supports both v1 (2020-2021) and v2 (2022 onwards) data.
#' 
#' @inheritParams spod_download_data
#' @inheritParams spod_available_data
#' @return An `sf` object (Simple Feature collection).
#' 
#' The columns include (for both v1 (2020-2021) and v2 (2022 onwards) data:
#' \describe{
#'   \item{id}{A character vector containing the unique identifier for each zone, to be matched with identifiers in the tabular data.}
#'   \item{geometry}{A `MULTIPOLYGON` column containing the spatial geometry of each zone, stored as an sf object.
#'   The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
#' }
#' Additionally, for v2 (2022 onwards) data:
#' \describe{
#'   \item{name}{A character vector with the name of the zone.}
#'   \item{population}{A numeric vector representing the population of each zone (as of 2022).}
#' }
#' @export
spod_get_zones <- function(
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  ver = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  ver <- as.integer(ver) # todo: add type safety check
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }

  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  if (ver == 1) {
    zones_sf <- spod_get_zones_v1(zones = zones, data_dir = data_dir, quiet = quiet)
  } else if (ver == 2) {
    zones_sf <- spod_get_zones_v2(zones = zones, data_dir = data_dir, quiet = quiet)
  }

  return(zones_sf)
}

#' Get available data list
#' 
#' Get a table with links to available data files for the specified data version. Optionally check (see arguments) if certain files have already been downloaded into the cache directory specified with SPANISH_OD_DATA_DIR environment variable or a custom path specified with `data_dir` argument.
#' 
#' @param ver Integer. Can be 1 or 2. The version of the data to use. v1 spans 2020-2021, v2 covers 2022 and onwards.
#' @inheritParams spod_available_data_v1
#' @inheritParams global_quiet_param
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
spod_available_data <- function(
  ver = 2,
  check_local_files = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir()
) {
  ver <- as.integer(ver) # todo: add type safety check
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }

  if (ver == 1) {
    return(spod_available_data_v1(data_dir = data_dir, check_local_files = check_local_files, quiet = quiet))
  } else if (ver == 2) {
    return(spod_available_data_v2(data_dir = data_dir, check_local_files = check_local_files, quiet = quiet))
  }
}

#' Get latest file list from the XML for MITMA open mobility data v2 (2022 onwards)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://movilidad-opendata.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @examples
#' if (FALSE) {
#'   spod_get_latest_v2_file_list()
#' }
#' @keywords internal
spod_get_latest_v2_file_list <- function(
    data_dir = spod_get_data_dir(),
    xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
) {
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}/data_links_v2_{current_date}.xml")

  # ensure dir exists
  if (!fs::dir_exists(dirname(current_filename))) {
    fs::dir_create(dirname(current_filename), recurse = TRUE)
  }

  message("Saving the file to: ", current_filename)
  xml_requested <- curl::curl_download(url = xml_url, destfile = current_filename, quiet = FALSE)
  return(current_filename)
}

#' Get the data dictionary
#'
#' This function retrieves the data dictionary for the specified data directory.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @inheritParams spod_available_data_v1
#' @inheritParams global_quiet_param
#' @inherit spod_available_data return
#' @importFrom rlang .data
#' @examples
#' # Get the data dictionary for the default data directory
#' if (FALSE) {
#'   metadata <- spod_available_data_v2()
#'   names(metadata)
#'   head(metadata)
#' }
#' @keywords internal
spod_available_data_v2 <- function(
  data_dir = spod_get_data_dir(),
  check_local_files = FALSE,
  quiet = FALSE
) {
  xml_files_list <- fs::dir_ls(glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}"), type = "file", regexp = "data_links_v2") |> sort()
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

  # lowercase GAU to avoid problems with case-sensitive matching
  files_table$local_path <- gsub("GAU", "gau", files_table$local_path)

  # now check if any of local files exist
  if( check_local_files == TRUE){
    files_table$downloaded <- fs::file_exists(files_table$local_path)
  }

  # add known file sizes from cached data
  file_sizes <- readr::read_csv(system.file("extdata", "url_file_sizes_v2.txt.gz", package = "spanishoddata"), show_col_types = FALSE)
  files_table <- dplyr::left_join(files_table, file_sizes, by = "target_url")

  # if there are files with missing sizes, impute them
  if (any(is.na(files_table$remote_file_size_mb))) {
    # impute uknown file sizes
    # primitive file categorisation 
    files_table <- files_table |>
      dplyr::mutate(
        cleaned_url = stringr::str_remove_all(.data$target_url, "/[0-9]{4}[-_][0-9]{2}[-_][0-9]{2}|/[0-9]{6,8}") |> 
                      stringr::str_remove("/[^/]+$"),
        file_category = dplyr::case_when(
          stringr::str_detect(.data$cleaned_url, "calidad") ~ "quality",
          stringr::str_detect(.data$cleaned_url, "rutas") ~ "routes",
          stringr::str_detect(.data$cleaned_url, "estudios_basicos") ~ paste0(
            "basic_studies_",
            dplyr::case_when(
              stringr::str_detect(.data$cleaned_url, "por-distritos") ~ "district_",
              stringr::str_detect(.data$cleaned_url, "por-municipios") ~ "municipal_",
              stringr::str_detect(.data$cleaned_url, "por-GAU") ~ "GAU_",
              TRUE ~ "unknown_"
            ),
            dplyr::case_when(
              stringr::str_detect(.data$cleaned_url, "viajes") ~ "trips_",
              stringr::str_detect(.data$cleaned_url, "personas") ~ "people_",
              stringr::str_detect(.data$cleaned_url, "pernoctaciones") ~ "overnight_",
              TRUE ~ "unknown_"
            ),
            ifelse(stringr::str_detect(.data$cleaned_url, "ficheros-diarios"), "daily", "monthly")
          ),
          TRUE ~ "other"
        )
      ) |>
      dplyr::select(-.data$cleaned_url)

    # Calculate mean file sizes by category
    size_by_file_category <- files_table |>
      dplyr::group_by(.data$file_category) |>
      dplyr::summarise(mean_file_size_mb = mean(.data$remote_file_size_mb, na.rm = TRUE))

    # Impute missing file sizes
    files_table <- dplyr::left_join(files_table, size_by_file_category, by = "file_category")
    files_table$remote_file_size_mb[is.na(files_table$remote_file_size_mb)] <- files_table$mean_file_size_mb
    files_table$mean_file_size_mb <- NULL
    files_table$file_category <- NULL
  }

  return(files_table)
}

#' Get the data directory
#'
#' This function retrieves the data directory from the environment variable SPANISH_OD_DATA_DIR.
#' If the environment variable is not set, it returns the temporary directory.
#' @inheritParams global_quiet_param
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

#' retrieves the zones data
#'
#' This function retrieves the zones data from the specified data directory.
#' It can retrieve either "distritos" or "municipios" zones data.
#'
#' @param data_dir The directory where the data is stored.
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`).
#' @inheritParams global_quiet_param
#' @return An `sf` object (Simple Feature collection) with 4 fields:
#' \describe{
#'   \item{id}{A character vector containing the unique identifier for each zone, to be matched with identifiers in the tabular data.}
#'   \item{name}{A character vector with the name of the zone.}
#'   \item{population}{A numeric vector representing the population of each zone (as of 2022).}
#'   \item{geometry}{A `MULTIPOLYGON` column containing the spatial geometry of each zone, stored as an sf object.
#'   The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
#' }
#' @examples
#' if (FALSE) {
#'   zones <- spod_get_zones_v2()
#' }
#' @keywords internal
spod_get_zones_v2 <- function(
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if gpkg files are already saved and load them if available
  expected_gpkg_path <- fs::path(
    data_dir,
    glue::glue(spod_subfolder_clean_data_cache(ver = 2),
      "/zones/{zones}_mitma.gpkg"
    )
  )
  if (fs::file_exists(expected_gpkg_path)) {
    if (isFALSE(quiet)) {
      message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
    }
    return(sf::read_sf(expected_gpkg_path))
  }
  
  # if no existing gpkg found above, contunue here with download and data cleanup
  metadata <- spod_available_data_v2(data_dir, check_local_files = TRUE)
  zones_regex <- glue::glue("(zonificacion_{zones}\\.*)|(poblacion\\.csv)|(relacion_ine_zonificacionMitma\\.csv)")
  sel_zones <- stringr::str_detect(metadata$local_path, zones_regex)
  metadata_zones <- metadata[sel_zones, ]
  metadata_zones_for_download <- metadata_zones[metadata_zones$downloaded == FALSE, ]
  if (nrow(metadata_zones_for_download) > 0){
    dir_names <- unique(fs::path_dir(metadata_zones_for_download$local_path))
    if (any(!fs::dir_exists(dir_names))) {
      fs::dir_create(dir_names, recurse = TRUE)
    }
    if (isFALSE(quiet)) {
      message("Downloading missing zones data...")
      curl::multi_download(
        urls = metadata_zones_for_download$target_url,
        destfiles = metadata_zones_for_download$local_path,
        resume = TRUE,
        progress = TRUE
      )
    }
  }
  
  zones_path <- fs::dir_ls(
    path = fs::path(data_dir, spod_subfolder_raw_data_cache(ver = 2)),
    regexp = glue::glue("zonificacion_{tolower(zones)}s?\\.shp$"),
    recurse = TRUE
  )

  zones_sf <- spod_clean_zones_v2(zones_path)
  fs::dir_create(fs::path_dir(expected_gpkg_path), recurse = TRUE)
  sf::st_write(
    zones_sf,
    expected_gpkg_path,
    delete_dsn = TRUE,
    delete_layer = TRUE
  )

  return(zones_sf)
  }

#' Fixes common issues in the zones data and cleans up variable names
#'
#' This function fixes any invalid geometries in the zones data and renames the "ID" column to "id". It also attacches the population counts and zone names provided in the csv files supplied by the original data provider.
#'
#' @param zones_path The path to the zones spatial data file.
#' @return A spatial object containing the cleaned zones data. 
#' @keywords internal
#'
spod_clean_zones_v2 <- function(zones_path) {
  # detect what kind of zones find out if it is distritos, municipios or GAU
  zones <- stringr::str_extract(zones_path, "distritos|municipios|gaus")
  
  suppressWarnings({
    zones_sf <- sf::read_sf(zones_path)
  })

  # fix geometry
  invalid_geometries <- !sf::st_is_valid(zones_sf)
  if (sum(invalid_geometries) > 0) {
    fixed_zones_sf <- sf::st_make_valid(zones_sf[invalid_geometries, ])
    zones_sf <- rbind(zones_sf[!invalid_geometries, ], fixed_zones_sf)
  }

  # lowercase id column name
  names(zones_sf)[names(zones_sf) == "ID"] <- "id"

  population <- readr::read_delim(
    glue::glue(fs::path_dir(zones_path), "/poblacion_{zones}.csv"),
    delim = "|",
    col_names = c("id", "population"),
    col_types = c("c", "i")
  )

  if (zones %in% c("distritos","gaus")) {
    zone_names <- readr::read_delim(
      glue::glue(fs::path_dir(zones_path), "/nombres_{zones}.csv"),
      skip = 1,
      delim = "|",
      col_names = c("id", "name"),
      col_types = c("c", "i")
    )
  } else if (zones == "municipios") {
    zone_names <- readr::read_delim(
      glue::glue(fs::path_dir(zones_path), "/nombres_{zones}.csv"),
      skip = 1,
      delim = "|",
      col_names = c("row", "id", "name"),
      col_types = c("i", "c", "i")
    ) |> 
      dplyr::select(-"row")
  }
 
  # combine zones with population and names
  zones_sf <- zones_sf |>
    dplyr::left_join(zone_names, by = "id") |> 
    dplyr::left_join(population, by = "id") |> 
    dplyr::select(-"geometry", "geometry")
  
  return(zones_sf)
}
