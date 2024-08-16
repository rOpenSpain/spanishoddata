#' Get latest file list from the XML for MITMA open mobiltiy data v1 (2020-2021)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://opendata-movilidad.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @examples
#' if (FALSE) {
#'   spod_get_latest_v1_file_list()
#' }
spod_get_latest_v1_file_list <- function(
    data_dir = spod_get_data_dir(),
    xml_url = "https://opendata-movilidad.mitma.es/RSS.xml"
) {
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue("{data_dir}/data_links_v1_{current_date}.xml")

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
#' @inheritParams global_quiet_param
#' @inherit spod_available_data return
#' @importFrom rlang .data
#' @examples
#' # Get the available v1 data list for the default data directory
#' if (FALSE) {
#'   metadata <- spod_available_data_v1()
#'   names(metadata)
#'   head(metadata)
#' }
spod_available_data_v1 <- function(
    data_dir = spod_get_data_dir(),
    # check_local_files (below) is FALSE by default to avoid excessive filesystem access, perhaps should be TRUE. Download functions use it to load the xml file, but we probably do not want the script to check all local cache directories every time we run a get data function. Perhaps it is better to offload this check to a separate function and have a csv file or some other way to keep track of the files that were downloaded and cached. An output of curl::multi_download() could be used for this purpose.
    check_local_files = FALSE,
    quiet = FALSE) {
  xml_files_list <- fs::dir_ls(data_dir, type = "file", regexp = "data_links_v1") |> sort()
  if (length(xml_files_list) == 0) {
    if (isFALSE(quiet)) {
      message("No data links xml files found, getting latest v1 data links xml")
    }
    latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
  } else {
    latest_data_links_xml_path <- utils::tail(xml_files_list, 1)
  }

  # Check if the XML file is 1 day old or older from its name
  file_date <- stringr::str_extract(latest_data_links_xml_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")

  if (file_date < format(Sys.Date(), format = "%Y-%m-%d")) {
    if (isFALSE(quiet)) {
      message("File list xml is 1 day old or older, getting latest data links xml")
    }
    latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
  } else {
    if (isFALSE(quiet)) {
      message("Using existing data links xml: ", latest_data_links_xml_path)
    }
  }

  if (length(latest_data_links_xml_path) == 0) {
    if (isFALSE(quiet)) {
      message("Getting latest data links xml")
    }
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

  # change txt.gz to csv.gz
  files_table$local_path <- gsub("\\.txt\\.gz", "\\.csv\\.gz", files_table$local_path)

  # now check if any of local files exist
  if( check_local_files == TRUE){
    files_table$downloaded <- fs::file_exists(files_table$local_path)
  }

  # add known file sizes from cached data
  file_sizes <- readr::read_csv(system.file("extdata", "url_file_sizes_v1.txt.gz", package = "spanishoddata"), show_col_types = FALSE)
  files_table <- dplyr::left_join(files_table, file_sizes, by = "target_url")

  # if there are files with missing sizes, impute them
  if (any(is.na(files_table$remote_file_size_mb))) {
    # impute uknown file sizes
    # primitive file categorisation
    # Extract file category from the target URL
    files_table <- files_table |>
      dplyr::mutate(
        file_category = stringr::str_extract(.data$target_url, "\\/maestra(\\d)-mitma-(distritos|municipios)\\/(ficheros-diarios|meses-completos)\\/")
      )

    # Set other category for non-categorized files
    files_table$file_category[is.na(files_table$file_category)] <- "other"

    # Calculate mean file sizes by category
    size_by_file_category <- files_table |>
      dplyr::group_by(.data$file_category) |>
      dplyr::summarise(mean_file_size_mb = mean(.data$remote_file_size_mb, na.rm = TRUE))

    # Impute missing file sizes
    files_table <- dplyr::left_join(files_table, size_by_file_category, by = "file_category")
    files_table$remote_file_size_mb[is.na(files_table$remote_file_size_mb)] <- files_table$mean_file_size_mb

    # Clean up temporary columns
    files_table <- files_table |>
      dplyr::select(-.data$mean_file_size_mb, -.data$file_category)
  }

  return(files_table)
}

#' Retrieves the zones for v1 data
#'
#' This function retrieves the zones data from the specified data directory.
#' It can retrieve either "distritos" or "municipios" zones data.
#'
#' @param data_dir The directory where the data is stored.
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`).
#' @inheritParams global_quiet_param
#' @return An `sf` object (Simple Feature collection) with 2 fields:
#' \describe{
#'   \item{id}{A character vector containing the unique identifier for each zone, to be matched with identifiers in the tabular data.}
#'   \item{geometry}{A `MULTIPOLYGON` column containing the spatial geometry of each zone, stored as an sf object.
#'   The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
#' }
#' @examples
#' if (FALSE) {
#'   zones <- spod_get_zones_v1()
#' }
spod_get_zones_v1 <- function(
    zones = c(
      "districts", "dist", "distr", "distritos",
      "municipalities", "muni", "municip", "municipios"
    ),
    data_dir = spod_get_data_dir(),
    quiet = FALSE
  ) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if gpkg files are already saved and load them if available
  expected_gpkg_path <- fs::path(
    data_dir,
    glue::glue(spod_subfolder_clean_data_cache(ver = 1),
      "/zones/{zones}_mitma.gpkg"
    )
  )
  if (fs::file_exists(expected_gpkg_path)) {
    if (isFALSE(quiet)) {
      message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
    }
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

  zones_path <- fs::dir_ls(
    path = fs::path(data_dir, spod_subfolder_raw_data_cache(ver = 1)),
    glob = glue::glue("*v1**{zones}/*.shp"),
    recurse = TRUE
  )
  zones_sf <- spod_clean_zones_v1(zones_path)
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
#' This function fixes any invalid geometries in the zones data and renames the "ID" column to "id".
#'
#' @param zones_path The path to the zones spatial data file.
#' @return A spatial object containing the cleaned zones data. 
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

  # bug fix for municipalities
  # if (grepl("municipios", zones_path)){
  #   zones[zones$id == "0490201",]$id <- "04902"
  #   zones[zones$id == "2800601",]$id <- "28006"
  #   zones[zones$id == "2810601",]$id <- "28106"
  #   zones[zones$id == "2812301",]$id <- "28123"
  #   zones[zones$id == "2812701",]$id <- "28127"
  # }

  return(zones)
}


#' Load the origin-destination data for specified dates
#'
#' This function retrieves the v1 (2020-2021) or v2 (2022 and onwards) origin-destination data for the specified dates. It checks if the requested data is already cached locally and downloads only missing files. When all the requested data is cached, it creates a `DuckDB` connection to the cache data folder and provides an table
#'
#' @inheritParams spod_download_data
#' @inheritParams spod_duckdb_limit_resources
#' @inheritParams global_quiet_param
#' @return A DuckDB table connection object. It can be manupulated using `dplyr` verbs, or can be loaded into memory using `dplyr::collect()`. The structure of the object is as follows:
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
#'
#' This object also contains the reference to the source DuckDB conneciton with the full view of the cached data. It can be accessed using `od_table$src$con`. See examples below. The connection includes two views:
#'
#'
#'  * `od_csv_raw` - a raw table view of all cached CSV files with the origin-destination data that has been previously cached in $SPANISH_OD_DATA_DIR
#'
#'  * `od_csv_clean` - a cleaned-up table view of `od_csv_raw` with column names and values translated and mapped to English. This still includes all cached data.
#'
#' View `od_csv_clean` has the same structure as the filtered view 'od_filtered', which is returned by `spod_get_od()` as a DuckDB table connection object. The view `od_csv_raw` has original Spanish column names and values and has the following structure:
#' \describe{
#'   \item{fecha}{\code{Date}. The date of the trip, including year, month, and day.}
#'   \item{origen}{\code{character}. The identifier for the origin location of the trip, formatted as a character string (e.g., '01001_AM').}
#'   \item{destino}{\code{character}. The identifier for the destination location of the trip, formatted as a character string (e.g., '01001_AM').}
#'   \item{actividad_origen}{\code{character}. The type of activity at the origin location (e.g., 'casa', 'trabajo').}
#'   \item{actividad_destino}{\code{character}. The type of activity at the destination location (e.g., 'otros', 'trabajo').}
#'   \item{residencia}{\code{character}. The code representing the residence of the individual making the trip (e.g., '01') according to the official INE classification.}
#'   \item{edad}{\code{character}. The age of the individual making the trip. This data is actaully filled with 'NA' values, which is why this column is removed in the cleaned-up and translated view described above.}
#'   \item{periodo}{\code{integer}. The time period during which the trip started, represented as an integer (e.g., 0, 1, 2).}
#'   \item{distancia}{\code{character}. The distance category of the trip, represented as a character string (e.g., '002-005' for 2-5 km).}
#'   \item{viajes}{\code{double}. The number of trips taken within the specified time period and distance.}
#'   \item{viajes_km}{\code{double}. The total length of all trips in kilometers for the specified time period and distance.}
#'   \item{day}{\code{double}. The day of the trip.}
#'   \item{month}{\code{double}. The month of the trip.}
#'   \item{year}{\code{double}. The year of the trip.}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # create a connection to the v1 data
#' Sys.setenv(SPANISH_OD_DATA_DIR = "~/path/to/your/cache/dir")
#' dates <- c("2020-02-14", "2020-03-14", "2021-02-14", "2021-02-14", "2021-02-15")
#' od_dist <- spod_get_od(zones = "distr", dates = dates)
#'
#' # od dist is a table view filtered to the specified dates
#'
#' # access the source connection with all dates
#' # list tables
#' DBI::dbListTables(od_dist$src$con)
#' }
spod_get_od <- function(
    zones = c(
      "districts", "dist", "distr", "distritos",
      "municipalities", "muni", "municip", "municipios"
    ),
    dates = NULL,
    data_dir = spod_get_data_dir(),
    quiet = FALSE,
    duck_max_mem = 3,
    duck_max_threads = parallelly::availableCores() - 1
) {
  # hardcode od as this is a wrapper to get origin-destiation data using spod_get() function
  type <- "od"
  
  duck_tbl_con <- spod_get(
    type = type,
    zones = zones,
    dates = dates,
    data_dir = data_dir,
    quiet = quiet,
    duck_max_mem = duck_max_mem,
    duck_max_threads = duck_max_threads
  )

  return(duck_tbl_con)
}

#' Get tabular data
#' 
#' @description This function creates a DuckDB lazy table connection object from the specified type and zones. It checks for missing data and downloads it if necessary.
#' 
#' 
#' @inheritParams spod_download_data
#' @inheritParams spod_duckdb_limit_resources
#' @inheritParams global_quiet_param
#' @return A DuckDB lazy table connection object. It can be manupulated using `dplyr` verbs, or can be loaded into memory using `dplyr::collect()`.
#' @keywords internal
#' @examples
#' \dontrun{
#' 
#' # create a connection to the v1 data
#' Sys.setenv(SPANISH_OD_DATA_DIR = "~/path/to/your/cache/dir")
#' dates <- c("2020-02-14", "2020-03-14", "2021-02-14", "2021-02-14", "2021-02-15")
#' od_dist <- spod_get_od(zones = "distr", dates = dates)
#'
#' # od dist is a table view filtered to the specified dates
#'
#' # access the source connection with all dates
#' # list tables
#' DBI::dbListTables(od_dist$src$con)
#' }
#' 
spod_get <- function(
  type = c(
    "od", "origin-destination",
    "os", "overnight_stays",
    "tpp", "trips_per_person"
  ),
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios"
  ),
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  duck_max_mem = 3,
  duck_max_threads = parallelly::availableCores() - 1,
  max_download_size_gb = 1
) {
  
  ver <- spod_infer_data_v_from_dates(dates)
  
  type <- match.arg(type)
  type <- spod_match_data_type(type = type)

  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)


  if (is.character(dates)) {
    if (all(dates != "cached")) {
      dates <- spod_dates_argument_to_dates_seq(dates = dates)
      # use the spot_download_data() function to download any missing data
      spod_download_data(
        type = type,
        zones = zones,
        dates = dates,
        max_download_size_gb = max_download_size_gb,
        data_dir = data_dir,
        return_output = FALSE
      )
    }
  }

  # create in memory duckdb connection
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv, dbdir = ":memory:", read_only = FALSE)

  # define memory and threads limits
  con <- spod_duckdb_limit_resources(
    con = con,
    duck_max_mem = duck_max_mem,
    duck_max_threads = duck_max_threads
  )

  # attach the folder with csv.gz files with predefined and cleaned up data types
  if (type == "od") {
    con <- spod_duckdb_od(
      con = con,
      zones = zones,
      ver = ver,
      data_dir = data_dir
    )
  } else if (type == "tpp") {
    message("trips per person data retrieval is not yet implemented")
    invisible(return(NULL))
    # con <- spod_duckdb_trips_per_person(
    #   con = con,
    #   zones = zones,
    #   ver = ver,
    #   data_dir = data_dir
    # )
  } else if (type == "os") {
    message("overnight stays data retrieval is not yet implemented")
    invisible(return(NULL))
    # con <- spod_duckdb_overnight_stays(
    #   con = con,
    #   zones = zones,
    #   ver = ver,
    #   data_dir = data_dir
    # )
  }
  
  clean_csv_view_name <- glue::glue("{type}_csv_clean")
  clean_filtered_csv_view_name <- glue::glue("{type}_csv_clean_filtered")

  # filter by date
  if (!is.character(dates)) {
    con <- spod_duckdb_filter_by_dates(
      con,
      clean_csv_view_name,
      clean_filtered_csv_view_name,
      dates
    )
  }

  # return either a full view of all available data (dates = "cached") or a view filtered to the specified dates
  if (all(!is.character(dates))) {
    return(dplyr::tbl(con, clean_filtered_csv_view_name))
  } else {
    return(dplyr::tbl(con, clean_csv_view_name))
  }
}
