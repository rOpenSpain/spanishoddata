#' Get latest file list from the XML for MITMA open mobility data v1 (2020-2021)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://opendata-movilidad.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @examples
#' if (FALSE) {
#'   spod_get_latest_v1_file_list()
#' }
#' @keywords internal
spod_get_latest_v1_file_list <- function(
    data_dir = spod_get_data_dir(),
    xml_url = "https://opendata-movilidad.mitma.es/RSS.xml"
) {
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}/data_links_v1_{current_date}.xml")

  # ensure dir exists
  if (!fs::dir_exists(dirname(current_filename))) {
    fs::dir_create(dirname(current_filename), recurse = TRUE)
  }

  message("Saving the file to: ", current_filename)
  xml_requested <- curl::multi_download(
    urls = xml_url,
    destfiles = current_filename
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
#' @keywords internal
spod_available_data_v1 <- function(
    data_dir = spod_get_data_dir(),
    # check_local_files (below) is FALSE by default to avoid excessive filesystem access, perhaps should be TRUE. Download functions use it to load the xml file, but we probably do not want the script to check all local cache directories every time we run a get data function. Perhaps it is better to offload this check to a separate function and have a csv file or some other way to keep track of the files that were downloaded and cached. An output of curl::multi_download() could be used for this purpose.
    check_local_files = FALSE,
    quiet = FALSE) {
  
  metadata_folder <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}")
  if(!fs::dir_exists(metadata_folder)){
    fs::dir_create(metadata_folder)
  }
  
  xml_files_list <- fs::dir_ls(metadata_folder, type = "file", regexp = "data_links_v1") |> sort()
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

  # replace all municipal data download links with districts links
  # this is to address the bugs described in detail in:
  # http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
  # http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
  # the decision was to use distrcit data and aggregate it to replicate municipal data
  files_table$target_url <- gsub("mitma-municipios", "mitma-distritos", files_table$target_url)
  files_table$target_url <- gsub("mitma_municipio", "mitma_distrito", files_table$target_url)

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
    files_table <- files_table |>
      dplyr::left_join(size_by_file_category, by = "file_category")
    files_table$remote_file_size_mb[is.na(files_table$remote_file_size_mb)] <- mean(files_table$mean_file_size_mb)

    # Clean up temporary columns
    files_table <- files_table |>
      dplyr::select(-"mean_file_size_mb", -"file_category")
  }
  
  # now check if any of local files exist
  if( check_local_files == TRUE){
    files_table$downloaded <- fs::file_exists(files_table$local_path)
  }

  return(files_table)
}

#' Downloads and extracts the raw v1 zones data
#'
#' This function ensures that the necessary v1 raw data for zones files are downloaded and extracted from the specified data directory.
#'
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`).
#' @param data_dir The directory where the data is stored.
#' @param quiet Boolean flag to control the display of messages.
#' @return The path to the downloaded and extracted file.
#' @keywords internal
spod_download_zones_v1 <- function(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni", "municip", "municipios"),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
zones <- match.arg(zones)
zones <- spod_zone_names_en2es(zones)

metadata <- spod_available_data(ver = 1, data_dir = data_dir, check_local_files = FALSE)

# download id relation files if missing
relation_files <- metadata[grepl("relaciones_(distrito|municipio)_mitma.csv", metadata$target_url),]
if (any(!fs::file_exists(relation_files$local_path))) {
  fs::dir_create(unique(fs::path_dir(relation_files$local_path)), recurse = TRUE)
  invisible(curl::multi_download(urls = relation_files$target_url, destfile = relation_files$local_path, resume = FALSE, progress = TRUE))
}

regex <- glue::glue("zonificacion_{zones}\\.")
sel_zones <- stringr::str_detect(metadata$target_url, regex)
metadata_zones <- metadata[sel_zones, ]
dir_name <- fs::path_dir(metadata_zones$local_path[1])
if (!fs::dir_exists(dir_name)) {
  fs::dir_create(dir_name, recurse = TRUE)
}

if (!fs::file_exists(metadata_zones$local_path)) {
  if (isFALSE(quiet)) message("Downloading the file to: ", metadata_zones$local_path)
  downloaded_file <- curl::multi_download(metadata_zones$target_url, destfiles = metadata_zones$local_path, resume = TRUE, progress = TRUE)
  downloaded_file <- downloaded_file$destfile
} else {
  if (isFALSE(quiet)) message("File already exists: ", metadata_zones$local_path)
  downloaded_file <- metadata_zones$local_path
}

if (isFALSE(quiet)) message("Unzipping the file: ", downloaded_file)
if (!fs::dir_exists(fs::path_dir(downloaded_file))){
  fs::dir_create(fs::path_dir(downloaded_file), recurse = TRUE)
}
utils::unzip(downloaded_file, exdir = paste0(fs::path_dir(downloaded_file), "/"))

# remove artifacts (remove __MACOSX if exists)
junk_path <- paste0(fs::path_dir(downloaded_file), "/__MACOSX")
if (fs::dir_exists(junk_path)) fs::dir_delete(junk_path)

return(metadata_zones$local_path)
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
#' @keywords internal
spod_get_zones_v1 <- function(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni", "municip", "municipios"),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
zones <- match.arg(zones)
zones <- spod_zone_names_en2es(zones)

metadata <- spod_available_data(ver = 1, data_dir = data_dir, check_local_files = FALSE)

# Ensure the raw data is downloaded and extracted
spod_download_zones_v1(zones, data_dir, quiet)

# check if gpkg files are already saved and load them if available
expected_gpkg_path <- fs::path(
  data_dir,
  glue::glue(spod_subfolder_clean_data_cache(ver = 1), "/zones/{zones}_mitma.gpkg")
)
if (fs::file_exists(expected_gpkg_path)) {
  if (isFALSE(quiet)) {
    message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
  }
  return(sf::read_sf(expected_gpkg_path))
}

zones_path <- fs::dir_ls(
  path = fs::path(data_dir, spod_subfolder_raw_data_cache(ver = 1)),
  glob = glue::glue("*v1**{zones}/*.shp"),
  recurse = TRUE
)

zones_sf <- spod_clean_zones_v1(zones_path, zones = zones)
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
#' @inheritParams spod_get_zones
#' @return A spatial object containing the cleaned zones data. 
#' @keywords internal
#' @importFrom rlang .data
#'
spod_clean_zones_v1 <- function(zones_path, zones) {
  
  if(fs::file_exists(zones_path) == FALSE) {
    stop("File does not exist: ", zones_path)
  }
  suppressWarnings({
    zones_sf <- sf::read_sf(zones_path)
  })
  invalid_geometries <- !sf::st_is_valid(zones_sf)
  if (sum(invalid_geometries) > 0) {
    fixed_zones_sf <- sf::st_make_valid(zones_sf[invalid_geometries, ])
    zones_sf <- rbind(zones_sf[!invalid_geometries, ], fixed_zones_sf)
  }
  names(zones_sf)[names(zones_sf) == "ID"] <- "id"

  # load and prepare id relations for districts
  relations_districts <- readr::read_delim(
    file = paste0(spod_get_data_dir(), "/",
      spod_subfolder_raw_data_cache(1),
      "relaciones_distrito_mitma.csv"),
    delim = "|", show_col_types = FALSE
  )
  relations_districts_col_names <- names(relations_districts)
  relations_districts_col_names <- gsub("distrito", "district", relations_districts_col_names)
  relations_districts_col_names <- gsub("municipio", "municipality", relations_districts_col_names)
  relations_districts_col_names <- gsub("^district$", "census_district", relations_districts_col_names)
  names(relations_districts) <- relations_districts_col_names
  
  # load and prepare id relations for municipalities
  relations_municipalities <- readr::read_delim(
    file = paste0(spod_get_data_dir(), "/",
      spod_subfolder_raw_data_cache(1),
      "relaciones_municipio_mitma.csv"),
    delim = "|", show_col_types = FALSE
  )
  relations_municipalities_col_names <- names(relations_municipalities)
  relations_municipalities_col_names <- gsub("municipio", "municipality", relations_municipalities_col_names)
  names(relations_municipalities) <- relations_municipalities_col_names

  # summarise districts relations including municipality data
  relations_districts_aggregated <- relations_districts |>
    dplyr::left_join(
      relations_municipalities |>
        dplyr::group_by(.data$municipality_mitma) |>
        dplyr::summarize(
          municipalities = paste(.data$municipality, collapse = "; ")
        ),
      by = "municipality_mitma") |> 
    dplyr::group_by(.data$district_mitma) |> 
    dplyr::summarize(
      census_districts = paste(.data$census_district, collapse = "; "),
      municipalities_mitma = paste(.data$municipality_mitma, collapse = "; "),
      municipalities = paste(.data$municipalities, collapse = "; ")
    )

  # summarise municipalities relations
  relations_municipalities_aggregated <- relations_municipalities |>
    dplyr::left_join(
      relations_districts |> 
        dplyr::group_by(.data$municipality_mitma) |>
        dplyr::summarize(
          census_districts = paste(.data$census_district, collapse = "; "),
          districts_mitma = paste(.data$district_mitma, collapse = "; ")
        )
      , by = "municipality_mitma") |>
    dplyr::group_by(.data$municipality_mitma) |> 
    dplyr::summarize(
      municipalities = paste(.data$municipality, collapse = "; "),
      districts_mitma = paste(.data$districts_mitma, collapse = "; "),
      census_districts = paste(.data$census_districts, collapse = "; ")
    )

  # cleanup duplacate ids in municipalities
  relations_municipalities_aggregated <- relations_municipalities_aggregated |> 
    dplyr::mutate(
      dplyr::across(
        c(.data$municipalities, .data$districts_mitma, .data$census_districts),
        spod_unique_separated_ids
      )
    )
  names(relations_municipalities_aggregated)[names(relations_municipalities_aggregated) == "municipality_mitma"] <- "id"
  
  # cleanup duplicate ids in districts
  relations_districts_aggregated <- relations_districts_aggregated |> 
    dplyr::mutate(
      dplyr::across(
        c(.data$census_districts, .data$municipalities_mitma), spod_unique_separated_ids
      )
    )
  names(relations_districts_aggregated)[names(relations_districts_aggregated) == "district_mitma"] <- "id"

  if (zones == "distritos") {
    zones_sf <- zones_sf |> 
      dplyr::left_join(relations_districts_aggregated, by = "id") |>
      dplyr::relocate(.data$geometry, .after = dplyr::last_col())
  } else if (zones == "municipios") {
    zones_sf <- zones_sf |> 
      dplyr::left_join(relations_municipalities_aggregated, by = "id") |>
      dplyr::relocate(.data$geometry, .after = dplyr::last_col())
  }

  # add metadata from v2 zones
  zones_v2_sf <- spod_get_zones_v2(zones = zones)
  zones_v2_sf <- zones_v2_sf[,c("id", "name")]
  names(zones_v2_sf)[names(zones_v2_sf) == "id"] <- "id_in_v2"
  names(zones_v2_sf)[names(zones_v2_sf) == "name"] <- "name_in_v2"
  suppressWarnings(
    zones_v2_sf_centroids <- zones_v2_sf |> sf::st_point_on_surface()
  )
  v2_to_v1 <- sf::st_join(zones_sf, zones_v2_sf_centroids, left = TRUE) |> 
    sf::st_drop_geometry() 
  v2_v_1ref <- v2_to_v1 |>
    dplyr::group_by(.data$id) |> 
      dplyr::summarize(
      names_in_v2_data = paste(.data$name_in_v2, collapse = "; "),
      ids_in_v2_data = paste(.data$id_in_v2, collapse = "; ")
    )
  eng_zones <- dplyr::if_else(zones == "distritos", true = "district", false = "municipality")
  names(v2_v_1ref)[names(v2_v_1ref) == "names_in_v2_data"] <- glue::glue("{eng_zones}_names_in_v2")
  names(v2_v_1ref)[names(v2_v_1ref) == "ids_in_v2_data"] <- glue::glue("{eng_zones}_ids_in_v2")
  

  zones_sf <- zones_sf |> 
    dplyr::left_join(v2_v_1ref, by = "id") |> 
    dplyr::relocate(.data$geometry, .after = dplyr::last_col())


  return(zones_sf)
}


#' Get tabular data
#' 
#' @description This function creates a DuckDB lazy table connection object from the specified type and zones. It checks for missing data and downloads it if necessary. The connnection is made to the raw CSV files in gzip archives, so analysing the data through this connection may be slow if you select more than a few days. You can manipulate this object using `{dplyr}` functions such as \link[dplyr]{select}, \link[dplyr]{filter}, \link[dplyr]{mutate}, \link[dplyr]{group_by}, \link[dplyr]{summarise}, etc. In the end of any sequence of commands you will need to add \link[dplyr]{collect} to execute the whole chain of data manipulations and load the results into memory in an R `data.frame`/`tibble`. See codebooks for v1 and v2 data in vignettes with `spod_codebook(1)` and `spod_codebook(2)` (\link{spod_codebook}).
#' 
#' If you want to analyse longer periods of time (especiially several months or even the whole data over several years), consider using the \link{spod_convert} and then \link{spod_connect}.
#' 
#' @param duckdb_target (Optional) The path to the duckdb file to save the data to, if a convertation from CSV is reuqested by the `spod_convert` function. If not specified, it will be set to ":memory:" and the data will be stored in memory.
#' @inheritParams spod_download
#' @inheritParams spod_duckdb_limit_resources
#' @inheritParams spod_duckdb_set_temp
#' @inheritParams global_quiet_param
#' @return A DuckDB lazy table connection object of class `tbl_duckdb_connection`.
#' @export
#' @examples
#' \dontrun{
#' 
#' # create a connection to the v1 data
#' Sys.setenv(SPANISH_OD_DATA_DIR = "~/path/to/your/cache/dir")
#' dates <- c("2020-02-14", "2020-03-14", "2021-02-14", "2021-02-14", "2021-02-15")
#' od_dist <- spod_get(type = "od", zones = "distr", dates = dates)
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
    "nt", "number_of_trips"
  ),
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  max_mem_gb = max(4, spod_available_ram() - 4),
  max_n_cpu = parallelly::availableCores() - 1,
  max_download_size_gb = 1,
  duckdb_target = ":memory:",
  temp_path = spod_get_temp_dir()
) {
  if (is.null(dates)) {
    message("No period specified in the `dates` argument. Please set `dates='cached_v1'` or `dates='cached_v2'` to convert all data that was previously downloaded. Alternatively, specify at least one date between 2020-02-14 and 2021-05-09 (for v1 data) or between 2022-01-01 onwards (for v2). Any missing data will be downloaded before conversion.")
  }
  
  
  type <- match.arg(type)
  type <- spod_match_data_type(type = type)
  
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)
  
  # check if user is requesting to just get all cached data
  cached_data_requested <- length(dates) == 1 &&
    all(as.character(dates) %in% c("cached_v1", "cached_v2"))
  
  
  if (isFALSE(cached_data_requested)) {
    dates <- spod_dates_argument_to_dates_seq(dates = dates)
    ver <- spod_infer_data_v_from_dates(dates)
    # use the spot_download_data() function to download any missing data
    spod_download(
      type = type,
      zones = zones,
      dates = dates,
      max_download_size_gb = max_download_size_gb,
      data_dir = data_dir,
      return_local_file_paths = FALSE
    )
  } else if (isTRUE(cached_data_requested)) {
    ver <- as.numeric(stringr::str_extract(dates, "(1|2)$"))
  }
  
  
  # create in memory duckdb connection
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv, dbdir = duckdb_target, read_only = FALSE)

  # define memory and threads limits
  con <- spod_duckdb_limit_resources(
    con = con,
    max_mem_gb = max_mem_gb,
    max_n_cpu = max_n_cpu
  )

  # attach the folder with csv.gz files with predefined and cleaned up data types
  if (type == "od") {
    con <- spod_duckdb_od(
      con = con,
      zones = zones,
      ver = ver,
      data_dir = data_dir
    )
  } else if (type == "nt") {
    con <- spod_duckdb_number_of_trips(
      con = con,
      zones = zones,
      ver = ver,
      data_dir = data_dir
    )
  } else if (type == "os") {
    con <- spod_duckdb_overnight_stays(
      con = con,
      zones = zones,
      ver = ver,
      data_dir = data_dir
    )
  }
  
  clean_csv_view_name <- glue::glue("{type}_csv_clean")
  clean_filtered_csv_view_name <- glue::glue("{type}_csv_clean_filtered")

  # filter by date, unless cached data requested
  if (isFALSE(cached_data_requested)) {
    con <- spod_duckdb_filter_by_dates(
      con,
      clean_csv_view_name,
      clean_filtered_csv_view_name,
      dates
    )
  }

  # if working with in-memory database
  # set temp path for intermediate spilling
  # https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling
  # if target were set as a database file, temp would be created at the same path
  # however, when the working in-memory on folder of CSV files, temp is created in the root of R working directory, which may be undesirable
  if ( duckdb_target == ":memory:" ) {
    con <- spod_duckdb_set_temp(con, temp_path = temp_path)
  }

  # return either a full view of all available data (dates = "cached") or a view filtered to the specified dates
  if (isFALSE(cached_data_requested)) {
    return(dplyr::tbl(con, clean_filtered_csv_view_name))
  } else if (isTRUE(cached_data_requested)) {
    return(dplyr::tbl(con, clean_csv_view_name))
  }
}
