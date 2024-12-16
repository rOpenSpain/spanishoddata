#' Get tabular mobility data
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
#' \donttest{
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
  temp_path = spod_get_temp_dir(),
  ignore_missing_dates = FALSE
) {

  # Validate inputs
  checkmate::assert_choice(type, choices = c("od", "origin-destination", "os", "overnight_stays", "nt", "number_of_trips"))
  checkmate::assert_choice(zones, choices = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ))
  checkmate::assert_flag(quiet)
  checkmate::assert_number(max_mem_gb, lower = 1)
  checkmate::assert_integerish(max_n_cpu, lower = 1, upper = parallelly::availableCores())
  checkmate::assert_number(max_download_size_gb, lower = 0.1)
  checkmate::assert_string(duckdb_target)
  checkmate::assert_directory_exists(data_dir, access = "rw")
  checkmate::assert_directory_exists(temp_path, access = "rw")
  checkmate::assert_flag(ignore_missing_dates)
  
  # simple null check is enough here, as spod_dates_arugument_to_dates_seq will do additional checks anyway
  if (is.null(dates)) {
    message("`dates` argument is undefined. Please set `dates='cached_v1'` or `dates='cached_v2'` to convert all data that was previously downloaded. Alternatively, specify at least one date between 2020-02-14 and 2021-05-09 (for v1 data) or between 2022-01-01 onwards (for v2). Any missing data will be downloaded before conversion. For more details on the dates argument, see ?spod_get.")
  }
  
  # normalise type
  type <- spod_match_data_type(type = type)
  # normalise zones
  zones <- spod_zone_names_en2es(zones)
  
  # check if user is requesting to just get all cached data
  cached_data_requested <- length(dates) == 1 &&
    all(as.character(dates) %in% c("cached_v1", "cached_v2"))
  
  
  if (isFALSE(cached_data_requested)) {
    dates <- spod_dates_argument_to_dates_seq(dates = dates)
    ver <- spod_infer_data_v_from_dates(
      dates = dates, ignore_missing_dates = ignore_missing_dates
    )
    # use the spot_download_data() function to download any missing data
    spod_download(
      type = type,
      zones = zones,
      dates = dates,
      max_download_size_gb = max_download_size_gb,
      data_dir = data_dir,
      quiet = quiet,
      return_local_file_paths = FALSE,
      ignore_missing_dates = ignore_missing_dates
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
