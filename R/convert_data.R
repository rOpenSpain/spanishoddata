#' Convert all downloaded v1 origin-destination data to duckdb
#'
#' @param save_dir The path to the directory where the duckdb files will be saved. If `NULL`, uses the default location in `data_dir` (set by the `SPANISH_OD_DATA_DIR` environment variable). Therefore, the default relative path is `<data_dir>/clean_data/v1/tabular/duckdb/od_<zones>.duckdb`.
#' @inheritParams spod_get_zones_v1
#' @inheritParams spod_duckdb_limit_resources
#' @param overwrite Logical. If `TRUE`, overwrites existing duckdb files. Defaults to `FALSE`.
#' @return Path to saved DuckDB file.
#' @export
spod_convert_od_v1_to_duckdb <- function(
    zones = c(
      "districts", "dist", "distr", "distritos",
      "municipalities", "muni", "municip", "municipios"
    ),
    data_dir = spod_get_data_dir(),
    save_dir = NULL,
    quiet = FALSE,
    duck_max_mem = 3,
    duck_max_threads = parallelly::availableCores(),
    overwrite = FALSE) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # if save_dir is NULL, use default location in data_dir
  if (is.null(save_dir)) {
    save_dir <- fs::path(
      data_dir,
      spod_subfolder_clean_data_cache(ver = 1),
      "tabular/duckdb/"
    )
  }

  # ensure save_dir exists
  if (!fs::dir_exists(save_dir)) fs::dir_create(save_dir, recurse = TRUE)

  # create duckdb save path
  duckdb_save_path <- glue::glue("{save_dir}/od_{zones}.duckdb")

  # check if duckdb file already exists
  if (fs::file_exists(duckdb_save_path)) {
    if (!overwrite) {
      message("Duckdb file already exists: ", duckdb_save_path)
      response <- readline(prompt = "Overwrite existing duckdb file? (yes/no) ")
      overwrite <- tolower(response) %in% c("y", "yes", "Yes")
      
      if (!overwrite) {
        message(glue::glue("Exiting without overwriting existing duckdb file. You may delete it from {duckdb_save_path} manually and rerun the function. Or rerun the function with `overwrite = TRUE`."))
        return()
      }
    }
    if (overwrite) {
      if (isFALSE(quiet)) {
        message(glue::glue("Overwriting existing duckdb file: ", duckdb_save_path))
      }
      fs::file_delete(duckdb_save_path)
    }
  }

  if (isFALSE(quiet)) message(glue::glue("Using {duck_max_mem} GB of memory and {duck_max_threads} threads. You may adjust this using the function arguments `duck_max_mem` and `duck_max_threads`."))
  if (isFALSE(quiet)) message(glue::glue("Converting cached v1 od data for {zones} to DuckDB: ", duckdb_save_path, "... This may take a while."))
  # add some indication on how long it may take from empirical experimentation
  # hopefully, the progress_bar feature will be implemented in duckdb R package soon, bug filed here https://github.com/duckdb/duckdb-r/issues/199

  # get dates of cached data
  # v1_meta <- spod_available_data_v1(check_local_files = TRUE)

  # v1_meta <- v1_meta[v1_meta$downloaded == TRUE,]
  # v1_meta <- v1_meta[grepl("maestra1", v1_meta$local_path),]
  # v1_meta <- v1_meta[grepl(zones, v1_meta$local_path),]

  # dates <- v1_meta$data_ymd

  # create duckdb connection
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv, dbdir = duckdb_save_path, read_only = FALSE)

  # define memory and threads limits
  con <- spod_duckdb_limit_resources(
    con = con,
    duck_max_mem = duck_max_mem,
    duck_max_threads = duck_max_threads
  )

  # connect to folder of CSVs with v1 od data
  con <- spod_duckdb_od_v1(con = con, zones = zones)
  # DBI::dbListTables(con)

  # import view of CSV files into duckdb
  DBI::dbExecute(con, "CREATE TABLE od AS SELECT * FROM od_csv_clean ;")

  DBI::dbDisconnect(con, shutdown = TRUE)
  duckdb::duckdb_shutdown(drv)

  message("Cached v1 origin-destination data imported to DuckDB at: ", duckdb_save_path)

  return(duckdb_save_path)
}
