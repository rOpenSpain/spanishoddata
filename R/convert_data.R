#' Convert all downloaded data to duckdb
#'
#' @param save_dir The path to the directory where the duckdb files will be saved. If `NULL`, uses the default location in `data_dir` (set by the `SPANISH_OD_DATA_DIR` environment variable). Therefore, the default relative path is `<data_dir>/clean_data/v1/tabular/duckdb/<type>_<zones>.duckdb`.
#' @inheritParams spod_get_zones
#' @inheritParams spod_download_data
#' @inheritParams spod_get
#' @inheritParams spod_duckdb_limit_resources
#' @param overwrite Logical. If `TRUE`, overwrites existing duckdb files. Defaults to `FALSE`.
#' @inheritParams global_quiet_param
#' @return Path to saved DuckDB file.
#' @export
spod_convert_to_duckdb <- function(
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
  save_path = NULL,
  overwrite = FALSE,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  duck_max_mem = 3,
  duck_max_threads = parallelly::availableCores() - 1,
  max_download_size_gb = 1
) {

  if (is.null(dates)) {
    message("No period specified in the `dates` argument. Please set `dates='cached_v1'` or `dates='cached_v2'` to convert all data that was previously downloaded. Alternatively, specify at least one date between 2020-02-14 and 2021-05-09 (for v1 data) or between 2022-01-01 onwards (for v2). Any missing data will be downloaded before conversion.")
  }

  ver <- spod_infer_data_v_from_dates(dates)
    
  type <- match.arg(type)
  type <- spod_match_data_type(type = type)

  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if user is requesting to just get all cached data
  cached_data_requested <- length(dates) == 1 &&
    all(as.character(dates) %in% c("cached_v1", "cached_v2"))  

  # if save_dir is NULL, use default location in data_dir
  if (is.null(save_path)) {
    save_path <- fs::path(
      data_dir,
      spod_subfolder_clean_data_cache(ver = ver),
      "tabular/duckdb/",
      glue::glue("{type}_{zones}.duckdb")
    )
    if (isFALSE(quiet)) {
      message("Using default save_path: ", save_path)
    }
  }

  # ensure save_dir exists
  save_dir <- fs::path_dir(save_path)
  if (!fs::dir_exists(save_dir)){
    fs::dir_create(save_dir, recurse = TRUE)
  }

  # check if duckdb file already exists
  if (fs::file_exists(save_path)) {
    if (!overwrite) {
      message("Duckdb file already exists: ", save_path)
      response <- readline(prompt = "Overwrite existing duckdb file? (yes/no) ")
      overwrite <- tolower(response) %in% c("y", "yes", "Yes")
      
      if (!overwrite) {
        message(glue::glue("Exiting without overwriting existing duckdb file. You may delete it from {save_path} manually and rerun the function. Or rerun the function with `overwrite = TRUE`."))
        return()
      }
    }
    if (overwrite) {
      if (isFALSE(quiet)) {
        message(glue::glue("Overwriting existing duckdb file: ", save_path))
      }
      fs::file_delete(save_path)
    }
  }

  if (isFALSE(quiet)){
    message(glue::glue("Using {duck_max_mem} GB of memory and {duck_max_threads} threads. You may adjust this using the function arguments `duck_max_mem` and `duck_max_threads`."))
    
    message(glue::glue("Converting cached v{ver} {type} data for {zones} to DuckDB: {save_path} \n... This may take some time. You can see if the process is still running by going to the {save_dir} folder and refreshing the folder content and checking if the file size of {basename(save_path)} is increasing after each refresh. The conversion speed depends on your processor speed, number of cores and the speed of your disk storage."))
  }
  # add some indication on how long it may take from empirical experimentation
  # hopefully, the progress_bar feature will be implemented in duckdb R package soon, bug filed here https://github.com/duckdb/duckdb-r/issues/199
  # if I change the code to import data in batches it slows down significantly

  # create duckdb view with a file target
  con <- spod_get(
    type = type,
    zones = zones,
    dates = dates,
    data_dir = data_dir,
    quiet = quiet,
    duck_max_mem = duck_max_mem,
    duck_max_threads = duck_max_threads,
    max_download_size_gb = max_download_size_gb,
    duckdb_target = save_path
  )

  # resolve the actual database connection from the returned table
  db_con <- con$src$con
    
  if( isTRUE(cached_data_requested) ){
    table_to_convert <- glue::glue("{type}_csv_clean")
  } else {
    table_to_convert <- glue::glue("{type}_csv_clean_filtered")
  }

  
  # prepare SQL query
  target_table_name <- gsub("\\..*", "", basename(save_path)) # experimental
  sql_import_query <- dplyr::sql(
    # glue::glue("CREATE TABLE {type} AS SELECT * FROM {table_to_convert} ;") should be standard
    glue::glue('CREATE TABLE {target_table_name} AS SELECT * FROM {table_to_convert} ;') # experimental - for the user friendly duckdb connection wrapper so that it can guess the table name from the file name, hopefully the user will not rename it
  )
    
  # import view of CSV files into duckdb
  DBI::dbExecute(
    db_con,
    statement = sql_import_query
  )

  # drop the views to csv files
  tables_list <- DBI::dbListTables(db_con)
  # find all views that contain csv in any case
  tables_list <- tables_list[stringr::str_detect(tables_list, "csv")]
  if (length(tables_list) > 0) {
    for(i in 1:length(tables_list)){
      sql_drop_view <- dplyr::sql(
        glue::glue(
          "DROP VIEW IF EXISTS {tables_list[i]};"
        )
      )
      DBI::dbExecute(
        db_con, 
        sql_drop_view
      )
    }
  }

  DBI::dbDisconnect(db_con, shutdown = TRUE)
  # duckdb::duckdb_shutdown(drv)

  message("Data imported into DuckDB database at: ", save_path)
  
  # a few instructions on how to use the duckdb file
  if (isFALSE(quiet)){
    message("You can start working with the imported data by runining:\n mydata <- spod_duckdb_connect('duckdb_path=", save_path, "')")
    message("You can then manipulate `mydata` using `dplyr` functions such as `select()`, `filter()`, `mutate()`, `group_by()`, `summarise()`, etc. In the end of any sequence of commands you will need to add `collect()` to execute the whole chain and load the results into memory in an R `data.frame`/`tibble`")
    message("For more in-depth usage of such tables, please refere to DuckDB documentation and examples at https://duckdb.org/docs/api/r#dbplyr . Some more useful examples can be found here https://arrow-user2022.netlify.app/data-wrangling#combining-arrow-with-duckdb .")
  }

  return(save_path)
}
