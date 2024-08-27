#' Connect to data converted to DuckDB
#' 
#' @description
#' This function allows the user to quickly connect to the data converted to DuckDB with the `spod_convert_to_duckdb()` function. This function is a simplificaiton of the connection process. It uses
#' 
#' @param data_path a path to the `DuckDB` database file with '.duckdb' extension, or a path to the folder with `parquet` files. Eigher one should have been created with the `spod_convert()` function.
#' @param target_table_name table name inside the database.
#' @inheritParams spod_duckdb_limit_resources
#' @inheritParams spod_duckdb_set_temp
#' @inheritParams global_quiet_param
#' @export
#' @return a DuckDB table connection object.
spod_connect <- function(
  data_path,
  target_table_name = NULL,
  quiet = FALSE,
  max_mem_gb = 4, # later increase that to be 4GB or perhaps 60% of available RAM, as for analysis the amount of memory that can be used can significanly affect the speed of aggregations.
  max_n_cpu = parallelly::availableCores() - 1,
  temp_path = spod_get_temp_dir()
){
  # determine if data_path is a folder or a duckdb file
  if (grepl("\\.duckdb$", data_path)) {
    duckdb_path <- data_path
    target_format <- "duckdb"
  } else {
    duckdb_path <- ":memory:"
    target_format <- "parquet"
  }
  
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = duckdb_path,
    read_only = TRUE
  )

  con <- spod_duckdb_limit_resources(
    con = con,
    max_mem_gb = max_mem_gb,
    max_n_cpu = max_n_cpu
  )

  if (target_format == "duckdb") {
    # try to guess the table name if not provided
    
    if (is.null(target_table_name)) {
      # try the same name as the file name
      target_table_name = gsub("\\..*", "", basename(duckdb_path)) # experimental
      tables_list <- DBI::dbListTables(con)
      if (target_table_name %in% tables_list) {
        # if the table with the same
        target_table_name = target_table_name
      } else {
        # pick the first table that does not contain CSV
        target_table_name = tables_list[!stringr::str_detect(tables_list, "csv")][1]
      }
    }
    tbl_con <- dplyr::tbl(con, target_table_name)
  }

  if (target_format == "parquet") {
    view_name <- basename(data_path)
    parquet_glob_path <- fs::path(data_path, "**", "*.parquet")
    
    DBI::dbExecute(
      con,
      dplyr::sql(
        glue::glue("
        CREATE VIEW {view_name} AS
        SELECT *
        FROM read_parquet(
          '{parquet_glob_path}',
          hive_partitioning = true
        ) ;
        ")
      )
    )
    
    # set temp path for intermediate spilling
    # https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling
    # we do not do the same for duckdb above, as there the temp is automatically created in the same folder as the database
    # however, when the target is parquet files, temp is created in the root of R working directory, which may be undesirable
    con <- spod_duckdb_set_temp(con, temp_path = temp_path)

    tbl_con <- dplyr::tbl(con, view_name)
  }


  return(tbl_con)
}

#' Safely disconnect from data and free memory
#' 
#' @description
#' This function is to ensure that `DuckDB` connections to CSV.gz files (created via `spod_get()`), as well as to `DuckDB` files or folders of `parquet` files (created via `spod_convert()`) are closed properly to prevent conflicting connections. Essentially this is just a wrapper around `DBI::dbDisconnect()` that reaches out into the `.$src$con` object of the `tbl_duckdb_connection` connection object that is returned to the user via `spod_get()` and `spod_connect()`. After disonnecting the database, it also frees up memory by running `gc()`.
#' @param tbl_con A `tbl_duckdb_connection` connection object that you get from either `spod_get()` or `spod_connect()`.
#' @param free_mem A `logical`. Whether to free up memory by running `gc()`. Defaults to `TRUE`.
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' od_distr <- spod_get("od", zones = "distr", dates <- c("2020-01-01", "2020-01-02"))
#' spod_disconnect(od_distr)
#' }
#' 
spod_disconnect <- function(tbl_con, free_mem = TRUE) {
  DBI::dbDisconnect(tbl_con$src$con, shutdown = TRUE)
  if (free_mem){
    gc()
  }
  return(invisible(NULL))
}
