#' Connect to data converted to `DuckDB` or hive-style `parquet` files
#' 
#' @description
#' This function allows the user to quickly connect to the data converted to DuckDB with the \link{spod_convert} function. This function simplifies the connection process. The user is free to use the `DBI` and `DuckDB` packages to connect to the data manually, or to use the `arrow` package to connect to the `parquet` files folder.
#' 
#' @param data_path a path to the `DuckDB` database file with '.duckdb' extension, or a path to the folder with `parquet` files. Eigher one should have been created with the \link{spod_convert} function.
#' @param target_table_name Default is `NULL`. When connecting to a folder of `parquet` files, this argument is ignored. When connecting to a `DuckDB` database, a `character` vector of length 1 with the table name to open from the database file. If not specified, it will be guessed from the `data_path` argument and from table names that are available in the database. If you have not manually interfered with the database, this should be guessed automatically and you do not need to specify it.
#' @inheritParams spod_duckdb_limit_resources
#' @inheritParams spod_duckdb_set_temp
#' @inheritParams global_quiet_param
#' @export
#' @return a `DuckDB` table connection object.
#' 
#' @examplesIf interactive()
#' \donttest{
#' # Set data dir for file downloads
#' spod_set_data_dir(tempdir())
#' 
#' # download and convert data
#' dates_1 <- c(start = "2020-02-17", end = "2020-02-18")
#' db_2 <- spod_convert(
#'  type = "number_of_trips",
#'  zones = "distr",
#'  dates = dates_1,
#'  overwrite = TRUE
#' )
#' 
#' # now connect to the converted data
#' my_od_data_2 <- spod_connect(db_2)
#' 
#' # disconnect from the database
#' spod_disconnect(my_od_data_2)
#' }
#' 
spod_connect <- function(
  data_path,
  target_table_name = NULL,
  quiet = FALSE,
  max_mem_gb = max(4, spod_available_ram() - 4),
  max_n_cpu = max(1, parallelly::availableCores() - 1),
  temp_path = spod_get_temp_dir()
){
  # Validate imputs
  checkmate::assert_access(data_path, access = 'r')
  checkmate::assert_character(target_table_name, null.ok = TRUE)
  checkmate::assert_flag(quiet)
  checkmate::assert_number(max_mem_gb, lower = 1)
  checkmate::assert_integerish(max_n_cpu, lower = 1, upper = parallelly::availableCores())
  checkmate::assert_directory_exists(temp_path, access = "rw")

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
        # if the table with the same name exists, use it
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
