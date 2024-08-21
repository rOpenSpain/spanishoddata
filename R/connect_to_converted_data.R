#' Connect to data converted to DuckDB
#' 
#' @description
#' This function allows the user to quickly connect to the data converted to DuckDB with the `spod_convert_to_duckdb()` function. This function is a simplificaiton of the connection process. It uses
#' 
#' @param duckdb_path a path to the DuckDB database.
#' @param target_table_name table name inside the database.
#' @inheritParams spod_duckdb_limit_resources
#' @export
#' @return a DuckDB table connection object.
spod_duckdb_connect <- function(
  duckdb_path,
  target_table_name = NULL,
  quiet = FALSE,
  duck_max_mem = 3,
  duck_max_threads = parallelly::availableCores() - 1
){
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = duckdb_path,
    read_only = TRUE
  )

  con <- spod_duckdb_limit_resources(
    con = con,
    duck_max_mem = duck_max_mem,
    duck_max_threads = duck_max_threads
  )

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

  return(tbl_con)
}
