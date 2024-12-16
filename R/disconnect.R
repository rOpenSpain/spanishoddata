#' Safely disconnect from data and free memory
#' 
#' @description
#' This function is to ensure that `DuckDB` connections to CSV.gz files (created via `spod_get()`), as well as to `DuckDB` files or folders of `parquet` files (created via `spod_convert()`) are closed properly to prevent conflicting connections. Essentially this is just a wrapper around `DBI::dbDisconnect()` that reaches out into the `.$src$con` object of the `tbl_duckdb_connection` connection object that is returned to the user via `spod_get()` and `spod_connect()`. After disonnecting the database, it also frees up memory by running `gc()`.
#' @param tbl_con A `tbl_duckdb_connection` connection object that you get from either `spod_get()` or `spod_connect()`.
#' @param free_mem A `logical`. Whether to free up memory by running `gc()`. Defaults to `TRUE`.
#' @return No return value, called for side effect of disconnecting from the database and freeing up memory.
#' @export
#' @examples
#' \dontrun{
#' od_distr <- spod_get("od", zones = "distr", dates <- c("2020-01-01", "2020-01-02"))
#' spod_disconnect(od_distr)
#' }
#' 
spod_disconnect <- function(
  tbl_con,
  free_mem = TRUE
) {
  # Validate imputs
  checkmate::assert_class(tbl_con, "tbl_duckdb_connection")
  checkmate::assert_flag(free_mem)

  DBI::dbDisconnect(tbl_con$src$con, shutdown = TRUE)
  if (free_mem){
    gc()
  }
  return(invisible(NULL))
}
