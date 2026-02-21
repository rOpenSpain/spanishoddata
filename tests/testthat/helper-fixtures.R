
# Helper functions for setting up test data fixtures

#' Setup a temporary data directory with bundled fixtures
#'
#' This function creates a temporary directory and populates it with
#' the bundled test fixtures (metadata, zones, OD data) to mimic
#' a populated cache directory.
#'
#' @return The path to the temporary directory
setup_test_data_dir <- function() {
  test_dir <- tempfile("spod_test_")
  dir.create(test_dir, recursive = TRUE)
  
  fixture_path <- system.file("testdata", package = "spanishoddata")
  if (fixture_path == "") {
    # Fallback for when running interactively without package load?
    fixture_path <- testthat::test_path("../../inst/testdata")
  }
  
  

  
  # 1. Setup Metadata Cache
  # -----------------------
  # Copy mock metadata to expected cache location so spod_available_data finds it
  meta_cache <- file.path(test_dir, spanishoddata:::spod_subfolder_metadata_cache())
  dir.create(meta_cache, recursive = TRUE)
  
  # The cache logic looks for pattern: metadata_s3_v{ver}_YYYY-MM-DD.rds
  # We copy our mock file to a name that guarantees it's picked up as "latest"
  mock_meta_v2 <- file.path(fixture_path, "metadata/available_data_s3_v2_mock.rds")
  if (file.exists(mock_meta_v2)) {
    # Use a future date to ensure it's NEVER considered stale by spod_available_data_s3
    target_name <- paste0("metadata_s3_v2_", Sys.Date() + 30, ".rds")
    file.copy(mock_meta_v2, file.path(meta_cache, target_name))
  }
  
  mock_meta_v1 <- file.path(fixture_path, "metadata/available_data_s3_v1_mock.rds")
  if (file.exists(mock_meta_v1)) {
    target_name <- paste0("metadata_s3_v1_", Sys.Date() + 30, ".rds")
    file.copy(mock_meta_v1, file.path(meta_cache, target_name))
  }
  
  # 2. Setup Zone Cache
  # -------------------
  # spod_get_zones checks 'clean_data/v{ver}/zones'
  clean_zones <- file.path(test_dir, "clean_data/v2/zones")
  dir.create(clean_zones, recursive = TRUE)
  
  # Copy all bundled zone GPKGs
  zone_fixtures <- list.files(file.path(fixture_path, "clean_data/v2/zones"), full.names = TRUE)
  if (length(zone_fixtures) > 0) {
    file.copy(zone_fixtures, clean_zones)
  }
  
  # 3. Setup Raw OD Data
  # --------------------
  # spod_duckdb_od expects 'raw_data_cache/v{ver}/estudios_basicos/por-distritos/viajes/ficheros-diarios/'
  raw_od <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios")
  dir.create(raw_od, recursive = TRUE)
  
  od_fixtures <- list.files(file.path(fixture_path, "raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios"), full.names = TRUE)
  

  
  if (length(od_fixtures) > 0) {
    file.copy(od_fixtures, raw_od, recursive = TRUE)
  }
  
  files_in_dest <- list.files(raw_od, recursive = TRUE)
  
  return(test_dir)
}

#' Context manager for mocked downloads
#'
#' Prevents actual downloads during tests.
#'
#' @param code The code to run
local_mock_download <- function(env = parent.frame()) {
  # Mock spod_download to do nothing (files are already in test_dir)
  testthat::local_mocked_bindings(
    spod_download = function(...)invisible(NULL),
    .package = "spanishoddata",
    .env = env
  )
}

#' Setup a temporary directory with a sample parquet file
#' 
#' Creates a parquet file using arrow (if available) or duckdb
#' to test spod_connect with parquet folders.
#' 
#' @return Path to the parquet folder
setup_parquet_dir <- function() {
  parquet_dir <- tempfile("spod_parquet_")
  dir.create(parquet_dir)
  
  # Create a simple data frame
  df <- data.frame(
    id = 1:10,
    val = letters[1:10]
  )
  
  parquet_file <- file.path(parquet_dir, "test.parquet")
  
  # Try using arrow if installed, otherwise skip (or fail if this is essential)
  # But for testing `spod_connect` which connects to parquet, we need a parquet file.
  # We can use duckdb to write it since we depend on duckdb.
  
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  DBI::dbWriteTable(con, "temp_df", df)
  DBI::dbExecute(con, paste0("COPY temp_df TO '", parquet_file, "' (FORMAT PARQUET)"))
  
  return(parquet_dir)
}

#' Setup a temporary duckdb database file
#' 
#' @return Path to the .duckdb file
setup_duckdb_file <- function() {
  db_file <- tempfile("spod_db_", fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file)
  
  df <- data.frame(id = 1:5, val = LETTERS[1:5])
  DBI::dbWriteTable(con, "my_table", df)
  
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(db_file)
}
