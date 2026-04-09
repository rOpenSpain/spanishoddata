test_that("spod_convert handles 'NA' in renta column using mini fixtures", {
  skip_if_not(requireNamespace("duckdb", quietly = TRUE))
  
  # 1. Setup temporary data directory using the package helper
  test_dir <- setup_test_data_dir()
  on.exit(unlink(test_dir, recursive = TRUE))
  spod_set_data_dir(test_dir, quiet = TRUE)
  
  # 2. Mock valid dates and downloads
  local_mock_valid_dates <- function(env = parent.frame()) {
    mock_dates <- as.Date("2022-02-01")
    mock_func <- function(ver = NULL) {
      if (is.null(ver) || ver == 2) return(mock_dates)
      return(as.Date(character(0)))
    }
    orig <- get("spod_get_valid_dates", envir = asNamespace("spanishoddata"))
    assignInNamespace("spod_get_valid_dates", mock_func, ns = "spanishoddata")
    withr::defer(assignInNamespace("spod_get_valid_dates", orig, ns = "spanishoddata"), envir = env)
  }
  local_mock_valid_dates()
  local_mock_download()
  
  # 3. Modify one of the mini fixtures to include 'NA' in renta for testing
  raw_file <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz")
  
  # Use cross-platform R functions to handle gz files
  tmp_csv <- tempfile(fileext = ".csv")
  gz_con <- gzfile(raw_file, "rt")
  lines <- readLines(gz_con)
  close(gz_con)
  
  # Change the first data row's renta to 'NA'
  parts <- strsplit(lines[2], "|", fixed = TRUE)[[1]]
  parts[11] <- "NA" # renta
  lines[2] <- paste(parts, collapse = "|")
  
  writeLines(lines, tmp_csv)
  # Re-compress
  dest_gz <- gzfile(raw_file, "wt")
  writeLines(lines, dest_gz)
  close(dest_gz)
  
  # 4. Run conversion
  # Use a name without dots other than .duckdb to avoid spod_connect guessing issues
  out_db_name <- "testrentana"
  out_db <- file.path(test_dir, paste0(out_db_name, ".duckdb"))
  expect_error(
    spod_convert(
      type = "od", zones = "distr", dates = "2022-02-01",
      save_format = "duckdb", save_path = out_db,
      overwrite = TRUE, quiet = TRUE
    ),
    NA
  )
  
  # 5. Verify data
  # Provide explicit table name to avoid guessing failures in CI
  tbl_con <- spod_connect(out_db, target_table_name = out_db_name)
  res <- tbl_con %>% dplyr::collect()
  spod_disconnect(tbl_con)
  expect_true(any(is.na(res$income)))
})

test_that("spod_convert handles 'NA' in distancia column using mini fixtures", {
  skip_if_not(requireNamespace("duckdb", quietly = TRUE))
  
  # 1. Setup temporary data directory
  test_dir <- setup_test_data_dir()
  on.exit(unlink(test_dir, recursive = TRUE))
  spod_set_data_dir(test_dir, quiet = TRUE)
  
  # 2. Mock valid dates and downloads
  local_mock_valid_dates <- function(env = parent.frame()) {
    mock_dates <- as.Date("2022-02-01")
    mock_func <- function(ver = NULL) {
      if (is.null(ver) || ver == 2) return(mock_dates)
      return(as.Date(character(0)))
    }
    orig <- get("spod_get_valid_dates", envir = asNamespace("spanishoddata"))
    assignInNamespace("spod_get_valid_dates", mock_func, ns = "spanishoddata")
    withr::defer(assignInNamespace("spod_get_valid_dates", orig, ns = "spanishoddata"), envir = env)
  }
  local_mock_valid_dates()
  local_mock_download()
  
  # 3. Modify one of the mini fixtures to include 'NA' in distancia for testing
  raw_file <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz")
  
  # Use cross-platform R functions to handle gz files
  gz_con <- gzfile(raw_file, "rt")
  lines <- readLines(gz_con)
  close(gz_con)
  
  # Change the first data row's distancia to 'NA'
  parts <- strsplit(lines[2], "|", fixed = TRUE)[[1]]
  parts[5] <- "NA" # distancia
  lines[2] <- paste(parts, collapse = "|")
  
  # Re-compress
  dest_gz <- gzfile(raw_file, "wt")
  writeLines(lines, dest_gz)
  close(dest_gz)
  
  # 4. Run conversion
  out_db_name <- "testdistanciana"
  out_db <- file.path(test_dir, paste0(out_db_name, ".duckdb"))
  expect_error(
    spod_convert(
      type = "od", zones = "distr", dates = "2022-02-01",
      save_format = "duckdb", save_path = out_db,
      overwrite = TRUE, quiet = TRUE
    ),
    NA
  )
  
  # 5. Verify data
  tbl_con <- spod_connect(out_db, target_table_name = out_db_name)
  res <- tbl_con %>% dplyr::collect()
  spod_disconnect(tbl_con)
  expect_true(any(is.na(res$distance)))
})
