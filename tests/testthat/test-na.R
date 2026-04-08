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
  tmp_csv <- tempfile(fileext = ".csv")
  system2("gzcat", c(raw_file, ">", tmp_csv))
  lines <- readLines(tmp_csv)
  parts <- strsplit(lines[2], "|", fixed = TRUE)[[1]]
  parts[11] <- "NA" # renta
  lines[2] <- paste(parts, collapse = "|")
  writeLines(lines, tmp_csv)
  system2("gzip", c("-c", tmp_csv, ">", raw_file))
  
  # 4. Run conversion
  out_db <- file.path(test_dir, "test_renta_na.duckdb")
  expect_error(
    spod_convert(
      type = "od", zones = "distr", dates = "2022-02-01",
      save_format = "duckdb", save_path = out_db,
      overwrite = TRUE, quiet = TRUE
    ),
    NA
  )
  
  # 5. Verify data
  tbl_con <- spod_connect(out_db)
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
  tmp_csv <- tempfile(fileext = ".csv")
  system2("gzcat", c(raw_file, ">", tmp_csv))
  lines <- readLines(tmp_csv)
  parts <- strsplit(lines[2], "|", fixed = TRUE)[[1]]
  parts[5] <- "NA" # distancia
  lines[2] <- paste(parts, collapse = "|")
  writeLines(lines, tmp_csv)
  system2("gzip", c("-c", tmp_csv, ">", raw_file))
  
  # 4. Run conversion
  out_db <- file.path(test_dir, "test_distancia_na.duckdb")
  expect_error(
    spod_convert(
      type = "od", zones = "distr", dates = "2022-02-01",
      save_format = "duckdb", save_path = out_db,
      overwrite = TRUE, quiet = TRUE
    ),
    NA
  )
  
  # 5. Verify data
  tbl_con <- spod_connect(out_db)
  res <- tbl_con %>% dplyr::collect()
  spod_disconnect(tbl_con)
  expect_true(any(is.na(res$distance)))
})
