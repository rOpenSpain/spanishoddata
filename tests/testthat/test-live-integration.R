library(testthat)
library(spanishoddata)

# This test file is designed for scheduled live integration tests on CI.
# It hits real APIs and S3 buckets to ensure the data provider infrastructure
# and the package's parsing logic remain compatible.

test_that("live integration: available data and single day download/get/convert", {
  skip_on_cran()
  skip_if_not(
    Sys.getenv("RUN_LIVE_TESTS") == "TRUE",
    "Skipping live integration tests. Set RUN_LIVE_TESTS='TRUE' to run."
  )
  
  # 1. Setup temporary directory
  test_dir <- tempfile("spod_live_")
  dir.create(test_dir)
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  test_date <- "2023-11-05"
  
  # 2. Test Available Data
  cli::cli_h2("Testing Available Data (v2)")
  meta <- spod_available_data(ver = 2, quiet = TRUE)
  expect_s3_class(meta, "tbl_df")
  expect_true(nrow(meta) > 10000)
  expect_true(all(c("target_url", "data_ymd", "file_size_bytes") %in% names(meta)))
  
  # 3. Test spod_get (Origin-Destination)
  cli::cli_h2("Testing spod_get (OD)")
  od_data <- spod_get(
    type = "od", 
    zones = "dist", 
    dates = test_date, 
    quiet = TRUE
  )
  
  # Expected columns from exploration
  expected_od_cols <- c(
    "date", "hour", "id_origin", "id_destination", "distance", 
    "activity_origin", "activity_destination", "study_possible_origin", 
    "study_possible_destination", "residence_province_ine_code", 
    "residence_province_name", "income", "age", "sex", "n_trips", 
    "trips_total_length_km", "year", "month", "day"
  )
  
  expect_s3_class(od_data, "tbl_duckdb_connection")
  expect_true(all(expected_od_cols %in% colnames(od_data)))
  
  # Verify we can collect data
  od_sample <- od_data |> head(10) |> dplyr::collect()
  expect_true(nrow(od_sample) > 0)
  expect_equal(as.character(od_sample$date[1]), test_date)
  
  # 4. Test spod_get (Number of Trips)
  cli::cli_h2("Testing spod_get (NT)")
  nt_data <- spod_get(
    type = "nt", 
    zones = "dist", 
    dates = test_date, 
    quiet = TRUE
  )
  
  expected_nt_cols <- c(
    "date", "id", "age", "sex", "n_trips", "n_persons", 
    "year", "month", "day"
  )
  expect_true(all(expected_nt_cols %in% colnames(nt_data)))
  
  # 5. Test Quick OD (GraphQL API)
  cli::cli_h2("Testing Quick OD (GraphQL)")
  qod <- spod_quick_get_od(
    date = test_date, 
    id_origin = "01001" # Alegría-Dulantzi
  )
  expect_s3_class(qod, "tbl_df")
  expect_true(nrow(qod) > 0)
  expect_true(all(c("date", "n_trips") %in% names(qod)))
  
  # 6. Test Conversion to Parquet
  cli::cli_h2("Testing Conversion to Parquet")
  save_path <- spod_convert(
    type = "od", 
    zones = "dist", 
    dates = test_date, 
    save_format = "parquet",
    quiet = TRUE
  )
  
  # Verify file existence in Hive structure
  parquet_file <- file.path(
    test_dir, 
    "clean_data/v2/tabular/parquet/od_distritos/year=2023/month=11/day=5/data_0.parquet"
  )
  expect_true(file.exists(parquet_file))
  expect_true(file.info(parquet_file)$size > 0)
  
  cli::cli_alert_success("Live integration tests completed successfully.")
})
