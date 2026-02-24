library(testthat)
library(spanishoddata)

# This test file is designed for scheduled live integration tests on CI.
# It hits real APIs and S3 buckets to ensure the data provider infrastructure
# and the package's parsing logic remain compatible.

test_that("live integration: v1 and v2 data validation with numerical expectations", {
  skip_on_cran()
  skip_if_not(
    Sys.getenv("RUN_LIVE_TESTS") == "TRUE",
    "Skipping live integration tests. Set RUN_LIVE_TESTS='TRUE' to run."
  )
  
  # Setup temporary directory for isolation
  test_dir <- tempfile("spod_live_")
  dir.create(test_dir)
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  v1_date <- "2020-02-14"
  v2_date <- "2023-11-05"
  
  # --- V1 INTEGRATION TESTS ---
  cli::cli_h1("Testing V1 Integration (2020-2021)")
  
  # 1. Get Zones V1
  cli::cli_h2("Testing spod_get_zones (V1)")
  v1_zones <- spod_get_zones("distr", ver = 1, quiet = TRUE)
  expect_s3_class(v1_zones, "sf")
  expect_equal(nrow(v1_zones), 2850) # Exact match for districts
  
  # 2. Get OD V1
  cli::cli_h2("Testing spod_get OD (V1)")
  v1_od <- spod_get(type = "od", zones = "dist", dates = v1_date, quiet = TRUE)
  expect_s3_class(v1_od, "tbl_duckdb_connection")
  
  v1_summary <- v1_od |> 
    dplyr::summarise(
      total_trips = sum(n_trips, na.rm = TRUE),
      total_rows = dplyr::n()
    ) |> 
    dplyr::collect()
  
  # Verify summary statistics with small tolerance (0.1% for trips)
  expect_equal(v1_summary$total_rows, 7232427)
  expect_equal(v1_summary$total_trips, 145131837, tolerance = 0.001)
  
  
  # --- V2 INTEGRATION TESTS ---
  cli::cli_h1("Testing V2 Integration (2022 onwards)")
  
  # 1. Get Zones V2
  cli::cli_h2("Testing spod_get_zones (V2)")
  v2_zones <- spod_get_zones("distr", ver = 2, quiet = TRUE)
  expect_s3_class(v2_zones, "sf")
  expect_equal(nrow(v2_zones), 3909) # Exact match for V2 districts
  
  # 2. Get OD V2
  cli::cli_h2("Testing spod_get OD (V2)")
  v2_od <- spod_get(type = "od", zones = "dist", dates = v2_date, quiet = TRUE)
  expect_s3_class(v2_od, "tbl_duckdb_connection")
  
  v2_od_summary <- v2_od |> 
    dplyr::summarise(
      total_trips = sum(n_trips, na.rm = TRUE),
      total_rows = dplyr::n()
    ) |> 
    dplyr::collect()
  
  expect_equal(v2_od_summary$total_rows, 15257106)
  expect_equal(v2_od_summary$total_trips, 104758528, tolerance = 0.001)
  
  # 3. Get NT V2
  cli::cli_h2("Testing spod_get NT (V2)")
  v2_nt <- spod_get(type = "nt", zones = "dist", dates = v2_date, quiet = TRUE)
  
  v2_nt_summary <- v2_nt |> 
    dplyr::summarise(
      total_persons = sum(n_persons, na.rm = TRUE),
      total_rows = dplyr::n()
    ) |> 
    dplyr::collect()
  
  expect_equal(v2_nt_summary$total_rows, 118717)
  expect_equal(v2_nt_summary$total_persons, 47399843, tolerance = 0.001)
  
  # 4. Quick OD (GraphQL)
  cli::cli_h2("Testing spod_quick_get_od (GraphQL)")
  qod <- spod_quick_get_od(date = v2_date, id_origin = "01001")
  expect_s3_class(qod, "tbl_df")
  expect_equal(nrow(qod), 4)
  expect_equal(sum(qod$n_trips), 3590, tolerance = 0.001)
  
  cli::cli_alert_success("Live integration tests for v1 and v2 completed successfully.")
})
