
# skip("Requires network access - skipped to prevent downloading RSS.xml")

test_that("spod_available_data retrieves metadata from cache", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  
  # Set data dir to our test dir with fixtures
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  # Mock to prevent real RSS.xml download
  local_mocked_bindings(
    spod_get_latest_v2_file_list = function(...) NULL,
    spod_get_latest_v1_file_list = function(...) NULL
  )
  
  # Fetch data (should hit the cached RDS)
  # We suppress messages to keep test output clean
  meta <- suppressMessages(spod_available_data(ver = 2, check_local_files = FALSE))
  
  expect_s3_class(meta, "tbl_df")
  expect_true(nrow(meta) > 0)
  
  # Check key columns
  expect_true("target_url" %in% names(meta))
  expect_true("pub_ts" %in% names(meta))
  
  # Our mock fixture has exactly 1 row (from generate_fixtures.R)
  # But we might have more if we copied other things? No, script made 1 row.
  expect_equal(nrow(meta), 1)
  
  # Check if URL looks correct
  expect_true(grepl("20220201_data\\.csv\\.gz", meta$target_url[1]))
})

test_that("spod_available_data retrieves v1 metadata from cache", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  # Mock to prevent real RSS.xml download
  local_mocked_bindings(
    spod_get_latest_v2_file_list = function(...) NULL,
    spod_get_latest_v1_file_list = function(...) NULL
  )
  
  # Fetch data for v1
  meta <- suppressMessages(spod_available_data(ver = 1, check_local_files = FALSE))
  
  expect_s3_class(meta, "tbl_df")
  expect_true(nrow(meta) > 0)
  
  # Check key columns
  expect_true("target_url" %in% names(meta))
  expect_true("pub_ts" %in% names(meta))
  expect_true("local_path" %in% names(meta))
  
  # Check if it picked up our mock
  expect_true(any(grepl("20200214_maestra_1_distritos", meta$target_url)))
})
