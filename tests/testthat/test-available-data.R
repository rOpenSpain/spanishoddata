
# skip("Requires network access - skipped to prevent downloading RSS.xml")

test_that("spod_available_data retrieves metadata from cache", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  
  # Set data dir to our test dir with fixtures
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  mock_meta_path_v2 <- system.file("testdata", "metadata", "available_data_s3_v2_mock.rds", package = "spanishoddata")
  if (mock_meta_path_v2 == "") mock_meta_path_v2 <- testthat::test_path("../../inst/testdata/metadata/available_data_s3_v2_mock.rds")
  mock_meta_v2 <- readRDS(mock_meta_path_v2)

  # Mock to prevent real RSS.xml download
  local_mocked_bindings(
    spod_available_data_s3 = function(ver, ...) {
      if (ver == 2) return(mock_meta_v2)
      return(tibble::tibble())
    },
    read_data_links_memoised = function(...) tibble::tibble(),
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
  
  mock_meta_path_v1 <- system.file("testdata", "metadata", "available_data_s3_v1_mock.rds", package = "spanishoddata")
  if (mock_meta_path_v1 == "") mock_meta_path_v1 <- testthat::test_path("../../inst/testdata/metadata/available_data_s3_v1_mock.rds")
  mock_meta_v1 <- readRDS(mock_meta_path_v1)

  # Mock to prevent real RSS.xml download
  local_mocked_bindings(
    spod_available_data_s3 = function(ver, ...) {
      if (ver == 1) return(mock_meta_v1)
      return(tibble::tibble())
    },
    read_data_links_memoised = function(...) tibble::tibble(),
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
