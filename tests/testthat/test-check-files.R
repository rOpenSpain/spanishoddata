library(testthat)
library(spanishoddata)

# Tests using real fixtures to verify ETag computation and consistency checking logic

test_that("spod_check_files detects inconsistency in fixtures", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  testthat::local_mocked_bindings(
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) return(as.Date(character(0)))
      if (ver == 2) return(as.Date("2022-02-01"))
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )
  
  expect_warning(
    res <- suppressMessages(spod_check_files(type = "od", zones = "dist", dates = "2022-02-01", quiet = FALSE)),
    "Some files are inconsistent"
  )
  
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)
  expect_false(all(res$local_file_consistent))
  
  expect_true(nchar(res$local_etag[1]) >= 32)
  expect_true(res$local_etag[1] != res$etag[1])
})

test_that("spod_check_files reports consistency when metadata matches", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  target_file <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz")
  real_etag <- spanishoddata:::spod_compute_s3_etag(target_file)
  
  meta_cache_path <- file.path(test_dir, spanishoddata:::spod_subfolder_metadata_cache())
  rds_files <- list.files(meta_cache_path, full.names = TRUE, pattern = "metadata_s3_v2_.*\\.rds")
  expect_true(length(rds_files) > 0)
  
  meta <- readRDS(rds_files[1])
  meta$etag <- real_etag
  saveRDS(meta, rds_files[1])
  
  testthat::local_mocked_bindings(
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) return(as.Date(character(0)))
      if (ver == 2) return(as.Date("2022-02-01"))
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )
  
  msgs <- capture_messages(
    res <- spod_check_files(type = "od", zones = "dist", dates = "2022-02-01", quiet = FALSE)
  )
  
  expect_match(msgs, "All checked files are consistent", all = FALSE)
  expect_true(all(res$local_file_consistent))
})

test_that("spod_check_files detects missing files in fixtures", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  target_file <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz")
  unlink(target_file)
  
  testthat::local_mocked_bindings(
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) return(as.Date(character(0)))
      if (ver == 2) return(as.Date("2022-02-01"))
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )
  
  expect_warning(
    res <- suppressMessages(spod_check_files(type = "od", zones = "dist", dates = "2022-02-01", quiet = FALSE)),
    "Some files.*are missing"
  )
  
  expect_equal(nrow(res), 0)
})
