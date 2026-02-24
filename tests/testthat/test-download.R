library(testthat)
library(spanishoddata)

# Use real data directory fixtures
# Tests integration of metadata cache -> decision logic -> download call

test_that("spod_download validates inputs", {
  expect_error(spod_download(type = "invalid"), "Must be element of set")
  expect_error(spod_download(zones = "invalid"), "Must be element of set")
})

test_that("spod_download uses local metadata fixture (no mock on available_data)", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  testthat::local_mocked_bindings(
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) {
        return(as.Date(character(0)))
      }
      if (ver == 2) {
        return(as.Date("2022-02-01"))
      }
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )

  called <- FALSE
  testthat::local_mocked_bindings(
    spod_download_in_batches = function(files) {
      called <<- TRUE
      return(files)
    },
    .package = "spanishoddata"
  )

  spod_download(type = "od", zones = "dist", dates = "2022-02-01", quiet = TRUE)

  expect_false(called)
})

test_that("spod_download triggers download when local file is missing", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  target_file <- file.path(
    test_dir,
    "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz"
  )
  if (file.exists(target_file)) {
    unlink(target_file)
  }

  testthat::local_mocked_bindings(
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) {
        return(as.Date(character(0)))
      }
      if (ver == 2) {
        return(as.Date("2022-02-01"))
      }
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )

  download_files <- NULL
  testthat::local_mocked_bindings(
    spod_download_in_batches = function(files) {
      download_files <<- files
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      files$local_file_size <- files$file_size_bytes
      return(files)
    },
    .package = "spanishoddata"
  )

  spod_download(type = "od", zones = "dist", dates = "2022-02-01", quiet = TRUE)

  expect_true(!is.null(download_files))
  expect_equal(nrow(download_files), 1)
  expect_equal(download_files$data_ymd[1], as.Date("2022-02-01"))
})

test_that("spod_download_in_batches executes download logic", {
  test_dir <- withr::local_tempdir()
  dest_file <- file.path(test_dir, "test.gz")

  files <- tibble::tibble(
    target_url = "https://example.com/file",
    local_path = dest_file,
    file_size_bytes = 10L,
    data_ymd = as.Date("2022-01-01")
  )

  testthat::local_mocked_bindings(
    spod_download_file = function(url, destfile, ...) {
      # Fixed typo: dest -> destfile
      writeBin(raw(10), destfile)
      return(0L)
    },
    spod_interactive = function() FALSE,
    .package = "spanishoddata"
  )

  res <- spod_download_in_batches(files, show_progress = FALSE)

  expect_true(res$complete_download)
  expect_true(file.exists(dest_file))
  expect_equal(file.info(dest_file)$size, 10L)
})

test_that("spod_download integration with file size mismatch", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  target_file <- file.path(
    test_dir,
    "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz"
  )
  writeLines("short", target_file)

  testthat::local_mocked_bindings(
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) {
        return(as.Date(character(0)))
      }
      if (ver == 2) {
        return(as.Date("2022-02-01"))
      }
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )

  download_triggered <- FALSE
  testthat::local_mocked_bindings(
    spod_download_in_batches = function(files) {
      download_triggered <<- TRUE
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      return(files)
    },
    .package = "spanishoddata"
  )

  spod_download(
    type = "od",
    zones = "dist",
    dates = "2022-02-01",
    check_local_files = TRUE,
    quiet = TRUE
  )

  expect_true(download_triggered)
})
