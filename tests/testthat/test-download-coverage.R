test_that("spod_download integration flow for v1", {
  test_dir <- withr::local_tempdir()
  
  # Mock dependencies
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      tibble::tibble(
        data_ymd = as.Date("2021-02-01"),
        local_path = "v1/dist/type/distritos/data.csv.gz",
        downloaded = FALSE,
        file_size_bytes = 100L,
        remote_file_size_mb = 0.0001,
        local_file_size = NA_real_,
        target_url = "http://mock/v1"
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2021-02-01"),
    spod_infer_data_v_from_dates = function(...) 1,
    spod_match_data_type_for_local_folders = function(...) "type",
    spod_zone_names_en2es = function(z) z,
    # Mock batch downloader to just write file and return success
    spod_download_in_batches = function(files) {
      writeLines("test", file.path(test_dir, "v1/dist/type/distritos/data.csv.gz"))
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      files$local_file_size <- 100L
      return(files)
    }
  )
  
  dir.create(file.path(test_dir, "v1/dist/type/distritos"), recursive = TRUE)
  
  msgs <- capture_messages(
    res <- spod_download(type = "od", zones = "dist", dates = "2021-02-01", data_dir = test_dir, quiet = FALSE, return_local_file_paths = TRUE)
  )
  expect_match(msgs, "Retrieved data", all = FALSE)
  
  names(res) <- NULL # strip names if any
  expect_equal(basename(res), "data.csv.gz")
})

test_that("spod_download integration flow for v2", {
  test_dir <- withr::local_tempdir()
  
  # Mock dependencies
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      tibble::tibble(
        data_ymd = as.Date("2022-02-01"),
        local_path = "v2/dist/type/data.csv.gz",
        downloaded = FALSE,
        file_size_bytes = 100L,
        remote_file_size_mb = 0.0001,
        local_file_size = NA_real_,
        target_url = "http://mock/v2"
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2022-02-01"),
    spod_infer_data_v_from_dates = function(...) 2,
    spod_match_data_type_for_local_folders = function(...) "type",
    spod_zone_names_en2es = function(z) z,
    spod_download_in_batches = function(files) {
      writeLines("test", file.path(test_dir, "v2/dist/type/data.csv.gz"))
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      files$local_file_size <- 100L
      return(files)
    }
  )
  
  dir.create(file.path(test_dir, "v2/dist/type"), recursive = TRUE)
  
  msgs <- capture_messages(
    res <- spod_download(type = "od", zones = "dist", dates = "2022-02-01", data_dir = test_dir, quiet = FALSE, return_local_file_paths = TRUE)
  )
  expect_match(msgs, "Retrieved data", all = FALSE)
  expect_equal(basename(res), "data.csv.gz")
})

test_that("spod_download input validation works", {
  test_dir <- withr::local_tempdir()
  
  # Type validation
  expect_error(spod_download(type = "invalid"), "Must be element of set")
  
  # Zones validation
  expect_error(spod_download(zones = "invalid"), "Must be element of set")
  
  # Max download size validation - need valid type/zones first
  expect_error(spod_download(type = "od", zones = "dist", max_download_size_gb = 0), "Assertion on 'max_download_size_gb' failed")
  
  # Data dir validation
  expect_error(spod_download(type = "od", zones = "dist", data_dir = "/non/existent/dir"), "Assertion on 'data_dir' failed")
})

test_that("spod_download shows message for NULL dates", {
  test_dir <- withr::local_tempdir()
  # Use try() to catch the error that happens later (dates validation) but check for the message first
  expect_message(
    try(spod_download(type = "od", zones = "dist", dates = NULL, data_dir = test_dir, quiet = FALSE), silent = TRUE),
    "dates.*argument is undefined"
  )
})

test_that("spod_download asks for confirmation for large downloads", {
  test_dir <- withr::local_tempdir()
  
  # Mock available data to return a large file
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      tibble::tibble(
        data_ymd = as.Date("2022-02-01"),
        local_path = "v2/dist/type/large_file.csv.gz",
        downloaded = FALSE,
        file_size_bytes = 2 * 1024^3, # 2 GB
        remote_file_size_mb = 2048,
        local_file_size = NA_real_
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2022-02-01"),
    spod_infer_data_v_from_dates = function(...) 2,
    spod_match_data_type_for_local_folders = function(...) "type",
    # Mock spod_readline to say "no"
    spod_readline = function(...) "no"
  )
  
  # Mock checkmate to avoid complex setup
  testthat::local_mocked_bindings(
    spod_zone_names_en2es = function(z) z
  )
  
  # Should show message and return NULL when user says no
  msgs <- capture_messages(
    res <- spod_download(
      type = "od", 
      zones = "dist", 
      dates = "2022-02-01", 
      data_dir = test_dir, 
      max_download_size_gb = 1, # limit is 1GB, file is 2GB
      quiet = FALSE
    )
  )
  expect_match(msgs, "Exiting without downloading", all = FALSE)
  
  expect_null(res)
})

test_that("spod_download proceeds with large download when confirmed", {
  test_dir <- withr::local_tempdir()
  
  # Mock dependencies
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      tibble::tibble(
        data_ymd = as.Date("2022-02-01"),
        local_path = "v2/dist/type/large_file.csv.gz",
        downloaded = FALSE,
        file_size_bytes = 2 * 1024^3, # 2 GB
        remote_file_size_mb = 2048,
        local_file_size = NA_real_
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2022-02-01"),
    spod_infer_data_v_from_dates = function(...) 2,
    spod_match_data_type_for_local_folders = function(...) "type",
    spod_zone_names_en2es = function(z) z,
    # Mock spod_readline to say "yes"
    spod_readline = function(...) "yes",
    # Mock actual download to do nothing but return success
    spod_download_in_batches = function(files) {
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      files$local_file_size <- files$file_size_bytes
      return(files)
    }
  )
  
  # Should proceed and print download message
  msgs <- capture_messages(
    spod_download(
      type = "od", 
      zones = "dist", 
      dates = "2022-02-01", 
      data_dir = test_dir, 
      max_download_size_gb = 1,
      quiet = FALSE
    )
  )
  expect_match(msgs, "Downloading approximately", all = FALSE)
})

test_that("spod_download checks local files with complete_download logic", {
  test_dir <- withr::local_tempdir()
  
  # Mock data where file exists but size doesn't match
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      tibble::tibble(
        data_ymd = as.Date("2022-02-01"),
        # Path must match regex in spod_download: v2.*dist.*type
        local_path = "v2/dist/type/test_file.csv.gz",
        downloaded = FALSE,
        file_size_bytes = 1000,
        remote_file_size_mb = 0.001,
        local_file_size = 500 # Size mismatch
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2022-02-01"),
    spod_infer_data_v_from_dates = function(...) 2,
    spod_match_data_type_for_local_folders = function(...) "type",
    spod_zone_names_en2es = function(z) z
  )
  
  # Mock downloader to assert it was called
  called <- FALSE
  testthat::local_mocked_bindings(
    spod_download_in_batches = function(files) {
      called <<- TRUE
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      return(files)
    }
  )
  
  spod_download(type = "od", zones = "dist", dates = "2022-02-01", data_dir = test_dir, quiet = TRUE)
  
  expect_true(called)
})

test_that("spod_download_in_batches skips existing valid files", {
  test_dir <- withr::local_tempdir()
  dest_file <- file.path(test_dir, "existing.bin")
  writeBin(raw(10), dest_file)
  
  files <- tibble::tibble(
    target_url = "http://mock/file",
    local_path = dest_file,
    file_size_bytes = 10L,
    data_ymd = as.Date("2022-01-01"),
    downloaded = FALSE,
    local_file_size = NA_integer_,
    complete_download = NA
  )
  
  called <- FALSE
  testthat::local_mocked_bindings(
    spod_download_file = function(...) {
      called <<- TRUE
      return(0L)
    },
    spod_interactive = function() FALSE
  )
  
  res <- spod_download_in_batches(files, show_progress = FALSE)
  
  expect_false(called) # Should NOT call download
  expect_true(res$complete_download)
  expect_equal(res$local_file_size, 10L)
})

test_that("spod_download_in_batches handles max retries failure", {
  skip_on_cran()
  
  test_dir <- withr::local_tempdir()
  dest_file <- file.path(test_dir, "bad.bin")
  
  files <- tibble::tibble(
    target_url = "http://mock/fail",
    local_path = dest_file,
    file_size_bytes = 10L,
    data_ymd = as.Date("2022-01-01")
  )
  
  call_count <- 0
  testthat::local_mocked_bindings(
    spod_download_file = function(url, destfile, ...) {
      call_count <<- call_count + 1
      writeBin(raw(5), destfile) # Always wrong size
      return(0L)
    },
    spod_interactive = function() FALSE
  )
  
  expect_warning(
    res <- spod_download_in_batches(files, max_retries = 2, show_progress = FALSE),
    "Failed to download"
  )
  
  expect_false(res$complete_download)
  expect_equal(call_count, 1 + 1) # Initial + 1 retry (max_retries=2 means 2 total attempts code-wise)
})

test_that("spod_download_in_batches runs speed test in interactive mode", {
  skip_on_cran()
  
  test_dir <- withr::local_tempdir()
  dest_file <- file.path(test_dir, "dest.bin")
  
  files <- tibble::tibble(
    target_url = "http://mock/speedtest",
    local_path = dest_file,
    file_size_bytes = 10L,
    data_ymd = as.Date("2022-01-01")
  )
  
  # Mock interactive to TRUE to trigger speed test
  testthat::local_mocked_bindings(
    spod_interactive = function() TRUE,
    spod_download_file = function(url, destfile, ...) {
      writeBin(raw(10), destfile)
      return(0L)
    }
  )
  
  # We need to mock url connection for the speed test (lines 626-638)
  # It uses url(url, "rb"). This is hard to mock without wrapping.
  # But we can use a local file URL!
  
  src_file <- file.path(test_dir, "source.bin")
  writeBin(as.raw(1:100), src_file)
  files$target_url <- paste0("file://", src_file)
  
  # Redirect output to capture progress bar
  output <- capture.output(
    res <- spod_download_in_batches(files, show_progress = TRUE, test_size = 10)
  )
  
  expect_true(res$complete_download)
  expect_true(res$complete_download)
})

test_that("spod_download_in_batches handles multiple files in matches", {
  skip_on_cran()
  
  test_dir <- withr::local_tempdir()
  f1 <- file.path(test_dir, "f1.bin")
  f2 <- file.path(test_dir, "f2.bin")
  
  files <- tibble::tibble(
    target_url = c("http://mock/1", "http://mock/2"),
    local_path = c(f1, f2),
    file_size_bytes = c(10L, 10L),
    data_ymd = as.Date(c("2022-01-01", "2022-01-02"))
  )
  
  # Mock download file
  testthat::local_mocked_bindings(
    spod_download_file = function(url, destfile, ...) {
      writeBin(raw(10), destfile)
      return(0L)
    },
    spod_interactive = function() FALSE
  )
  
  # Set batch size to 1 to force loop iterations
  res <- spod_download_in_batches(files, batch_size = 1, show_progress = FALSE)
  
  expect_true(all(res$complete_download))
  expect_true(file.exists(f1))
  expect_true(file.exists(f2))
})

test_that("spod_download with check_local_files=FALSE skips local file validation", {
  test_dir <- withr::local_tempdir()
  local_file <- file.path(test_dir, "v2/distritos/viajes/test.csv.gz")
  dir.create(dirname(local_file), recursive = TRUE)
  
  # Create a file with wrong size
  writeBin(raw(50), local_file)  # File is 50 bytes
  
  # Mock available_data to return file info  
  # When check_local_files=FALSE, local_file_size should be NA
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      list_arg <- list(...)
      check_local <- list_arg$check_local_files
      
      tibble::tibble(
        data_ymd = as.Date("2022-02-01"),
        local_path = local_file,
        downloaded = if (isTRUE(check_local)) TRUE else FALSE,  # Only mark as downloaded when checked
        file_size_bytes = 100L,  # Expected: 100 bytes
        remote_file_size_mb = 0.0001,
        local_file_size = if (isTRUE(check_local)) 50 else NA_real_  # Size check only when enabled
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2022-02-01"),
    spod_infer_data_v_from_dates = function(...) 2,
    spod_match_data_type_for_local_folders = function(...) "viajes",
    spod_zone_names_en2es = function(z) "distritos"
  )
  
  download_called <- FALSE
  testthat::local_mocked_bindings(
    spod_download_in_batches = function(files) {
      download_called <<- TRUE
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      return(files)
    }
  )
  
  # With check_local_files=FALSE, should still trigger download because local_file_size is NA
  # which means complete_download will be FALSE
  spod_download(
    type = "od",
    zones = "dist",
    dates = "2022-02-01",
    data_dir = test_dir,
    check_local_files = FALSE,
    quiet = TRUE
  )
  
  # This test documents current behavior: when check_local_files=FALSE,
  # file is still downloaded because size comparison fails (NA != expected)
  expect_true(download_called)
})

test_that("spod_download with check_local_files=TRUE detects size mismatch", {
  test_dir <- withr::local_tempdir()
  local_file <- file.path(test_dir, "v2/distritos/viajes/test.csv.gz")
  dir.create(dirname(local_file), recursive = TRUE)
  
  # Create a file with wrong size
  writeBin(raw(50), local_file)  # File is 50 bytes
  
  # Mock available_data to return file info with different expected size
  testthat::local_mocked_bindings(
    spod_available_data = function(...) {
      tibble::tibble(
        data_ymd = as.Date("2022-02-01"),
        local_path = local_file,
        downloaded = TRUE,
        file_size_bytes = 100L,  # Expected: 100 bytes
        remote_file_size_mb = 0.0001,
        local_file_size = 50  # Actual: 50 bytes (mismatch!)
      )
    },
    spod_dates_argument_to_dates_seq = function(...) as.Date("2022-02-01"),
    spod_infer_data_v_from_dates = function(...) 2,
    spod_match_data_type_for_local_folders = function(...) "viajes",
    spod_zone_names_en2es = function(z) "distritos"
  )
  
  download_called <- FALSE
  testthat::local_mocked_bindings(
    spod_download_in_batches = function(files) {
      download_called <<- TRUE
      files$downloaded <- TRUE
      files$complete_download <- TRUE
      files$local_file_size <- files$file_size_bytes
      return(files)
    }
  )
  
  # With check_local_files=TRUE (default), should trigger download due to size mismatch
  spod_download(
    type = "od",
    zones = "dist",
    dates = "2022-02-01",
    data_dir = test_dir,
    check_local_files = TRUE,
    quiet = TRUE
  )
  
  # SHOULD download because size mismatch detected
  expect_true(download_called)
})
