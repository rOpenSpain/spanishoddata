# skip("Requires network access - skipped to prevent downloading RSS.xml")

test_that("spod_available_data falls back to XML on S3 error (v1)", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Get a valid URL and size from the real package data
  real_data <- readRDS(system.file(
    "extdata",
    "available_data_v1.rds",
    package = "spanishoddata"
  ))
  target_entry <- real_data[1, ]

  # Mock S3 failure
  local_mocked_bindings(
    spod_available_data_s3 = function(...) stop("S3 bucket unavailable"),
    read_data_links_xml = function(...) {
      tibble::tibble(
        target_url = target_entry$target_url,
        pub_ts = as.POSIXct("2020-02-14", tz = "UTC"),
        file_size_bytes = 99999, # This should be ignored in favor of RDS
        etag = target_entry$etag
      )
    }
  )

  # Should warn about S3 failure or just message?
  msgs <- capture_messages(
    capture.output(
      res <- spod_available_data(ver = 1, quiet = FALSE),
      file = NULL
    )
  )
  expect_match(msgs, "S3 fetch failed", all = FALSE)

  expect_true(nrow(res) == 1)
  # Check that we got the size from RDS, not the 99999 from XML
  expected_mb <- round(target_entry$true_remote_file_size_bytes / 1024^2, 2)
  expect_equal(res$remote_file_size_mb, expected_mb)
})

test_that("spod_available_data v1 handles file size imputation", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock S3 failure + XML return with missing file size
  local_mocked_bindings(
    spod_available_data_s3 = function(...) stop("S3 fail"),
    read_data_links_xml = function(...) {
      tibble::tibble(
        target_url = "https://opendata-movilidad.mitma.es/maestra1-mitma-distritos/ficheros-diarios/2020-02/20200215_maestra1_mitma_distrito.txt.gz",
        pub_ts = as.POSIXct("2020-02-15", tz = "UTC"),
        file_size_bytes = NA_real_,
        etag = "def"
      )
    }
  )

  # Note: The code calls readRDS(system.file(...)) to get known sizes.
  # We use a URL that is likely not in the bundled RDS, so left_join results in NA size.
  # This triggers the imputation logic.

  msgs <- capture_messages(
    capture.output(
      res <- spod_available_data(ver = 1, quiet = FALSE),
      file = NULL
    )
  )
  expect_match(msgs, "S3 fetch failed", all = FALSE)

  # Since we have only 1 file and it has NA size, and imputation takes mean of category.
  # But if all are NA, mean is NA.
  # Code says: files_table$remote_file_size_mb[is.na(...)] <- mean(...)
  # If mean is NaN (0/0), it assigns NaN.
  # Let's see if it crashes.
  expect_true(nrow(res) == 1)
})

test_that("check_local_files = TRUE works", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock to prevent real RSS.xml download
  local_mocked_bindings(
    spod_get_latest_v2_file_list = function(...) NULL,
    spod_get_latest_v1_file_list = function(...) NULL
  )

  res <- spod_available_data(ver = 2, check_local_files = TRUE, quiet = TRUE)
  expect_true("downloaded" %in% names(res))
  expect_true("local_file_size" %in% names(res))
})

test_that("spod_available_data rejects invalid version number", {
  test_dir <- withr::local_tempdir()

  expect_error(
    spod_available_data(ver = 3, data_dir = test_dir),
    "Invalid version number.*Must be 1.*or 2"
  )

  expect_error(
    spod_available_data(ver = 0, data_dir = test_dir),
    "Invalid version number.*Must be 1.*or 2"
  )
})

test_that("spod_get_latest_v1_file_list handles download failure", {
  test_dir <- withr::local_tempdir()

  # Mock download.file to fail
  testthat::local_mocked_bindings(
    download.file = function(...) stop("Network error"),
    .package = "utils"
  )

  expect_error(
    spod_get_latest_v1_file_list(data_dir = test_dir, quiet = TRUE),
    "Network error"
  )
})

test_that("spod_get_latest_v2_file_list handles download failure", {
  test_dir <- withr::local_tempdir()

  # Mock download.file to fail
  testthat::local_mocked_bindings(
    download.file = function(...) stop("Network error"),
    .package = "utils"
  )

  expect_error(
    spod_get_latest_v2_file_list(data_dir = test_dir, quiet = TRUE),
    "Network error"
  )
})

test_that("spod_available_data_v2 handles file size imputation with categories", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock S3 failure and XML with missing sizes
  testthat::local_mocked_bindings(
    spod_available_data_s3 = function(...) stop("S3 unavailable"),
    read_data_links_memoised = function(...) {
      tibble::tibble(
        target_url = c(
          "https://movilidad-opendata.mitma.es/estudios_basicos/por-distritos/viajes/ficheros-diarios/2022-01/20220101_personas_distritos.csv.gz",
          "https://movilidad-opendata.mitma.es/estudios_basicos/por-distritos/viajes/ficheros-diarios/2022-01/20220102_personas_distritos.csv.gz"
        ),
        pub_ts = as.POSIXct(c("2022-01-01", "2022-01-02"), tz = "UTC"),
        file_size_bytes = c(1000000, NA_real_),
        etag = c("abc", "def")
      )
    }
  )

  msgs <- capture_messages(
    capture.output(
      res <- spod_available_data_v2(
        data_dir = test_dir,
        quiet = FALSE,
        use_s3 = TRUE
      ),
      file = NULL
    )
  )
  expect_match(msgs, "S3 fetch failed", all = FALSE)

  # Check imputation happened
  expect_true(nrow(res) == 2)
  expect_true("size_imputed" %in% names(res))
})

test_that("spod_available_data with use_s3=FALSE uses XML directly", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock XML reading
  mock_xml_called <- FALSE
  testthat::local_mocked_bindings(
    read_data_links_memoised = function(...) {
      mock_xml_called <<- TRUE
      tibble::tibble(
        target_url = "https://test.com/file.csv.gz",
        pub_ts = as.POSIXct("2022-01-01", tz = "UTC"),
        file_size_bytes = 1000000,
        etag = "abc"
      )
    }
  )

  res <- spod_available_data_v2(
    data_dir = test_dir,
    use_s3 = FALSE,
    quiet = TRUE
  )

  expect_true(mock_xml_called)
  expect_true(nrow(res) > 0)
})

test_that("read_data_links_xml with force=TRUE re-downloads", {
  test_dir <- withr::local_tempdir()
  metadata_folder <- file.path(test_dir, spod_subfolder_metadata_cache())
  dir.create(metadata_folder, recursive = TRUE)

  # Create an old XML file
  old_xml <- file.path(
    metadata_folder,
    paste0("data_links_v1_", Sys.Date() - 1, ".xml")
  )
  writeLines(
    "<xml><link>test</link><pubDate>Mon, 14 Feb 2020 00:00:00 +0000</pubDate></xml>",
    old_xml
  )

  # Mock the download function
  download_called <- FALSE
  mock_download <- function(...) {
    download_called <<- TRUE
    new_xml <- file.path(
      metadata_folder,
      paste0("data_links_v1_", Sys.Date(), ".xml")
    )
    writeLines(
      "<xml><link>new</link><pubDate>Mon, 14 Feb 2020 00:00:00 +0000</pubDate></xml>",
      new_xml
    )
    return(new_xml)
  }

  res <- read_data_links_xml(
    metadata_folder = metadata_folder,
    data_dir = test_dir,
    force = TRUE,
    quiet = TRUE,
    latest_file_function = mock_download,
    ver = 1
  )

  expect_true(download_called)
  expect_true(nrow(res) > 0)
})

test_that("read_data_links_xml uses cached file when not stale", {
  test_dir <- withr::local_tempdir()
  metadata_folder <- file.path(test_dir, spod_subfolder_metadata_cache())
  dir.create(metadata_folder, recursive = TRUE)

  # Create a fresh XML file (today's date)
  fresh_xml <- file.path(
    metadata_folder,
    paste0("data_links_v1_", Sys.Date(), ".xml")
  )
  writeLines(
    "<xml><link>cached</link><pubDate>Mon, 14 Feb 2020 00:00:00 +0000</pubDate></xml>",
    fresh_xml
  )

  # Mock the download function - should NOT be called
  download_called <- FALSE
  mock_download <- function(...) {
    download_called <<- TRUE
    stop("Should not download!")
  }

  res <- read_data_links_xml(
    metadata_folder = metadata_folder,
    data_dir = test_dir,
    force = FALSE,
    quiet = TRUE,
    latest_file_function = mock_download,
    ver = 1
  )

  expect_false(download_called)
  expect_true(nrow(res) > 0)
  expect_equal(res$target_url[1], "cached")
})

test_that("spod_available_data v2 falls back to XML on S3 error", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock S3 to fail and prevent real XML download
  testthat::local_mocked_bindings(
    spod_available_data_s3 = function(...) stop("S3 error"),
    spod_get_latest_v2_file_list = function(...) {
      xml_file <- file.path(test_dir, "metadata/data_links_v2_2024-01-01.xml")
      dir.create(dirname(xml_file), recursive = TRUE, showWarnings = FALSE)
      writeLines(
        "<rss><channel><item><link>https://test.com/file.csv.gz</link><pubDate>Mon, 01 Jan 2024 00:00:00 +0000</pubDate></item></channel></rss>",
        xml_file
      )
      return(xml_file)
    }
  )

  # Should fall back to XML and show message
  msgs <- capture_messages(
    capture.output(
      res <- spod_available_data(ver = 2, use_s3 = TRUE, quiet = FALSE),
      file = NULL
    )
  )
  expect_match(msgs, "S3 fetch failed", all = FALSE)

  expect_s3_class(res, "data.frame")
})
