# Prepare the testing environment using bundled xml files to avoid downloading data from the internet

extdata_path <- system.file("extdata", package = "spanishoddata")
gz_files <- list.files(
  extdata_path,
  pattern = "(data_links_.*\\.xml\\.gz)|(url_file_sizes_v[1-2]\\.csv\\.gz)",
  full.names = TRUE
)

if (length(gz_files) == 0) {
  stop("No gzipped XML files found.")
}

# Create a temporary directory
test_data_dir <- tempfile()
dir.create(test_data_dir, recursive = TRUE)
# Create metadata directory
metadata_dir <- paste0(test_data_dir, "/", spod_subfolder_metadata_cache())
dir.create(metadata_dir, recursive = TRUE)

current_date <- format(Sys.time(), format = "%Y-%m-%d", usetz = FALSE)

# Copy and rename gzipped XML files to the temporary directory
for (gz_file in gz_files) {
  if (grepl("v1", gz_file)) {
    file.copy(
      gz_file,
      file.path(metadata_dir, paste0("data_links_v1_", current_date, ".xml.gz"))
    )
  } else if (grepl("v2", gz_file)) {
    file.copy(
      gz_file,
      file.path(metadata_dir, paste0("data_links_v2_", current_date, ".xml.gz"))
    )
  }
}

# Set the environment variable to the test directory
Sys.setenv(SPANISH_OD_DATA_DIR = test_data_dir)
# Sys.getenv("SPANISH_OD_DATA_DIR")

test_that("single ISO date input", {
  dates <- "2023-07-01"
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, as.Date("2023-07-01"))
})

test_that("single YYYYMMDD date input", {
  dates <- "20230701"
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, as.Date("2023-07-01"))
})

test_that("vector of ISO dates", {
  dates <- c("2023-07-01", "2023-07-03", "2023-07-05")
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, as.Date(c("2023-07-01", "2023-07-03", "2023-07-05")))
})

test_that("vector of YYYYMMDD dates", {
  dates <- c("20230701", "20230703", "20230705")
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, as.Date(c("2023-07-01", "2023-07-03", "2023-07-05")))
})

test_that("date range in ISO format", {
  dates <- "2023-07-01_2023-07-05"
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(
    result,
    seq.Date(
      from = as.Date("2023-07-01"),
      to = as.Date("2023-07-05"),
      by = "day"
    )
  )
})

test_that("date range in YYYYMMDD format", {
  dates <- "20230701_20230705"
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(
    result,
    seq.Date(
      from = as.Date("2023-07-01"),
      to = as.Date("2023-07-05"),
      by = "day"
    )
  )
})

test_that("named vector date range in ISO format", {
  dates <- c(start = "2023-07-01", end = "2023-07-05")
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(
    result,
    seq.Date(
      from = as.Date("2023-07-01"),
      to = as.Date("2023-07-05"),
      by = "day"
    )
  )
})

test_that("named vector date range in YYYYMMDD format", {
  dates <- c(start = "20230701", end = "20230705")
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(
    result,
    seq.Date(
      from = as.Date("2023-07-01"),
      to = as.Date("2023-07-05"),
      by = "day"
    )
  )
})

test_that("regex pattern matching dates", {
  dates <- "^202307"
  result <- spod_dates_argument_to_dates_seq(dates)
  expected_dates <- seq.Date(
    from = as.Date("2023-07-01"),
    to = as.Date("2023-07-31"),
    by = "day"
  )
  expect_equal(result, expected_dates)
})

test_that("invalid input type", {
  dates <- 20230701
  expect_error(
    spod_dates_argument_to_dates_seq(dates),
    "Invalid date input format. Please provide a character vector or Date object."
  )
})

test_that("dates span both v1 and v2 data", {
  dates <- c("2021-05-09", "2022-01-01")
  expect_error(
    spod_dates_argument_to_dates_seq(dates),
    "Dates found in both v1 and v2 data."
  )
})

# clean up
unlink(test_data_dir, recursive = TRUE)
