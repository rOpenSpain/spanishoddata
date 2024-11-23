# Prepare the testing environment using bundled xml files to avoid downloading data from the internet

extdata_path <- system.file("extdata", package = "spanishoddata")
gz_files <- list.files(extdata_path, pattern = "(data_links_.*\\.xml\\.gz)|(url_file_sizes_v[1-2]\\.csv\\.gz)", full.names = TRUE)

if (length(gz_files) == 0) stop("No gzipped XML files found.")

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
    file.copy(gz_file, file.path(metadata_dir, paste0("data_links_v1_", current_date, ".xml.gz")))
  } else if (grepl("v2", gz_file)) {
    file.copy(gz_file, file.path(metadata_dir, paste0("data_links_v2_", current_date, ".xml.gz")))
  }
}

# Set the environment variable to the test directory
Sys.setenv(SPANISH_OD_DATA_DIR = test_data_dir)
# Sys.getenv("SPANISH_OD_DATA_DIR")

test_that("spod_quick_get_od fails out of range dates", {
  expect_error(
    spod_quick_get_od(
      date = "2021-12-31",
    ),
    ".*Must be within valid range.*"
  )
})

test_that("spod_quick_get_od fails on invalid date format", {
  expect_error(
    spod_quick_get_od(
      date = "202212-31"
    ),
    ".*Invalid date format.*"
  )
})

test_that("spod_quick_get_od fails on incorrect distances", {
  expect_error(
    spod_quick_get_od(
      date = "2022-01-01",
      distances = c("invalid", "0-200")
    ),
    ".*Invalid distance value.*"
  )
})

test_that("spod_quick_get_od fails on negarive min_trips", {
  expect_error(
    spod_quick_get_od(
      date = "2022-01-01",
      min_trips = -1
    ),
    ".*Invalid minimum number of trips.*"
  )
})

test_that("spod_quick_get_od fails on invalid municipality IDs", {
  expect_error(
    spod_quick_get_od(
      date = "2022-01-01",
      id_origin = "invalid"
    ),
    ".*Invalid municipality IDs detected.*"
  )
  
  expect_error(
    spod_quick_get_od(
      date = "2022-01-01",
      id_destination = "invalid"
    ),
    ".*Invalid municipality IDs detected.*"
  )
  
  expect_error(
    spod_quick_get_od(
      date = "2022-01-01",
      id_origin = "invalid",
      id_destination = "invalid"
    ),
    ".*Invalid municipality IDs detected.*"
  )
})
