Sys.setenv(SPANISH_OD_DATA_DIR = tempdir())

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
  expect_equal(result, seq.Date(from = as.Date("2023-07-01"), to = as.Date("2023-07-05"), by = "day"))
})

test_that("date range in YYYYMMDD format", {
  dates <- "20230701_20230705"
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, seq.Date(from = as.Date("2023-07-01"), to = as.Date("2023-07-05"), by = "day"))
})

test_that("named vector date range in ISO format", {
  dates <- c(start = "2023-07-01", end = "2023-07-05")
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, seq.Date(from = as.Date("2023-07-01"), to = as.Date("2023-07-05"), by = "day"))
})

test_that("named vector date range in YYYYMMDD format", {
  dates <- c(start = "20230701", end = "20230705")
  result <- spod_dates_argument_to_dates_seq(dates)
  expect_equal(result, seq.Date(from = as.Date("2023-07-01"), to = as.Date("2023-07-05"), by = "day"))
})

test_that("regex pattern matching dates", {
  dates <- "^202307"
  result <- spod_dates_argument_to_dates_seq(dates)
  expected_dates <- seq.Date(from = as.Date("2023-07-01"), to = as.Date("2023-07-31"), by = "day")
  expect_equal(result, expected_dates)
})

test_that("invalid input type", {
  dates <- 20230701
  expect_error(spod_dates_argument_to_dates_seq(dates), "Invalid date input format. Please provide a character vector or Date object.")
})

test_that("dates span both v1 and v2 data", {
  dates <- c("2021-05-09", "2022-01-01")
  expect_error(spod_dates_argument_to_dates_seq(dates),
    "Dates found in both v1 and v2 data.")
})

test_that("dates that are out of availabe range of v1 data", {
  dates <- c("2020-01-01", "2021-01-01")
  expect_error(spod_dates_argument_to_dates_seq(dates),
    "Some dates do not match the available data.")
})
