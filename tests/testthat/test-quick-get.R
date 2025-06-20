test_that("spod_quick_get_od fails out of range dates", {
  skip_on_ci()
  skip_on_cran()
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

test_that("spod_quick_get_od fails on negative min_trips", {
  expect_error(
    spod_quick_get_od(
      date = "2022-01-02",
      min_trips = -1
    ),
    ".*Assertion.*failed.*"
  )
})

test_that("spod_quick_get_od fails on invalid municipality IDs", {
  expect_error(
    spod_quick_get_od(
      date = "2022-01-03",
      id_origin = "invalid"
    ),
    ".*Invalid municipality ID.*"
  )

  expect_error(
    spod_quick_get_od(
      date = "2022-01-04",
      id_destination = "invalid"
    ),
    ".*Invalid municipality ID.*"
  )

  expect_error(
    spod_quick_get_od(
      date = "2022-01-05",
      id_origin = "invalid",
      id_destination = "invalid"
    ),
    ".*Invalid municipality ID.*"
  )
})
