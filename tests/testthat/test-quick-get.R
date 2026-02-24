test_that("spod_quick_get_od fails out of range dates", {
  skip_on_ci()
  skip_on_cran()

  # Mock valid dates to return something that makes "2021-12-31" technically out of range but valid for checking
  # Actually, the error happens because the function calls spod_graphql_valid_dates which does a network call.
  # We should mock that helper to avoid network calls.

  local_mocked_bindings(
    spod_graphql_valid_dates_memoised = function() {
      return(as.Date("2022-01-01"))
    }
  )

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

test_that("spod_quick_get_od fetches data (mocked)", {
  # Mock valid dates
  orig_dates_func <- spod_graphql_valid_dates_memoised
  mock_dates_func <- function() {
    return(as.Date("2022-01-01"))
  }
  assignInNamespace(
    "spod_graphql_valid_dates_memoised",
    mock_dates_func,
    ns = "spanishoddata"
  )
  on.exit(
    assignInNamespace(
      "spod_graphql_valid_dates_memoised",
      orig_dates_func,
      ns = "spanishoddata"
    ),
    add = TRUE
  )

  # Mock query function
  orig_query_func <- spod_query_od_memoised
  mock_query_func <- function(date_fmt, id_origin, ...) {
    # Return dummy tibble matching structure
    dplyr::tibble(
      date = as.Date(date_fmt, format = "%Y%m%d"),
      id_origin = ifelse(all(is.na(id_origin)), "01001", id_origin[1]),
      id_destination = "01002",
      n_trips = 100,
      trips_total_length_km = 500
    )
  }
  assignInNamespace(
    "spod_query_od_memoised",
    mock_query_func,
    ns = "spanishoddata"
  )
  on.exit(
    assignInNamespace(
      "spod_query_od_memoised",
      orig_query_func,
      ns = "spanishoddata"
    ),
    add = TRUE
  )

  # Run test
  # Note: 01001 is a valid municipality code (Alegría-Dulantzi)
  res <- spod_quick_get_od(date = "2022-01-01", id_origin = "01001")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$n_trips, 100)
  expect_equal(res$id_origin, "01001")
})

test_that("spod_quick_get_zones fetches geometries (mocked)", {
  # Mock geometry fetch
  orig_geo_func <- spod_fetch_municipalities_json_memoised
  mock_geo_func <- function() {
    # Create simple sf object
    geom <- sf::st_sfc(sf::st_point(c(0, 0)))
    sf::st_sf(
      id = "01001",
      name = "Mock Muni",
      population = 1000,
      geometry = geom
    )
  }
  assignInNamespace(
    "spod_fetch_municipalities_json_memoised",
    mock_geo_func,
    ns = "spanishoddata"
  )
  on.exit(
    assignInNamespace(
      "spod_fetch_municipalities_json_memoised",
      orig_geo_func,
      ns = "spanishoddata"
    ),
    add = TRUE
  )

  res <- spod_quick_get_zones(zones = "muni")

  expect_s3_class(res, "sf")
  expect_true("population" %in% names(res))
  expect_equal(res$id[1], "01001")
})
