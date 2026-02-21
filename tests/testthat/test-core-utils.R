test_that("spod_dates_argument_to_dates_seq processes inputs correctly", {
  # Mock valid dates to avoid network/cache calls
  # v1: 2020-01-01 to 2020-01-10
  # v2: 2022-01-01 to 2022-01-10
  mock_v1_dates <- seq.Date(
    as.Date("2020-01-01"),
    as.Date("2020-01-10"),
    by = "day"
  )
  mock_v2_dates <- seq.Date(
    as.Date("2022-01-01"),
    as.Date("2022-01-10"),
    by = "day"
  )

  # We need to mock spod_get_valid_dates using assignInNamespace since it's used internally
  # However, spod_dates_argument_to_dates_seq calls spod_expand_dates_from_regex which calls spod_get_valid_dates
  # And spod_dates_argument_to_dates_seq calls spod_is_data_version_overlaps which calls spod_get_valid_dates

  # Helper to mock the function
  local_mock_valid_dates <- function(env = parent.frame()) {
    mock_func <- function(ver = NULL) {
      if (ver == 1) {
        return(mock_v1_dates)
      }
      if (ver == 2) {
        return(mock_v2_dates)
      }
      stop("Invalid version")
    }

    # We use mockery stub if possible, but assignInNamespace is robust for internal calls
    # Check if we can use local_mocked_bindings (preferred if exported or visible)
    # Since spod_get_valid_dates is exported, we might be able to use it, but internal calls might bypass it.
    # Let's use assignInNamespace to be safe as per previous experience.

    orig <- get("spod_get_valid_dates", envir = asNamespace("spanishoddata"))
    assignInNamespace("spod_get_valid_dates", mock_func, ns = "spanishoddata")
    withr::defer(
      assignInNamespace("spod_get_valid_dates", orig, ns = "spanishoddata"),
      envir = env
    )
  }

  local_mock_valid_dates()

  # 1. Single Date
  expect_equal(
    spod_dates_argument_to_dates_seq("2020-01-01"),
    as.Date("2020-01-01")
  )
  expect_equal(
    spod_dates_argument_to_dates_seq("20200101"),
    as.Date("2020-01-01")
  )

  # 2. Vector
  dates_vec <- c("2020-01-01", "2020-01-03")
  expect_equal(spod_dates_argument_to_dates_seq(dates_vec), as.Date(dates_vec))

  # 3. Range String (underscore)
  expect_equal(
    spod_dates_argument_to_dates_seq("2020-01-01_2020-01-03"),
    as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
  )
  expect_equal(
    spod_dates_argument_to_dates_seq("20200101_20200103"),
    as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
  )

  # 4. Named Vector Range
  range_vec <- c(start = "2020-01-01", end = "2020-01-03")
  expect_equal(
    spod_dates_argument_to_dates_seq(range_vec),
    as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
  )

  # 5. Regex
  # This relies on spod_expand_dates_from_regex which uses our mocked dates
  # Pattern to match 2020-01-0X
  expect_equal(
    spod_dates_argument_to_dates_seq("^2020010[1-2]$"),
    as.Date(c("2020-01-01", "2020-01-02"))
  )

  # 6. Cached keywords
  expect_equal(spod_dates_argument_to_dates_seq("cached_v1"), "cached_v1")
})

test_that("spod_infer_data_v_from_dates infers versions correctly", {
  # Mock valid dates again
  mock_v1_dates <- seq.Date(
    as.Date("2020-01-01"),
    as.Date("2020-01-10"),
    by = "day"
  )
  mock_v2_dates <- seq.Date(
    as.Date("2022-01-01"),
    as.Date("2022-01-10"),
    by = "day"
  )

  local_mock_valid_dates <- function(env = parent.frame()) {
    mock_func <- function(ver = NULL) {
      if (ver == 1) {
        return(mock_v1_dates)
      }
      if (ver == 2) {
        return(mock_v2_dates)
      }
      stop("Invalid version")
    }
    orig <- get("spod_get_valid_dates", envir = asNamespace("spanishoddata"))
    assignInNamespace("spod_get_valid_dates", mock_func, ns = "spanishoddata")
    withr::defer(
      assignInNamespace("spod_get_valid_dates", orig, ns = "spanishoddata"),
      envir = env
    )
  }
  local_mock_valid_dates()

  # v1 dates
  expect_equal(spod_infer_data_v_from_dates(as.Date("2020-01-01")), 1)
  expect_equal(
    spod_infer_data_v_from_dates(as.Date(c("2020-01-01", "2020-01-02"))),
    1
  )

  # v2 dates
  expect_equal(spod_infer_data_v_from_dates(as.Date("2022-01-01")), 2)

  # Cached keywords
  expect_equal(spod_infer_data_v_from_dates("cached_v1"), 1)
  expect_equal(spod_infer_data_v_from_dates("cached_v2"), 2)

  # Overlap/Mixed -> should error (returns NULL invisible if overlap check fails?)
  # logic: spod_infer calls spod_is_data_version_overlaps which STOPS if overlap.
  expect_error(
    spod_infer_data_v_from_dates(as.Date(c("2020-01-01", "2022-01-01"))),
    "Dates found in both v1 and v2 data"
  )

  # Missing dates
  # logic: stops if missing and ignore_missing_dates = FALSE
  expect_error(
    spod_infer_data_v_from_dates(
      as.Date("2025-01-01"),
      ignore_missing_dates = FALSE
    ),
    "All requested dates are missing"
  )

  # Ignore missing dates
  # Logic: if some valid, infer from them. If none valid, return NULL?
  # 2025-01-01 is missing. 2020-01-01 is v1.
  expect_equal(
    spod_infer_data_v_from_dates(
      as.Date(c("2020-01-01", "2025-01-01")),
      ignore_missing_dates = TRUE
    ),
    1
  )
})

test_that("spod_zone_names_en2es normalizes names", {
  # English to Spanish
  expect_equal(spod_zone_names_en2es("districts"), "distritos")
  expect_equal(spod_zone_names_en2es("muni"), "municipios")
  expect_equal(spod_zone_names_en2es("lua"), "gau")

  # Passthrough
  expect_equal(spod_zone_names_en2es("distritos"), "distritos")
  expect_equal(spod_zone_names_en2es("municipios"), "municipios")

  # Case insensitive
  expect_equal(spod_zone_names_en2es("Districts"), "distritos")
})
