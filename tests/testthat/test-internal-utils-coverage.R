test_that("spod_infer_data_v_from_dates handles ignore_missing_dates", {
  # Mock valid dates
  local_mocked_bindings(
    spod_get_valid_dates = function(ver) {
      if (ver == 1) as.Date("2020-01-01") else as.Date("2022-01-01")
    },
    .package = "spanishoddata"
  )

  # Case 1: All missing (error)
  expect_error(
    spod_infer_data_v_from_dates(as.Date("2030-01-01")),
    "All requested dates are missing"
  )

  # Case 2: Some missing (default error)
  expect_error(
    spod_infer_data_v_from_dates(as.Date(c("2020-01-01", "2030-01-01"))),
    "Some dates do not match"
  )

  # Case 3: Some missing (ignore = TRUE)
  res <- spod_infer_data_v_from_dates(
    as.Date(c("2020-01-01", "2030-01-01")),
    ignore_missing_dates = TRUE
  )
  expect_equal(res, 1)

  # Case 4: No valid remaining (ignore = TRUE) -> NULL
  res_null <- spod_infer_data_v_from_dates(
    as.Date("2030-01-01"),
    ignore_missing_dates = TRUE
  )
  expect_null(res_null)

  # Case 5: helper cached request
  expect_equal(
    spod_infer_data_v_from_dates("cached_v1"),
    1
  )
  expect_equal(
    spod_infer_data_v_from_dates("cached_v2"),
    2
  )
})

test_that("spod_graphql_valid_dates works (mocked)", {
  # Clear memoise cache to ensure we test the request logic
  memoise::forget(spod_get_hmac_secret_memoised)

  # Mock httr2::req_perform to return a valid response object
  local_mocked_bindings(
    req_perform = function(...) {
      structure(
        list(
          method = "GET",
          url = "http://mock",
          status_code = 200,
          headers = list(),
          body = charToRaw(
            '<html><body><script>var import_meta_env = {"HMAC_SECRET": "1234567890"};</script></body></html>'
          )
        ),
        class = "httr2_response"
      )
    },
    .package = "httr2"
  )

  # Mock httr2::resp_body_json
  local_mocked_bindings(
    resp_body_json = function(...) {
      list(
        data = list(
          dates_find_available_dates_basic = data.frame(
            startDate = "2024-01-01",
            endDate = "2024-01-02"
          )
        )
      )
    },
    .package = "httr2"
  )

  withr::local_options(spanishoddata.graphql_api_endpoint = "http://mock")

  dates <- spod_graphql_valid_dates()
  expect_equal(dates, as.Date(c("2024-01-01", "2024-01-02")))
})

test_that("spod_assert_package handles missing package", {
  # We should use a package name that definitely doesn't exist
  pkg_name <- "NonExistentPackageXYZ"

  expect_message(
    expect_error(
      spod_assert_package(pkg_name)
      # We don't check the error message string because it comes from rlang and varies
    ),
    "required package is not installed"
  )
})

test_that("spod_match_data_type handles NULL/Warning", {
  expect_error(spod_match_data_type("invalid_type"), "should be one of")
  expect_error(
    spod_match_data_type_for_local_folders("invalid_type", 1),
    "should be one of"
  )
})
