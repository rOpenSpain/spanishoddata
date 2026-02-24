library(spanishoddata)

test_that("spod_get_hmac_secret parses secret from HTML", {
  mock_html <- '
    <html>
    <body>
    <script>
    window.import_meta_env = {"HMAC_SECRET":"this-is-a-test-secret-long-enough"};
    </script>
    </body>
    </html>
  '

  # Mock spod_httr2_req_perform
  m_resp <- structure(
    list(body = charToRaw(mock_html)),
    class = "httr2_response"
  )

  with_mocked_bindings(
    spod_httr2_req_perform = function(...) m_resp,
    spod_httr2_resp_body_string = function(...) mock_html,
    {
      secret <- spod_get_hmac_secret()
      expect_equal(secret, "this-is-a-test-secret-long-enough")
    }
  )
})

test_that("spod_get_hmac_secret fails if script missing", {
  mock_html <- "<html><body></body></html>"
  m_resp <- structure(
    list(body = charToRaw(mock_html)),
    class = "httr2_response"
  )

  with_mocked_bindings(
    spod_httr2_req_perform = function(...) m_resp,
    spod_httr2_resp_body_string = function(...) mock_html,
    {
      expect_error(spod_get_hmac_secret(), "Could not uniquely locate")
    }
  )
})

test_that("spod_query_od_raw handles API response", {
  mock_data <- list(
    data = list(
      journeys_municipality_find_by_criteria_basic = data.frame(
        origin = "01001",
        destination = "01002",
        journeys = 150,
        journeysKm = 300,
        stringsAsFactors = FALSE
      )
    )
  )

  with_mocked_bindings(
    spod_httr2_req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    spod_httr2_resp_body_json = function(...) mock_data,
    spod_session_token_and_signature = function(...) {
      list(x_session_token = "token", x_signature = "sig")
    },
    {
      res <- spod_query_od_raw(
        date_fmt = "20220101",
        graphql_distances = "D_05_2",
        id_origin = NA,
        id_destination = NA,
        min_trips = 100,
        graphql_query = list()
      )

      expect_s3_class(res, "tbl_df")
      expect_equal(nrow(res), 1)
      expect_equal(res$n_trips, 150)
      expect_equal(res$date, as.Date("2022-01-01"))
    }
  )
})

test_that("spod_query_od_raw stops on empty data", {
  mock_data <- list(
    data = list(
      journeys_municipality_find_by_criteria_basic = list()
    )
  )

  with_mocked_bindings(
    spod_httr2_req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    spod_httr2_resp_body_json = function(...) mock_data,
    spod_session_token_and_signature = function(...) {
      list(x_session_token = "token", x_signature = "sig")
    },
    {
      expect_error(
        spod_query_od_raw(
          date_fmt = "20220101",
          graphql_distances = "D_05_2",
          id_origin = NA,
          id_destination = NA,
          min_trips = 100,
          graphql_query = list()
        ),
        "No data for 20220101"
      )
    }
  )
})

test_that("spod_graphql_valid_dates parses date ranges", {
  mock_data <- list(
    data = list(
      dates_find_available_dates_basic = data.frame(
        startDate = "2022-01-01",
        endDate = "2022-01-02",
        stringsAsFactors = FALSE
      )
    )
  )

  with_mocked_bindings(
    spod_httr2_req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    spod_httr2_resp_body_json = function(...) mock_data,
    spod_session_token_and_signature = function(...) {
      list(x_session_token = "token", x_signature = "sig")
    },
    {
      dates <- spod_graphql_valid_dates()
      expect_equal(length(dates), 2)
      expect_true(as.Date("2022-01-01") %in% dates)
      expect_true(as.Date("2022-01-02") %in% dates)
    }
  )
})

test_that("spod_fetch_municipalities_json_memoised logic works", {
  # The anonymous function inside memoise is hard to test directly
  # But we can test the effect by mocking sf::st_read

  geom <- sf::st_sfc(sf::st_point(c(0, 0)))
  mock_sf <- sf::st_sf(
    ID = "01001",
    name = "Test",
    population = "1000", # Note: it's a string in JSON usually, and then converted
    geometry = geom
  )

  # Forget current cache to ensure it runs
  memoise::forget(spod_fetch_municipalities_json_memoised)

  with_mocked_bindings(
    spod_sf_st_read = function(...) mock_sf,
    {
      res <- spod_fetch_municipalities_json_memoised()
      expect_s3_class(res, "sf")
      expect_equal(res$id, "01001")
      expect_equal(res$population, 1000)
    }
  )
})
