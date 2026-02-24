test_that("spod_get returns DuckDB connection with cached data", {
  # Mock valid dates to include our fixture date
  orig_valid_dates <- spod_get_valid_dates
  mock_valid_dates <- function(ver, ...) {
    if (ver == 2) {
      return(as.Date("2022-02-01"))
    }
    return(as.Date(character(0)))
  }

  assignInNamespace(
    "spod_get_valid_dates",
    mock_valid_dates,
    ns = "spanishoddata"
  )
  on.exit(
    {
      assignInNamespace(
        "spod_get_valid_dates",
        orig_valid_dates,
        ns = "spanishoddata"
      )
    },
    add = TRUE
  )

  # Setup test data
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock download (should not be triggered if cache hits, but good safety)
  local_mock_download()

  # Use assignInNamespace to robustly mock internal function
  # This works even for internal calls within the package
  mock_func <- function(...) {
    # Find the mock file we copied in setup_test_data_dir
    meta_dir <- file.path(test_dir, spod_subfolder_metadata_cache())
    files <- list.files(meta_dir, pattern = "metadata_s3_v2", full.names = TRUE)
    if (length(files) == 0) {
      stop("Mock metadata not found in test dir")
    }
    readRDS(files[1])
  }

  # Save original and mock
  orig_func <- get(
    "spod_available_data_v2",
    envir = asNamespace("spanishoddata")
  )
  # Note: assignInNamespace is locked for base/recommended packages but fine for us
  assignInNamespace("spod_available_data_v2", mock_func, ns = "spanishoddata")
  withr::defer(assignInNamespace(
    "spod_available_data_v2",
    orig_func,
    ns = "spanishoddata"
  ))

  # 1. Test OD data retrieval
  # Filter to the specific date we have in fixtures
  dates <- "2022-02-01"

  # "dist" zones + "od" type
  # This triggers:
  # - spod_available_data (mocked above)
  # - spod_get_zones (hits mock GPKG)
  # - spod_duckdb_od (hits mock CSV.gz)

  res <- spod_get(type = "od", zones = "dist", dates = dates)

  expect_s3_class(res, "tbl_duckdb_connection")

  # Collect to verify content
  df <- res |> dplyr::collect()

  expect_true(nrow(df) > 0)
  expect_true("n_trips" %in% names(df))
  expect_true("id_origin" %in% names(df))

  # Verify we got our specific filtered data (Alava IDs)
  expect_true(any(df$id_origin %in% c("01001", "01002")))

  # Clean up connection
  spod_disconnect(res)
})

test_that("spod_get handles missing dates gracefully (mocked)", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  local_mock_download()

  # Mock again for this test block (assignInNamespace persists per session but we deferred revert)
  # But we defer happens at end of test block.
  # So we re-apply here.

  mock_func <- function(...) {
    meta_dir <- file.path(test_dir, spod_subfolder_metadata_cache())
    files <- list.files(meta_dir, pattern = "metadata_s3_v2", full.names = TRUE)
    if (length(files) == 0) {
      stop("Mock metadata not found in test dir")
    }
    readRDS(files[1])
  }

  orig_func <- get(
    "spod_available_data_v2",
    envir = asNamespace("spanishoddata")
  )
  # Note: assignInNamespace is locked for base/recommended packages but fine for us
  assignInNamespace("spod_available_data_v2", mock_func, ns = "spanishoddata")
  withr::defer(assignInNamespace(
    "spod_available_data_v2",
    orig_func,
    ns = "spanishoddata"
  ))

  # Request a date we don't have
  dates <- "2025-01-01"

  # Since our mock metadata ONLY has 2022-02-01 (or whatever we put in fixture),
  # 2025-01-01 will be considered "not valid" or "not available".

  expect_error(
    spod_get(type = "od", zones = "dist", dates = dates),
    "All requested dates are missing"
  )
})
