test_that("V1 metadata correctly maps municipios to distritos in local_path", {
  # Mock read_data_links_xml to return a row with a municipality URL

  mock_meta <- tibble::tibble(
    target_url = "https://opendata-movilidad.mitma.es/maestra1-mitma-municipios/ficheros-diarios/2021-02/20210201_maestra_1_mitma_municipio.txt.gz",
    pub_ts = as.POSIXct("2021-03-01", tz = "UTC")
  )

  # Ensure SPANISH_OD_DATA_DIR is set to avoid warnings
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = tempdir()))

  testthat::with_mocked_bindings(
    {
      # Call the internal function directly
      res <- spod_available_data_v1(
        data_dir = tempdir(),
        check_local_files = FALSE
      )

      # Check target_url was substituted
      expect_match(res$target_url[1], "distrito")

      # Check local_path was generated AFTER substitution (regression test)
      expect_match(res$local_path[1], "distritos")
      expect_false(grepl("municipio", res$local_path[1]))

      # Ensure it correctly identifies as origin-destination and municipalities
      expect_equal(as.character(res$type[1]), "origin-destination")
      expect_equal(as.character(res$zones[1]), "municipalities")
    },
    read_data_links_xml = function(...) mock_meta,
    .package = "spanishoddata"
  )
})

test_that("spod_available_data_v1 handles missing metadata RDS gracefully", {
  mock_meta <- tibble::tibble(
    target_url = "https://example.com/test.txt.gz",
    pub_ts = as.POSIXct("2021-03-01", tz = "UTC")
  )

  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = tempdir()))

  testthat::with_mocked_bindings(
    {
      # Mock spod_get_v1_meta_path to return a non-existent path
      res <- spod_available_data_v1(data_dir = tempdir(), use_s3 = FALSE)

      # Should still return a table even if sizes are missing
      expect_s3_class(res, "data.frame")
      expect_true(nrow(res) > 0)
    },
    read_data_links_xml = function(...) mock_meta,
    spod_get_v1_meta_path = function() "/non/existent/path.rds",
    .package = "spanishoddata"
  )
})

test_that("spod_available_data_v1 correctly redirects file sizes in S3 block", {
  # Create a dummy RDS file
  test_dir <- withr::local_tempdir()
  dummy_rds <- file.path(test_dir, "dummy.rds")
  saveRDS(
    tibble::tibble(
      target_url = character(0),
      etag = character(0),
      true_etag = character(0),
      true_remote_file_size_bytes = numeric(0)
    ),
    dummy_rds
  )

  # Mock S3 return with both a district and a municipality entry
  mock_s3 <- tibble::tibble(
    target_url = c(
      "https://opendata-movilidad.mitma.es/maestra1-mitma-distritos/ficheros-diarios/2021-02/20210201_maestra_1_mitma_distrito.txt.gz",
      "https://opendata-movilidad.mitma.es/maestra1-mitma-municipios/ficheros-diarios/2021-02/20210201_maestra_1_mitma_municipio.txt.gz"
    ),
    pub_ts = rep(as.POSIXct("2021-03-01", tz = "UTC"), 2),
    file_size_bytes = c(2048, 1024),
    etag = c("e1", "e2")
  )

  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = tempdir()))

  testthat::with_mocked_bindings(
    {
      res <- spod_available_data_v1(data_dir = tempdir(), use_s3 = TRUE)

      # Both should now point to district URL
      expect_true(all(grepl("distrito", res$target_url)))
    },
    spod_available_data_s3 = function(...) mock_s3,
    spod_get_v1_meta_path = function() dummy_rds,
    .package = "spanishoddata"
  )
})

test_that("spod_download fails loudly when no data is found (safety switch)", {
  test_dir <- withr::local_tempdir()
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock available_data with expected columns but no rows
  empty_meta <- tibble::tibble(
    target_url = character(0),
    local_path = character(0),
    data_ymd = as.Date(character(0)),
    file_size_bytes = numeric(0),
    local_file_size = numeric(0)
  )

  testthat::with_mocked_bindings(
    {
      expect_error(
        spod_download(
          type = "od",
          zones = "dist",
          dates = "2021-02-01",
          data_dir = test_dir,
          ignore_missing_dates = FALSE,
          quiet = TRUE
        ),
        "No data files found for the requested criteria"
      )

      # Should not error if ignore_missing_dates = TRUE
      expect_silent(
        spod_download(
          type = "od",
          zones = "dist",
          dates = "2021-02-01",
          data_dir = test_dir,
          ignore_missing_dates = TRUE,
          quiet = TRUE
        )
      )
    },
    spod_available_data = function(...) empty_meta,
    spod_get_valid_dates = function(ver, ...) {
      if (ver == 1) {
        return(as.Date("2021-02-01"))
      }
      if (ver == 2) {
        return(as.Date("2022-02-01"))
      }
      return(as.Date(character(0)))
    },
    .package = "spanishoddata"
  )
})
