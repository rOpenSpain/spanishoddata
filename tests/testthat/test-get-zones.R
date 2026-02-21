
test_that("spod_get_zones returns cached zones", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))

  # Mock download to ensure no network calls happen
  local_mock_download()

  # This should pick up "distritos_mitma.gpkg" from clean_data/v2/zones/
  zones <- spod_get_zones("dist", ver = 2, quiet = TRUE)

  expect_s3_class(zones, "sf")
  expect_true("id" %in% names(zones))
  
  # Check for our specific filtered filtered IDs (Alava)
  # fixtures generated in data-raw/generate_fixtures.R filtered for 01001, 01002
  expect_true("01001" %in% zones$id)
  expect_true("01002" %in% zones$id)
  
  # Ensure we only have the sampled data (2 rows) to confirm it used our fixture
  expect_equal(nrow(zones), 2)
  
  # Check geometry column
  expect_s3_class(sf::st_geometry(zones), "sfc_MULTIPOLYGON")
})

test_that("spod_get_zones returns municipalities", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  local_mock_download()
  
  zones <- spod_get_zones("muni", ver = 2, quiet = TRUE)
  expect_s3_class(zones, "sf")
  expect_true(nrow(zones) > 0)
})

test_that("spod_get_zones returns GAUs", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  local_mock_download()
  
  zones <- spod_get_zones("gau", ver = 2, quiet = TRUE)
  expect_s3_class(zones, "sf")
  expect_true(nrow(zones) > 0)
})

test_that("spod_get_zones validates version", {
  expect_error(spod_get_zones("dist", ver = 3), "Invalid version number")
})
