# Coverage tests for R/folders.R

test_that("spod_subfolder_raw_data_cache validates version number", {
  expect_error(
    spod_subfolder_raw_data_cache(ver = 3),
    "Invalid version number"
  )
  expect_equal(spod_subfolder_raw_data_cache(ver = 1), "raw_data_cache/v1")
  expect_equal(spod_subfolder_raw_data_cache(ver = 2), "raw_data_cache/v2")
})

test_that("spod_subfolder_clean_data_cache validates version number", {
  expect_error(
    spod_subfolder_clean_data_cache(ver = 0),
    "Invalid version number"
  )
  expect_equal(spod_subfolder_clean_data_cache(ver = 1), "clean_data/v1")
  expect_equal(spod_subfolder_clean_data_cache(ver = 2), "clean_data/v2")
})
