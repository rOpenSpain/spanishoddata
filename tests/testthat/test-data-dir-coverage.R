
test_that("spod_set_data_dir quiet argument works", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  
  # When quiet = FALSE, it should message
  msgs <- capture_messages(
    spod_set_data_dir(test_dir, quiet = FALSE)
  )
  expect_match(msgs, "Data directory successfully set to", all = FALSE)
  
  # When quiet = TRUE, it should be silent
  expect_silent(
    spod_set_data_dir(test_dir, quiet = TRUE)
  )
})

test_that("spod_get_data_dir warns when env var not set", {
  # Unset env var
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = NA))
  
  # Should warn if quiet = FALSE (default)
  expect_warning(
    dir <- spod_get_data_dir(quiet = FALSE),
    "SPANISH_OD_DATA_DIR is not set"
  )
  expect_true(dir == fs::path_abs(tempdir()))
  
  # Should be silent if quiet = TRUE
  # Note: The warning is only emitted if quiet = FALSE explicitly?
  # The code: if (isFALSE(quiet)) warning(...)
  # If quiet = TRUE, no warning.
  expect_silent(
    spod_get_data_dir(quiet = TRUE)
  )
})
