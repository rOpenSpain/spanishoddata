
test_that("spod_get_data_dir returns default or set directory", {
  # Save original env var to restore later
  old_dir <- Sys.getenv("SPANISH_OD_DATA_DIR")
  withr::defer(if (old_dir == "") Sys.unsetenv("SPANISH_OD_DATA_DIR") else Sys.setenv(SPANISH_OD_DATA_DIR = old_dir))

  # Case 1: Default (unset)
  Sys.unsetenv("SPANISH_OD_DATA_DIR")
  expect_warning(
    dir <- spod_get_data_dir(),
    "SPANISH_OD_DATA_DIR is not set"
  )
  expect_true(nchar(as.character(dir)) > 0)

  # Case 2: Set via function
  temp_dir <- tempfile()
  spod_set_data_dir(temp_dir, quiet = TRUE)
  
  # Normalize both to avoid double slash issues / trailing slash issues
  actual <- normalizePath(as.character(spod_get_data_dir()), mustWork = FALSE)
  expected <- normalizePath(temp_dir, mustWork = FALSE)
  expect_equal(actual, expected)
  
  expect_equal(normalizePath(Sys.getenv("SPANISH_OD_DATA_DIR"), mustWork = FALSE), expected)
})



test_that("spod_set_data_dir validates input", {
  expect_error(spod_set_data_dir(123), "Must be of type 'character'")
  expect_error(spod_set_data_dir(c("a", "b")), "Must have length 1")
})

test_that("spod_get_data_dir respects env var", {
  temp_dir <- tempfile()
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = temp_dir))
  
  # Check equality as character to avoid fs_path class issues
  expect_equal(normalizePath(as.character(spod_get_data_dir()), mustWork = FALSE), normalizePath(temp_dir, mustWork = FALSE))
})
