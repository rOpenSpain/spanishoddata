
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

test_that("spod_set_data_dir creates a missing directory and sets the env var", {
  parent <- withr::local_tempdir()
  new_dir <- file.path(parent, "subdir_that_does_not_exist_yet")
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = NA))

  # Directory must not exist before the call
  expect_false(dir.exists(new_dir))

  spod_set_data_dir(new_dir, quiet = TRUE)

  # Behaviour: directory is created and env var is set to its absolute path
  expect_true(dir.exists(new_dir))
  expect_equal(
    Sys.getenv("SPANISH_OD_DATA_DIR"),
    as.character(fs::path_abs(new_dir))
  )
})

test_that("spod_set_data_dir on the create-new-dir path respects quiet", {
  # Existing test 'quiet argument works' only covers the case where the
  # directory already exists. This one covers the verbose branch that
  # narrates directory creation.
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = NA))

  parent_silent <- withr::local_tempdir()
  expect_silent(
    spod_set_data_dir(file.path(parent_silent, "new1"), quiet = TRUE)
  )

  parent_verbose <- withr::local_tempdir()
  expect_message(
    spod_set_data_dir(file.path(parent_verbose, "new2"), quiet = FALSE)
  )
})

test_that("spod_set_data_dir surfaces an error when the path cannot be created", {
  # Trigger the error branch by placing a regular file where a parent
  # directory is expected. Behaviour of this trick on Windows is unreliable.
  skip_on_os("windows")

  parent <- withr::local_tempdir()
  blocker <- file.path(parent, "blocker")
  writeLines("", blocker)
  bad_path <- file.path(blocker, "child")
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = NA))

  expect_error(
    suppressMessages(spod_set_data_dir(bad_path, quiet = TRUE))
  )
  # Env var must remain unset on failure
  expect_equal(Sys.getenv("SPANISH_OD_DATA_DIR"), "")
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
