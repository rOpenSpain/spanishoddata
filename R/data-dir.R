#' Set the data directory
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This function sets the data directory in the environment variable SPANISH_OD_DATA_DIR, so that all other functions in the package can access the data. It also creates the directory if it doesn't exist.
#'
#' @param data_dir The data directory to set.
#' @inheritParams global_quiet_param
#' @return Nothing. If quiet is `FALSE`, prints a message with the path and confirmation that the path exists.
#' @export
#' @examples
#' spod_set_data_dir(tempdir())
#'
spod_set_data_dir <- function(
  data_dir,
  quiet = FALSE
) {
  checkmate::assert_character(data_dir, len = 1, null.ok = FALSE)
  checkmate::assert_flag(quiet)

  data_dir_abs_path <- fs::path_abs(data_dir)

  tryCatch(
    {
      # Check if the directory exists; if not, attempt to create it
      if (!dir.exists(data_dir_abs_path)) {
        if (isFALSE(quiet)) {
          message(
            "Data directory ",
            data_dir_abs_path,
            " does not exist. Attempting to create it."
          )
        }
        fs::dir_create(data_dir_abs_path, recurse = TRUE)
      }

      # Check for write permissions
      test_file <- fs::path(data_dir_abs_path, ".test_write")
      file.create(test_file)
      fs::file_delete(test_file)
      if (isFALSE(quiet)) {
        message("Data directory is writeable.")
      }

      # Set the environment variable
      Sys.setenv(SPANISH_OD_DATA_DIR = data_dir_abs_path)

      if (isFALSE(quiet)) {
        message("Data directory successfully set to: ", data_dir_abs_path)
      }
    },
    error = function(e) {
      message(
        "Error: Unable to create or access the directory at '",
        data_dir_abs_path,
        "'."
      )
      message(
        "This may be due to write access restrictions or system permissions issues."
      )
      message(
        "Please verify that you have write permissions for the specified path and try again."
      )
      stop(e) # Re-throw the error for debugging purposes, if needed
    }
  )

  return(invisible(TRUE))
}

#' Get the data directory
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This function retrieves the data directory from the environment variable SPANISH_OD_DATA_DIR (and previously set by [spod_set_data_dir()]).
#' If the environment variable is not set, it returns the temporary directory.
#' @inheritParams global_quiet_param
#' @return A `character` vector of length 1 containing the path to the data directory where the package will download and convert the data.
#' @export
#' @examples
#' spod_set_data_dir(tempdir())
#' spod_get_data_dir()
#'
spod_get_data_dir <- function(quiet = FALSE) {
  checkmate::assert_flag(quiet)
  data_dir_env <- Sys.getenv("SPANISH_OD_DATA_DIR")
  if (data_dir_env == "") {
    if (isFALSE(quiet)) {
      warning(
        "Warning: SPANISH_OD_DATA_DIR is not set. Using the temporary directory, which is not recommended, as the data will be deleted when the session ends.\n\n To set the data directory, use `spod_set_data_dir('/path/to/data')` or set SPANISH_OD_DATA_DIR permanently in the environment by editing the `.Renviron` file locally for current project with `usethis::edit_r_environ('project')` or `file.edit('.Renviron')` or globally for all projects with `usethis::edit_r_environ('user')` or `file.edit('~/.Renviron')`."
      )
    }
    data_dir_env <- tempdir() # if not set, use the temp directory
  }
  # check if dir exists and create it if it doesn't
  data_dir_env_abs <- fs::path_abs(data_dir_env)
  if (!dir.exists(data_dir_env_abs)) {
    fs::dir_create(data_dir_env_abs, recurse = TRUE)
  }
  data_dir_env_real <- fs::path_abs(data_dir_env_abs)
  return(data_dir_env_real)
}
