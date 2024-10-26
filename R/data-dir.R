#' Get the data directory
#'
#' This function retrieves the data directory from the environment variable SPANISH_OD_DATA_DIR.
#' If the environment variable is not set, it returns the temporary directory.
#' @inheritParams global_quiet_param
#' @return The data directory.
#' @export
#' @keywords internal
spod_get_data_dir <- function(quiet = FALSE) {
  data_dir_env <- Sys.getenv("SPANISH_OD_DATA_DIR")
  if (data_dir_env == "") {
    if (isFALSE(quiet)) warning("Warning: SPANISH_OD_DATA_DIR is not set. Using the temporary directory, which is not recommended, as the data will be deleted when the session ends.\n\n To set the data directory, use `Sys.setenv(SPANISH_OD_DATA_DIR = '/path/to/data')` or set SPANISH_OD_DATA_DIR permanently in the environment by editing the `.Renviron` file locally for current project with `usethis::edit_r_environ('project')` or `file.edit('.Renviron')` or globally for all projects with `usethis::edit_r_environ('user')` or `file.edit('~/.Renviron')`.")
    data_dir_env <- tempdir() # if not set, use the temp directory
  } else {
    data_dir_env <- fs::path_real(data_dir_env)
  }
  # check if dir exists and create it if it doesn't
  if (!fs::dir_exists(data_dir_env)) {
    fs::dir_create(data_dir_env)
  }
  return(data_dir_env)
}
