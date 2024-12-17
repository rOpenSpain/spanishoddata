#' Get available data list
#' 
#' Get a table with links to available data files for the specified data version. Optionally check (see arguments) if certain files have already been downloaded into the cache directory specified with SPANISH_OD_DATA_DIR environment variable (set by \link{spod_set_data_dir}) or a custom path specified with `data_dir` argument.
#' 
#' @param ver Integer. Can be 1 or 2. The version of the data to use. v1 spans 2020-2021, v2 covers 2022 and onwards.
#' @inheritParams spod_available_data_v1
#' @inheritParams global_quiet_param
#' @return A tibble with links, release dates of files in the data, dates of data coverage, local paths to files, and the download status.
#' \describe{
#'   \item{target_url}{\code{character}. The URL link to the data file.}
#'   \item{pub_ts}{\code{POSIXct}. The timestamp of when the file was published.}
#'   \item{file_extension}{\code{character}. The file extension of the data file (e.g., 'tar', 'gz').}
#'   \item{data_ym}{\code{Date}. The year and month of the data coverage, if available.}
#'   \item{data_ymd}{\code{Date}. The specific date of the data coverage, if available.}
#'   \item{local_path}{\code{character}. The local file path where the data is stored.}
#'   \item{downloaded}{\code{logical}. Indicator of whether the data file has been downloaded locally. This is only available if `check_local_files` is `TRUE`.}
#' }
#' @export
#' @examplesIf interactive()
#' \donttest{
#' 
#' # Set data dir for file downloads
#' spod_set_data_dir(tempdir())
#' 
#' # Get available data list for v1 (2020-2021) data
#' spod_available_data(ver = 1)
#' 
#' # Get available data list for v2 (2022 onwards) data
#' spod_available_data(ver = 2)
#' 
#' # Get available data list for v2 (2022 onwards) data
#' # while also checking for local files that are already downloaded
#' spod_available_data(ver = 2, check_local_files = TRUE)
#' }
#' 
spod_available_data <- function(
  ver = 2,
  check_local_files = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir()
) {
  # Validate input
  checkmate::assertIntegerish(ver, max.len = 1)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 (for v1 2020-2021 data) or 2 (for v2 2022 onwards).")
  }
  checkmate::assert_flag(check_local_files)
  checkmate::assert_flag(quiet)
  checkmate::assert_directory_exists(data_dir, access = "rw")

  if (ver == 1) {
    return(spod_available_data_v1(data_dir = data_dir, check_local_files = check_local_files, quiet = quiet))
  } else if (ver == 2) {
    return(spod_available_data_v2(data_dir = data_dir, check_local_files = check_local_files, quiet = quiet))
  }
}

#' Get latest file list from the XML for MITMA open mobility data v1 (2020-2021)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://opendata-movilidad.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @keywords internal
spod_get_latest_v1_file_list <- function(
  data_dir = spod_get_data_dir(),
  xml_url = "https://opendata-movilidad.mitma.es/RSS.xml"
) {
if (!dir.exists(data_dir)) {
  fs::dir_create(data_dir)
}

current_date <- format(Sys.Date(), format = "%Y-%m-%d")
current_filename <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}/data_links_v1_{current_date}.xml")

# ensure dir exists
if (!dir.exists(dirname(current_filename))) {
  fs::dir_create(dirname(current_filename), recurse = TRUE)
}

message("Saving the file to: ", current_filename)
xml_requested <- curl::multi_download(
  urls = xml_url,
  destfiles = current_filename
)
return(current_filename)
}

#' Get the available v1 data list
#'
#' This function provides a table of the available data list of MITMA v1 (2020-2021), both remote and local.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param check_local_files Whether to check if the local files exist. Defaults to `FALSE`.
#' @inheritParams global_quiet_param
#' @inherit spod_available_data return
#' @importFrom rlang .data
#' @keywords internal
spod_available_data_v1 <- function(
  data_dir = spod_get_data_dir(),
  # check_local_files (below) is FALSE by default to avoid excessive filesystem access, perhaps should be TRUE. Download functions use it to load the xml file, but we probably do not want the script to check all local cache directories every time we run a get data function. Perhaps it is better to offload this check to a separate function and have a csv file or some other way to keep track of the files that were downloaded and cached. An output of curl::multi_download() could be used for this purpose.
  check_local_files = FALSE,
  quiet = FALSE) {

metadata_folder <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}")
if(!dir.exists(metadata_folder)){
  fs::dir_create(metadata_folder)
}

xml_files_list <- fs::dir_ls(metadata_folder, type = "file", regexp = "data_links_v1") |> sort()
if (length(xml_files_list) == 0) {
  if (isFALSE(quiet)) {
    message("No data links xml files found, getting latest v1 data links xml")
  }
  latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
} else {
  latest_data_links_xml_path <- utils::tail(xml_files_list, 1)
}

# Check if the XML file is 1 day old or older from its name
file_date <- stringr::str_extract(latest_data_links_xml_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")

if (file_date < format(Sys.Date(), format = "%Y-%m-%d")) {
  if (isFALSE(quiet)) {
    message("File list xml is 1 day old or older, getting latest data links xml")
  }
  latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
} else {
  if (isFALSE(quiet)) {
    message("Using existing data links xml: ", latest_data_links_xml_path)
  }
}

if (length(latest_data_links_xml_path) == 0) {
  if (isFALSE(quiet)) {
    message("Getting latest data links xml")
  }
  latest_data_links_xml_path <- spod_get_latest_v1_file_list(data_dir = data_dir)
}

x_xml <- xml2::read_xml(latest_data_links_xml_path)

files_table <- tibble::tibble(
  target_url = xml2::xml_find_all(x = x_xml, xpath = "//link") |> xml2::xml_text(),
  pub_date = xml2::xml_find_all(x = x_xml, xpath = "//pubDate") |> xml2::xml_text()
)

files_table$pub_ts <- lubridate::dmy_hms(files_table$pub_date)
files_table$file_extension <- tools::file_ext(files_table$target_url)
files_table <- files_table[files_table$file_extension != "", ]
files_table$pub_date <- NULL

files_table$data_ym <- lubridate::ym(stringr::str_extract(files_table$target_url, "[0-9]{4}-[0-9]{2}"))
files_table$data_ymd <- lubridate::ymd(stringr::str_extract(files_table$target_url, "[0-9]{8}"))
# order by pub_ts
files_table <- files_table[order(files_table$pub_ts, decreasing = TRUE), ]
files_table$local_path <- file.path(
  data_dir,
  stringr::str_replace(files_table$target_url, ".*mitma.es/", spod_subfolder_raw_data_cache(ver = 1))
)

files_table$local_path <- stringr::str_replace_all(files_table$local_path, "\\/\\/\\/|\\/\\/", "/")

# change path for daily data files to be in hive-style format
files_table$local_path <- gsub("([0-9]{4})-([0-9]{2})\\/[0-9]{6}([0-9]{2})_", "year=\\1\\/month=\\2\\/day=\\3\\/", files_table$local_path)

# fix paths for files that are in '0000-referencia' folder
files_table$local_path <- gsub("0000-referencia\\/([0-9]{4})([0-9]{2})([0-9]{2})_", "year=\\1\\/month=\\2\\/day=\\3\\/", files_table$local_path)

# replace 2 digit month with 1 digit month
files_table$local_path <- gsub("month=0([1-9])", "month=\\1", files_table$local_path)

# replace 2 digit day with 1 digit day
files_table$local_path <- gsub("day=0([1-9])", "day=\\1", files_table$local_path)

# change txt.gz to csv.gz
files_table$local_path <- gsub("\\.txt\\.gz", "\\.csv\\.gz", files_table$local_path)

# replace all municipal data download links with districts links
# this is to address the bugs described in detail in:
# http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
# http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
# the decision was to use distrcit data and aggregate it to replicate municipal data
files_table$target_url <- gsub("mitma-municipios", "mitma-distritos", files_table$target_url)
files_table$target_url <- gsub("mitma_municipio", "mitma_distrito", files_table$target_url)

# add known file sizes from cached data
file_sizes <- readr::read_csv(system.file("extdata", "url_file_sizes_v1.txt.gz", package = "spanishoddata"), show_col_types = FALSE)
files_table <- dplyr::left_join(files_table, file_sizes, by = "target_url")

# if there are files with missing sizes, impute them
if (any(is.na(files_table$remote_file_size_mb))) {
  # impute uknown file sizes
  # primitive file categorisation
  # Extract file category from the target URL
  files_table <- files_table |>
    dplyr::mutate(
      file_category = stringr::str_extract(.data$target_url, "\\/maestra(\\d)-mitma-(distritos|municipios)\\/(ficheros-diarios|meses-completos)\\/")
    )

  # Set other category for non-categorized files
  files_table$file_category[is.na(files_table$file_category)] <- "other"

  # Calculate mean file sizes by category
  size_by_file_category <- files_table |>
    dplyr::group_by(.data$file_category) |>
    dplyr::summarise(mean_file_size_mb = mean(.data$remote_file_size_mb, na.rm = TRUE))

  # Impute missing file sizes
  files_table <- files_table |>
    dplyr::left_join(size_by_file_category, by = "file_category")
  files_table$remote_file_size_mb[is.na(files_table$remote_file_size_mb)] <- mean(files_table$mean_file_size_mb)

  # Clean up temporary columns
  files_table <- files_table |>
    dplyr::select(-"mean_file_size_mb", -"file_category")
}

# now check if any of local files exist
if( check_local_files == TRUE){
  files_table$downloaded <- fs::file_exists(files_table$local_path)
}

return(files_table)
}

#' Get latest file list from the XML for MITMA open mobility data v2 (2022 onwards)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://movilidad-opendata.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @keywords internal
spod_get_latest_v2_file_list <- function(
    data_dir = spod_get_data_dir(),
    xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
) {
  if (!dir.exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}/data_links_v2_{current_date}.xml")

  # ensure dir exists
  if (!dir.exists(dirname(current_filename))) {
    fs::dir_create(dirname(current_filename), recurse = TRUE)
  }

  message("Saving the file to: ", current_filename)
  xml_requested <- curl::multi_download(
    urls = xml_url,
    destfiles = current_filename
  )
  return(current_filename)
}

#' Get the data dictionary
#'
#' This function retrieves the data dictionary for the specified data directory.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @inheritParams spod_available_data_v1
#' @inheritParams global_quiet_param
#' @inherit spod_available_data return
#' @importFrom rlang .data
#' @keywords internal
spod_available_data_v2 <- function(
  data_dir = spod_get_data_dir(),
  check_local_files = FALSE,
  quiet = FALSE
) {
  
  metadata_folder <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}")
  if(!dir.exists(metadata_folder)){
    fs::dir_create(metadata_folder)
  }

  xml_files_list <- fs::dir_ls(metadata_folder, type = "file", regexp = "data_links_v2") |> sort()
  if (length(xml_files_list) == 0) {
    if (isFALSE(quiet)) {
      message("No data links xml files found, getting latest v2 data links xml.")
    }
    latest_data_links_xml_path <- spod_get_latest_v2_file_list(data_dir = data_dir)
  } else {
    latest_data_links_xml_path <- utils::tail(xml_files_list, 1)
  }

  # Check if the XML file is 1 day old or older from its name
  file_date <- stringr::str_extract(latest_data_links_xml_path, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  
  if (file_date < format(Sys.Date(), format = "%Y-%m-%d")) {
    if (isFALSE(quiet)) {
      message("File list xml is 1 day old or older, getting latest data links xml")
    }
    latest_data_links_xml_path <- spod_get_latest_v2_file_list(data_dir = data_dir)
  } else {
    if (isFALSE(quiet)) {
      message("Using existing data links xml: ", latest_data_links_xml_path)
    }
  }

  if (length(latest_data_links_xml_path) == 0) {
    if (isFALSE(quiet)) {
      message("Getting latest data links xml")
    }
    latest_data_links_xml_path <- spod_get_latest_v2_file_list(data_dir = data_dir)
  }
  
  
  x_xml <- xml2::read_xml(latest_data_links_xml_path)

  files_table <- tibble::tibble(
    target_url = xml2::xml_find_all(x = x_xml, xpath = "//link") |> xml2::xml_text(),
    pub_date = xml2::xml_find_all(x = x_xml, xpath = "//pubDate") |> xml2::xml_text()
  )

  files_table$pub_ts <- lubridate::dmy_hms(files_table$pub_date)
  files_table$file_extension <- tools::file_ext(files_table$target_url)
  files_table <- files_table[files_table$file_extension != "", ]
  files_table$pub_date <- NULL

  files_table$data_ym <- lubridate::ym(stringr::str_extract(files_table$target_url, "[0-9]{4}-[0-9]{2}"))
  files_table$data_ymd <- lubridate::ymd(stringr::str_extract(files_table$target_url, "[0-9]{8}"))
  # order by pub_ts
  files_table <- files_table[order(files_table$pub_ts, decreasing = TRUE), ]
  files_table$local_path <- file.path(
    data_dir,
    stringr::str_replace(files_table$target_url, ".*mitma.es/",
      spod_subfolder_raw_data_cache(ver = 2))
  )
  files_table$local_path <- stringr::str_replace_all(files_table$local_path, "\\/\\/\\/|\\/\\/", "/")

  # change path for daily data files to be in hive-style format
  # TODO: check if this is needed for estudios completo and rutas
  files_table$local_path <- gsub("([0-9]{4})-([0-9]{2})\\/[0-9]{6}([0-9]{2})_", "year=\\1\\/month=\\2\\/day=\\3\\/", files_table$local_path)

  # replace 2 digit month with 1 digit month
  files_table$local_path <- gsub("month=0([1-9])", "month=\\1", files_table$local_path)

  # replace 2 digit day with 1 digit day
  files_table$local_path <- gsub("day=0([1-9])", "day=\\1", files_table$local_path)

  # lowercase GAU to avoid problems with case-sensitive matching
  files_table$local_path <- gsub("GAU", "gau", files_table$local_path)

  # now check if any of local files exist
  if( check_local_files == TRUE){
    files_table$downloaded <- fs::file_exists(files_table$local_path)
  }

  # add known file sizes from cached data
  file_sizes <- readr::read_csv(system.file("extdata", "url_file_sizes_v2.txt.gz", package = "spanishoddata"), show_col_types = FALSE)
  files_table <- dplyr::left_join(files_table, file_sizes, by = "target_url")

  # if there are files with missing sizes, impute them
  if (any(is.na(files_table$remote_file_size_mb))) {
    # impute uknown file sizes
    # primitive file categorisation 
    files_table <- files_table |>
      dplyr::mutate(
        cleaned_url = stringr::str_remove_all(.data$target_url, "/[0-9]{4}[-_][0-9]{2}[-_][0-9]{2}|/[0-9]{6,8}") |> 
                      stringr::str_remove("/[^/]+$"),
        file_category = dplyr::case_when(
          stringr::str_detect(.data$cleaned_url, "calidad") ~ "quality",
          stringr::str_detect(.data$cleaned_url, "rutas") ~ "routes",
          stringr::str_detect(.data$cleaned_url, "estudios_basicos") ~ paste0(
            "basic_studies_",
            dplyr::case_when(
              stringr::str_detect(.data$cleaned_url, "por-distritos") ~ "district_",
              stringr::str_detect(.data$cleaned_url, "por-municipios") ~ "municipal_",
              stringr::str_detect(.data$cleaned_url, "por-GAU") ~ "GAU_",
              TRUE ~ "unknown_"
            ),
            dplyr::case_when(
              stringr::str_detect(.data$cleaned_url, "viajes") ~ "trips_",
              stringr::str_detect(.data$cleaned_url, "personas") ~ "people_",
              stringr::str_detect(.data$cleaned_url, "pernoctaciones") ~ "overnight_",
              TRUE ~ "unknown_"
            ),
            ifelse(stringr::str_detect(.data$cleaned_url, "ficheros-diarios"), "daily", "monthly")
          ),
          TRUE ~ "other"
        )
      ) |>
      dplyr::select(-"cleaned_url")

    # Calculate mean file sizes by category
    size_by_file_category <- files_table |>
      dplyr::group_by(.data$file_category) |>
      dplyr::summarise(mean_file_size_mb = mean(.data$remote_file_size_mb, na.rm = TRUE))

    # Impute missing file sizes
    files_table <- dplyr::left_join(files_table, size_by_file_category, by = "file_category")
    files_table <- files_table |>
      dplyr::mutate(size_imputed = ifelse(is.na(.data$remote_file_size_mb), TRUE, FALSE))
    if(length(files_table$remote_file_size_mb[is.na(files_table$remote_file_size_mb)]) > 0){
      files_table <- files_table |>
        dplyr::mutate(remote_file_size_mb = ifelse(is.na(.data$remote_file_size_mb), .data$mean_file_size_mb, .data$remote_file_size_mb))
    }
    files_table$mean_file_size_mb <- NULL
    files_table$file_category <- NULL
  } else {
    files_table$size_imputed <- FALSE
  }

  return(files_table)
}
