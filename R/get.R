#' Get latest file list from the XML for MITMA open mobiltiy data v2 (2022 onwards)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://movilidad-opendata.mitma.es/RSS.xml".
#' @param current_timestamp The current timestamp to keep track of the version of the remote file list. Defaults to the current date.
#'
#' @return The path to the downloaded XML file.
#' @export
#' @examples
#' if (FALSE) {
#' get_latest_v2_xml()
#' }
get_latest_v2_xml = function(
    data_dir = get_data_dir(),
    xml_url = "https://movilidad-opendata.mitma.es/RSS.xml",
    current_timestamp = format(Sys.time(), format = "%Y-%m-01", usetz = FALSE, tz = "UTC")) {
  if (!fs::dir_exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_filename = glue::glue("{data_dir}/data_links_{current_timestamp}.xml")

  message("Saving the file to: ", current_filename)
  xml_requested = curl::curl_download(url = xml_url, destfile = current_filename, quiet = FALSE)
  return(current_filename)
}

#' Get the data dictionary
#'
#' This function retrieves the data dictionary for the specified data directory.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `get_data_dir()`.
#' @return The data dictionary.
#' @export
#' @examples
#' # Get the data dictionary for the default data directory
#' metadata = get_metadata()
#' names(metadata)
#' head(metadata)
get_metadata = function(data_dir = get_data_dir()) {
  xml_files_list = fs::dir_ls(data_dir, type = "file", regexp = "data_links_") |> sort()
  latest_data_links_xml_path = utils::tail(xml_files_list, 1)
  if (length(latest_data_links_xml_path) == 0) {
    message("Getting latest data links xml")
    latest_data_links_xml_path = get_latest_v2_xml(data_dir = data_dir)
  } else {
    message("Using existing data links xml: ", latest_data_links_xml_path)
  }

  x_xml = xml2::read_xml(latest_data_links_xml_path)

  download_dt = tibble::tibble(
    target_url = xml2::xml_find_all(x = x_xml, xpath = "//link") |> xml2::xml_text(),
    pub_date = xml2::xml_find_all(x = x_xml, xpath = "//pubDate") |> xml2::xml_text()
  )

  download_dt$pub_ts = lubridate::dmy_hms(download_dt$pub_date)
  download_dt$file_extension = tools::file_ext(download_dt$target_url)
  download_dt = download_dt[download_dt$file_extension != "", ]
  download_dt$pub_date = NULL

  download_dt$data_ym = lubridate::ym(stringr::str_extract(download_dt$target_url, "[0-9]{4}-[0-9]{2}"))
  download_dt$data_ymd = lubridate::ymd(stringr::str_extract(download_dt$target_url, "[0-9]{8}"))
  # order by pub_ts
  download_dt = download_dt[order(download_dt$pub_ts, decreasing = TRUE), ]
  download_dt$local_path = file.path(
    data_dir,
    stringr::str_replace(download_dt$target_url, "https://movilidad-opendata.mitma.es/", "")
  )
  download_dt$local_path = stringr::str_replace_all(download_dt$local_path, "\\/\\/\\/|\\/\\/", "/")

  return(download_dt)
}

get_data_dir = function() {
  data_dir_env = Sys.getenv("SPANISH_OD_DATA_DIR")
  if (data_dir_env == "") {
    data_dir_env = tempdir()
  }
  return(data_dir_env)
}

#' Retrieves the zones data
#'
#' This function retrieves the zones data from the specified data directory.
#' It can retrieve either "distritos" or "municipios" zones data.
#'
#' @param data_dir The directory where the data is stored.
#' @param type The type of zones data to retrieve ("distritos" or "municipios").
#' @return A spatial object containing the zones data.
#' @export
#' @examples
#' zones = get_zones()
get_zones = function(
  data_dir = get_data_dir(),
  type = "distritos") {
  metadata = get_metadata(data_dir)
  regex = glue::glue("zonificacion_{type}\\.")
  sel_distritos = stringr::str_detect(metadata$target_url, regex)
  metadata_distritos = metadata[sel_distritos, ]
  dir_name = dirname(metadata_distritos$local_path[1])
  if (!fs::dir_exists(dir_name)) {
    fs::dir_create(dir_name)
  }
  for (i in 1:nrow(metadata_distritos)) {
    if (!fs::file_exists(metadata_distritos$local_path[i])) {
      message("Downloading ", metadata_distritos$target_url[i])
      curl::curl_download(url = metadata_distritos$target_url[i], destfile = metadata_distritos$local_path[i], quiet = FALSE)
    }
  }
  sel_shp = stringr::str_detect(metadata_distritos$local_path, "\\.shp$")
  shp_file = metadata_distritos$local_path[sel_shp]
  suppressWarnings({
    return(sf::read_sf(shp_file))
  })
}

#' Retrieves the origin-destination data
#'
#' This function downloads data from URLs such as
#' https://movilidad-opendata.mitma.es/estudios_basicos/por-distritos/viajes/ficheros-diarios/2024-03/20240301_Viajes_distritos.csv.gz
#' if the file does not exist in the data directory.
#'
#' @param data_dir The directory where the data is stored.
#' @param subdir The subdirectory where the data is stored.
#' @param date_regex The regular expression to match the date of the data to download.
#' @param read_fun The function to read the data. Defaults to `duckdb::tbl_file`.
#' @return The local path of the downloaded file (`download_od`), or a data frame with the origin-destination data (`get_od`).
#' @export
#' @examples
#' # Download the origin-destination data for the first two days of March 2024
#' if (FALSE) {
#' od_20240301_20240302 = get_od(date_regex = "2024-03-0[1-2]")
#' }
get_od = function(
  data_dir = get_data_dir(),
  subdir = "estudios_basicos/por-distritos/viajes/ficheros-diarios",
  date_regex = "2024030[1-2]",
  read_fun = duckdb::tbl_file
) {
  file_paths = download_od(data_dir = data_dir, subdir = subdir, date_regex = date_regex)
  if (identical(read_fun, readr::read_csv)) {
    return(purrr::map_dfr(file_paths, read_fun))
  }
  drv = duckdb::duckdb()
  con = DBI::dbConnect(drv)
  # file.exists(file_paths[1])
  # od1 = duckdb::tbl_file(con, file_paths[2])
  od_list = purrr::map(file_paths, ~duckdb::tbl_file(con, .))
}
download_od = function(
  data_dir = get_data_dir(),
  subdir = "estudios_basicos/por-distritos/viajes/ficheros-diarios",
  date_regex = "2024030[1-2]"
) {
  regex = glue::glue("{subdir}*.+{date_regex}_Viajes_distritos.csv.gz")
  metadata = get_metadata(data_dir)
  sel_od = stringr::str_detect(metadata$target_url, regex)
  metadata_od = metadata[sel_od, ]
  metadata_od[[1]]
  dir_name = dirname(metadata_od$local_path[1])
  if (!fs::dir_exists(dir_name)) {
    fs::dir_create(dir_name)
  }
  for (i in 1:nrow(metadata_od)) {
    if (!fs::file_exists(metadata_od$local_path[i])) {
      message("Downloading ", metadata_od$target_url[i])
      curl::curl_download(url = metadata_od$target_url[i], destfile = metadata_od$local_path[i], quiet = FALSE)
    }
  }
  return(metadata_od$local_path)
}
