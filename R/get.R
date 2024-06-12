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
#' data_dict = get_data_dictionary()
#' head(data_dict)
get_data_dictionary = function(data_dir = get_data_dir()) {
  xml_files_list = fs::dir_ls(data_dir, type = "file", regexp = "data_links_") |> sort()
  latest_data_links_xml_path = utils::tail(xml_files_list, 1)
  if (length(latest_data_links_xml_path) == 0) {
    message("Getting latest data links xml")
    latest_data_links_xml_path = get_latest_v2_xml(data_dir = data_dir)
  }

  x_xml = xml2::read_xml(latest_data_links_xml_path)

  download_dt = data.frame(
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
  data_dir_env = Sys.getenv("SPANISHOD_DATA_DIR")
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
get_zones = function(
  data_dir = get_data_dir(),
  type = "distritos") {
  shp_extensions = c("cpg", "dbf", "prj", "qmd", "qpj", "shp", "shx")
  base_url = "https://movilidad-opendata.mitma.es/zonificacion"
  if (type == "distritos") {
  zone_data_folder = file.path(data_dir, "zonificacion_distritos")

  if (!fs::dir_exists(zone_data_folder)) {
    base_url = paste0(base_url, "/zonificacion_distritos/zonificacion_distritos")
    fs::dir_create(file.path(data_dir, "zonificacion_distritos"))
    for (ext in shp_extensions) {
    url = paste0(base_url, ".", ext)
    file = file.path(zone_data_folder, basename(url))
    utils::download.file(url, file)
    }
  }

  return(sf::read_sf(zone_data_folder))
  }
}
