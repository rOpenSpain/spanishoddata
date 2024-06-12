
get_latest_v2_xml <- function(
  data_dir = tempdir(),
  xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
  ) {
    if (!fs::dir_exists(data_dir)) {
        fs::dir_create(data_dir)
    }
    
    current_timestamp <- format(Sys.time(), format = "%Y-%m-01", usetz = FALSE, tz = "UTC")
    current_filename <- glue::glue("cache/{current_timestamp}_data_links.xml")
    xml_requested <- curl::curl_download(url = xml_url, destfile = current_filename, quiet = FALSE)
}

load_latest_v2_xml <- function(data_dir = "data/raw_data/v2/") {
    xml_files_list <- fs::dir_ls("cache/", type = "file", regexp = "data_links.xml") |> sort()
    latest_data_links_xml_path <- tail(xml_files_list, 1)
    
    x_xml <- xml2::read_xml(latest_data_links_xml_path)
    
    download_dt <- data.table::data.table(
        target_url = xml2::xml_find_all(x = x_xml, xpath = "//link") |> xml2::xml_text(),
        pub_date = xml2::xml_find_all(x = x_xml, xpath = "//pubDate") |> xml2::xml_text()
    )
    
    download_dt[, pub_ts := lubridate::dmy_hms(pub_date)]
    download_dt[, file_extension := tools::file_ext(target_url)]
    download_dt <- download_dt[file_extension != ""]
    download_dt[, pub_date := NULL]
    
    download_dt[, data_ym := lubridate::ym(str_extract(target_url, "[0-9]{4}-[0-9]{2}"))]
    download_dt[, data_ymd := lubridate::ymd(str_extract(target_url, "[0-9]{8}"))]
    
    setorder(download_dt, pub_ts)
    
    download_dt[, local_path := paste0(data_dir, stringr::str_replace(target_url, "https://movilidad-opendata.mitma.es/", ""))]
    download_dt[, local_path := stringr::str_replace_all(local_path, "\\/\\/\\/|\\/\\/", "/")]
    
    return(download_dt)
}