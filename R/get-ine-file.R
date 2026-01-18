get_ine <- function() {
  nov_2019_march_june_2020_url <- "https://www.ine.es/covid/datos_disponibles.zip"
  save_folder <- "private/ine_data"
  fs::dir_create(save_folder, recurse = TRUE)
  download_status <- download.file(
    url = nov_2019_march_june_2020_url,
    destfile = paste0(save_folder, "/datos_disponibles.zip"),
    method = "libcurl",
    mode = "wb"
  )
  if (download_status != 0) {
    stop("Failed to download INE data.")
  }
  unzip(
    zipfile = paste0(save_folder, "/datos_disponibles.zip"),
    exdir = save_folder
  )
  file_list <- fs::dir_ls(
    save_folder,
    regexp = "(DIA.*\\.zip$)|Noviembre",
    recurse = TRUE
  )
  head(file_list)
  # Unzip each archive
  zip_contents <- purrr::map(
    file_list,
    ~ unzip(
      zipfile = .x,
      list = TRUE
    )
  )
  zip_contents[1]
}
