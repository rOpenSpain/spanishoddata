#-------------------------Package Installer--------------------------

library(pacman)
package_list <- c("data.table", "tidyverse", "xml2", "fs", "glue", "httr", "lubridate", "curl", "httr2", "tictoc")
pacman::p_load(char = package_list)
rm(package_list)

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

#----------------------End of Package Installer----------------------

#------------------------------Options-------------------------------

data.table::setDTthreads(threads = parallel::detectCores())
options(scipen = 999)

#---------------------------End of Options---------------------------


# root_folder <- "/scratch/users/egor.kotov/r_mitma_get_data/"
root_folder <- paste0(getwd(), "")

#---------------------------End of Options---------------------------


# get v2 data -------------------------------------------------------------


spod_get_latest_v2_xml <- function(){
  
  if( fs::dir_exists("cache") == F ) { fs::dir_create("cache") }
  
  xml_url <- "https://movilidad-opendata.mitma.es/RSS.xml"
  current_timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", usetz = F, tz = "UTC")
  current_filename <- glue("cache/{current_timestamp}_data_links.xml")
  xml_requested <- curl::curl_download(url = xml_url, destfile = current_filename, quiet = F)
  
}

load_latest_v2_xml <- function(data_dir = "data/raw_data/v2//"){
  
  xml_files_list <- fs::dir_ls("cache/", type = "file", regexp = "data_links.xml") |> sort()
  latest_data_links_xml_path <- tail(xml_files_list, 1)
  
  x_xml <- read_xml(latest_data_links_xml_path)
  
  download_dt <- data.table( target_url = xml_find_all(x = x_xml, xpath = "//link") %>% xml2::xml_text(),
                             pub_date = xml_find_all(x = x_xml, xpath = "//pubDate") %>% xml2::xml_text())
  
  download_dt[ , pub_ts := dmy_hms(pub_date)]
  download_dt[ , file_extension := tools::file_ext(target_url), ]
  download_dt <- download_dt[ file_extension != "" ]
  download_dt[ , pub_date := NULL, ]
  
  download_dt[ , data_ym := ym( str_extract(target_url, "[0-9]{4}-[0-9]{2}") ), ]
  download_dt[ , data_ymd := ymd( str_extract(target_url, "[0-9]{8}" )), ]
  # glimpse(download_dt)
  
  download_dt <- download_dt[ file_extension != "" ]
  # the links are now fixed, so we do not need to replace part of the URL
  # download_dt[ , target_url := str_replace(target_url, "opendata-movilidad", "movilidad-opendata"), ]
  
  setorder(download_dt, pub_ts)
  
  
  download_dt[ , local_path := paste0(data_dir, str_replace(target_url, "https://movilidad-opendata.mitma.es/", "")), ]
  download_dt[ , local_path := gsub("\\/\\/\\/|\\/\\/", "\\/", local_path), ]
  
  return(download_dt)
}


# start here --------------------------------------------------------------



get_or_update_v2_data <- function() {
  
  # data that was first published in 2022 with coverage starting with 2022-01
  # https://www.mitma.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad
  
  data_dir <- "data/raw_data/mitma_mobility/v2//"
  if( fs::dir_exists(data_dir) == F ) { fs::dir_create(data_dir) }
  
  spod_get_latest_v2_xml()
  
  download_dt <- load_latest_v2_xml(data_dir = data_dir)
  
  # only download daily data (as month archives contain same data and sometimes are missing certain days) and only for census districts
  # download_dt_sub <- download_dt[ !grep("meses-completos|GAU|municipios", target_url)  ]
  download_dt_sub <- download_dt[ !grep("estudios_basicos.*meses-completos|GAU", target_url)  ]
  # download_dt_sub |> filter(grepl("obligada", local_path))
  # download_dt_sub <- download_dt[ !grep("ficheros-diarios|GAU", target_url)  ]
  to_download_file_sizes <- download_dt_sub$target_url |> map_dbl( ~get_file_size(.x),  .progress = T)
  download_dt_sub$file_size_server <- to_download_file_sizes
  
  folder_structure <- unique(dirname(download_dt_sub$local_path))
  dir_create(folder_structure, recurse = TRUE)
  
  # httr::config("ssl_verifypeer")
  # httr::set_config(config(ssl_verifypeer = FALSE))
  # httr::set_config(config(ssl_verifypeer = TRUE))
  
  
  # only download new files
  existing_file_list <- fs::dir_ls(data_dir, recurse = T, type = "file")
  length(existing_file_list)
  download_dt_sub[order(data_ymd)]
  download_dt_missing <- download_dt_sub[ ! local_path %in% gsub("\\/\\/\\/|\\/\\/", "\\/", existing_file_list) ]
  # download_dt_missing <- download_dt_sub[ pub_ts > "2024-01-01" ]
  nrow(download_dt_sub)
  nrow(download_dt_missing)
  sum(download_dt_missing$file_size_server/(10^9))
  
  curl_milti_down_report <- curl::multi_download(urls = download_dt_missing$target_url,
                                                 destfiles = download_dt_missing$local_path, progress = T, resume = T, verbose = F)
  log_timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", usetz = F, tz = "UTC")
  log_filename <- glue("cache/{log_timestamp}_download_log.csv")
  write_delim(curl_milti_down_report, log_filename, delim = ";")
  
  # download all zonification
  fs::dir_create(unique(dirname(download_dt[ grepl("zonificacion", target_url), local_path])))
  curl_milti_down_report_zonification <- curl::multi_download(urls = download_dt[ grepl("zonificacion", target_url), target_url],
                                                 destfiles = download_dt[ grepl("zonificacion", target_url), local_path], progress = T, resume = T, verbose = F)
  
 
}

verify_downloaded_data <- function(){
  
  local_files_list <- fs::dir_ls("data/raw_data/mitma_mobility/v2/", type = "file", recurse = T)
  length(local_files_list)
  
  # tools::md5sum(local_files_list[1])
  
  # local_file_sizes <- fs::file_size(local_files_list)
  # tail(unclass(local_file_sizes), 1)
  
  download_dt <- load_latest_v2_xml(data_dir = "data/raw_data/mitma_mobility/v2//")
  download_existing_files <- download_dt[ local_path %in% local_files_list]
  
  
  # 30 seconds
  tic()
  future::plan(future::multisession, workers = 10)
  file_sizes <- download_existing_files$target_url |>
    furrr::future_map_dbl(~get_file_size(x_url = .x), .progress = T)
  toc()
  future::plan(future::sequential)
  
  download_existing_files[ , remote_file_size := file_sizes, ]
  
  download_existing_files[ , local_file_size := unclass(fs::file_size(local_path)), ]
  
  download_existing_files[ local_file_size != remote_file_size ]
  
  
  file_list_for_md5 <- split(download_existing_files$local_path, ceiling(seq_along(download_existing_files$local_path) / 200))
  future::plan(future::multisession, workers = 8)
  tic()
  mdf5_checksums <- file_list_for_md5 |> furrr::future_map(~ tools::md5sum(.x), .progress = T)
  toc()
  future::plan(future::sequential)
  
  download_existing_files$md5 <- do.call(c, mdf5_checksums)
  
  # created on HYDRA
  fwrite(download_existing_files, "data/raw_data/v2/file_checksums.csv")
  
  # after upload from HYDRA check in the HPC
  download_existing_files <- fread("data/raw_data/v2/file_checksums.csv")
  download_existing_files[ , local_path := str_extract(local_path, "data\\/raw_data\\/v2.*"), ]
  exist_lower_tmp <- data.table(real_files = local_files_list, local_path = tolower(local_files_list))
  download_existing_files <- download_existing_files |> left_join(exist_lower_tmp, by = "local_path")
  
  hpc_file_list_for_md5 <- split(download_existing_files$real_files, ceiling(seq_along(download_existing_files$real_files) / 200))
  tic() # 153 seconds in HPC
  future::plan(future::multisession, workers = 8)
  mdf5_checksums_hpc <- hpc_file_list_for_md5 |> furrr::future_map(~ tools::md5sum(.x), .progress = T)
  toc()
  future::plan(future::sequential)
  
  download_existing_files$md5_hpc <- do.call(c, mdf5_checksums_hpc)
  download_existing_files[ md5 != md5_hpc, .N ]
  download_existing_files[ md5 == md5_hpc, .N ] == nrow(download_existing_files)
  # all files are intact
}


get_file_size <- function(x_url){
  
  url <- URLencode(x_url)
  
  # Perform a HEAD request to get headers
  req <- request(url) %>%
    req_method(method = "HEAD")
  
  resp <- req |> req_perform()
  
  # Extract the Content-Length header
  file_size <- resp |> resp_header("Content-Length") |> as.numeric()
  
  return(file_size)
}


# get v1 data -------------------------------------------------------------



main_2020_2021 <- function() {
  
  # 2020-2021 data
  if( fs::dir_exists("cache") == F ) { fs::dir_create("cache") }
  
  xml_url <- "https://opendata-movilidad.mitma.es/RSS.xml"
  current_timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", usetz = F, tz = "UTC")
  current_filename <- glue("cache/{current_timestamp}_data_links_2020_2021.xml")
  xml_requested <- GET(url = xml_url, write_disk(current_filename, overwrite = T))
  
  x_xml <- read_xml(current_filename)
  
  download_dt <- data.table( target_url = xml_find_all(x = x_xml, xpath = "//link") %>% xml2::xml_text(),
                             pub_date = xml_find_all(x = x_xml, xpath = "//pubDate") %>% xml2::xml_text())
  
  download_dt[ , pub_ts := dmy_hms(pub_date)]
  download_dt[ , file_extension := tools::file_ext(target_url), ]
  download_dt <- download_dt[ file_extension != "" ]
  download_dt[ , pub_date := NULL, ]
  
  download_dt[ , data_ym := ym( str_extract(target_url, "[0-9]{4}-[0-9]{2}") ), ]
  download_dt[ , data_ymd := ymd( str_extract(target_url, "[0-9]{8}" )), ]
  glimpse(download_dt)
  
  
  download_dt <- download_dt[ file_extension != "" ]
  # download_dt[ , target_url := str_replace(target_url, "opendata-movilidad", "movilidad-opendata"), ]
  
  # fwrite(download_dt, "download_list.csv")
  
  setorder(download_dt, pub_ts)
  
  if( fs::dir_exists("raw_data/2020-2021") == F ) { fs::dir_create("raw_data/2020-2021") }
  download_dt[ , local_path := paste0("raw_data/2020-2021/", str_replace(target_url, "https://opendata-movilidad.mitma.es/", "")),]
  folder_structure <- unique(dirname(download_dt$local_path))
  dir_create(folder_structure, recurse = TRUE)
  
  
  # only download new files
  existing_file_list <- fs::dir_ls("raw_data/2020-2021/", recurse = T, type = "file")
  download_dt_missing <- download_dt[ ! local_path %in% existing_file_list ]
  
  
  for( i in 1:nrow(download_dt_missing) ){
    
    print(paste0("i = ", i))
    print(paste0("Working on file: ", download_dt_missing[i, local_path]))
    
    print(paste0("No file yet. Downloading file ", download_dt_missing[i, local_path]))
    
    size_resp <- httr::RETRY(verb = "HEAD", url = URLencode(download_dt_missing[i, target_url]) )
    url_file_size <- httr::headers(size_resp)[["Content-Length"]]
    
    
    RETRY(verb = "GET", url = URLencode(download_dt_missing[i, target_url]), write_disk(download_dt_missing[i, local_path], FALSE), progress())
    
    downloaded_disk_file_size <- as.numeric(fs::file_size(download_dt_missing[i, local_path]))
    print(paste0("Downloaded file size: ", downloaded_disk_file_size))
    fwrite(append = TRUE,
           cbind(data.table(ts = format(Sys.time(),
                                        format = "%Y-%m-%dT%H-%M-%SZ",
                                        usetz = F, tz = "UTC"), download_dt_missing[i,]),
                 url_file_size = url_file_size,
                 disk_file_size = downloaded_disk_file_size,
                 status = if_else(url_file_size == downloaded_disk_file_size, "OK", "error size mismatch")),
           file = "cache/download_log_new_files.csv")
    
    Sys.sleep(1)
  }
  
  
}



main_2020_2021_old_func <- function() {
  
  
  # 2020-2021 data
  if( dir_exists("cache") == F ) { dir_create("cache") }
  
  xml_url <- "https://opendata-movilidad.mitma.es/RSS.xml"
  current_timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", usetz = F, tz = "UTC")
  current_filename <- glue("cache/{current_timestamp}_data_links.xml")
  xml_requested <- GET(url = xml_url, write_disk(current_filename, overwrite = T))
  
  x_xml <- read_xml(current_filename)
  
  download_dt <- data.table( target_url = xml_find_all(x = x_xml, xpath = "//link") %>% xml2::xml_text(),
                             pub_date = xml_find_all(x = x_xml, xpath = "//pubDate") %>% xml2::xml_text())
  download_dt[ , pub_ts := dmy_hms(pub_date)]
  download_dt[ , file_extension := tools::file_ext(target_url), ]
  download_dt <- download_dt[ file_extension != "" ]
  download_dt[ , pub_date := NULL, ]
  
  download_dt[ , data_ym := ym( str_extract(target_url, "[0-9]{4}-[0-9]{2}") ), ]
  download_dt[ , data_ymd := ymd( str_extract(target_url, "[0-9]{8}" )), ]
  glimpse(download_dt)
  download_dt %>% ggplot(aes(y = data_ym, x = as.Date(pub_ts))) + geom_jitter(alpha = 0.05) + scale_y_date(breaks = "2 months") + scale_x_date(breaks = "6 weeks")
  download_dt[ pub_ts > "2021-03-01"][order(-data_ym)]
  
  download_dt <- download_dt[ !grepl("ficheros-diarios", target_url) ]
  download_dt <- download_dt[ !grepl("meses-completos", target_url) ]
  download_dt <- download_dt[ 
    grepl("maestra1-mitma-distritos/ficheros-diarios/2021-05", target_url) |  
      grepl("maestra1-mitma-municipios/ficheros-diarios/2021-05", target_url) |  
      grepl("maestra2-mitma-distritos/ficheros-diarios/2021-03", target_url) |  
      grepl("maestra2-mitma-distritos/ficheros-diarios/2021-04", target_url) |  
      grepl("maestra2-mitma-distritos/ficheros-diarios/2021-05", target_url) |  
      grepl("maestra2-mitma-municipios/ficheros-diarios/2021-05", target_url)
  ]
  
  if( dir_exists("raw_data") == F ) { dir_create("raw_data") }
  download_dt[ , local_path := paste0("raw_data/", str_replace(target_url, "https://opendata-movilidad.mitma.es/", "")),]
  folder_structure <- unique(dirname(download_dt$local_path))
  dir_create(folder_structure, recurse = TRUE)
  
  for( i in 1:nrow(download_dt) ){
    
    if ( file_exists(download_dt[i, local_path]) == F){
      GET(url = download_dt[i, target_url], write_disk(download_dt[i, local_path], FALSE), progress())
    }
    
    if( file_exists(download_dt[i, local_path]) == T) {
      
      fwrite(append = TRUE, cbind(data.table(ts = format(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", usetz = F, tz = "UTC"), download_dt[i,]), status = "OK"), file = "cache/download_log.csv")
      print(i)
      print(paste0("OK: ", download_dt[i, target_url]))
      
    } else {
      
      fwrite(append = TRUE, cbind(data.table(ts = format(Sys.time(), format = "%Y-%m-%dT%H-%M-%SZ", usetz = F, tz = "UTC"), download_dt[i,]), status = "FAIL"), file = "cache/download_log.csv")
      print(i)
      print(paste0("FAIL: ", download_dt[i, target_url]))
      
    }
  }
  
  
}
