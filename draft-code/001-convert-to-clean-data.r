#-------------------------Package Installer--------------------------

library(pacman)
package_list <- c("data.table", "tidyverse", "arrow", "fs", "lubridate", "tictoc", "R.utils", "furrr", "duckdb", "bench", "stringr", "sf", "conflicted")
pacman::p_load(char = package_list)
rm(package_list)

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::day)

#----------------------End of Package Installer----------------------

#------------------------------Options-------------------------------

data.table::setDTthreads(threads = parallel::detectCores())
options(scipen = 999)

#---------------------------End of Options---------------------------

# https://arrow-user2022.netlify.app/data-wrangling.html#example-5-basic-joins
# https://www.christophenicault.com/post/large_dataframe_arrow_duckdb/


root_folder <- "/scratch/users/egor.kotov/spain_mobility/"
# root_folder <- "./"

main <- function(){
  
  check_for_missing_days_in_files()
  # spatial_import()
  
  
  
}


# import v2 data ----------------------------------------------------------



# spatial data ------------------------------------------------------------

spatial_import <- function() {
  
  
  if( fs::dir_exists(here::here(root_folder, "data/clean_data/mitma_mobility/v2/spatial/")) == F ) { fs::dir_create(here::here(root_folder, "data/clean_data/mitma_mobility/v2/spatial/")) }
  

# all population ----------------------------------------------------------

  pop_all <- fread("data/raw_data/mitma_mobility/v2/zonificacion/poblacion.csv")
  length(unique(pop_all$distrito))
  length(unique(pop_all$municipio))
  pop_all[ municipio == "01059", sum(poblacion)]
  # municip_pop[ id == "01059"]
  
  
  # id_relations <- fread("raw_data/2022/zonificacion/relacion_ine_zonificacionMitma.csv")
  # 
  # large_urban_areas <- st_read(here::here(root_folder, "raw_data/v2/zonificacion/zonificacion_GAU/zonificacion_gaus.shp"))
  # large_urban_areas <- large_urban_areas |> rename(id = ID)
  # large_urban_areas_names <- fread(here::here(root_folder, "raw_data/v2/zonificacion/zonificacion_GAU/nombres_gaus.csv"), encoding = "UTF-8")
  # setnames(large_urban_areas_names, old = 1:2, new = c("id", "name"))
  # large_urban_areas_pop <- fread(here::here(root_folder, "raw_data/v2/zonificacion/zonificacion_GAU/poblacion_gaus.csv"),
  #                                skip = 1, col.names = c("id", "population"))
  # 
  # # is_valid <- st_is_valid(large_urban_areas)
  # # sum(!is_valid)
  # large_urban_areas <- large_urban_areas |>
  #   left_join(large_urban_areas_names, by = "id") |> 
  #   left_join(large_urban_areas_pop, by = "id")
  # 
  # st_write(large_urban_areas, here::here(root_folder, "clean_data/spatial/large_urban_areas.gpkg"), delete_dsn = T)
  # saveRDS(large_urban_areas, here::here(root_folder, "clean_data/spatial/large_urban_areas_sf.rds"))
  # 
  
  municip <- st_read(here::here(root_folder, "data/raw_data/mitma_mobility/v2/zonificacion/zonificacion_municipios/zonificacion_municipios.shp"))
  municip_names <- fread(here::here(root_folder, "data/raw_data/mitma_mobility/v2/zonificacion/zonificacion_municipios/nombres_municipios.csv"), encoding = "UTF-8", skip = 1, col.names = c("n", "id", "name"))
  municip_pop <- fread(here::here(root_folder, "data/raw_data/mitma_mobility/v2/zonificacion/zonificacion_municipios/poblacion_municipios.csv"),
                       skip = 1, col.names = c("id", "population"))
  municip |> filter(ID == "01001")
  municip_pop[ id == "01001"]
  pop_all[ municipio == "01001"]
  municip_pop[ id == "01002"]
  
  
  
  municip <- municip |>
    rename(id = ID) |>
    left_join(municip_names, by = "id") |>
    left_join(municip_pop, by = "id")
  
  is_valid <- st_is_valid(municip)
  sum(!is_valid)
  municip_valid <- municip[is_valid,]
  municip_invalid <- municip[!is_valid,]
  municip_invalid_fixed <- st_make_valid(municip_invalid)
  # mapview::mapview(municip[!is_valid,])
  # mapview::mapview(municip_invalid_fixed)
  municip_fixed <- rbind(municip_valid, municip_invalid_fixed)
  nrow(municip_fixed) == nrow(municip)
  
  # glimpse(municip_fixed)
  
  
  
  st_write(municip_fixed, here::here(root_folder, "data/clean_data/mitma_mobility/v2/spatial/municipalities.gpkg"), delete_dsn = T)
  saveRDS(municip_fixed, here::here(root_folder, "data/clean_data/mitma_mobility/v2/spatial/municipalities_sf.rds"))
  
  # municip_centroids <- st_read(here::here(root_folder, "data/raw_data/v2/zonificacion/zonificacion_municipios/zonificacion_municipios_centroides.shp"))
  
  
  census_districts <- st_read(here::here(root_folder, "data/raw_data/v2/zonificacion/zonificacion_distritos/zonificacion_distritos.shp"))
  census_districts_pop <- fread(here::here(root_folder, "data/raw_data/v2/zonificacion/zonificacion_distritos/poblacion_distritos.csv"),
                                skip = 1, col.names = c("id", "population"))
  census_districts_names <- fread(here::here(root_folder, "raw_data/v2/zonificacion/zonificacion_distritos/nombres_distritos.csv"), encoding = "UTF-8",
                                  skip = 1, col.names = c("id", "name"))
  
  census_districts <- census_districts |> 
    rename(id = ID) |> 
    left_join(census_districts_names, by = "id") |> 
    left_join(census_districts_pop, by = "id")
  
  is_valid <- st_is_valid(census_districts)
  sum(!is_valid)
  census_districts_valid <- census_districts[is_valid,]
  census_districts_invalid <- census_districts[!is_valid,]
  census_districts_invalid_fixed <- st_make_valid(census_districts_invalid)
  # mapview::mapview(census_districts[!is_valid,])
  # mapview::mapview(census_districts_invalid_fixed)
  census_districts_fixed <- rbind(census_districts_valid, census_districts_invalid_fixed)
  nrow(census_districts_fixed) == nrow(census_districts)
  
  
  st_write(census_districts_fixed, here::here(root_folder, "data/clean_data/mitma_mobility/v2/spatial/census_districts.gpkg"), delete_dsn = T)
  saveRDS(census_districts_fixed, here::here(root_folder, "data/clean_data/mitma_mobility/v2/spatial/census_districts_sf.rds"))
  
  
  
  
}



# tabular data ------------------------------------------------------------

import_dropped_locations_reference_files <- function(){
  
  dropped_list_files <- fs::dir_ls("data/raw_data/mitma_mobility/v2/estudios_basicos/calidad/ficheros-diarios/",
                             recurse = T, type = "file", regexp = "csv$")
  
  tic(); dropped_dt <- dropped_list_files |> map(~fread(.x), .progress = T) |> rbindlist() ; toc()
  dropped_dt
  
}

check_files <- function(){
  # get all monthly archives
  monthly_archives_file_list <- fs::dir_ls(here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/"),
                                           recurse = T, type = "file", regexp = "[0-9]{6}.*\\.tar")
  
  monthly_archives_content_list <- monthly_archives_file_list |>  map( ~ untar(.x, list = T) )
  monthly_archives_content <- unlist(monthly_archives_content_list)
  monthly_archives_content <- monthly_archives_content[ monthly_archives_content != "/"]
  # untar("raw_data/2022/estudios_basicos/por-distritos/personas/meses-completos/202209_Personas_dia_distritos.tar", list = T)
  # untar("raw_data/2022/estudios_basicos/por-distritos/personas/meses-completos/202210_Personas_dia_distritos.tar", list = T)
  # 
  
  # get all daily archives
  daily_archives_file_list <- fs::dir_ls(here::here(root_folder, "/raw_data/v2/estudios_basicos/por-distritos/"),
                                         recurse = T, type = "file", regexp = "[0-9]{8}.*\\.csv\\.gz")
  
  daily_archives_files <- data.table(relative_path = daily_archives_file_list)
  daily_archives_files[ , file_name := basename(relative_path)]
  # daily_archives_files
  
  # what is missing from monthly archives
  all(daily_archives_files$file_name %in% monthly_archives_content)
  daily_archives_files[ ! file_name %in% monthly_archives_content]
  # what is missing in individual daily archives
  monthly_archives_content[ ! monthly_archives_content %in% daily_archives_files$file_name ]
  all(monthly_archives_content %in% daily_archives_files$file_name)
  
  # basically, all individual files are complete, no need to extract the large archives
  
}


check_for_corrupt_files <- function(){
  
  x <- fread("~/jobs/mitma_gzip_check_log_16080437.csv")
  x |> glimpse()
  table(x$result)
  x |> filter(result == "failure")
  
  library(nat.utils)
  # is.gzip("/scratch/users/egor.kotov/r_mitma_get_data/data/raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/2023-05/20230503_Viajes_distritos.csv.gz")
  x_file_list <- fs::dir_ls("/scratch/users/egor.kotov/r_mitma_get_data/data/raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/", type = "file", regexp = "gz", recurse = T)
  length(x_file_list)
  plan(multisession, 24)
  x_check <- x_file_list |> future_map( ~ is.gzip(.x) , .progress = T)
  plan(sequential)
  x_check <- unlist(x_check)
  sum(!x_check)
}

check_for_missing_days_in_files <- function(){
  
  files_list <- fs::dir_ls("data/raw_data/v2/estudios_basicos/", type = "file", regexp = "gz", recurse = T)
  
  files_dt <- data.table(local_path = files_list)
  files_dt[ , date := ymd(str_extract(local_path, "[0-9]{8}")), ]
  
  start_date <- min(files_dt$date)
  end_date <- max(files_dt$date)
  
  all_days_dt <- data.table(date = seq(from = start_date, to = end_date, by = 1))
  
  all_days_and_files <- all_days_dt |> full_join(files_dt, by = "date")
  n_missing_dates_from_files <- all_days_and_files[is.na(local_path), .N]
  return(paste0("There are ", n_missing_dates_from_files, " missing files."))
}

import_all_to_duck_and_arrow <- function() {
  
  duck_mem <- 120
  duck_threads <- 24
  
  tic() ; import_basic_daily_overnight_stays_to_duck(
    load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-distritos/pernoctaciones/ficheros-diarios//"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 27 sec, H02 - 26, H11 - 35, HPC INT - 40
  # tic() ; import_basic_daily_overnight_stays_to_duck(
  #   load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/pernoctaciones/ficheros-diarios/"),
  #   duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 17 sec, H02 - 22, H11 - 22, HPC INT - 15.9
  tic() ; import_basic_daily_overnight_stays_to_duck(
    load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-municipios/pernoctaciones/ficheros-diarios//"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 29 sec, H02 - 27, H11 - 27, HPC INT - 16.2
  
  tic() ; import_basic_daily_persons_to_duck(
    load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-distritos/personas/ficheros-diarios//"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 10 sec, H02 - 16, H11 - 13, HPC INT - 146
  # tic() ; import_basic_daily_persons_to_duck(
  #   load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/personas/ficheros-diarios/"),
  #   duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 5 sec, H02 - 8, H11 - 8, HPC INT - 6.7
  tic() ; import_basic_daily_persons_to_duck(
    load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-municipios/personas/ficheros-diarios//"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 7 sec, H02 - 9, H11 - 9, HPC INT - 7.35
  
  
  
  tic() ; import_basic_daily_trips_to_duck(
    load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios//"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 41 min, HPC INT - 22
  # tic() ; import_basic_daily_trips_to_duck(
  #   load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/viajes/ficheros-diarios/"),
  #   duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 41 min, HPC INT - 22
  tic() ; import_basic_daily_trips_to_duck(
    load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-municipios/viajes/ficheros-diarios//"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 76 min, HPC INT - 35
  
  
  # IMPORTING ARROW/PARQUET
  
  tic() ; import_basic_daily_persons(load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-distritos/personas/ficheros-diarios//")) ; toc() # HYDRA01 - 20 sec, HPC INT - 8
  # tic() ; import_basic_daily_persons(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/personas/ficheros-diarios/")) ; toc() # HYDRA01 - 12 sec, HPC INT - 2.9
  tic() ; import_basic_daily_persons(load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-municipios/personas/ficheros-diarios//")) ; toc() # HYDRA01 - 12 sec, HPC INT - 3.3
  
  
  tic() ; import_basic_daily_overnight_stays(load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-distritos/pernoctaciones/ficheros-diarios//")) ; toc() # HYDRA01 - 76 sec, HPC INT - 30
  # tic() ; import_basic_daily_overnight_stays(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/pernoctaciones/ficheros-diarios/")) ; toc() # HYDRA01 - 46 sec, HPC INT - 9.2
  tic() ; import_basic_daily_overnight_stays(load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-municipios/pernoctaciones/ficheros-diarios//")) ; toc() # HYDRA01 - 52 sec, HPC INT - 13.2
  
  
  
  Sys.time(); tic() ; import_basic_daily_trips(load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-municipios/viajes/ficheros-diarios//")) ; toc() # 90 minutes, HPC INT - 64 minutes
  Sys.time() ;tic() ; import_basic_daily_trips(load_from = here::here(root_folder, "data/raw_data/mitma_mobility/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios//")) ; toc() # 90 minutes, HPC INT - 85 minutes
  # tic() ; import_basic_daily_trips(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/viajes/ficheros-diarios/")) ; toc() # 90 minutes, HPC INT - 22
  
  
  # convert arrow to duck
  x_path <- "data/clean_data/mitma_mobility/v2/tabular/duck/municipalities/"
  fs::dir_create(x_path)
  duckdb_path <- paste0(x_path, "trips.duckdb")
  drv <- duckdb(dbdir = duckdb_path, read_only = FALSE)
  xdb <- DBI::dbConnect(drv)
  
  dbSendStatement(xdb, paste0("PRAGMA memory_limit='", duck_mem, "GB'") )
  dbSendStatement(xdb, paste0("PRAGMA threads='", duck_threads, "'") )
  dbSendStatement(xdb, "SET enable_progress_bar='true';")
  dbGetQuery(xdb, "SELECT current_setting('enable_progress_bar');")
  dbSendStatement(xdb, "PRAGMA enable_print_progress_bar;")
  parquet_path <- "data/clean_data/mitma_mobility/v2/tabular/parquet/municipalities/trips/"
  dbSendStatement(xdb, paste0("CREATE VIEW trips_parquet AS SELECT * FROM read_parquet('", parquet_path,"**/*.parquet');"))
  
  Sys.time()
  tic()
  dbSendStatement(xdb, "CREATE TABLE trips AS SELECT * FROM trips_parquet")
  toc()
  # dbDisconnect(xdb)
  # duckdb_shutdown(drv)
  
  x_path <- "data/clean_data/mitma_mobility/v2/tabular/duck/census_districts/"
  fs::dir_create(x_path)
  duckdb_path <- paste0(x_path, "trips.duckdb")
  drv <- duckdb(dbdir = duckdb_path, read_only = FALSE)
  xdb <- DBI::dbConnect(drv)
  
  dbSendStatement(xdb, paste0("SET memory_limit='", duck_mem, "GB';") )
  dbSendStatement(xdb, paste0("SET threads=", duck_threads, ";") )
  dbSendStatement(xdb, "SET enable_progress_bar='true';")
  dbGetQuery(xdb, "SELECT current_setting('enable_progress_bar');")
  dbGetQuery(xdb, "SELECT current_setting('memory_limit');")
  dbGetQuery(xdb, "SELECT current_setting('threads');")
  dbSendStatement(xdb, "PRAGMA enable_print_progress_bar;")
  parquet_path <- "data/clean_data/mitma_mobility/v2/tabular/parquet/census_districts/trips/"
  dbSendStatement(xdb, paste0("CREATE VIEW trips_parquet AS SELECT * FROM read_parquet('", parquet_path,"**/*.parquet');"))
  
  Sys.time()
  tic()
  dbSendStatement(xdb, "CREATE TABLE trips AS SELECT * FROM trips_parquet")
  toc()
    
}


import_all_to_duckdb <- function(){
  
  duck_mem <- 20
  duck_threads <- 30
  
  tic() ; import_basic_daily_overnight_stays_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/pernoctaciones/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 27 sec, H02 - 26, H11 - 35
  tic() ; import_basic_daily_overnight_stays_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/pernoctaciones/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 17 sec, H02 - 22, H11 - 22
  tic() ; import_basic_daily_overnight_stays_to_duck(
    load_from =here::here(root_folder,  "/raw_data/v2/estudios_basicos/por-municipios/pernoctaciones/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 29 sec, H02 - 27, H11 - 27
  
  tic() ; import_basic_daily_persons_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/personas/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 10 sec, H02 - 16, H11 - 13
  tic() ; import_basic_daily_persons_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/personas/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 5 sec, H02 - 8, H11 - 8
  tic() ; import_basic_daily_persons_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-municipios/personas/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 7 sec, H02 - 9, H11 - 9
  
  
  tic() ; import_basic_daily_trips_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 35 min, H02 - 66, H11 - 30
  tic() ; import_basic_daily_trips_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/viajes/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 18 min, H02 - __, H11 - 21
  tic() ; import_basic_daily_trips_to_duck(
    load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-municipios/viajes/ficheros-diarios/"),
    duck_mem = duck_mem, duck_threads = duck_threads) ; toc() # HYDRA01 - 37 min, H02 - 66, H11 - 60
  
  
  
}


import_all_to_arrow <- function(){
  
  
  tic() ; import_basic_daily_persons(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/personas/ficheros-diarios/")) ; toc() # HYDRA01 - 4 sec
  tic() ; import_basic_daily_persons(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/personas/ficheros-diarios/")) ; toc() # HYDRA01 - 6 sec
  tic() ; import_basic_daily_persons(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-municipios/personas/ficheros-diarios/")) ; toc() # HYDRA01 - 6 sec
  
  
  tic() ; import_basic_daily_overnight_stays(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/pernoctaciones/ficheros-diarios/")) ; toc() # HYDRA01 - 39 sec
  tic() ; import_basic_daily_overnight_stays(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/pernoctaciones/ficheros-diarios/")) ; toc() # HYDRA01 - 20 sec
  tic() ; import_basic_daily_overnight_stays(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-municipios/pernoctaciones/ficheros-diarios/")) ; toc() # HYDRA01 - 26 sec
  
  
  
  tic() ; import_basic_daily_trips(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/")) ; toc() # 50-60 minutes
  tic() ; import_basic_daily_trips(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-GAU/viajes/ficheros-diarios/")) ; toc() # about 20 minutes
  tic() ; import_basic_daily_trips(load_from = here::here(root_folder, "raw_data/v2/estudios_basicos/por-municipios/viajes/ficheros-diarios/")) ; toc() # about 30 minutes
  
  
}



import_basic_daily_persons <- function(load_from, root_save_to = here::here(root_folder, "data/clean_data/mitma_mobility/v2/tabular/parquet/")) {
  
  
  spatial_scale <- str_extract(load_from, "por-distritos|por-GAU|por-municipios")
  spatial_scale_dict <- c("por-distritos" = "census_districts",
                          "por-GAU" = "large_urban_areas",
                          "por-municipios" = "municipalities")
  
  file_set <- str_extract(load_from, "personas|pernoctaciones|viajes")
  file_set_dict <- c("personas" = "persons",
                     "pernoctaciones" = "overnight_stays",
                     "viajes" = "trips")
  
  parquet_save_path <- paste0(root_save_to,
                              spatial_scale_dict[spatial_scale], "/",
                              file_set_dict[file_set], "/"
  )
  
  if ( fs::dir_exists(parquet_save_path) == F ) { fs::dir_create(parquet_save_path)}
  
  # identify existing partitions
  existing_parquet_files <- fs::dir_ls(parquet_save_path, recurse = T, regexp = "parquet")
  existing_parquet_dt <- data.table(relative_path = existing_parquet_files)
  existing_parquet_dt[ , year := str_extract(relative_path, "(?<=year=)[0-9]{4}") |> as.integer(), ]
  existing_parquet_dt[ , month := str_extract(relative_path, "(?<=month=)[0-9]{1,2}") |> as.integer(), ]
  
  
  
  import_file_list <- fs::dir_ls(load_from, regexp = "gz", recurse = T)
  
  import_dt <- data.table(relative_path = import_file_list)
  import_dt[ , year_month := ym(str_extract(relative_path, "[0-9]{4}-[0-9]{2}"))]
  import_dt[ , year := lubridate::year(year_month), ]
  import_dt[ , month := lubridate::month(year_month), ]
  
  files_to_import <- import_dt |> anti_join(existing_parquet_dt, by = c("year", "month")) |> pull(relative_path)
  
  x <- open_dataset(files_to_import,
                    format = "text",
                    delim = "|",
                    skip = 1,
                    schema = schema(fecha = arrow::timestamp(),
                                    zona_pernoctacion = string(),
                                    edad = string(),
                                    sexo = string(),
                                    numero_viajes = string(),
                                    personas = double())
                    , timestamp_parsers = "%Y%m%d"
                    # , use_threads = T
  )
  
  
  xx <- x |>
    rename(full_date = fecha,
           zone_overnight = zona_pernoctacion,
           age = edad,
           sex = sexo,
           n_trips_fact = numero_viajes,
           n_persons = personas) |>
    mutate(full_date = lubridate::date(full_date)) |>
    mutate(year = as.integer(lubridate::year(full_date)),
           month = as.integer(lubridate::month(full_date)),
           day = as.integer(lubridate::day(full_date)),
           sex = case_when(
             sex == 'mujer' ~ 'female',
             sex == 'hombre' ~ 'male')
    )
  
  xx |>
    group_by(year, month) |> 
    write_dataset(path = parquet_save_path, format = "parquet", existing_data_behavior = "delete_matching")
  
}



import_basic_daily_overnight_stays <- function(load_from, root_save_to = here::here(root_folder, "data/clean_data/mitma_mobility/v2/tabular/parquet/")) {
  
  
  spatial_scale <- str_extract(load_from, "por-distritos|por-GAU|por-municipios")
  spatial_scale_dict <- c("por-distritos" = "census_districts",
                          "por-GAU" = "large_urban_areas",
                          "por-municipios" = "municipalities")
  
  file_set <- str_extract(load_from, "personas|pernoctaciones|viajes")
  file_set_dict <- c("personas" = "persons",
                     "pernoctaciones" = "overnight_stays",
                     "viajes" = "trips")
  
  parquet_save_path <- paste0(root_save_to,
                              spatial_scale_dict[spatial_scale], "/",
                              file_set_dict[file_set], "/"
  )
  
  if ( fs::dir_exists(parquet_save_path) == F ) { fs::dir_create(parquet_save_path)}
  
  # identify existing partitions
  existing_parquet_files <- fs::dir_ls(parquet_save_path, recurse = T, regexp = "parquet")
  existing_parquet_dt <- data.table(relative_path = existing_parquet_files)
  existing_parquet_dt[ , year := str_extract(relative_path, "(?<=year=)[0-9]{4}") |> as.integer(), ]
  existing_parquet_dt[ , month := str_extract(relative_path, "(?<=month=)[0-9]{1,2}") |> as.integer(), ]
  
  
  
  import_file_list <- fs::dir_ls(load_from, regexp = "gz", recurse = T)
  
  import_dt <- data.table(relative_path = import_file_list)
  import_dt[ , year_month := ym(str_extract(relative_path, "[0-9]{4}-[0-9]{2}"))]
  import_dt[ , year := lubridate::year(year_month), ]
  import_dt[ , month := lubridate::month(year_month), ]
  
  files_to_import <- import_dt |> anti_join(existing_parquet_dt, by = c("year", "month")) |> pull(relative_path)
  
  
  x <- open_dataset(files_to_import,
                    format = "text",
                    delim = "|",
                    skip = 1,
                    schema = schema(fecha = arrow::timestamp(),
                                    zona_residencia = string(),
                                    zona_pernoctacion = string(),
                                    personas = double())
                    , timestamp_parsers = "%Y%m%d"
                    # , use_threads = T
  )
  
  
  xx <- x |>
    rename(full_date = fecha,
           zone_residence = zona_residencia,
           zone_overnight = zona_pernoctacion,
           n_persons = personas) |>
    mutate(full_date = lubridate::date(full_date)) |>
    mutate(year = as.integer(lubridate::year(full_date)),
           month = as.integer(lubridate::month(full_date)),
           day = as.integer(lubridate::day(full_date))
    )
  
  xx |>
    group_by(year, month) |> 
    write_dataset(path = parquet_save_path, format = "parquet", existing_data_behavior = "delete_matching")
  
}



import_basic_daily_trips <- function(load_from, root_save_to = here::here(root_folder, "data/clean_data/mitma_mobility/v2/tabular/parquet/")) {
  
  spatial_scale <- str_extract(load_from, "por-distritos|por-GAU|por-municipios")
  spatial_scale_dict <- c("por-distritos" = "census_districts",
                          "por-GAU" = "large_urban_areas",
                          "por-municipios" = "municipalities")
  
  file_set <- str_extract(load_from, "personas|pernoctaciones|viajes")
  file_set_dict <- c("personas" = "persons",
                     "pernoctaciones" = "overnight_stays",
                     "viajes" = "trips")
  
  parquet_save_path <- paste0(root_save_to,
                              spatial_scale_dict[spatial_scale], "/",
                              file_set_dict[file_set], "/"
  )
  
  if ( fs::dir_exists(parquet_save_path) == F ) { fs::dir_create(parquet_save_path)}
  
  # identify existing partitions
  existing_parquet_files <- fs::dir_ls(parquet_save_path, recurse = T, regexp = "parquet")
  existing_parquet_dt <- data.table(relative_path = existing_parquet_files)
  existing_parquet_dt[ , year := str_extract(relative_path, "(?<=year=)[0-9]{4}") |> as.integer(), ]
  existing_parquet_dt[ , month := str_extract(relative_path, "(?<=month=)[0-9]{1,2}") |> as.integer(), ]
  
  
  
  import_file_list <- fs::dir_ls(load_from, regexp = "gz", recurse = T)
  
  import_dt <- data.table(relative_path = import_file_list)
  import_dt[ , year_month := ym(str_extract(relative_path, "[0-9]{4}-[0-9]{2}"))]
  import_dt[ , year := lubridate::year(year_month), ]
  import_dt[ , month := lubridate::month(year_month), ]
  
  files_to_import <- import_dt |> anti_join(existing_parquet_dt, by = c("year", "month")) |> pull(relative_path)
  
  
  x <- open_dataset(files_to_import,
                    format = "text",
                    delim = "|",
                    skip = 1,
                    schema = schema(fecha = arrow::timestamp(),
                                    periodo = int32(),
                                    origen = string(),
                                    destino = string(),
                                    distancia = string(),
                                    actividad_origen = string(),
                                    actividad_destino = string(),
                                    estudio_origen_posible = string(),
                                    estudio_destino_posible = string(),
                                    residencia = int32(),
                                    renta = string(),
                                    edad = string(),
                                    sexo = string(),
                                    viajes = double(),
                                    viajes_km = double())
                    , timestamp_parsers = "%Y%m%d"
                    # , use_threads = T
  )
  
  
  xx <- x |>
    rename(full_date = fecha,
           time_slot = periodo,
           id_origin = origen,
           id_destination = destino,
           distance_fact = distancia,
           activity_origin = actividad_origen,
           activity_destination = actividad_destino,
           study_possible_origin = estudio_origen_posible,
           study_possible_destination = estudio_destino_posible,
           id_ine_residence = residencia,
           income_fact = renta,
           age_fact = edad,
           sex = sexo,
           n_trips = viajes,
           trips_total_length_km = viajes_km) |>
    mutate(
      activity_origin = case_when(
        activity_origin == "casa" ~ "home",
        activity_origin == "frecuente" ~ "frequent_activity",
        activity_origin == "no_frecuente" ~ "infrequent_activity",
        activity_origin == "trabajo_estudio" ~ "work_or_study"
      ),
      activity_destination = case_when(
        activity_destination == "casa" ~ "home",
        activity_destination == "frecuente" ~ "frequent_activity",
        activity_destination == "no_frecuente" ~ "infrequent_activity",
        activity_destination == "trabajo_estudio" ~ "work_or_study"
      ),
      study_possible_origin = case_when(
        study_possible_origin == "no" ~ FALSE,
        study_possible_origin == "si" ~ TRUE),
      study_possible_destination = case_when(
        study_possible_destination == "no" ~ FALSE,
        study_possible_destination == "si" ~ TRUE),
      sex = case_when(
        sex == 'mujer' ~ 'female',
        sex == 'hombre' ~ 'male')
    ) |> 
    mutate(full_date = lubridate::date(full_date)) |>
    mutate(year = as.integer(lubridate::year(full_date)),
           month = as.integer(lubridate::month(full_date)),
           day = as.integer(lubridate::day(full_date))
    )
  
  
  xx |>
    group_by(year, month, day) |> 
    write_dataset(path = parquet_save_path, format = "parquet", existing_data_behavior = "delete_matching")
  
  
}


convert_parquet_to_duckdb_for_loop <- function(parquet_folder){
  
  # set the path to the folder containing the parquet files
  parquet_folder <- "clean_data/mitma_mobility/tabular/census_districts/trips/"
  
  # create a connection to a new DuckDB database
  drv <- duckdb(dbdir = "./test2.duckdb")
  con <- dbConnect(drv, read_only = FALSE)
  
  dbExecute(con, "PRAGMA memory_limit='5GB'")
  # dbListTables(con)
  
  # get a list of all the parquet files in the folder
  parquet_files_list <- fs::dir_ls(parquet_folder, pattern = "\\.parquet$", recurse = T, type = "file")
  # parquet_files <- paste0("'", paste0(parquet_files_list, collapse = "', '"), "'")
  
  parquet_file_1 <- parquet_files_list[[1]]
  
  parquet_files <- parquet_files_list[ ! parquet_files_list %in% parquet_file_1 ]
  
  tic()
  dbSendStatement(conn = con, statement = paste0("CREATE TABLE trips AS SELECT * FROM '", parquet_file_1, "';"))
  toc()
  
  
  
  dbDisconnect(con, shutdown=TRUE)
  duckdb_shutdown(drv)
  rm(drv, con)
  # rm(con)
  
  
  Sys.sleep(15)
  
  tic()
  for(i in parquet_files) {
    print(i)
    tic()
    # Sys.sleep(2)
    drv <- duckdb(dbdir = "./test2.duckdb")
    con <- dbConnect(drv, dbdir = "./test2.duckdb", read_only = FALSE)
    dbExecute(con, "PRAGMA memory_limit='5GB'")
    dbSendStatement(conn = con, statement = paste0("COPY trips FROM '", i, "' (FORMAT PARQUET);") )
    # dbSendStatement(conn = con, statement = paste0("INSERT INTO trips SELECT * FROM '", i, "';"))
    # dbListTables(con)
    Sys.sleep(2)
    dbDisconnect(con, shutdown=TRUE)
    rm(drv, con)
    # duckdb_shutdown(drv)
    Sys.sleep(2)
    toc()
  }
  toc()
  
  # 
  # tic()
  # for(i in parquet_files) {
  #   print(i)
  #   tic()
  #   # Sys.sleep(2)
  #   drv <- duckdb(dbdir = "./test2.duckdb")
  #   con <- dbConnect(drv, dbdir = "./test2.duckdb", read_only = FALSE)
  #   dbExecute(con, "PRAGMA memory_limit='5GB'")
  #   Sys.sleep(1)
  #   dbDisconnect(con, shutdown=TRUE)
  #   rm(drv, con)
  #   # duckdb_shutdown(drv)
  #   Sys.sleep(2)
  #   toc()
  # }
  # toc()
  # 
  
  # duckdb::duckdb_shutdown(duckdb())
  
  open_dataset(parquet_folder) |> tally() |> collect()
  tbl(con, "test") |> glimpse()
  
  
  dbDisconnect(con)
  gc()
  
  
  
}



convert_parquet_to_duckdb <- function(parquet_folder){
  
  # set the path to the folder containing the parquet files
  parquet_folder <- "clean_data/mitma_mobility/tabular/census_districts/overnight_stays/"
  
  # create a connection to a new DuckDB database
  con <- dbConnect(duckdb::duckdb(), dbdir = "./test22.duckdb", read_only = FALSE)
  
  dbExecute(con, "PRAGMA memory_limit='5GB'")
  
  # get a list of all the parquet files in the folder
  parquet_files_list <- fs::dir_ls(parquet_folder, pattern = "\\.parquet$", recurse = T, type = "file")
  parquet_files <- paste0("'", paste0(parquet_files_list, collapse = "', '"), "'")
  
  tic()
  # dbSendStatement(conn = con, statement = paste0("CREATE TABLE trips AS SELECT * FROM read_parquet([", parquet_files, "]);"))
  parquet_folder <- "clean_data/mitma_mobility/tabular/census_districts/overnight_stays/*/*/*.parquet"
  dbSendStatement(conn = con, statement = paste0("CREATE TABLE trips AS SELECT * FROM parquet_scan('", parquet_folder, "');") )
  dbSendStatement(con, "DROP TABLE trips")
  toc()
  dbListTables(con)
  
  open_dataset(parquet_folder) |> tally() |> collect()
  tbl(con, "test") |> glimpse()
  
  
  dbDisconnect(con)
  
  
  
}



review_imports <- function() {
  
  persons_cd_arrow <- open_dataset("clean_data/mitma_mobility/tabular/census_districts/persons/")
  overnight_cd_stays_arrow <- open_dataset("clean_data/mitma_mobility/tabular/census_districts/overnight_stays/")
  # trips_arrow <- open_dataset("clean_data/mitma_mobility/tabular/census_districts/")
  
  
  persons_cd_arrow |> glimpse()
  overnight_cd_stays_arrow |> glimpse()
  
  overnight_cd_stays_arrow |> select(zone_residence) |> 
    write_parquet(sink = "zone_fact.parquet")
  
  overnight_cd_stays_arrow |> select(zone_residence) |> mutate(zone_residence = as.character(zone_residence)) |> 
    write_parquet(sink = "zone_char.parquet")
  
  chr <- read_parquet("zone_char.parquet")
  fac <- read_parquet("zone_fact.parquet")
  
  chr |> glimpse()
  fac |> glimpse()
  
  
  
  
  
  
}


# import using duckdb from csv --------------------------------------------

# are character vectors stored as factors in parquet even if they are not dictionaries?
# https://www.christophenicault.com/post/large_dataframe_arrow_duckdb/


import_basic_daily_overnight_stays_to_duck <- function(load_from,
                                                       root_save_to = here::here(root_folder, "data/clean_data/mitma_mobility/v2/tabular/duck/"),
                                                       duck_mem = 5,
                                                       duck_threads = 10
) {
  
  # load_from <- "./raw_data/v2/estudios_basicos/por-distritos/pernoctaciones/ficheros-diarios/"
  
  spatial_scale <- str_extract(load_from, "por-distritos|por-GAU|por-municipios")
  spatial_scale_dict <- c("por-distritos" = "census_districts",
                          "por-GAU" = "large_urban_areas",
                          "por-municipios" = "municipalities")
  
  file_set <- str_extract(load_from, "personas|pernoctaciones|viajes")
  file_set_dict <- c("personas" = "persons",
                     "pernoctaciones" = "overnight_stays",
                     "viajes" = "trips")
  
  duck_save_path <- paste0(root_save_to, "/",
                           spatial_scale_dict[spatial_scale], "/",
                           file_set_dict[file_set], ".duckdb"
  )
  
  
  
  if( fs::dir_exists( fs::path_dir(duck_save_path) ) == FALSE ) { fs::dir_create( fs::path_dir(duck_save_path) ) }
  
  # import_file_list <- fs::dir_ls(load_from, regexp = "gz", recurse = T)
  
  # import_dt <- data.table(relative_path = import_file_list)
  # import_dt[ , Year_Month := ym(str_extract(relative_path, "[0-9]{4}-[0-9]{2}"))]
  # import_dt[ , Year := year(Year_Month), ]
  # import_dt[ , Month := month(Year_Month), ]
  
  # files_to_import <- import_file_list
  
  # drv <- duckdb()
  drv <- duckdb(dbdir = duck_save_path)
  con <- dbConnect(drv, read_only = FALSE)
  
  dbSendStatement(con, paste0("PRAGMA memory_limit='", duck_mem, "GB'") )
  dbSendStatement(con, paste0("PRAGMA threads='", duck_threads, "'") )
  
  dbSendStatement(con, "PRAGMA enable_progress_bar;")
  dbSendStatement(con, "PRAGMA enable_print_progress_bar;")
  # dbSendStatement(con, "PRAGMA temp_directory='tmp/tmp.tmp';")
  
  
  dbSendStatement(con, paste0("CREATE view all_csv_files AS SELECT * FROM read_csv_auto('", paste0(load_from, "*/*.csv.gz"), "', delim='|');"))
  
  # Create view to fix variable types
  dbSendStatement(con, "CREATE VIEW overnight_stays_view AS SELECT
                  CAST (regexp_replace(fecha, '(^[0-9]{4})([0-9]{2})([0-9]{2}$)', '\\1-\\2-\\3') AS DATE) AS full_date,
                  zona_residencia AS zone_residence,
                  zona_pernoctacion AS zone_overnight,
                  personas AS n_persons,
                  year(full_date) AS year,
                  month(full_date) AS month,
                  day(full_date) AS day,
                  FROM all_csv_files ;")
  
  # Create ENUMs for factor variables
  dbSendStatement(con, "CREATE TYPE ZONES_ENUM AS ENUM ( SELECT zone_residence FROM overnight_stays_view UNION SELECT zone_overnight FROM overnight_stays_view);")
  
  # Create final table from view with type-fixed and recoded variables and the ENUMs
  dbSendStatement(con, "CREATE TABLE overnight_stays AS SELECT
                        full_date,
                        CAST (zone_residence AS ZONES_ENUM) AS zone_residence,
                        CAST (zone_overnight AS ZONES_ENUM) AS zone_overnight,
                        n_persons,
                        year,
                        month,
                        day,
                        FROM overnight_stays_view ;")
  
  
  dbSendQuery(con, "DROP VIEW all_csv_files")
  dbSendQuery(con, "DROP VIEW overnight_stays_view")
  
  dbDisconnect(con, shutdown = T)
  duckdb::duckdb_shutdown(drv)
  rm(con, drv)
}


import_basic_daily_persons_to_duck <- function(load_from,
                                               root_save_to = here::here(root_folder, "data/clean_data/mitma_mobility/v2/tabular/duck/"),
                                               duck_mem = 5,
                                               duck_threads = 10) {
  
  # load_from <- "./raw_data/v2/estudios_basicos/por-distritos/personas/ficheros-diarios/"
  
  spatial_scale <- str_extract(load_from, "por-distritos|por-GAU|por-municipios")
  spatial_scale_dict <- c("por-distritos" = "census_districts",
                          "por-GAU" = "large_urban_areas",
                          "por-municipios" = "municipalities")
  
  file_set <- str_extract(load_from, "personas|pernoctaciones|viajes")
  file_set_dict <- c("personas" = "persons",
                     "pernoctaciones" = "overnight_stays",
                     "viajes" = "trips")
  
  duck_save_path <- paste0(root_save_to, "/",
                           spatial_scale_dict[spatial_scale], "/",
                           file_set_dict[file_set], ".duckdb"
  )
  
  
  
  if( fs::dir_exists( fs::path_dir(duck_save_path) ) == FALSE ) { fs::dir_create( fs::path_dir(duck_save_path) ) }
  
  # import_file_list <- fs::dir_ls(load_from, regexp = "gz", recurse = T)
  
  # import_dt <- data.table(relative_path = import_file_list)
  # import_dt[ , Year_Month := ym(str_extract(relative_path, "[0-9]{4}-[0-9]{2}"))]
  # import_dt[ , Year := year(Year_Month), ]
  # import_dt[ , Month := month(Year_Month), ]
  
  # files_to_import <- import_file_list
  
  # drv <- duckdb()
  drv <- duckdb(dbdir = duck_save_path)
  con <- dbConnect(drv, read_only = FALSE)
  
  dbSendStatement(con, paste0("PRAGMA memory_limit='", duck_mem, "GB'") )
  dbSendStatement(con, paste0("PRAGMA threads='", duck_threads, "'") )
  
  dbSendStatement(con, "PRAGMA enable_progress_bar;")
  dbSendStatement(con, "PRAGMA enable_print_progress_bar;")
  # dbSendStatement(con, "PRAGMA temp_directory='tmp/tmp.tmp';")
  
  
  dbSendStatement(con, statement = paste0("CREATE VIEW all_csv_files AS SELECT * FROM read_csv_auto('", paste0(load_from, "*/*.csv.gz"), "', delim='|');"))
  # dbSendStatement(con, statement = paste0("CREATE VIEW all_csv_files AS SELECT * FROM read_csv_auto('", import_file_list[[1]], "', delim='|');"))
  # tbl(con, "all_csv_files")
  
  # Create view to fix variable types and recode values to English
  dbSendStatement(con, "CREATE VIEW persons_view AS SELECT
                  CAST (regexp_replace(fecha, '(^[0-9]{4})([0-9]{2})([0-9]{2}$)', '\\1-\\2-\\3') AS DATE) AS full_date,
                  zona_pernoctacion AS zone_overnight,
                  edad AS age_fact,
                  CASE sexo
                    WHEN 'mujer' THEN 'female'
                    WHEN 'hombre' THEN 'male'
                    END AS sex,
                  numero_viajes AS n_trips_fact,
                  personas AS n_persons,
                  year(full_date) AS year,
                  month(full_date) AS month,
                  day(full_date) AS day,
                  FROM all_csv_files ;")
  
  # create ENUMs for factor variables
  dbSendStatement(con, "CREATE TYPE ZONES_ENUM AS ENUM ( SELECT zone_overnight FROM persons_view );
                  CREATE TYPE AGE_ENUM AS ENUM ( SELECT age_fact FROM persons_view );
                  CREATE TYPE SEX_ENUM AS ENUM ( SELECT sex FROM persons_view );
                  CREATE TYPE NTRIPS_ENUM AS ENUM ( SELECT n_trips_fact FROM persons_view ); ")
  # dbFetch( dbSendStatement(con, "SELECT enum_range(NULL::SEX_ENUM) AS my_enum_range;") )
  
  # create final table from view with type-fixed and recoded variables and the ENUMs
  dbSendStatement(con, "CREATE TABLE persons AS SELECT
                        full_date,
                        CAST (zone_overnight AS ZONES_ENUM) AS zone_overnight,
                        CAST (age_fact AS AGE_ENUM) AS age_fact,
                        CAST (sex as SEX_ENUM) AS sex,
                        CAST (n_trips_fact AS NTRIPS_ENUM) AS n_trips_fact,
                        n_persons,
                        year,
                        month,
                        day,
                        FROM persons_view ;")
  
  dbSendQuery(con, "DROP VIEW all_csv_files")
  dbSendQuery(con, "DROP VIEW persons_view")
  
  # dbDisconnect(con, shutdown = T)
  duckdb::duckdb_shutdown(drv)
  # rm(con, drv)
}


import_basic_daily_trips_to_duck <- function(load_from,
                                             root_save_to = here::here(root_folder, "data/clean_data/mitma_mobility/v2/tabular/duck/"),
                                             duck_mem = 50,
                                             duck_threads = 30) {
  
  # load_from <- "./raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/"
  
  spatial_scale <- str_extract(load_from, "por-distritos|por-GAU|por-municipios")
  spatial_scale_dict <- c("por-distritos" = "census_districts",
                          "por-GAU" = "large_urban_areas",
                          "por-municipios" = "municipalities")
  
  file_set <- str_extract(load_from, "personas|pernoctaciones|viajes")
  file_set_dict <- c("personas" = "persons",
                     "pernoctaciones" = "overnight_stays",
                     "viajes" = "trips")
  
  duck_save_path <- paste0(root_save_to, "/",
                           spatial_scale_dict[spatial_scale], "/",
                           file_set_dict[file_set], ".duckdb"
  )
  
  
  
  if( fs::dir_exists( fs::path_dir(duck_save_path) ) == FALSE ) { fs::dir_create( fs::path_dir(duck_save_path) ) }
  
  import_file_list <- fs::dir_ls(load_from, regexp = "gz", recurse = T)
  
  # import_dt <- data.table(relative_path = import_file_list)
  # import_dt[ , Year_Month := ym(str_extract(relative_path, "[0-9]{4}-[0-9]{2}"))]
  # import_dt[ , Year := year(Year_Month), ]
  # import_dt[ , Month := month(Year_Month), ]
  
  # files_to_import <- import_file_list
  
  # drv <- duckdb()
  drv <- duckdb(dbdir = duck_save_path)
  con <- dbConnect(drv, read_only = FALSE)
  
  dbSendStatement(con, paste0("PRAGMA memory_limit='", duck_mem, "GB'") )
  dbSendStatement(con, paste0("PRAGMA threads='", duck_threads, "'") )
  
  
  # rs <- dbSendStatement(con, "PRAGMA database_list;")
  # dbFetch(rs)
  dbSendStatement(con, "PRAGMA enable_progress_bar;")
  dbSendStatement(con, "PRAGMA enable_print_progress_bar;")
  # dbSendStatement(con, "PRAGMA temp_directory='tmp/tmp.tmp';")
  
  
  dbSendStatement(con, statement = paste0("CREATE VIEW all_csv_files AS SELECT * FROM read_csv_auto('", paste0(load_from, "*/*"), "', delim='|');"))
  # dbSendStatement(con, statement = paste0("CREATE VIEW all_csv_files AS SELECT * FROM read_csv_auto('", import_file_list[[1]], "', delim='|');"))
  tbl(con, "all_csv_files") |> glimpse()
  
  # Create view to fix variable types and recode values to English
  dbSendStatement(con, "CREATE VIEW trips_view AS SELECT
                        CAST (regexp_replace(fecha, '(^[0-9]{4})([0-9]{2})([0-9]{2}$)', '\\1-\\2-\\3') AS DATE) AS full_date,
                        CAST (periodo AS INTEGER) AS time_slot,
                        origen AS id_origin,
                        destino AS id_destination,
                        distancia AS distance_fact,
                        CASE actividad_origen
                          WHEN 'casa' THEN 'home'
                          WHEN 'frecuente' THEN 'frequent_activity'
                          WHEN 'no_frecuente' THEN 'infrequent_activity'
                          WHEN 'trabajo_estudio' THEN 'work_or_study'
                        END AS activity_origin,
                        CASE actividad_destino
                          WHEN 'casa' THEN 'home'
                          WHEN 'frecuente' THEN 'frequent_activity'
                          WHEN 'no_frecuente' THEN 'infrequent_activity'
                          WHEN 'trabajo_estudio' THEN 'work_or_study'
                        END AS activity_destination,
                        CASE estudio_origen_posible
                            WHEN 'si' THEN TRUE
                            WHEN 'no' THEN FALSE
                          END AS study_possible_origin,
                        CASE estudio_destino_posible
                            WHEN 'si' THEN TRUE
                            WHEN 'no' THEN FALSE
                          END AS study_possible_destination,
                        residencia AS id_ine_residence,
                        renta AS income_fact,
                        edad AS age_fact,
                        CASE sexo
                              WHEN 'mujer' THEN 'female'
                              WHEN 'hombre' THEN 'male'
                              WHEN 'NA' THEN 'NA'
                              END AS sex,
                        viajes AS n_trips,
                        viajes_km AS trips_total_length_km,
                        year(full_date) AS year,
                        month(full_date) AS month,
                        day(full_date) AS day,
                        FROM all_csv_files;")
  # tbl(con, "trips_view") |> glimpse()
  # tbl(con, "trips_view") |> head(15) |> collect()
  
  # tbl(con, "all_csv_files") |> group_by(estudio_origen_posible) |> tally()
  # tbl(con, "trips_view") |> group_by(study_possible_origin) |> tally()
  # dbSendStatement(con, "DROP VIEW trips_view")
  
  # create ENUMs for factor variables # takes 23 minutes on HYDRA02
  # tic()
  dbSendStatement(con,
                  "CREATE TYPE ZONES_ENUM AS ENUM ( SELECT id_origin FROM trips_view UNION SELECT id_destination FROM trips_view );
  
  CREATE TYPE ACTIV_ENUM AS ENUM ( SELECT activity_origin FROM trips_view UNION SELECT activity_destination FROM trips_view);
  
  CREATE TYPE DISTANCE_ENUM AS ENUM ( SELECT distance_fact FROM trips_view );
  
  CREATE TYPE ID_INE_RESID_ENUM AS ENUM ( SELECT id_ine_residence FROM trips_view );
                  
  CREATE TYPE INCOME_ENUM AS ENUM ( SELECT income_fact FROM trips_view );
                  
  CREATE TYPE AGE_ENUM AS ENUM ( SELECT age_fact FROM trips_view );
                  
  CREATE TYPE SEX_ENUM AS ENUM ( SELECT sex FROM trips_view ); ")
  # toc()
  # tbl(con, "trips_view") |> glimpse()
  
  # Create final table from view with type-fixed and recoded variables and the ENUMs # takes 20 minutes on HYDRA 02s
  # tic()
  dbSendStatement(con, "CREATE TABLE trips as SELECT
                        full_date,
                        time_slot,
                        CAST (id_origin AS ZONES_ENUM) AS id_origin,
                        CAST (id_destination AS ZONES_ENUM) AS id_destination,
                        CAST (distance_fact AS DISTANCE_ENUM) AS distance_fact,
                        CAST (activity_origin AS ACTIV_ENUM) AS activity_origin,
                        CAST (activity_destination AS ACTIV_ENUM) AS activity_destination,
                        study_possible_origin,
                        study_possible_destination,
                        CAST (id_ine_residence AS ID_INE_RESID_ENUM) AS id_ine_residence,
                        CAST (income_fact AS INCOME_ENUM) AS income_fact,
                        CAST (age_fact AS AGE_ENUM) AS age_fact,
                        CAST (sex AS SEX_ENUM) AS sex,
                        n_trips,
                        trips_total_length_km,
                        year,
                        month,
                        day
                        FROM trips_view;")
  # toc()
  
  # tbl(con, "trips") |> glimpse()
  # tbl(con, "trips") |> group_by(sex) |> tally()
  # tbl(con, "trips") |> group_by(activity_origin) |> tally()
  # tbl(con, "trips") |> group_by(study_possible_origin) |> tally()
  
  
  # dbListTables(con)
  dbSendQuery(con, "DROP VIEW all_csv_files")
  dbSendQuery(con, "DROP VIEW trips_view")
  
  # od_mx <- open_dataset("clean_data/mitma_mobility/tabular/census_districts/trips/")
  # 
  # tbl(con, "trips") |> glimpse()
  # tic() ; tbl(con, "trips") |> group_by(sex, income_fact, age_fact) |> summarise(mean(n_trips)) |> collect() ; toc()
  # tic() ; od_mx |> group_by(sex, income_fact, age_fact) |> summarise(mean(n_trips)) |> collect() ; toc()
  # 
  # tic() ; duck_summary <- tbl(con, "trips") |> group_by(sex, income_fact, age_fact) |> tally() |> arrange(desc(n)) |> collect() ; toc()
  # tic() ; parquet_summary <- od_mx |> group_by(sex, income_fact, age_fact) |> tally()|> arrange(desc(n)) |> collect() ; toc()
  # 
  # sum(duck_summary$n) / sum(parquet_summary$sn)
  # duck_summary
  # parquet_summary
  # 
  # tic() ; tbl(con, "trips")  |> summarize(min(full_date), max(full_date)) |> collect() ; toc()
  # tic() ; od_mx              |> summarize(min(full_date), max(full_date)) |> collect() ; toc()
  # 
  # tic() ; tbl(con, "trips")  |> group_by(year, month) |> tally() |> collect() ; toc()
  # tic() ; od_mx              |> group_by(year, month) |> tally() |> collect() ; toc()
  # 
  # 
  
  dbDisconnect(con, shutdown = T)
  duckdb::duckdb_shutdown(drv)
  rm(con, drv)
  
  
}


compare_duck_arrow_record_count <- function(){
  
  ar <- open_dataset("clean_data/mitma_mobility/tabular/census_districts/overnight_stays/")
  dc_drv <- duckdb(dbdir = "clean_data/duck/census_districts/overnight_stays.duckdb", read_only = T)
  dc <- dbConnect(dc_drv)
  
  ar_may <- ar |> filter(Month == 5) |> collect() |> setDT()
  dc_may <- tbl(dc, "overnight_stays") |> filter(month == 5) |> collect() |> setDT()
  
  ars <- ar_may |> group_by(Day) |> tally() |> setDT()
  dcs <- dc_may |> group_by(day) |> tally() |> setDT()
  
  dcs[1, n] / ars[1, n]
  
  nrow(unique(ar_may))
  nrow(unique(dc_may))
  
  dc_may[ ! duplicated(dc_may) ]
  dc_may[ n_persons == 2456.311]
  
  which(ars$n != dcs$n)
  
  
  
}

