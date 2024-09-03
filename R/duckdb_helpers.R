#' Creates a duckdb connection to origin-destination data
#'
#' This function creates a duckdb connection to the origin-destination data stored in CSV.gz files.
#'
#' @param con A duckdb connection object. If not specified, a new in-memory connection will be created.
#' @inheritParams spod_available_data
#' @inheritParams spod_download_data
#' @return A duckdb connection object with 2 views:
#'
#'  * `od_csv_raw` - a raw table view of all cached CSV files with the origin-destination data that has been previously cached in $SPANISH_OD_DATA_DIR
#'
#'  * `od_csv_clean` - a cleaned-up table view of `od_csv_raw` with column names and values translated and mapped to English. This still includes all cached data.
#'
#' The structure of the cleaned-up views `od_csv_clean` is as follows:
#'
#' \describe{
#'   \item{date}{\code{Date}. The full date of the trip, including year, month, and day.}
#'   \item{id_origin}{\code{factor}. The identifier for the origin location of the trip, formatted as a code (e.g., '01001_AM').}
#'   \item{id_destination}{\code{factor}. The identifier for the destination location of the trip, formatted as a code (e.g., '01001_AM').}
#'   \item{activity_origin}{\code{factor}. The type of activity at the origin location (e.g., 'home', 'work'). \strong{Note:} Only available for district level data.}
#'   \item{activity_destination}{\code{factor}. The type of activity at the destination location (e.g., 'home', 'other'). \strong{Note:} Only available for district level data.}
#'   \item{residence_province_ine_code}{\code{factor}. The province of residence for the group of individual making the trip, encoded according to the INE classification. \strong{Note:} Only available for district level data.}
#'   \item{residence_province_name}{\code{factor}. The province of residence for the group of individuals making the trip (e.g., 'Cuenca', 'Girona'). \strong{Note:} Only available for district level data.}
#'   \item{time_slot}{\code{integer}. The time slot (the hour of the day) during which the trip started, represented as an integer (e.g., 0, 1, 2).}
#'   \item{distance}{\code{factor}. The distance category of the trip, represented as a code (e.g., '002-005' for 2-5 km).}
#'   \item{n_trips}{\code{double}. The number of trips taken within the specified time slot and distance.}
#'   \item{trips_total_length_km}{\code{double}. The total length of all trips in kilometers for the specified time slot and distance.}
#'   \item{year}{\code{double}. The year of the trip.}
#'   \item{month}{\code{double}. The month of the trip.}
#'   \item{day}{\code{double}. The day of the trip.}
#' }
#'
#' The structure of the original data in `od_csv_raw` is as follows:
#'
#' \describe{
#'   \item{fecha}{\code{Date}. The date of the trip, including year, month, and day.}
#'   \item{origen}{\code{character}. The identifier for the origin location of the trip, formatted as a character string (e.g., '01001_AM').}
#'   \item{destino}{\code{character}. The identifier for the destination location of the trip, formatted as a character string (e.g., '01001_AM').}
#'   \item{actividad_origen}{\code{character}. The type of activity at the origin location (e.g., 'casa', 'trabajo').}
#'   \item{actividad_destino}{\code{character}. The type of activity at the destination location (e.g., 'otros', 'trabajo').}
#'   \item{residencia}{\code{character}. The code representing the residence of the individual making the trip (e.g., '01') according to the official INE classification.}
#'   \item{edad}{\code{character}. The age of the individual making the trip. This data is actaully filled with 'NA' values, which is why this column is removed in the cleaned-up and translated view described above.}
#'   \item{periodo}{\code{integer}. The time period during which the trip started, represented as an integer (e.g., 0, 1, 2).}
#'   \item{distancia}{\code{character}. The distance category of the trip, represented as a character string (e.g., '002-005' for 2-5 km).}
#'   \item{viajes}{\code{double}. The number of trips taken within the specified time period and distance.}
#'   \item{viajes_km}{\code{double}. The total length of all trips in kilometers for the specified time period and distance.}
#'   \item{day}{\code{double}. The day of the trip.}
#'   \item{month}{\code{double}. The month of the trip.}
#'   \item{year}{\code{double}. The year of the trip.}
#' }
#' @keywords internal
spod_duckdb_od <- function(
    con = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE),
    zones = c(
      "districts", "dist", "distr", "distritos",
      "municipalities", "muni", "municip", "municipios",
      "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
    ),
    ver = NULL,
    data_dir = spod_get_data_dir()
) {
    
  ver <- as.integer(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check that gau is not selected with ver = 1
  if (ver == 1 && zones == "gau") {
    stop("The 'gau' zones are not available in v1 data.")
  }

  if (ver == 1) {
    # selecting districts files for v1 to avoid issues with municipalities # this is to address the bugs described in detail in:
    # http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
    # http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
    # the decision was to use distrcit data and aggregate it to replicate municipal data
    csv_folder <- paste0(
      data_dir, "/",
      spod_subfolder_raw_data_cache(ver = ver),
      "/maestra1-mitma-distritos",
      "/ficheros-diarios/"
    )
  } else if (ver == 2) {
    csv_folder <- paste0(
      data_dir, "/",
      spod_subfolder_raw_data_cache(ver = ver),
      "/estudios_basicos/por-", spod_zone_names_en2es(zones),
      "/viajes/ficheros-diarios/"
    )
  }

  # create view to the raw TXT/CSV.gz files
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-od-{zones}-raw-csv-view.sql"))
  )

  # create ENUMs

  # zones ENUMs from uniqe ids of relevant zones
  spatial_data <- spod_get_zones(zones,
    ver = ver,
    data_dir = data_dir,
    quiet = TRUE
  )
  
  if( ver == 1 ) {
    unique_ids <- unique(spatial_data$id)
  } else if( ver == 2 ) {
    unique_ids <- c("externo", unique(spatial_data$id))
  }
  
  DBI::dbExecute(
    con,
    dplyr::sql(
      paste0(
        "CREATE TYPE ZONES_ENUM AS ENUM ('",
        paste0(unique_ids, collapse = "','"),
        "');"
      )
    )
  )

  # create ACTIV_ENUM (for all except for municipalities in v1 data)
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-od-enum-activity-en.sql"))
  )

  # create DISTANCE_ENUM
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-od-enum-distance.sql"))
  )

  con <- spod_duckdb_create_province_enum(con)
  
  
  # v2 only enums
  if (ver == 2) {
    # income ENUM
    DBI::dbExecute(
      con,
      spod_read_sql(glue::glue("v{ver}-od-enum-income.sql"))
    )
    
    # age ENUM
    DBI::dbExecute(
      con,
      spod_read_sql(glue::glue("v{ver}-od-enum-age.sql"))
    )
    
    # sex ENUM
    DBI::dbExecute(
      con,
      spod_read_sql(glue::glue("v{ver}-od-enum-sex-en.sql"))
    )
  }

  # create od_csv_clean view
  if (ver == 1 && zones == "municipios") {
    # this will be picked up by the sql loaded below if neccessary
    relations_districts_municipalities <- here::here(
        data_dir,
        spod_subfolder_raw_data_cache(1),
        "relaciones_distrito_mitma.csv"
    )
  }
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-od-{zones}-clean-csv-view-en.sql"))
  )

  # return the connection as duckdb object
  return(con)
}

#' Create a duckdb number of trips table
#' 
#' @description
#' This function creates a duckdb connection to the number of trips data stored in a folder of CSV.gz files.
#' @inheritParams spod_duckdb_od
#' @inheritParams spod_available_data
#' @inheritParams spod_download_data
#' 
#' @return A duckdb connection with 2 views.
#' 
#' @keywords internal
spod_duckdb_number_of_trips <- function(
  con = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE),
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  ver = NULL,
  data_dir = spod_get_data_dir()
) {
  
  ver <- as.integer(ver)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 or 2.")
  }
  
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check that gau is not selected with ver = 1
  if (ver == 1 && zones == "gau") {
    stop("The 'gau' zones are not available in v1 data.")
  }

  if (ver == 1) {
    # selecting districts files for v1 to avoid issues with municipalities # this is to address the bugs described in detail in:
    # http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
    # http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
    # the decision was to use distrcit data and aggregate it to replicate municipal data
    csv_folder <- paste0(
      data_dir, "/",
      spod_subfolder_raw_data_cache(ver = ver),
      "/maestra2-mitma-distritos",
      "/ficheros-diarios/"
    )
  } else if (ver == 2) {
    csv_folder <- paste0(
      data_dir, "/",
      spod_subfolder_raw_data_cache(ver = ver),
      "/estudios_basicos/por-", spod_zone_names_en2es(zones),
      "/viajes/ficheros-diarios/"
    )
  }

  # create view of csv files and preset variable types

  # create view to the raw TXT/CSV.gz files
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-tpp-{zones}-raw-csv-view.sql"))
  )

  # create ENUMs

  # zones ENUMs from uniqe ids of relevant zones
  spatial_data <- spod_get_zones(zones,
    ver = ver,
    data_dir = data_dir,
    quiet = TRUE
  )
  
  if( ver == 1 ) {
    unique_ids <- unique(spatial_data$id)
  } else if( ver == 2 ) {
    # unique_ids <- c("externo", unique(spatial_data$id)) # double check
  }
  
  DBI::dbExecute(
    con,
    dplyr::sql(
      paste0(
        "CREATE TYPE ZONES_ENUM AS ENUM ('",
        paste0(unique_ids, collapse = "','"),
        "');"
      )
    )
  )

  # create N_TRIPS_ENUM
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-tpp-enum-ntrips.sql"))
  )

  
  # v2 only enums
  if (ver == 2) {
    ### TODO - check if any v2 specific ones
  }

  # create od_csv_clean view
  if (ver == 1 && zones == "municipios") {
    # this will be picked up by the sql loaded below if neccessary
    relations_districts_municipalities <- here::here(
        data_dir,
        spod_subfolder_raw_data_cache(1),
        "relaciones_distrito_mitma.csv"
    )
  }
  DBI::dbExecute(
    con,
    spod_read_sql(glue::glue("v{ver}-tpp-{zones}-clean-csv-view-en.sql"))
  )

  return(con)
}

#' Filter a duckdb conenction by dates
#' 
#' @description
#' IMPORTANT: This function assumes that the table or view that is being filtered has separate `year`, `month` and `day` columns with integer values. This is done so that the filtering is faster on CSV files that are stored in a folder structure with hive-style `/year=2020/month=2/day=14/`.
#' 
#' 
#' @param con A duckdb connection
#' @param source_view_name The name of the source duckdb "view" (the virtual table, in the context of current package likely connected to a folder of CSV files).
#' @param new_view_name The name of the new duckdb "view" (the virtual table, in the context of current package likely connected to a folder of CSV files).
#' @inheritParams spod_dates_argument_to_dates_seq
#' @param source_view_name The name of the source duckdb "view" (the virtual table, in the context of current package likely connected to a folder of CSV files)
#' @keywords internal
spod_duckdb_filter_by_dates <- function(con, source_view_name, new_view_name, dates) {
  # prepare query to filter by dates
  query <- dplyr::sql(
    glue::glue(
      "CREATE VIEW {new_view_name} AS SELECT * FROM {source_view_name} ",
      spod_sql_where_dates(dates)
    )
  )

  # create a view with a filter to the desired dates
  DBI::dbSendStatement(con, query)

  return(con)
}

spod_duckdb_create_province_enum <- function(con) {
  # LOAD SQL STATEMENT to create province names ENUM
  province_names_enum_sql <- readLines(
    system.file("extdata/sql-queries/province_names_enum.sql",
      package = "spanishoddata"
    )
  ) |>
    paste(collapse = "\n") |> 
    dplyr::sql()

  # create INE_PROV_NAME_ENUM
  DBI::dbSendStatement(
    con,
    province_names_enum_sql
  )

  # also create INE encoded INE_PROV_CODE_ENUM
  
  # format codes 1:52 as 2 digits with leading zeros
  ine_codes <- sprintf("%02d", 1:52)

  # create INE_PROV_CODE_ENUM
  DBI::dbSendStatement(
    con,
    dplyr::sql(
      paste0(
        "CREATE TYPE INE_PROV_CODE_ENUM AS ENUM ('",
        paste0(ine_codes, collapse = "','"),
        "');"
      )
    )
  )

  return(con)
}

#' Generate a WHERE part of an SQL query from a sequence of dates
#' @param dates A Dates vector of dates to process.
#' @return A character vector of the SQL query.
#' @keywords internal
spod_sql_where_dates <- function(dates) {
  # Extract unique year, month, and day combinations from the dates
  date_parts <- data.frame(
    year = format(dates, "%Y"),
    month = format(dates, "%m"),
    day = format(dates, "%d")
  )

  # Get distinct rows and sort them by year, month, and day
  date_parts <- date_parts[!duplicated(date_parts), ]
  date_parts <- date_parts[order(date_parts$year, date_parts$month, date_parts$day), ]

  # Create the WHERE conditions for each unique date
  where_conditions <- stats::aggregate(day ~ year + month, data = date_parts, FUN = function(x) paste(x, collapse = ", "))
  where_conditions$condition <- paste0(
    "(year = ", where_conditions$year,
    " AND month = ", where_conditions$month,
    " AND day IN (", where_conditions$day, "))"
  )

  # Combine all conditions into a single WHERE clause
  sql_query <- paste0(
    "WHERE ",
    paste(where_conditions$condition, collapse = " OR ")
  )

  return(sql_query)
}

#' Set maximum memory and number of threads for a DuckDB connection
#' @param con A duckdb connection
#' @param max_mem_gb The maximum memory to use in GB. A conservative default is 3 GB, which should be enough for resaving the data to DuckDB form a folder of CSV.gz files while being small enough to fit in memory of most even old computers. For data analysis using the already converted data (in DuckDB or Parquet format) or with the raw CSV.gz data, it is recommended to increase it according to available resources.
#' @param max_n_cpu The maximum number of threads to use. Defaults to the number of available cores minus 1.
#' @return The duckdb connection.
#' @keywords internal
spod_duckdb_limit_resources <- function(
  con,
  max_mem_gb = max(4, spod_available_ram() - 4),
  max_n_cpu = parallelly::availableCores() - 1
) {
  DBI::dbExecute(
    con,
    dplyr::sql(
      glue::glue("SET max_memory='{max_mem_gb}GB';")
    )
  )

  DBI::dbExecute(
    con,
    dplyr::sql(
      glue::glue("SET threads='{max_n_cpu}';")
    )
  )

  return(con)
}

#' Load an SQL query, glue it, dplyr::sql it
#' @description
#' Load an SQL query from a specified file in package installation directory, glue::collapse it, glue::glue it in case of any variables that need to be replaced, and dplyr::sql it for additional safety.
#' 
#' @return Text of the SQL query of class `sql`/`character`.
#' @param sql_file_name The name of the SQL file to load from the package installation directory.
#' @keywords internal
spod_read_sql <- function(sql_file_name) {
  sql_file_path <- glue::glue("extdata/sql-queries/{sql_file_name}")
  
  sql_query <- readLines(
    system.file(sql_file_path, package = "spanishoddata")) |> 
    glue::glue_collapse(sep = "\n") |> 
    glue::glue(.envir = parent.frame()) |> 
    dplyr::sql()
  
  return(sql_query)
}

#' Set temp file for DuckDB connection
#' @param con A duckdb connection
#' @param temp_path The path to the temp folder for DuckDB for \href{https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling}{intermediate spilling} in case the set memory limit and/or physical memory of the computer is too low to perform the query. By default this is set to the `temp` directory in the data folder defined by SPANISH_OD_DATA_DIR environment variable. Otherwise, for queries on folders of CSV files or parquet files, the temporary path would be set to the current R working directory, which probably is undesirable, as the current working directory can be on a slow storage, or storage that may have limited space, compared to the data folder.
#' @return A duckdb connection.
#' @keywords internal
spod_duckdb_set_temp <- function(
  con,
  temp_path = spod_get_temp_dir()
) {
  DBI::dbExecute(
    con,
    dplyr::sql(
      glue::glue("SET temp_directory='{temp_path}';")
    )
  )

  return(con)
}
