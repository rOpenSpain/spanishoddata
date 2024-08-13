#' Creates a duckdb connection to v1 OD data
#'
#' This function creates a duckdb connection to the v1 OD data.
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
#'   \item{full_date}{\code{Date}. The full date of the trip, including year, month, and day.}
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
      "lau", "large_urban_areas", "gau", "grandes_areas_urbanas"
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
    csv_folder <- paste0(
      data_dir, "/",
      spod_subfolder_raw_data_cache(ver = ver),
      "/maestra1-mitma-", spod_zone_names_en2es(zones),
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
  raw_csv_view_sql_file <- glue::glue("extdata/sql-queries/v{ver}-od-{zones}-raw-csv-view.sql")

  # Load SQL statement
  sql_connect_csv <- readLines(
    system.file(raw_csv_view_sql_file, package = "spanishoddata")
  ) |> 
    glue::glue_collapse(sep = "\n") |> 
    glue::glue() |> 
    dplyr::sql()

  # Execute SQL statement
  DBI::dbExecute(con, sql_connect_csv)

  # create ENUMs

  # zones ENUMs from uniqe ids of relevant zones
  spatial_data <- spod_get_zones(zones,
    ver = ver,
    data_dir = data_dir,
    quiet = TRUE
  )
  
  unique_ids <- unique(spatial_data$id)
  
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
  if ( !( zones == "municipios" & ver == 1) ) {
    # LOAD SQL STATEMENT
    sql_create_activ_enum <- readLines(
      system.file(
        glue::glue("extdata/sql-queries/v{ver}-od-enum-activity-en.sql"),
        package = "spanishoddata"
      )
    ) |> 
      glue::glue_collapse(sep = "\n") |> 
      dplyr::sql()
    # EXECUTE SQL STATEMENT
    DBI::dbExecute(con, sql_create_activ_enum)
  }

  # create DISTANCE_ENUM
  sql_create_distance_enum <- readLines(
    system.file(
      glue::glue("extdata/sql-queries/v{ver}-od-enum-distance.sql"),
      package = "spanishoddata"
    )
  ) |> 
    glue::glue_collapse(sep = "\n") |> 
    dplyr::sql()
  
  DBI::dbExecute(con, sql_create_distance_enum)

  if ( !( zones == "municipios" & ver == 1) ) {
    # create named INE province ENUM (for all except for municipalities in v1 data)
    spod_duckdb_create_province_enum(con)
  }
  
  # v2 only enums
  if (ver == 2) {
    # income ENUM
    sql_create_income_enum <- readLines(
      system.file(
        glue::glue("extdata/sql-queries/v{ver}-od-enum-income.sql"),
        package = "spanishoddata"
      )
    ) |> glue::glue_collapse(sep = "\n") |> 
      dplyr::sql()
    DBI::dbExecute(con, sql_create_income_enum)
    
    # age ENUM
    sql_create_age_enum <- readLines(
      system.file(
        glue::glue("extdata/sql-queries/v{ver}-od-enum-age.sql"),
        package = "spanishoddata"
      )
    ) |>
      glue::glue_collapse(sep = "\n") |> 
      dplyr::sql()
    DBI::dbExecute(con, sql_create_age_enum)
    
    # sex ENUM
    sql_create_sex_enum <- readLines(
      system.file(
        glue::glue("extdata/sql-queries/v{ver}-od-enum-sex-en.sql"),
        package = "spanishoddata"
      )
    ) |>
      glue::glue_collapse(sep = "\n") |> 
      dplyr::sql()
    DBI::dbExecute(con, sql_create_sex_enum)
  }

  # load sql statement to create od_csv_clean view
  od_csv_clean_sql <- readLines(
    system.file(
      glue::glue("extdata/sql-queries/v{ver}-od-{zones}-clean-od_csv-en.sql"),
      package = "spanishoddata"
    )
  ) |> 
    glue::glue_collapse(sep = "\n") |> 
    dplyr::sql()

  DBI::dbExecute(con, od_csv_clean_sql)


  # return the connection as duckdb object
  return(con)
}


#' Filter a duckdb conenction by dates
#' @param con A duckdb connection
#' @param source_view_name The name of the source duckdb "view" (the virtual table, in the context of current package likely connected to a folder of CSV files).
#' @param new_view_name The name of the new duckdb "view" (the virtual table, in the context of current package likely connected to a folder of CSV files).
#' @inheritParams spod_dates_argument_to_dates_seq
#' @param source_view_name The name of the source duckdb "view" (the virtual table, in the context of current package likely connected to a folder of CSV files)
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
#' @param duck_max_mem The maximum memory to use in GB. A conservative default is 3 GB, which should be enough for resaving the data to DuckDB form a folder of CSV.gz files while being small enough to fit in memory of most even old computers. For data analysis using the already converted data (in DuckDB or Parquet format) or with the raw CSV.gz data, it is recommended to increase it according to available resources.
#' @param duck_max_threads The maximum number of threads to use. Defaults to the number of available cores minus 1.
spod_duckdb_limit_resources <- function(
    con,
    duck_max_mem = 3, # in GB, default to 3 GB, should be enough to resave the data and small enough to fit in memory of most even old computers
    duck_max_threads = parallelly::availableCores() - 1 # leave one core for other tasks by default
    ) {
  DBI::dbExecute(
    con,
    dplyr::sql(
      glue::glue("SET max_memory='{duck_max_mem}GB';")
    )
  )

  DBI::dbExecute(
    con,
    dplyr::sql(
      glue::glue("SET threads='{duck_max_threads}';")
    )
  )

  return(con)
}
