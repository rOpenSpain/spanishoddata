
#' Function to create a duckdb connection to v1 OD data
#' 
#' This function creates a duckdb connection to the v1 OD data.
#' @inheritParams spod_download_data
#' @return A duckdb connection object with 3 views:
#'  
#'  * `all_od_v1_csv_files` - a raw table view of all cached CSV files with the origin-destination data that has been previously cached in $SPANISH_OD_DATA_DIR
#'  
#'  * `od` - a cleaned-up table view of `all_od_v1_csv_files` with column names and values translated and mapped to English. This still includes all cached data.
#' 
#'  * `od_filtered` - a filtered view of `od` with the desired dates.
#' 
#' The structure of the cleaned-up views `od` and `od_filtered` is as follows:
#' 
#' \describe{
#'   \item{full_date}{\code{Date}. The full date of the trip, including year, month, and day.}
#'   \item{id_origin}{\code{factor}. The identifier for the origin location of the trip, formatted as a code (e.g., '01001_AM').}
#'   \item{id_destination}{\code{factor}. The identifier for the destination location of the trip, formatted as a code (e.g., '01001_AM').}
#'   \item{activity_origin}{\code{factor}. The type of activity at the origin location (e.g., 'home', 'work'). \strong{Note:} Only available for district level data.}
#'   \item{activity_destination}{\code{factor}. The type of activity at the destination location (e.g., 'home', 'other'). \strong{Note:} Only available for district level data.}
#'   \item{residence_province}{\code{factor}. The province of residence for the individual making the trip (e.g., 'Cuenca', 'Girona'). Provinces are stored as factors, and are encoded in a way that the province code can be used for queries. \strong{Note:} Only available for district level data.}
#'   \item{time_slot}{\code{integer}. The time slot during which the trip started, represented as an integer (e.g., 0, 1, 2).}
#'   \item{distance}{\code{factor}. The distance category of the trip, represented as a code (e.g., '002-005' for 2-5 km).}
#'   \item{n_trips}{\code{double}. The number of trips taken within the specified time slot and distance.}
#'   \item{trips_total_length_km}{\code{double}. The total length of all trips in kilometers for the specified time slot and distance.}
#'   \item{year}{\code{double}. The year of the trip.}
#'   \item{month}{\code{double}. The month of the trip.}
#'   \item{day}{\code{double}. The day of the trip.}
#' }
#' 
#' The structure of the original data in `all_od_v1_csv_files` is as follows:
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
spod_duckdb_od_v1 <- function(
  zones = c("districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lau", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  dates = NULL,
  data_dir = spod_get_data_dir()
) {
  ver <- 1
  
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)
  
  csv_folder <- paste0(
    data_dir, "/",
    spod_subfolder_raw_data_cache(ver = ver),
    "/maestra1-mitma-", spod_zone_names_en2es(zones),
    "/ficheros-diarios/"
  )

  # create in memory duckdb connection
  drv <- duckdb::duckdb()
  con <- DBI::dbConnect(drv, dbdir = ":memory:", read_only = TRUE)

  # create view of csv files and preset variable types
  if (zones == "distritos") {
    DBI::dbSendStatement(con,
      dplyr::sql(
        glue::glue(
          "CREATE VIEW all_od_v1_csv_files AS SELECT *
          FROM read_csv_auto('{csv_folder}**/*.txt.gz', delim='|', header=TRUE, hive_partitioning=TRUE,
          columns={{
            'fecha': 'DATE',
            'origen': 'VARCHAR',
            'destino': 'VARCHAR',
            'actividad_origen': 'VARCHAR',
            'actividad_destino': 'VARCHAR',
            'residencia': 'VARCHAR',
            'edad': 'VARCHAR',
            'periodo': 'INTEGER',
            'distancia': 'VARCHAR',
            'viajes': 'DOUBLE',
            'viajes_km': 'DOUBLE'
          }},
          dateformat='%Y%m%d');"
        )
      )
    )
  } else if (zones  == "municipios") {
    DBI::dbSendStatement(con,
      dplyr::sql(
        glue::glue(
          "CREATE VIEW all_od_v1_csv_files AS SELECT *
          FROM read_csv_auto('{csv_folder}**/*.txt.gz', delim='|', header=TRUE, hive_partitioning=TRUE,
          columns={{
            'fecha': 'DATE',
            'origen': 'VARCHAR',
            'destino': 'VARCHAR',
            'periodo': 'INTEGER',
            'distancia': 'VARCHAR',
            'viajes': 'DOUBLE',
            'viajes_km': 'DOUBLE'
          }},
          dateformat='%Y%m%d');"
        )
      )
    )
  }

  # preview table
  # DBI::dbGetQuery(con, "SELECT * FROM all_od_v1_csv_files LIMIT 10") |> dplyr::glimpse() # for debugging

  # create ENUMs
  
  # zones ENUMs
  zones <- spod_zone_names_en2es(zones)
  spatial_data <- spod_get_zones_v1(zones, quiet = TRUE)
  unique_ids <- unique(spatial_data$id)
  DBI::dbSendStatement(
    con,
    dplyr::sql(
      paste0("CREATE TYPE ZONES_ENUM AS ENUM ('",
        paste0(unique_ids, collapse = "','"),
        "');"
      )
    )
  )

  # create ACTIV_ENUM
  if (zones == "distritos") {
    DBI::dbSendStatement(
    con,
    dplyr::sql("CREATE TYPE ACTIV_ENUM AS ENUM ('home', 'work', 'other')")
    )
  }

  # create DISTANCE_ENUM    
  DBI::dbSendStatement(
    con,
    dplyr::sql("CREATE TYPE DISTANCE_ENUM AS ENUM ('002-005', '005-010', '010-050', '0005-002', '050-100', '100+');")
  )

  # create INE province ENUM
  if (zones == "distritos") {
    spod_duckdb_create_province_enum(con)
    # DBI::dbGetQuery(con, "SELECT enum_range(NULL::INE_PROV_ENUM)") # check that it was created, remove this line when package is stable
    # for debugging
    # DBI::dbSendStatement(con, "DROP TYPE INE_PROV_ENUM") # remove this line when package is stable
    
    # create second view with desired data types including ENUMs
    # create view to fix variable types and recode values to English
    # NOTE: thsi raises non-ASCII character WARNING on R CMD check, so will need to store this query in a text file
    # load when_then_provinces from a system file in inst/extdata/sql-queries/when-recode-provinces.txt
    when_then_provinces <- readLines(
      system.file(
        "extdata/sql-queries/when-recode-provinces.txt",
        package = "spanishoddata")) |>
      paste(collapse = "\n")

    # now execute the query pasting in the contents of when_then_provinces
    DBI::dbSendStatement(con,
      dplyr::sql(
        glue::glue(
          "CREATE VIEW od AS SELECT
                            fecha AS full_date,
          CAST(origen AS ZONES_ENUM) AS id_origin,
          CAST(destino AS ZONES_ENUM) AS id_destination,
          CAST(CASE actividad_origen
            WHEN 'casa' THEN 'home'
            WHEN 'otros' THEN 'other'
            WHEN 'trabajo_estudio' THEN 'work'
            END AS ACTIV_ENUM) AS activity_origin,
          CAST(CASE actividad_destino
            WHEN 'casa' THEN 'home'
            WHEN 'otros' THEN 'other'
            WHEN 'trabajo_estudio' THEN 'work_or_study'
            END AS ACTIV_ENUM) AS activity_destination,
            CAST (CASE residencia
                {when_then_provinces}
                END AS INE_PROV_ENUM) AS residence_province,
          periodo AS time_slot,
          CAST(distancia AS DISTANCE_ENUM) AS distance,
          viajes AS n_trips,
          viajes_km AS trips_total_length_km,
          year AS year,
          month AS month,
          day AS day
          FROM all_od_v1_csv_files;"
        )
      )
    )
  } else if (zones == "municipios") {
    DBI::dbSendStatement(con,
      dplyr::sql(
        "CREATE VIEW od AS SELECT
          fecha AS full_date,
          CAST(origen AS ZONES_ENUM) AS id_origin,
          CAST(destino AS ZONES_ENUM) AS id_destination,
          periodo AS time_slot,
          CAST(distancia AS DISTANCE_ENUM) AS distance,
          viajes AS n_trips,
          viajes_km AS trips_total_length_km,
          year AS year,
          month AS month,
          day AS day
        FROM all_od_v1_csv_files;"
      )
    )
  }

  # preview result for debugging
  # DBI::dbGetQuery(con, "SELECT * FROM trips_view LIMIT 10") |> dplyr::glimpse()

  # prepare query to filter by dates
  query <- dplyr::sql(
    paste0(
      "CREATE VIEW od_filtered AS SELECT * FROM od ",
      spod_sql_where_dates(dates)
    )
  )
  
  # create a view with a filter to the desired dates
  DBI::dbSendStatement(con, query)

  # preview the new view for debugging
  # DBI::dbGetQuery(con, "SELECT * FROM trips LIMIT 10") |> dplyr::glimpse()

  # return the connection as duckdb object
  return(con)
}


spod_duckdb_create_province_enum <- function(con){
  
  # load provinces with non-ASCII names
  provinces_enum <- readLines(
    system.file("extdata/sql-queries/provinces-enum.txt",
      package = "spanishoddata")) |>
    paste(collapse = "\n")
  
  # create INE_PROV_ENUM
  DBI::dbSendStatement(
    con,
    dplyr::sql(
      glue::glue(
        "CREATE TYPE INE_PROV_ENUM AS ENUM (
        {provinces_enum}
        );"
      )
    )
  )

  # for debugging
  # DBI::dbGetQuery(con, "SELECT enum_range(NULL::INE_PROV_ENUM)") # check that it was created, remove this line when package is stable
  # DBI::dbSendStatement(con, "DROP TYPE INE_PROV_ENUM") # remove this line when package is stable

  return(con)
}
