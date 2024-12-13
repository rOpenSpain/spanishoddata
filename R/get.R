#' Get zones
#' 
#' @description
#' Get spatial zones for the specified data version. Supports both v1 (2020-2021) and v2 (2022 onwards) data.
#' 
#' @inheritParams spod_download
#' @inheritParams spod_available_data
#' @return An `sf` object (Simple Feature collection).
#' 
#' The columns for v1 (2020-2021) data include:
#' \describe{
#'   \item{id}{A character vector containing the unique identifier for each district, assigned by the data provider. This `id` matches the `id_origin`, `id_destination`, and `id` in district-level origin-destination and number of trips data.}
#'   \item{census_districts}{A string with semicolon-separated identifiers of census districts classified by the Spanish Statistical Office (INE) that are spatially bound within the polygons for each `id`.}
#'   \item{municipalities_mitma}{A string with semicolon-separated municipality identifiers (as assigned by the data provider) corresponding to each district `id`.}
#'   \item{municipalities}{A string with semicolon-separated municipality identifiers classified by the Spanish Statistical Office (INE) corresponding to each `id`.}
#'   \item{district_names_in_v2/municipality_names_in_v2}{A string with semicolon-separated district names (from the v2 version of this data) corresponding to each district `id` in v1.}
#'   \item{district_ids_in_v2/municipality_ids_in_v2}{A string with semicolon-separated district identifiers (from the v2 version of this data) corresponding to each district `id` in v1.}
#'   \item{geometry}{A `MULTIPOLYGON` column containing the spatial geometry of each district, stored as an sf object. The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
#' }
#' 
#' The columns for v2 (2022 onwards) data include:
#' \describe{
#'   \item{id}{A character vector containing the unique identifier for each zone, assigned by the data provider.}
#'   \item{name}{A character vector with the name of each district.}
#'   \item{population}{A numeric vector representing the population of each district (as of 2022).}
#'   \item{census_sections}{A string with semicolon-separated identifiers of census sections corresponding to each district.}
#'   \item{census_districts}{A string with semicolon-separated identifiers of census districts as classified by the Spanish Statistical Office (INE) corresponding to each district.}
#'   \item{municipalities}{A string with semicolon-separated identifiers of municipalities classified by the Spanish Statistical Office (INE) corresponding to each district.}
#'   \item{municipalities_mitma}{A string with semicolon-separated identifiers of municipalities, as assigned by the data provider, that correspond to each district.}
#'   \item{luas_mitma}{A string with semicolon-separated identifiers of LUAs (Local Urban Areas) from the provider, associated with each district.}
#'   \item{district_ids_in_v1/municipality_ids_in_v1}{A string with semicolon-separated district identifiers from v1 data corresponding to each district in v2. If no match exists, it is marked as `NA`.}
#'   \item{geometry}{A `MULTIPOLYGON` column containing the spatial geometry of each district, stored as an sf object. The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
#' }
#' 
#' @export
spod_get_zones <- function(
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  ver = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  # Validate inputs
  checkmate::assert_choice(zones, choices = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ))

  checkmate::assertIntegerish(ver, max.len = 1)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 (for v1 2020-2021 data) or 2 (for v2 2022 onwards).")
  }
  
  
  checkmate::assert_directory_exists(data_dir, access = "rw")
  checkmate::assert_flag(quiet)
  
  # normalise zones
  zones <- spod_zone_names_en2es(zones)
  
  if (ver == 1) {
    zones_sf <- spod_get_zones_v1(zones = zones, data_dir = data_dir, quiet = quiet)
  } else if (ver == 2) {
    zones_sf <- spod_get_zones_v2(zones = zones, data_dir = data_dir, quiet = quiet)
  }

  return(zones_sf)
}



#' retrieves the zones data
#'
#' This function retrieves the zones data from the specified data directory.
#' It can retrieve either "distritos" or "municipios" zones data.
#'
#' @param data_dir The directory where the data is stored.
#' @param zones The zones for which to download the data. Can be `"districts"` (or `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish `"municipios"`).
#' @inheritParams global_quiet_param
#' @return An `sf` object (Simple Feature collection) with 4 fields:
#' \describe{
#'   \item{id}{A character vector containing the unique identifier for each zone, to be matched with identifiers in the tabular data.}
#'   \item{name}{A character vector with the name of the zone.}
#'   \item{population}{A numeric vector representing the population of each zone (as of 2022).}
#'   \item{geometry}{A `MULTIPOLYGON` column containing the spatial geometry of each zone, stored as an sf object.
#'   The geometry is projected in the ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY dimensions.}
#' }
#' @examples
#' if (FALSE) {
#'   zones <- spod_get_zones_v2()
#' }
#' @keywords internal
spod_get_zones_v2 <- function(
  zones = c(
    "districts", "dist", "distr", "distritos",
    "municipalities", "muni", "municip", "municipios",
    "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"
  ),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
) {
  zones <- match.arg(zones)
  zones <- spod_zone_names_en2es(zones)

  # check if gpkg files are already saved and load them if available
  expected_gpkg_path <- fs::path(
    data_dir,
    glue::glue(spod_subfolder_clean_data_cache(ver = 2),
      "/zones/{zones}_mitma.gpkg"
    )
  )
  if (fs::file_exists(expected_gpkg_path)) {
    if (isFALSE(quiet)) {
      message("Loading .gpkg file that already exists in data dir: ", expected_gpkg_path)
    }
    return(sf::read_sf(expected_gpkg_path))
  }
  
  # if no existing gpkg found above, continue here with download and data cleanup
  metadata <- spod_available_data_v2(data_dir, check_local_files = TRUE)
  zones_regex <- glue::glue("(zonificacion_{zones}\\.*)|(poblacion\\.csv)|(relacion_ine_zonificacionMitma\\.csv)")
  sel_zones <- stringr::str_detect(metadata$local_path, zones_regex)
  metadata_zones <- metadata[sel_zones, ]
  metadata_zones_for_download <- metadata_zones[metadata_zones$downloaded == FALSE, ]
  if (nrow(metadata_zones_for_download) > 0){
    dir_names <- unique(fs::path_dir(metadata_zones_for_download$local_path))
    if (any(!dir.exists(dir_names))) {
      fs::dir_create(dir_names, recurse = TRUE)
    }
    if (isFALSE(quiet)) {
      message("Downloading missing zones data...")
    }
    curl::multi_download(
      urls = metadata_zones_for_download$target_url,
      destfiles = metadata_zones_for_download$local_path,
      resume = TRUE,
      progress = TRUE
    )
  }
  
  zones_path <- fs::dir_ls(
    path = fs::path(data_dir, spod_subfolder_raw_data_cache(ver = 2)),
    regexp = glue::glue("zonificacion_{tolower(zones)}s?\\.shp$"),
    recurse = TRUE
  )

  zones_sf <- spod_clean_zones_v2(zones_path)
  fs::dir_create(fs::path_dir(expected_gpkg_path), recurse = TRUE)
  sf::st_write(
    zones_sf,
    expected_gpkg_path,
    delete_dsn = TRUE,
    delete_layer = TRUE
  )

  return(zones_sf)
  }

#' Fixes common issues in the zones data and cleans up variable names
#'
#' This function fixes any invalid geometries in the zones data and renames the "ID" column to "id". It also attacches the population counts and zone names provided in the csv files supplied by the original data provider.
#'
#' @param zones_path The path to the zones spatial data file.
#' @return A spatial object containing the cleaned zones data. 
#' @importFrom stats median
#' @keywords internal
#'
spod_clean_zones_v2 <- function(zones_path) {
  # detect what kind of zones find out if it is distritos, municipios or GAU
  zones <- stringr::str_extract(zones_path, "distritos|municipios|gaus")

  if(fs::file_exists(zones_path) == FALSE) {
    stop("File does not exist: ", zones_path)
  }
  suppressWarnings({
    zones_sf <- sf::read_sf(zones_path)
  })

  # fix geometry
  invalid_geometries <- !sf::st_is_valid(zones_sf)
  if (sum(invalid_geometries) > 0) {
    fixed_zones_sf <- sf::st_make_valid(zones_sf[invalid_geometries, ])
    zones_sf <- rbind(zones_sf[!invalid_geometries, ], fixed_zones_sf)
  }

  # lowercase id column name
  names(zones_sf)[names(zones_sf) == "ID"] <- "id"

  population <- readr::read_delim(
    glue::glue(fs::path_dir(zones_path), "/poblacion_{zones}.csv"),
    delim = "|",
    col_names = c("id", "population"),
    col_types = c("c", "i")
  )

  if (zones %in% c("distritos","gaus")) {
    zone_names <- readr::read_delim(
      glue::glue(fs::path_dir(zones_path), "/nombres_{zones}.csv"),
      skip = 1,
      delim = "|",
      col_names = c("id", "name"),
      col_types = c("c", "i")
    )
  } else if (zones == "municipios") {
    zone_names <- readr::read_delim(
      glue::glue(fs::path_dir(zones_path), "/nombres_{zones}.csv"),
      skip = 1,
      delim = "|",
      col_names = c("row", "id", "name"),
      col_types = c("i", "c", "i")
    ) |> 
      dplyr::select(-"row")
  }
 
  # zones reference
  zones_ref <- readr::read_delim(
    glue::glue(spod_get_data_dir(quiet = TRUE), "/", spod_subfolder_raw_data_cache(ver = 2), "zonificacion/relacion_ine_zonificacionMitma.csv"),
    delim = "|",
    col_types = rep("c", 6)
  )

  zone_mitma <- glue::glue("{gsub('s$', '', zones)}_mitma")
  
  zones_ref_renamed <- zones_ref |>
    dplyr::rename(
      census_sections = "seccion_ine",
      census_districts = "distrito_ine",
      municipalities = "municipio_ine",
      districts_mitma = "distrito_mitma",
      municipalities_mitma = "municipio_mitma",
      luas_mitma = "gau_mitma",
      id = zone_mitma
    )
  
  zones_ref_aggregated <- zones_ref_renamed |>
  dplyr::group_by(.data$id) |>
  dplyr::summarise(
    dplyr::across(
    .cols = dplyr::everything(),
    .fns = ~ paste(.x, collapse = "; "),
    .names = "{.col}"
  ))

  # cleanup duplacate ids in zones_ref_aggregated
  zones_ref_aggregated <- zones_ref_aggregated |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = spod_unique_separated_ids
      )
    )

  # combine zones with population, names, and zones reference
  zones_sf <- zones_sf |>
    dplyr::left_join(zone_names, by = "id") |> 
    dplyr::left_join(population, by = "id") |> 
    dplyr::left_join(zones_ref_aggregated, by = "id") |>
    dplyr::relocate(.data$geometry, .after = dplyr::last_col())


  # load v1 zones to join ids, unless it's gau zones
  if(zones != "gaus") {
    spod_download_zones_v1(zones = zones, quiet = TRUE)
    zones_v1_path <- fs::dir_ls(
      path = fs::path(spod_get_data_dir(), spod_subfolder_raw_data_cache(ver = 1)),
      glob = glue::glue("*v1**{zones}/*.shp"),
      recurse = TRUE
    )
    suppressWarnings({
      zones_v1_sf <- sf::read_sf(zones_v1_path)
    })
    invalid_geometries <- !sf::st_is_valid(zones_v1_sf)
    if (sum(invalid_geometries) > 0) {
      fixed_zones_v1_sf <- sf::st_make_valid(zones_v1_sf[invalid_geometries, ])
      zones_v1_sf <- rbind(zones_v1_sf[!invalid_geometries, ], fixed_zones_sf)
    }
    
    names(zones_v1_sf)[names(zones_v1_sf) == "ID"] <- "id_in_v1"

    suppressWarnings(
      zones_v2_sf_centroids <- zones_sf |> sf::st_point_on_surface()
    )
    v2_to_v1 <- sf::st_join(zones_v1_sf, zones_v2_sf_centroids, left = TRUE) |> 
      sf::st_drop_geometry()
    v2_v_1ref <- v2_to_v1 |>
      dplyr::group_by(.data$id) |> 
        dplyr::summarize(
        ids_in_v1_data = paste(.data$id_in_v1, collapse = "; ")
      )
    eng_zones <- dplyr::if_else(zones == "distritos", true = "district", false = "municipality")
    names(v2_v_1ref)[names(v2_v_1ref) == "ids_in_v1_data"] <- glue::glue("{eng_zones}_ids_in_v1")

    zones_sf <- zones_sf |> 
      dplyr::left_join(v2_v_1ref, by = "id") |> 
      dplyr::relocate(.data$geometry, .after = dplyr::last_col())
  }
  
  return(zones_sf)
}
