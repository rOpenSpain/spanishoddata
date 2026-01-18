
# Script to generate minimal test fixtures from cached real data
# Run this after explore_data.R has downloaded the 2022-02-01 data

devtools::load_all()

# Setup paths
data_dir <- "~/home/nosync/cache/mitms/"
Sys.setenv(SPANISH_OD_DATA_DIR = data_dir)
fixture_dir <- "inst/testdata"
if (!dir.exists(fixture_dir)) dir.create(fixture_dir, recursive = TRUE)

# 1. Generate Zone Fixtures (v2)
# -----------------------------
message("Generating zone fixtures...")

# Helper to save zone sample
save_zone_sample <- function(type, filename) {
  zones_sf <- spod_get_zones(type, ver = 2)
  # Select just 2 small zones in Alava (01)
  # For districts and municipalities, IDs start with 01
  # For GAUs, let's just pick top 2
  if (type == "gau") {
    sample <- head(zones_sf, 2)
  } else {
    sample <- zones_sf |> dplyr::filter(grepl("^01", id)) |> head(2)
  }
  
  zone_dir <- file.path(fixture_dir, "clean_data/v2/zones")
  if (!dir.exists(zone_dir)) dir.create(zone_dir, recursive = TRUE)
  
  sf::write_sf(sample, file.path(zone_dir, filename), delete_dsn = TRUE)
  message("Saved zone fixture: ", file.path(zone_dir, filename))
}

save_zone_sample("dist", "distritos_mitma.gpkg")
save_zone_sample("muni", "municipios_mitma.gpkg")
save_zone_sample("gau", "gaus_mitma.gpkg")

target_ids <- c("01001", "01002")

# 2. Generate OD Data Fixtures (v2)
# ---------------------------------
message("Generating OD data fixtures...")
dates <- "2022-02-01"
od_v2 <- spod_get(type = "od", zones = "dist", dates = dates)

# Filter for flows specifically between these two districts (or involving them)
# To ensure non-empty results, we'll take anything originating from them AND destined to them
od_sample <- od_v2 |> 
  dplyr::filter(id_origin %in% target_ids & id_destination %in% target_ids) |> 
  dplyr::collect() |>
  head(100) # Capped at 100 rows for size

# Renaming to match raw data expectation (Spanish headers)
# Based on error message: columns = { 'fecha', 'periodo', 'origen', 'destino', 'distancia', 'actividad_origen', 'actividad_destino', 'estudio_origen_posible', 'estudio_destino_posible', 'residencia', 'renta', 'edad', 'sexo', 'viajes', 'viajes_km'}
od_sample_spanish <- od_sample |>
  dplyr::transmute(
    fecha = format(date, "%Y%m%d"),
    periodo = 0L,
    origen = as.character(id_origin),
    destino = as.character(id_destination),
    distancia = "2-10",
    actividad_origen = "casa",
    actividad_destino = "trabajo_estudio",
    estudio_origen_posible = "0",
    estudio_destino_posible = "0",
    residencia = "01",
    renta = "10-15",
    edad = "25-45",
    sexo = "hombre",
    viajes = n_trips,
    viajes_km = 10.5
  ) 
  
# Determine path structure that spod_get expects
# raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/viajes_distrito_YYYY-MM-DD.csv.gz

# 2.2 Define Mock Metadata with Hive Path
# ---------------------------------------
# Update path to match Hive partitioning required by SQL
hive_path <- "raw_data_cache/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1/data.csv.gz"

mock_meta <- tibble::tibble(
  data_ymd = as.Date("2022-02-01"),
  remote_file_size_mb = 0.001,
  local_file_size = 166,
  local_path = hive_path,
  target_url = "https://movilidad-opendata.mitma.es/estudios_basicos/por-distritos/viajes/ficheros-diarios/2022-02/20220201_data.csv.gz",
  file_size_bytes = 166,
  last_modified = Sys.time(),
  etag = "mock_etag",
  pub_ts = Sys.time(),
  downloaded = TRUE,
  complete_download = TRUE # Pretend it's already downloaded
)

# Save mock metadata
meta_dir <- file.path(fixture_dir, "metadata")
if (!dir.exists(meta_dir)) dir.create(meta_dir, recursive = TRUE)
saveRDS(mock_meta, file.path(meta_dir, "available_data_s3_v2_mock.rds"))
message("Saved metadata fixture: ", file.path(meta_dir, "available_data_s3_v2_mock.rds"))

# 3. Create Sample OD Data CSV (Gzipped) in Hive Structure
# --------------------------------------------------------
# Path matches the mock metadata
od_dir <- file.path(fixture_dir, "raw_data/v2/estudios_basicos/por-distritos/viajes/ficheros-diarios/year=2022/month=2/day=1")
dir.create(od_dir, recursive = TRUE)

# Using generated hive path
od_file <- file.path(od_dir, "data.csv.gz")
readr::write_delim(od_sample_spanish, od_file, delim = "|")
message("Saved OD fixture: ", od_file)

message("Fixture generation complete.")

message("Fixture generation complete.")
