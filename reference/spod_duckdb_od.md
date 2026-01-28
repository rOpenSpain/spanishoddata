# Creates a duckdb connection to origin-destination data

This function creates a duckdb connection to the origin-destination data
stored in CSV.gz files.

## Usage

``` r
spod_duckdb_od(
  con = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE),
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  ver = NULL,
  data_dir = spod_get_data_dir()
)
```

## Arguments

- con:

  A duckdb connection object. If not specified, a new in-memory
  connection will be created.

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`) for both data versions. Additionaly, these can be
  `"large_urban_areas"` (or `"lua"`, or the original Spanish
  `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

## Value

A `duckdb` connection object with 2 views:

- `od_csv_raw` - a raw table view of all cached CSV files with the
  origin-destination data that has been previously cached in
  \$SPANISH_OD_DATA_DIR

- `od_csv_clean` - a cleaned-up table view of `od_csv_raw` with column
  names and values translated and mapped to English. This still includes
  all cached data.

The structure of the cleaned-up views `od_csv_clean` is as follows:

- date:

  `Date`. The full date of the trip, including year, month, and day.

- id_origin:

  `factor`. The identifier for the origin location of the trip,
  formatted as a code (e.g., '01001_AM').

- id_destination:

  `factor`. The identifier for the destination location of the trip,
  formatted as a code (e.g., '01001_AM').

- activity_origin:

  `factor`. The type of activity at the origin location (e.g., 'home',
  'work'). **Note:** Only available for district level data.

- activity_destination:

  `factor`. The type of activity at the destination location (e.g.,
  'home', 'other'). **Note:** Only available for district level data.

- residence_province_ine_code:

  `factor`. The province of residence for the group of individual making
  the trip, encoded according to the INE classification. **Note:** Only
  available for district level data.

- residence_province_name:

  `factor`. The province of residence for the group of individuals
  making the trip (e.g., 'Cuenca', 'Girona'). **Note:** Only available
  for district level data.

- hour:

  `integer`. The time slot (the hour of the day) during which the trip
  started, represented as an integer (e.g., 0, 1, 2).

- distance:

  `factor`. The distance category of the trip, represented as a code
  (e.g., '002-005' for 2-5 km).

- n_trips:

  `double`. The number of trips taken within the specified time slot and
  distance.

- trips_total_length_km:

  `double`. The total length of all trips in kilometers for the
  specified time slot and distance.

- year:

  `double`. The year of the trip.

- month:

  `double`. The month of the trip.

- day:

  `double`. The day of the trip.

The structure of the original data in `od_csv_raw` is as follows:

- fecha:

  `Date`. The date of the trip, including year, month, and day.

- origen:

  `character`. The identifier for the origin location of the trip,
  formatted as a character string (e.g., '01001_AM').

- destino:

  `character`. The identifier for the destination location of the trip,
  formatted as a character string (e.g., '01001_AM').

- actividad_origen:

  `character`. The type of activity at the origin location (e.g.,
  'casa', 'trabajo').

- actividad_destino:

  `character`. The type of activity at the destination location (e.g.,
  'otros', 'trabajo').

- residencia:

  `character`. The code representing the residence of the individual
  making the trip (e.g., '01') according to the official INE
  classification.

- edad:

  `character`. The age of the individual making the trip. This data is
  actaully filled with 'NA' values, which is why this column is removed
  in the cleaned-up and translated view described above.

- periodo:

  `integer`. The time period during which the trip started, represented
  as an integer (e.g., 0, 1, 2).

- distancia:

  `character`. The distance category of the trip, represented as a
  character string (e.g., '002-005' for 2-5 km).

- viajes:

  `double`. The number of trips taken within the specified time period
  and distance.

- viajes_km:

  `double`. The total length of all trips in kilometers for the
  specified time period and distance.

- day:

  `double`. The day of the trip.

- month:

  `double`. The month of the trip.

- year:

  `double`. The year of the trip.
