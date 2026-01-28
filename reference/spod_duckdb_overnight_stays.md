# Create a duckdb overnight stays table

This function creates a duckdb connection to the overnight stays data
stored in a folder of CSV.gz files.

## Usage

``` r
spod_duckdb_overnight_stays(
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
