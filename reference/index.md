# Package index

## Analysing up to 1 week of data

Quickly download and analyse just a few days of mobility data

- [`spod_available_data()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_available_data.md)
  **\[stable\]** : Get available data list
- [`spod_get_zones()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_zones.md)
  **\[stable\]** : Get zones
- [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
  **\[stable\]** : Get tabular mobility data
- [`spod_disconnect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_disconnect.md)
  **\[stable\]** : Safely disconnect from data and free memory

## Analysing long time periods (months or even years)

Download data for longer periods, convert them into analysis ready
format such as `DuckDB` or `Parquet` for out-of-memory analysis of this
large data

- [`spod_available_data()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_available_data.md)
  **\[stable\]** : Get available data list

- [`spod_get_zones()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_zones.md)
  **\[stable\]** : Get zones

- [`spod_download()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_download.md)
  **\[stable\]** : Download the data files of specified type, zones, and
  dates

- [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
  **\[stable\]** : Convert data from plain text to duckdb or parquet
  format

- [`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md)
  **\[stable\]** :

  Connect to data converted to `DuckDB` or hive-style `parquet` files

- [`spod_disconnect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_disconnect.md)
  **\[stable\]** : Safely disconnect from data and free memory

## Analysing up to 1 day of trips with no extra variables

Quickly get a single day of flows between municipalities (without hourly
data or any other attributes) for 2022 and onwards

- [`spod_quick_get_od()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_quick_get_od.md)
  **\[experimental\]** : Get daily trip counts per origin-destionation
  municipality from 2022 onward
- [`spod_quick_get_zones()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_quick_get_zones.md)
  **\[experimental\]** : Get the municipalities geometries

## Helper functions

- [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
  **\[stable\]** : View codebooks for v1 and v2 open mobility data
- [`spod_available_data()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_available_data.md)
  **\[stable\]** : Get available data list
- [`spod_get_valid_dates()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_valid_dates.md)
  **\[stable\]** : Get valid dates for the specified data version
- [`spod_set_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md)
  **\[stable\]** : Set the data directory
- [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md)
  **\[stable\]** : Get the data directory
- [`spod_cite()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_cite.md)
  : Cite the package and the data
- [`spod_check_files()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_check_files.md)
  **\[experimental\]** : Check cached files consistency against
  checksums from S3
