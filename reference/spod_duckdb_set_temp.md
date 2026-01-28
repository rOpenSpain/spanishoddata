# Set temp file for DuckDB connection

Set temp file for DuckDB connection

## Usage

``` r
spod_duckdb_set_temp(con, temp_path = spod_get_temp_dir())
```

## Arguments

- con:

  A duckdb connection

- temp_path:

  The path to the temp folder for DuckDB for [intermediate
  spilling](https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling)
  in case the set memory limit and/or physical memory of the computer is
  too low to perform the query. By default this is set to the `temp`
  directory in the data folder defined by SPANISH_OD_DATA_DIR
  environment variable (set by
  [`spod_set_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md))).
  Otherwise, for queries on folders of CSV files or parquet files, the
  temporary path would be set to the current R working directory, which
  probably is undesirable, as the current working directory can be on a
  slow storage, or storage that may have limited space, compared to the
  data folder.

## Value

A `duckdb` connection.
