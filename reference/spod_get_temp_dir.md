# Get temporary directory for DuckDB intermediate spilling

Get the The path to the temp folder for DuckDB for [intermediate
spilling](https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling)
in case the set memory limit and/or physical memory of the computer is
too low to perform the query.

## Usage

``` r
spod_get_temp_dir(data_dir = spod_get_data_dir())
```

## Arguments

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

## Value

A `character` string with the path to the temp folder for `DuckDB` for
[intermediate
spilling](https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling).
