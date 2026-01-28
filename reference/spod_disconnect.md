# Safely disconnect from data and free memory

**\[stable\]**

This function is to ensure that `DuckDB` connections to CSV.gz files
(created via
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)),
as well as to `DuckDB` files or folders of `parquet` files (created via
[`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md))
are closed properly to prevent conflicting connections. Essentially this
is just a wrapper around
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
that reaches out into the `.$src$con` object of the
`tbl_duckdb_connection` connection object that is returned to the user
via
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
and
[`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md).
After disonnecting the database, it also frees up memory by running
[`gc()`](https://rdrr.io/r/base/gc.html).

## Usage

``` r
spod_disconnect(tbl_con, free_mem = TRUE)
```

## Arguments

- tbl_con:

  A `tbl_duckdb_connection` connection object that you get from either
  [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
  or
  [`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md).

- free_mem:

  A `logical`. Whether to free up memory by running
  [`gc()`](https://rdrr.io/r/base/gc.html). Defaults to `TRUE`.

## Value

No return value, called for side effect of disconnecting from the
database and freeing up memory.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
# Set data dir for file downloads
spod_set_data_dir(tempdir())

# basic example
# create a connection to the v1 data without converting
# this creates a duckdb database connection to CSV files
od_distr <- spod_get(
 "od",
 zones = "distr",
 dates = c("2020-03-01", "2020-03-02")
)
# disconnect from the database connection
spod_disconnect(od_distr)

# Advanced example
# download and convert data
dates_1 <- c(start = "2020-02-17", end = "2020-02-19")
db_2 <- spod_convert(
 type = "od",
 zones = "distr",
 dates = dates_1,
 overwrite = TRUE
)

# now connect to the converted data
my_od_data_2 <- spod_connect(db_2)

# disconnect from the database
spod_disconnect(my_od_data_2)
# }
}
```
