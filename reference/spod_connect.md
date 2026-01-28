# Connect to data converted to `DuckDB` or hive-style `parquet` files

**\[stable\]**

This function allows the user to quickly connect to the data converted
to DuckDB with the
[spod_convert](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
function. This function simplifies the connection process. The user is
free to use the `DBI` and `DuckDB` packages to connect to the data
manually, or to use the `arrow` package to connect to the `parquet`
files folder.

## Usage

``` r
spod_connect(
  data_path,
  target_table_name = NULL,
  quiet = FALSE,
  max_mem_gb = NULL,
  max_n_cpu = max(1, parallelly::availableCores() - 1),
  temp_path = spod_get_temp_dir()
)
```

## Arguments

- data_path:

  a path to the `DuckDB` database file with '.duckdb' extension, or a
  path to the folder with `parquet` files. Eigher one should have been
  created with the
  [spod_convert](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
  function.

- target_table_name:

  Default is `NULL`. When connecting to a folder of `parquet` files,
  this argument is ignored. When connecting to a `DuckDB` database, a
  `character` vector of length 1 with the table name to open from the
  database file. If not specified, it will be guessed from the
  `data_path` argument and from table names that are available in the
  database. If you have not manually interfered with the database, this
  should be guessed automatically and you do not need to specify it.

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

- max_mem_gb:

  `integer` value of the maximum operating memory to use in GB. `NULL`
  by default, delegates the choice to the `DuckDB` engine which usually
  sets it to 80% of available memory. Caution, in HPC use, the amount of
  memory available to your job may be determined incorrectly by the
  `DuckDB` engine, so it is recommended to set this parameter explicitly
  according to your job's memory limits.

- max_n_cpu:

  The maximum number of threads to use. Defaults to the number of
  available cores minus 1.

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

a `DuckDB` table connection object.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
# Set data dir for file downloads
spod_set_data_dir(tempdir())

# download and convert data
dates_1 <- c(start = "2020-02-17", end = "2020-02-18")
db_2 <- spod_convert(
 type = "number_of_trips",
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
