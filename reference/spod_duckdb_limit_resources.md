# Set maximum memory and number of threads for a `DuckDB` connection

Set maximum memory and number of threads for a `DuckDB` connection

## Usage

``` r
spod_duckdb_limit_resources(
  con,
  max_mem_gb = NULL,
  max_n_cpu = max(1, parallelly::availableCores() - 1)
)
```

## Arguments

- con:

  A `duckdb` connection

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

## Value

A `duckdb` connection.
