# Load an SQL query, glue it, dplyr::sql it

Load an SQL query from a specified file in package installation
directory, glue::collapse it, glue::glue it in case of any variables
that need to be replaced, and dplyr::sql it for additional safety.

## Usage

``` r
spod_read_sql(sql_file_name)
```

## Arguments

- sql_file_name:

  The name of the SQL file to load from the package installation
  directory.

## Value

Text of the SQL query of class `sql`/`character`.
