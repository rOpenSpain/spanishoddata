# Set the data directory

**\[stable\]**

This function sets the data directory in the environment variable
SPANISH_OD_DATA_DIR, so that all other functions in the package can
access the data. It also creates the directory if it doesn't exist.

## Usage

``` r
spod_set_data_dir(data_dir, quiet = FALSE)
```

## Arguments

- data_dir:

  The data directory to set.

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

## Value

Nothing. If quiet is `FALSE`, prints a message with the path and
confirmation that the path exists.

## Examples

``` r
spod_set_data_dir(tempdir())
#> Data directory is writeable.
#> Data directory successfully set to: /tmp/RtmpfZEhU5
```
