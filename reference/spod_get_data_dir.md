# Get the data directory

**\[stable\]**

This function retrieves the data directory from the environment variable
SPANISH_OD_DATA_DIR (and previously set by
[`spod_set_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md)).
If the environment variable is not set, it returns the temporary
directory.

## Usage

``` r
spod_get_data_dir(quiet = FALSE)
```

## Arguments

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

## Value

A `character` vector of length 1 containing the path to the data
directory where the package will download and convert the data.

## Examples

``` r
spod_set_data_dir(tempdir())
#> Data directory is writeable.
#> Data directory successfully set to: /tmp/Rtmp4FfTDz
spod_get_data_dir()
#> /tmp/Rtmp4FfTDz
```
