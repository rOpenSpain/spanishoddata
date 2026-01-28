# Downloads and extracts the raw v1 zones data

This function ensures that the necessary v1 raw data for zones files are
downloaded and extracted from the specified data directory.

## Usage

``` r
spod_download_zones_v1(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios"),
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  metadata = NULL
)
```

## Arguments

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`).

- data_dir:

  The directory where the data is stored.

- quiet:

  Boolean flag to control the display of messages.

- metadata:

  Optional metadata table returned by
  [`spod_available_data()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_available_data.md).

## Value

A `character` string containing the path to the downloaded and extracted
file.
