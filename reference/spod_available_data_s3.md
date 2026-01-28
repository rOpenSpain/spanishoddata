# Get available data list from Amazon S3 storage

Get a table with links to available data files for the specified data
version from Amazon S3 storage.

## Usage

``` r
spod_available_data_s3(
  ver = c(1, 2),
  force = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir()
)
```

## Arguments

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

- force:

  Logical. If `TRUE`, force re-download of metadata. For Amazon S3 this
  queries the S3 bucket for the XML file it re-downloads. If `FALSE`,
  only update the available data list if it is older than 1 day.

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

## Value

A tibble with links, release dates of files in the data, dates of data
coverage, local paths to files, and the download status.
