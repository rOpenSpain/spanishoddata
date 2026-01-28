# Get the available v1 data list

This function provides a table of the available data list of MITMA v1
(2020-2021), both remote and local.

## Usage

``` r
spod_available_data_v1(
  data_dir = spod_get_data_dir(),
  check_local_files = FALSE,
  use_s3 = TRUE,
  force = FALSE,
  quiet = FALSE
)
```

## Arguments

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

- check_local_files:

  Logical. Whether to check if the local files exist and get the file
  size. Defaults to `FALSE`.

- use_s3:

  **\[experimental\]** Logical. If `TRUE`, use Amazon S3 to get
  available data list, which does not require downloading the XML file
  and caching it locally, which may be a bit faster. If `FALSE`, use the
  XML file to get available data list.

- force:

  Logical. If `TRUE`, force re-download of metadata. For Amazon S3 this
  queries the S3 bucket for the XML file it re-downloads. If `FALSE`,
  only update the available data list if it is older than 1 day.

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

## Value

A tibble with links, release dates of files in the data, dates of data
coverage, local paths to files, and the download status.

- target_url:

  `character`. The URL link to the data file.

- pub_ts:

  `POSIXct`. The timestamp of when the file was published.

- file_extension:

  `character`. The file extension of the data file (e.g., 'tar', 'gz').

- data_ym:

  `Date`. The year and month of the data coverage, if available.

- data_ymd:

  `Date`. The specific date of the data coverage, if available.

- study:

  `factor`. Study category derived from the URL (e.g., 'basic',
  'complete', 'routes').

- type:

  `factor`. Data type category derived from the URL (e.g.,
  'number_of_trips', 'origin-destination', 'overnight_stays',
  'data_quality', 'metadata').

- period:

  `factor`. Temporal granularity category derived from the URL (e.g.,
  'day', 'month').

- zones:

  `factor`. Geographic zone classification derived from the URL (e.g.,
  'districts', 'municipalities', 'large_urban_areas').

- local_path:

  `character`. The local file path where the data is (or going to be)
  stored.

- downloaded:

  `logical`. Indicator of whether the data file has been downloaded
  locally. This is only available if `check_local_files` is `TRUE`.
