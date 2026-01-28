# Check cached files consistency against checksums from S3

**\[experimental\]**

**WARNING: The checks may fail for May 2022 data and for some 2025 data,
as the remote cheksums that are used for checking the file consistency
are incorrect. We are working on solving this in future updates, for
now, kindly rely on the built-in file size checks of
[`spod_download`](https://rOpenSpain.github.io/spanishoddata/reference/spod_download.md),
[`spod_get`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md),
and
[`spod_convert`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md).**
This function checks downloaded data files whether they are consistent
with their checksums in Amazon S3 by computing ETag for each file. This
involves computing MD5 for each part of the file and concatenating them
and computing MD5 again on the resulting concatenated MD5s. This may
take very long time if you check all files, so use with caution.

## Usage

``` r
spod_check_files(
  type = c("od", "origin-destination", "os", "overnight_stays", "nt", "number_of_trips"),
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  ignore_missing_dates = FALSE,
  n_threads = 1
)
```

## Arguments

- type:

  The type of data to download. Can be `"origin-destination"` (or ust
  `"od"`), or `"number_of_trips"` (or just `"nt"`) for v1 data. For v2
  data `"overnight_stays"` (or just `"os"`) is also available. More data
  types to be supported in the future. See codebooks for v1 and v2 data
  in vignettes with `spod_codebook(1)` and `spod_codebook(2)`
  ([spod_codebook](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)).

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`) for both data versions. Additionaly, these can be
  `"large_urban_areas"` (or `"lua"`, or the original Spanish
  `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).

- dates:

  A `character` or `Date` vector of dates to process. Kindly keep in
  mind that v1 and v2 data follow different data collection
  methodologies and may not be directly comparable. Therefore, do not
  try to request data from both versions for the same date range. If you
  need to compare data from both versions, please refer to the
  respective codebooks and methodology documents. The v1 data covers the
  period from 2020-02-14 to 2021-05-09, and the v2 data covers the
  period from 2022-01-01 to the present until further notice. The true
  dates range is checked against the available data for each version on
  every function run.

  The possible values can be any of the following:

  - For the
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
    and
    [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
    functions, the `dates` can be set to "cached_v1" or "cached_v2" to
    request data from cached (already previously downloaded) v1
    (2020-2021) or v2 (2022 onwards) data. In this case, the function
    will identify and use all data files that have been downloaded and
    cached locally, (e.g. using an explicit run of
    [`spod_download()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_download.md),
    or any data requests made using the
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
    or
    [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
    functions).

  - A single date in ISO (YYYY-MM-DD) or YYYYMMDD format. `character` or
    `Date` object.

  - A vector of dates in ISO (YYYY-MM-DD) or YYYYMMDD format.
    `character` or `Date` object. Can be any non-consecutive sequence of
    dates.

  - A date range

    - eigher a `character` or `Date` object of length 2 with clearly
      named elements `start` and `end` in ISO (YYYY-MM-DD) or YYYYMMDD
      format. E.g. `c(start = "2020-02-15", end = "2020-02-17")`;

    - or a `character` object of the form `YYYY-MM-DD_YYYY-MM-DD` or
      `YYYYMMDD_YYYYMMDD`. For example, `2020-02-15_2020-02-17` or
      `20200215_20200217`.

  - A regular expression to match dates in the format `YYYYMMDD`.
    `character` object. For example, `^202002` will match all dates in
    February 2020.

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md)
  which returns the value of the environment variable
  `SPANISH_OD_DATA_DIR` or a temporary directory if the variable is not
  set. To set the data directory, use
  [spod_set_data_dir](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md).

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

- ignore_missing_dates:

  Logical. If `TRUE`, the function will not raise an error if the some
  of the specified dates are missing. Any dates that are missing will be
  skipped, however the data for any valid dates will be acquired.
  Defaults to `FALSE`.

- n_threads:

  Numeric. Number of threads to use for file verificaiton. Defaults
  to 1. When set to 2 or more threads, uses `future.mirai` as a backend
  for parallelization, resulting in significant (~4x) speedup, unless
  disk read speed is a bottleneck.

## Value

A tibble similar to the output of `spod_available_data`, but with an
extra column `local_file_consistent`, where `TRUE` indicates that the
file cheksum matches the expected checksums in Amazon S3. Note: some v1
(2020-2021) files were not stored correctly on S3 and their ETag
checksums are incorrectly reported by Amazon S3, so their true file
sizes and ETag checksums were cached inside the `spanishoddata` package.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
spod_set_data_dir(tempdir())
spod_download(
 type = "number_of_trips",
 zones = "distr",
 dates = "2020-03-14"
)

# now check the consistency
check_results <- spod_check_files(
  type = "number_of_trips",
  zones = "distr",
  dates = "2020-03-14"
)
all(check_results$local_file_consistent)
# }
}
```
