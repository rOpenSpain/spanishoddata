# Filter a duckdb conenction by dates

IMPORTANT: This function assumes that the table or view that is being
filtered has separate `year`, `month` and `day` columns with integer
values. This is done so that the filtering is faster on CSV files that
are stored in a folder structure with hive-style
`/year=2020/month=2/day=14/`.

## Usage

``` r
spod_duckdb_filter_by_dates(con, source_view_name, new_view_name, dates)
```

## Arguments

- con:

  A duckdb connection

- source_view_name:

  The name of the source duckdb "view" (the virtual table, in the
  context of current package likely connected to a folder of CSV files)

- new_view_name:

  The name of the new duckdb "view" (the virtual table, in the context
  of current package likely connected to a folder of CSV files).

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

## Value

A `duckdb` connection with original views and a new filtered view.
