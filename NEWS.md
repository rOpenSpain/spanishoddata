# spanishoddata (development version)

# spanishoddata 0.2.2

## Breaking changes

* `time_slot` column has been removed from the output of `spod_get()` and `spod_convert()`. Please use the `hour` column instead. Deprecation message removed.

## Bug fixes

* Fixed failing fallback on XML when Amazon S3 is unavailable.

## Other changes

* `spanishoddata` paper is out: **Kotov, E.**, Vidal-Tortosa, E., Cantú-Ros, O. G., Burrieza-Galán, J., Herranz, R., Gullón Muñoz-Repiso, T., & Lovelace, R. (2026). spanishoddata: A package for accessing and working with Spanish Open Mobility Big Data. *Environment and Planning B: Urban Analytics and City Science*. [DOI:10.1177/23998083251415040](https://doi.org/10.1177/23998083251415040){target='_blank'}

* Package now has comprehensive unit tests coverage.

* Tests on live data run weekly on GitHub workers to alert maintainers of any issues.

* `spanishoddata` package and paper are now featured in [DuckDB Library](https://duckdb.org/library/spanishoddata-r-package/)

# spanishoddata 0.2.1 (2025-07-29)

## Bug fixes

* Fixed incorrect reaggregation of district level data into municipal level data for v1 (2020-2021) data (PR [#170](https://github.com/rOpenSpain/spanishoddata/pull/170))

## Improvements

* Minor documentation improvements and updates to direct users to the `spod_set_data_dir()` function to set the data directory instead of advising to set the environment variable manually.

# spanishoddata 0.2.0 (2025-06-15)

## New features

* `spod_quick_get_zones()` is a new function to quickly get municipality geometries to match with the data retrieved with `spod_quick_get_od()` (PR [#163](https://github.com/rOpenSpain/spanishoddata/pull/163)). This function is experimental, just as the `spod_quick_get_od()` function, as the API of the Spanish Ministry of Transport may change in the future. It is only intended for quick analysis in educational or other demonstration purposes, as it downloads very little data compared to the regular `spod_get_od()`, `spod_download()` and `spod_convert()` functions. The requests are cached in memory of the current R session with `memoise` package, so repeated calls to `spod_quick_get_zones()` will not cause repeated requests to the API and will allow the user to get the data faster from repeat calls.

* Experimental `spod_check_files()` function allows to check consistency of downloaded files with Amazon S3 checksums (PR [#165](https://github.com/rOpenSpain/spanishoddata/pull/165)). ETags for v1 data are stored with the package, and for v2 data they are fetched from Amazon S3. The checks may fail for May 2022 data and for some 2025 data, as the remote cheksums that are used for checking the file consistency are incorrect. We are working on solving this in future updates, for now, kindly rely on the built-in file size checks of `spod_download()`, `spod_get()`, and `spod_convert()`.

## Improvements

* `spod_get()` and `spod_convert()` are now up to x100,000 faster when you have all (or a lot of) data downloaded, but only requesting several days in the call to `spod_get()` or `spod_convert()`. This is thanks to a new smarter filtering strategy (issue [#159](https://github.com/rOpenSpain/spanishoddata/issues/159), PR [#166](https://github.com/rOpenSpain/spanishoddata/pull/166)).

* Metadata is now fetched from Amazon S3 storage of the original data files, which allows validation of downloaded files (issue [#126](https://github.com/rOpenSpain/spanishoddata/issues/126)) with both size and checksum. PR [#165](https://github.com/rOpenSpain/spanishoddata/pull/165).

* Metadata fetched by `spod_available_data()` has extra columns such as data `type`, `zones` and `period`, see help `?spod_available_data()` for details.

* Memory allocation is now delegated to `DuckDB` engine, which by default uses 80% of available RAM. Beware that in some HPC environments this may detect more memory than is actually available to your job, so set the limit manually to 80% of RAM available to your job with `max_mem_gb` argument of `spod_get()`, `spod_convert()`, `spod_connect()` functions. This will also improve performance in some cases, as DuckDB is more efficient than R at memory allocation (PR [#167](https://github.com/rOpenSpain/spanishoddata/pull/167)).

## Bug fixes

* More reliable, but still multi-threaded data file downloads using base R `utils::download.file()` instead of `curl::multi_download()` which failed on some connections (issue [#127](https://github.com/rOpenSpain/spanishoddata/issues/127)), so now `curl` dependency is no longer required. PR [#165](https://github.com/rOpenSpain/spanishoddata/pull/165).

* `spod_quick_get_od()` is working again. We fixed it to work with the updated API of the Spanish Ministry of Transport (PR [#163](https://github.com/rOpenSpain/spanishoddata/pull/163), issue [#162](https://github.com/rOpenSpain/spanishoddata/issues/162)). This function will remain experimental, just as the `spod_quick_get_zones()` function, as the API of the Spanish Ministry of Transport may change in the future. It is only intended for quick analysis in educational or other demonstration purposes, as it downloads very little data compared to the regular `spod_get_od()`, `spod_download()` and `spod_convert()` functions. The requests are cached in memory of the current R session with `memoise` package, so repeated calls to `spod_quick_get_od()` will not cause repeated requests to the API and will allow the user to get the data faster from repeat calls.

* `spod_convert()` now accepts `overwrite = 'update'` with `save_format = 'parquet'` ([#161](https://github.com/rOpenSpain/spanishoddata/pull/161)) previously it failed because of the incorrect check that asserted only `TRUE` or `FALSE` ([#160](https://github.com/rOpenSpain/spanishoddata/issues/160))

# spanishoddata 0.1.1 (2025-04-09)

## New features

* `spod_cite()` function to easily cite the package and the data ([#134](https://github.com/rOpenSpain/spanishoddata/pull/134))

## Breaking changes

* `hour` column is superseeded by `time_slot` column in the output of `spod_get()` and `spod_convert()`. `time_slot` is deprecated. It is still present in the tables, but will be removed in the end of 2025 but going forward please use the new `hour` column. Otherwise it is exactly the same as before, this is just a name change. (#132)

## Other changes

* `spod_quick_get()` does not rely on metadata download anymore and can be used without setting the data directory with `spod_set_data_dir()` (and therefore does not cause a warning if the data directory is not set).

* `hour` (ex-`time_slot`) column is now right next to the date column in the output of `spod_get()` and `spod_convert()` (#)

* maximum available CPU cores check is now turned off to improve compatibility when running the package from within a container in high performance computing environments (see [#130](https://github.com/rOpenSpain/spanishoddata/issues/130) and [#140](https://github.com/rOpenSpain/spanishoddata/pull/140) for details)

* minor documentation improvements and updates

* minor bug fixes

# spanishoddata 0.1.0 (2024-12-18)

* Initial CRAN submission.
