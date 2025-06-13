# spanishoddata (development version)

## New features

* `spod_quick_get_zones()` is a new function to quickly get municipality geometries to match with the data retrieved with `spod_quick_get_od()` (PR [#163](https://github.com/rOpenSpain/spanishoddata/pull/163)). Requests to get geometies are cached in memory of the current R session with `memoise` package. This function is experimental, just as the `spod_quick_get_od()` function, as the API of the Spanish Ministry of Transport may change in the future. It is only intended for quick analysis in educational or other demonstration purposes, as it downloads very little data compared to the regular `spod_get_od()`, `spod_download()` and `spod_convert()` functions.

* Experimental `spod_check_files()` function allows to check consistency of downloaded files with Amazon S3 checksums (PR [#165](https://github.com/rOpenSpain/spanishoddata/pull/165)). ETags for v1 data are stored with the package, and for v2 data they are fetched from Amazon S3. The checks may fail for May 2022 data and for some 2025 data, as the remote cheksums that are used for checking the file consistency are incorrect. We are working on solving this in future updates, for now, kindly rely on the built-in file size checks of `spod_download()`, `spod_get()`, and `spod_convert()`.

## Improvements

* Metadata is now fetched from Amazon S3 storage of the original data files, which allows validation of downloaded files (issue [#126](https://github.com/rOpenSpain/spanishoddata/issues/126)) with both size and checksum. PR [#165](https://github.com/rOpenSpain/spanishoddata/pull/165).

## Bug fixes

* More reliable, but still multi-threaded data file downloads using base R `utils::download.file()` instead of `curl::multi_download()` which failed on some connections (issue [#127](https://github.com/rOpenSpain/spanishoddata/issues/127)), so now `curl` dependency is no longer required. PR [#165](https://github.com/rOpenSpain/spanishoddata/pull/165).

* `spod_quick_get_od()` is working again. We fixed it to work with the updated API of the Spanish Ministry of Transport (PR [#163](https://github.com/rOpenSpain/spanishoddata/pull/163), issue [#162](https://github.com/rOpenSpain/spanishoddata/issues/162)). This function will remain experimental, just as the `spod_quick_get_zones()` function, as the API of the Spanish Ministry of Transport may change in the future. It is only intended for quick analysis in educational or other demonstration purposes, as it downloads very little data compared to the regular `spod_get_od()`, `spod_download()` and `spod_convert()` functions.

* `spod_convert()` now accepts `overwrite = 'update'` with `save_format = 'parquet'` ([#161](https://github.com/rOpenSpain/spanishoddata/pull/161)) previously it failed because of the incorrect check that asserted only `TRUE` or `FALSE` ([#160](https://github.com/rOpenSpain/spanishoddata/issues/160))

# spanishoddata 0.1.1

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

# spanishoddata 0.1.0

* Initial CRAN submission.
