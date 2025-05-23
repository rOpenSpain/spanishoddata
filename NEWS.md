# spanishoddata (development version)

## New features

* `spod_quick_get_zones()` is a new function to quickly get municipality geometries to match with the data retrieved with `spod_quick_get_od()` [#163](https://github.com/rOpenSpain/spanishoddata/pull/163). Requests to get geometies are cached in memory of the current R session with `memoise` package.

## Bug fixes

* `spod_quick_get_od()` is working again. We fixed it to work with the updated API of the Spanish Ministry of Transport (PR [#163](https://github.com/rOpenSpain/spanishoddata/pull/163), issue [#162](https://github.com/rOpenSpain/spanishoddata/issues/162)). It will remain experimental, as the API may change in the future.

* `spod_convert()` can now accept `overwrite = 'update'` with `save_format = 'parquet'` ([#161](https://github.com/rOpenSpain/spanishoddata/pull/161)) previously it failed because of the incorrect check that asserted only `TRUE` or `FALSE` ([#160](https://github.com/rOpenSpain/spanishoddata/issues/160))

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
