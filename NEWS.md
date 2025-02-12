# spanishoddata (development version)

## New features

* `spod_cite()` function to easily cite the package and the data (#134)

## Breaking changes

* `hour` column is superseeded by `time_slot` column in the output of `spod_get()` and `spod_convert()`. `time_slot` is deprecated. It is still present in the tables, but will be removed in the end of 2025 but going forward please use the new `hour` column. Otherwise it is exactly the same as before, this is just a name change. (#132)
* `date_ym` column in the output of `spod_available_data()` is now of type `character`, as it should be to be truly in `year-month` format. Previously it was of type `Date`, which meant all `year-month` data was actually `year-month-day`, with day set to 1.

## Other changes

* `spod_quick_get()` does not rely on metadata download anymore and can be used without setting the data directory with `spod_set_data_dir()` (and therefore does not cause a warning if the data directory is not set).

* `hour` (ex-`time_slot`) column is now right next to the date column in the output of `spod_get()` and `spod_convert()` (#)

* maximum available CPU cores check is now turned off to improve compatibility when running the package from within a container in high performance computing environments (see #130 and #140 for details)

* minor documentation improvements and updates

* minor bug fixes

# spanishoddata 0.1.0

* Initial CRAN submission.
