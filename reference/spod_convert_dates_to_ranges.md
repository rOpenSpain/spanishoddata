# Convert dates to ranges

This internal helper function reduces a vector of dates to a vector of
date ranges to shorten the warning and error messages that mention the
valid date ranges.

## Usage

``` r
spod_convert_dates_to_ranges(dates)
```

## Arguments

- dates:

  A `character` vector of dates.

## Value

A `character` vector of date ranges.
