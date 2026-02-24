# Filter available data by version, zones, type and dates

Internal helper to filter available_data tibble based on
version-specific grepl patterns. Extracted for testability.

## Usage

``` r
spod_filter_available_files(
  available_data,
  ver,
  zones,
  matched_type,
  dates_to_use
)
```

## Arguments

- available_data:

  Tibble from spod_available_data

- ver:

  Integer. Data version (1 or 2)

- zones:

  Character. Zone name in Spanish (e.g., "distritos")

- matched_type:

  Character. Data type in Spanish (e.g., "viajes")

- dates_to_use:

  Date vector. Dates to filter by

## Value

Filtered tibble
