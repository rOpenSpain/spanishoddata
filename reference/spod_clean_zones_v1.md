# Fixes common issues in the zones data and cleans up variable names

This function fixes any invalid geometries in the zones data and renames
the "ID" column to "id".

## Usage

``` r
spod_clean_zones_v1(zones_path, zones, quiet = FALSE)
```

## Arguments

- zones_path:

  The path to the zones spatial data file.

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`) for both data versions. Additionaly, these can be
  `"large_urban_areas"` (or `"lua"`, or the original Spanish
  `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

## Value

A spatial object containing the cleaned zones data.
