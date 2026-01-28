# Fixes common issues in the zones data and cleans up variable names

This function fixes any invalid geometries in the zones data and renames
the "ID" column to "id". It also attacches the population counts and
zone names provided in the csv files supplied by the original data
provider.

## Usage

``` r
spod_clean_zones_v2(zones_path)
```

## Arguments

- zones_path:

  The path to the zones spatial data file.

## Value

A spatial \`sf“ object containing the cleaned zones data.
