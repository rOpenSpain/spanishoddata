# Translate zone names from English to Spanish

Translate zone names from English to Spanish

## Usage

``` r
spod_zone_names_en2es(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas")
)
```

## Arguments

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`) for both data versions. Additionaly, these can be
  `"large_urban_areas"` (or `"lua"`, or the original Spanish
  `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).

## Value

A `character` string with the translated zone name. Or `NULL` if the
zone name is not recognized.
