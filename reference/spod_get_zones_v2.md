# Retrieves the zones v2 data

This function retrieves the zones data from the specified data
directory. It can retrieve either "distritos" or "municipios" zones
data.

## Usage

``` r
spod_get_zones_v2(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  data_dir = spod_get_data_dir(),
  quiet = FALSE
)
```

## Arguments

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`).

- data_dir:

  The directory where the data is stored.

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

## Value

An `sf` object (Simple Feature collection) with 4 fields:

- id:

  A character vector containing the unique identifier for each zone, to
  be matched with identifiers in the tabular data.

- name:

  A character vector with the name of the zone.

- population:

  A numeric vector representing the population of each zone (as of
  2022).

- geometry:

  A `MULTIPOLYGON` column containing the spatial geometry of each zone,
  stored as an sf object. The geometry is projected in the ETRS89 / UTM
  zone 30N coordinate reference system (CRS), with XY dimensions.
