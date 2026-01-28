# Get the municipalities geometries

**\[experimental\]**

This function fetches the municipalities (for now this is the only
option) geometries from the mapas-movilidad website and returns a `sf`
object with the municipalities geometries. This is intended for use with
the flows data retrieved by the
[`spod_quick_get_od()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_quick_get_od.md)
function. An interactive web map with this data is available at
<https://mapas-movilidad.transportes.gob.es/>. These municipality
geometries only include Spanish municipalities (and not the NUTS3
regions in Portugal and France) and do not contain extra columns that
you can get with the
[`spod_get_zones()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_zones.md)
function. The function caches the retrieved geometries in memory of the
current R session to reduce the number of requests to the
mapas-movilidad website.

## Usage

``` r
spod_quick_get_zones(zones = "municipalities")
```

## Arguments

- zones:

  A character string specifying the zones to retrieve. Valid values are
  "municipalities", "muni", "municip", and "municipios". Defaults to
  "municipalities".

## Value

A `sf` object with the municipalities geometries to match with the data
retrieved with
[`spod_quick_get_od()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_quick_get_od.md).

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
municipalities_sf <- spod_quick_get_zones()
# }
}
```
