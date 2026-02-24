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

For detailed zone definitions and methodology, see Ministerio de
Transportes, Movilidad y Agenda Urbana (MITMA) (2021). *Análisis de la
movilidad en España con tecnología Big Data durante el estado de alarma
para la gestión de la crisis del COVID-19 (Analysis of mobility in Spain
with Big Data technology during the state of alarm for COVID-19 crisis
management)*.
<https://cdn.mitma.gob.es/portal-web-drupal/covid-19/bigdata/mitma_-_estudio_movilidad_covid-19_informe_metodologico_v3.pdf>.
for v1 data and Ministerio de Transportes y Movilidad Sostenible (MITMS)
(2024). *Estudio de movilidad de viajeros de ámbito nacional aplicando
la tecnología Big Data. Informe metodológico (Study of National Traveler
mobility Using Big Data Technology. Methodological Report)*.
<https://www.transportes.gob.es/recursos_mfom/paginabasica/recursos/a3_informe_metodologico_estudio_movilidad_mitms_v8.pdf>.
for v2 data.

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

## References

Ministerio de Transportes y Movilidad Sostenible (MITMS) (2024).
“Estudio de la movilidad con Big Data (Study of mobility with Big
Data).” Data License:
<https://www.transportes.gob.es/el-ministerio/buen-gobierno/licencia_datos>,
<https://www.transportes.gob.es/ministerio/proyectos-singulares/estudio-de-movilidad-con-big-data>.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
municipalities_sf <- spod_quick_get_zones()
# }
}
```
