# Get daily trip counts per origin-destionation municipality from 2022 onward

**\[experimental\]**

**WARNING: this function may stop working at any time, as the API may
change**. This function provides a quick way to get daily aggregated (no
hourly data) trip counts per origin-destination municipality from v2
data (2022 onward). Compared to
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md),
which downloads large CSV files, this function downloads the data
directly from the GraphQL API. An interactive web map with this data is
available at <https://mapas-movilidad.transportes.gob.es/>. No data
aggregation is performed on your computer (unlike in
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)),
so you do not need to worry about memory usage and do not have to use a
powerful computer with multiple CPU cores just to get this simple data.
Only about 1 MB of data is downloaded for a single day. The limitation
of this function is that it can only retrieve data for a single day at a
time and only with total number of trips and total km travelled. So it
is not possible to get any of the extra variables available in the full
dataset via
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md).

For detailed data descriptions, see package vignettes using
[`spod_codebook(ver = 1)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and
[`spod_codebook(ver = 2)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and official methodology documents in **References** section.

## Usage

``` r
spod_quick_get_od(
  date = NA,
  min_trips = 100,
  distances = c("500m-2km", "2-10km", "10-50km", "50+km"),
  id_origin = NA,
  id_destination = NA
)
```

## Arguments

- date:

  A character or Date object specifying the date for which to retrieve
  the data. If date is a character, the date must be in "YYYY-MM-DD" or
  "YYYYMMDD" format.

- min_trips:

  A numeric value specifying the minimum number of journeys per
  origin-destination pair to retrieve. Defaults to 100 to reduce the
  amount of data returned. Can be set to 0 to retrieve all data.

- distances:

  A character vector specifying the distances to retrieve. Valid values
  are "500m-2km", "2-10km", "10-50km", and "50+km". Defaults to
  `c("500m-2km", "2-10km", "10-50km", "50+km")`. The resulting data will
  not have number of trips per category of distance. Therefore, if you
  want to retrieve the number of trips per distance category, you need
  to make 4 separate calls to this function or use
  [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
  instead to get the full data from source CSV files.

- id_origin:

  A character vector specifying the origin municipalities to retrieve.
  If not provided, all origin municipalities will be included. Valid
  municipality IDs can be found in the dataset returned by
  `spod_get_zones(zones = "muni", ver = 2)`.

- id_destination:

  A character vector specifying the target municipalities to retrieve.
  If not provided, all target municipalities will be included. Valid
  municipality IDs can be found in the dataset returned by
  `spod_get_zones(zones = "muni", ver = 2)`.

## Value

A `tibble` containing the flows for the specified date, minimum number
of journeys, distances and origin-destination pairs if specified. The
columns are:

- date:

  The date of the trips.

- id_origin:

  The origin municipality ID.

- id_destination:

  The target municipality ID.

- n_trips:

  The number of trips between the origin and target municipality.

- trips_total_length_km:

  The total length of trips in kilometers.

## References

- **For the official website of the mobility study**: Ministerio de
  Transportes y Movilidad Sostenible (MITMS) (2024). “Estudio de la
  movilidad con Big Data (Study of mobility with Big Data).” Data
  License:
  <https://www.transportes.gob.es/el-ministerio/buen-gobierno/licencia_datos>,
  <https://www.transportes.gob.es/ministerio/proyectos-singulares/estudio-de-movilidad-con-big-data>.

- **For v1 data methodology**: Ministerio de Transportes, Movilidad y
  Agenda Urbana (MITMA) (2021). *Análisis de la movilidad en España con
  tecnología Big Data durante el estado de alarma para la gestión de la
  crisis del COVID-19 (Analysis of mobility in Spain with Big Data
  technology during the state of alarm for COVID-19 crisis management)*.
  <https://cdn.mitma.gob.es/portal-web-drupal/covid-19/bigdata/mitma_-_estudio_movilidad_covid-19_informe_metodologico_v3.pdf>.

- **For v2 data methodology**: Ministerio de Transportes y Movilidad
  Sostenible (MITMS) (2024). *Estudio de movilidad de viajeros de ámbito
  nacional aplicando la tecnología Big Data. Informe metodológico (Study
  of National Traveler mobility Using Big Data Technology.
  Methodological Report)*.
  <https://www.transportes.gob.es/recursos_mfom/paginabasica/recursos/a3_informe_metodologico_estudio_movilidad_mitms_v8.pdf>.

- **For the `spanishoddata` R package**: Kotov E, Vidal-Tortosa E,
  Cantú-Ros OG, Burrieza-Galán J, Herranz R, Gullón Muñoz-Repiso T,
  Lovelace R (2026). “spanishoddata: A package for accessing and working
  with Spanish Open Mobility Big Data.” *Environment and Planning B:
  Urban Analytics and City Science*. ISSN 2399-8083,
  [doi:10.1177/23998083251415040](https://doi.org/10.1177/23998083251415040)
  .

Use
[`spod_cite()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_cite.md)
to cite the package and the data with correct plain text, markdown, or
BibTeX formats.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
od_1000 <- spod_quick_get_od(
  date = "2022-01-01",
  min_trips = 1000
)
# }
}
```
