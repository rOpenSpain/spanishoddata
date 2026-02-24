# Get zones

**\[stable\]**

Get spatial zones for the specified data version. Supports both v1
(2020-2021) and v2 (2022 onwards) data.

For detailed data descriptions, see package vignettes using
[`spod_codebook(ver = 1)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and
[`spod_codebook(ver = 2)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and official methodology documents in **References** section.

## Usage

``` r
spod_get_zones(
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  ver = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE
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

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md)
  which returns the value of the environment variable
  `SPANISH_OD_DATA_DIR` or a temporary directory if the variable is not
  set. To set the data directory, use
  [spod_set_data_dir](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md).

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

## Value

An `sf` object (Simple Feature collection).

The columns for v1 (2020-2021) data include:

- id:

  A character vector containing the unique identifier for each district,
  assigned by the data provider. This `id` matches the `id_origin`,
  `id_destination`, and `id` in district-level origin-destination and
  number of trips data.

- census_districts:

  A string with semicolon-separated identifiers of census districts
  classified by the Spanish Statistical Office (INE) that are spatially
  bound within the polygons for each `id`.

- municipalities_mitma:

  A string with semicolon-separated municipality identifiers (as
  assigned by the data provider) corresponding to each district `id`.

- municipalities:

  A string with semicolon-separated municipality identifiers classified
  by the Spanish Statistical Office (INE) corresponding to each `id`.

- district_names_in_v2/municipality_names_in_v2:

  A string with semicolon-separated district names (from the v2 version
  of this data) corresponding to each district `id` in v1.

- district_ids_in_v2/municipality_ids_in_v2:

  A string with semicolon-separated district identifiers (from the v2
  version of this data) corresponding to each district `id` in v1.

- geometry:

  A `MULTIPOLYGON` column containing the spatial geometry of each
  district, stored as an sf object. The geometry is projected in the
  ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY
  dimensions.

The columns for v2 (2022 onwards) data include:

- id:

  A character vector containing the unique identifier for each zone,
  assigned by the data provider.

- name:

  A character vector with the name of each district.

- population:

  A numeric vector representing the population of each district (as of
  2022).

- census_sections:

  A string with semicolon-separated identifiers of census sections
  corresponding to each district.

- census_districts:

  A string with semicolon-separated identifiers of census districts as
  classified by the Spanish Statistical Office (INE) corresponding to
  each district.

- municipalities:

  A string with semicolon-separated identifiers of municipalities
  classified by the Spanish Statistical Office (INE) corresponding to
  each district.

- municipalities_mitma:

  A string with semicolon-separated identifiers of municipalities, as
  assigned by the data provider, that correspond to each district.

- luas_mitma:

  A string with semicolon-separated identifiers of LUAs (Local Urban
  Areas) from the provider, associated with each district.

- district_ids_in_v1/municipality_ids_in_v1:

  A string with semicolon-separated district identifiers from v1 data
  corresponding to each district in v2. If no match exists, it is marked
  as `NA`.

- geometry:

  A `MULTIPOLYGON` column containing the spatial geometry of each
  district, stored as an sf object. The geometry is projected in the
  ETRS89 / UTM zone 30N coordinate reference system (CRS), with XY
  dimensions.

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
# get polygons for municipalities for the v2 data
municip_v2 <- spod_get_zones(zones = "municipalities", ver = 2)

# get polygons for the districts for the v1 data
distr_v1 <- spod_get_zones(zones = "districts", ver = 1)
# }
}
```
