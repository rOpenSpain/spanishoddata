# Get available data list

**\[stable\]**

Get a table with links to available data files for the specified data
version. Optionally check (see arguments) the file size and availability
of data files previously downloaded into the cache directory specified
with SPANISH_OD_DATA_DIR environment variable (set by
[`spod_set_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md))
or a custom path specified with `data_dir` argument. By default the data
is fetched from Amazon S3 bucket where the data is stored. If that
fails, the function falls back to downloading an XML file from the
Spanish Ministry of Transport website. You can also control this
behaviour with `use_s3` argument.

For detailed data descriptions, see package vignettes using
[`spod_codebook(ver = 1)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and
[`spod_codebook(ver = 2)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and official methodology documents in **References** section.

## Usage

``` r
spod_available_data(
  ver = 2,
  check_local_files = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir(),
  use_s3 = TRUE,
  force = FALSE
)
```

## Arguments

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

- check_local_files:

  Logical. Whether to check if the local files exist and get the file
  size. Defaults to `FALSE`.

- quiet:

  A `logical` value indicating whether to suppress messages. Default is
  `FALSE`.

- data_dir:

  The directory where the data is stored. Defaults to the value returned
  by
  [`spod_get_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_data_dir.md).

- use_s3:

  **\[experimental\]** Logical. If `TRUE`, use Amazon S3 to get
  available data list, which does not require downloading the XML file
  and caching it locally, which may be a bit faster. If `FALSE`, use the
  XML file to get available data list.

- force:

  Logical. If `TRUE`, force re-download of metadata. For Amazon S3 this
  queries the S3 bucket for the XML file it re-downloads. If `FALSE`,
  only update the available data list if it is older than 1 day.

## Value

A tibble with links, release dates of files in the data, dates of data
coverage, local paths to files, and the download status.

- target_url:

  `character`. The URL link to the data file.

- pub_ts:

  `POSIXct`. The timestamp of when the file was published.

- file_extension:

  `character`. The file extension of the data file (e.g., 'tar', 'gz').

- data_ym:

  `Date`. The year and month of the data coverage, if available.

- data_ymd:

  `Date`. The specific date of the data coverage, if available.

- study:

  `factor`. Study category derived from the URL (e.g., 'basic',
  'complete', 'routes').

- type:

  `factor`. Data type category derived from the URL (e.g.,
  'number_of_trips', 'origin-destination', 'overnight_stays',
  'data_quality', 'metadata').

- period:

  `factor`. Temporal granularity category derived from the URL (e.g.,
  'day', 'month').

- zones:

  `factor`. Geographic zone classification derived from the URL (e.g.,
  'districts', 'municipalities', 'large_urban_areas').

- local_path:

  `character`. The local file path where the data is (or going to be)
  stored.

- downloaded:

  `logical`. Indicator of whether the data file has been downloaded
  locally. This is only available if `check_local_files` is `TRUE`.

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

# Set data dir for file downloads
spod_set_data_dir(tempdir())

# Get available data list for v1 (2020-2021) data
spod_available_data(ver = 1)

# Get available data list for v2 (2022 onwards) data
spod_available_data(ver = 2)

# Get available data list for v2 (2022 onwards) data
# while also checking for local files that are already downloaded
spod_available_data(ver = 2, check_local_files = TRUE)
# }
}
```
