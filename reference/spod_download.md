# Download the data files of specified type, zones, and dates

**\[stable\]**

This function downloads the data files of the specified type, zones,
dates and data version.

For detailed data descriptions, see package vignettes using
[`spod_codebook(ver = 1)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and
[`spod_codebook(ver = 2)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and official methodology documents in **References** section.

## Usage

``` r
spod_download(
  type = c("od", "origin-destination", "os", "overnight_stays", "nt", "number_of_trips"),
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  dates = NULL,
  max_download_size_gb = 1,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  return_local_file_paths = FALSE,
  ignore_missing_dates = FALSE,
  check_local_files = TRUE
)
```

## Arguments

- type:

  The type of data to download. Can be `"origin-destination"` (or ust
  `"od"`), or `"number_of_trips"` (or just `"nt"`) for v1 data. For v2
  data `"overnight_stays"` (or just `"os"`) is also available. More data
  types to be supported in the future. See codebooks for v1 and v2 data
  in vignettes with `spod_codebook(1)` and `spod_codebook(2)`
  ([spod_codebook](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)).

- zones:

  The zones for which to download the data. Can be `"districts"` (or
  `"dist"`, `"distr"`, or the original Spanish `"distritos"`) or
  `"municipalities"` (or `"muni"`, `"municip"`, or the original Spanish
  `"municipios"`) for both data versions. Additionaly, these can be
  `"large_urban_areas"` (or `"lua"`, or the original Spanish
  `"grandes_areas_urbanas"`, or `"gau"`) for v2 data (2022 onwards).

- dates:

  A `character` or `Date` vector of dates to process. Kindly keep in
  mind that v1 and v2 data follow different data collection
  methodologies and may not be directly comparable. Therefore, do not
  try to request data from both versions for the same date range. If you
  need to compare data from both versions, please refer to the
  respective codebooks and methodology documents. The v1 data covers the
  period from 2020-02-14 to 2021-05-09, and the v2 data covers the
  period from 2022-01-01 to the present until further notice. The true
  dates range is checked against the available data for each version on
  every function run.

  The possible values can be any of the following:

  - For the
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
    and
    [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
    functions, the `dates` can be set to "cached_v1" or "cached_v2" to
    request data from cached (already previously downloaded) v1
    (2020-2021) or v2 (2022 onwards) data. In this case, the function
    will identify and use all data files that have been downloaded and
    cached locally, (e.g. using an explicit run of `spod_download()`, or
    any data requests made using the
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
    or
    [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
    functions).

  - A single date in ISO (YYYY-MM-DD) or YYYYMMDD format. `character` or
    `Date` object.

  - A vector of dates in ISO (YYYY-MM-DD) or YYYYMMDD format.
    `character` or `Date` object. Can be any non-consecutive sequence of
    dates.

  - A date range

    - eigher a `character` or `Date` object of length 2 with clearly
      named elements `start` and `end` in ISO (YYYY-MM-DD) or YYYYMMDD
      format. E.g. `c(start = "2020-02-15", end = "2020-02-17")`;

    - or a `character` object of the form `YYYY-MM-DD_YYYY-MM-DD` or
      `YYYYMMDD_YYYYMMDD`. For example, `2020-02-15_2020-02-17` or
      `20200215_20200217`.

  - A regular expression to match dates in the format `YYYYMMDD`.
    `character` object. For example, `^202002` will match all dates in
    February 2020.

- max_download_size_gb:

  The maximum download size in gigabytes. Defaults to 1.

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

- return_local_file_paths:

  Logical. If `TRUE`, the function returns a character vector of the
  paths to the downloaded files. If `FALSE`, the function returns
  `NULL`.

- ignore_missing_dates:

  Logical. If `TRUE`, the function will not raise an error if the some
  of the specified dates are missing. Any dates that are missing will be
  skipped, however the data for any valid dates will be acquired.
  Defaults to `FALSE`.

- check_local_files:

  Logical. Whether to check the file size of local files against known
  remote file sizes on the Amazon S3 storage. Defaults to `TRUE`, which
  fetches the metadata from Amazon S3. This setting ensures your
  downloaded files are not broken, so it is recommended to keep it
  `TRUE`.

## Value

Nothing. If `return_local_file_paths = TRUE`, a `character` vector of
the paths to the downloaded files.

## Details

Download the data files of specified type, zones, and dates

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

# Download the number of trips on district level for the a date range in March 2020
spod_download(
  type = "number_of_trips", zones = "districts",
  dates = c(start = "2020-03-20", end = "2020-03-21")
)

# Download the number of trips on district level for select dates in 2020 and 2021
spod_download(
  type = "number_of_trips", zones = "dist",
  dates = c("2020-03-20", "2020-03-24", "2021-03-20", "2021-03-24")
)

# Download the number of trips on municipality level using regex for a date range in March 2020
# (the regex will capture the dates 2020-03-20 to 2020-03-24)
spod_download(
  type = "number_of_trips", zones = "municip",
  dates = "2020032[0-4]"
)
# }
}
```
