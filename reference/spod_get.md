# Get tabular mobility data

**\[stable\]**

This function creates a DuckDB lazy table connection object from the
specified type and zones. It checks for missing data and downloads it if
necessary. The connnection is made to the raw CSV files in gzip
archives, so analysing the data through this connection may be slow if
you select more than a few days. You can manipulate this object using
`dplyr` functions such as
[`select`](https://dplyr.tidyverse.org/reference/select.html),
[`filter`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html),
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html),
[`summarise`](https://dplyr.tidyverse.org/reference/summarise.html),
etc. In the end of any sequence of commands you will need to add
[`collect`](https://dplyr.tidyverse.org/reference/compute.html) to
execute the whole chain of data manipulations and load the results into
memory in an R `data.frame`/`tibble`. See codebooks for v1 and v2 data
in vignettes with
[`spod_codebook`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)`(1)`
and
[`spod_codebook`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)`(2)`.

If you want to analyse longer periods of time (especiially several
months or even the whole data over several years), consider using the
[`spod_convert`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
and then
[`spod_connect`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md).

If you want to quickly get the origin-destination data with flows
aggregated for a single day at municipal level and without any extra
socio-economic variables, consider using the
[`spod_quick_get_od`](https://rOpenSpain.github.io/spanishoddata/reference/spod_quick_get_od.md)
function.

For detailed data descriptions, see package vignettes using
[`spod_codebook(ver = 1)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and
[`spod_codebook(ver = 2)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and official methodology documents in **References** section.

## Usage

``` r
spod_get(
  type = c("od", "origin-destination", "os", "overnight_stays", "nt", "number_of_trips"),
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios", "lua", "large_urban_areas", "gau", "grandes_areas_urbanas"),
  dates = NULL,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  max_mem_gb = NULL,
  max_n_cpu = max(1, parallelly::availableCores() - 1),
  max_download_size_gb = 1,
  duckdb_target = ":memory:",
  temp_path = spod_get_temp_dir(),
  ignore_missing_dates = FALSE
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

  - For the `spod_get()` and
    [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
    functions, the `dates` can be set to "cached_v1" or "cached_v2" to
    request data from cached (already previously downloaded) v1
    (2020-2021) or v2 (2022 onwards) data. In this case, the function
    will identify and use all data files that have been downloaded and
    cached locally, (e.g. using an explicit run of
    [`spod_download()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_download.md),
    or any data requests made using the `spod_get()` or
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

- max_mem_gb:

  `integer` value of the maximum operating memory to use in GB. `NULL`
  by default, delegates the choice to the `DuckDB` engine which usually
  sets it to 80% of available memory. Caution, in HPC use, the amount of
  memory available to your job may be determined incorrectly by the
  `DuckDB` engine, so it is recommended to set this parameter explicitly
  according to your job's memory limits.

- max_n_cpu:

  The maximum number of threads to use. Defaults to the number of
  available cores minus 1.

- max_download_size_gb:

  The maximum download size in gigabytes. Defaults to 1.

- duckdb_target:

  (Optional) The path to the duckdb file to save the data to, if a
  convertation from CSV is reuqested by the `spod_convert` function. If
  not specified, it will be set to ":memory:" and the data will be
  stored in memory.

- temp_path:

  The path to the temp folder for DuckDB for [intermediate
  spilling](https://duckdb.org/2024/07/09/memory-management.html#intermediate-spilling)
  in case the set memory limit and/or physical memory of the computer is
  too low to perform the query. By default this is set to the `temp`
  directory in the data folder defined by SPANISH_OD_DATA_DIR
  environment variable (set by
  [`spod_set_data_dir()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md))).
  Otherwise, for queries on folders of CSV files or parquet files, the
  temporary path would be set to the current R working directory, which
  probably is undesirable, as the current working directory can be on a
  slow storage, or storage that may have limited space, compared to the
  data folder.

- ignore_missing_dates:

  Logical. If `TRUE`, the function will not raise an error if the some
  of the specified dates are missing. Any dates that are missing will be
  skipped, however the data for any valid dates will be acquired.
  Defaults to `FALSE`.

## Value

A DuckDB lazy table connection object of class `tbl_duckdb_connection`.

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

# create a connection to the v1 data
spod_set_data_dir(tempdir())
dates <- c("2020-02-14", "2020-03-14", "2021-02-14", "2021-02-14", "2021-02-15")
nt_dist <- spod_get(type = "number_of_trips", zones = "distr", dates = dates)

# nt_dist is a table view filtered to the specified dates

# for advanced users only
# access the source connection with all dates
# list tables
DBI::dbListTables(nt_dist$src$con)

# disconnect
spod_disconnect(nt_dist)
# }
}
```
