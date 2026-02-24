# Convert data from plain text to duckdb or parquet format

**\[stable\]**

Converts data for faster analysis into either `DuckDB` file or into
`parquet` files in a hive-style directory structure. Running analysis on
these files is sometimes 100x times faster than working with raw CSV
files, espetially when these are in gzip archives. To connect to
converted data, please use 'mydata \<-
[spod_connect](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md)(data_path
= path_returned_by_spod_convert)' passing the path to where the data was
saved. The connected `mydata` can be analysed using `dplyr` functions
such as [select](https://dplyr.tidyverse.org/reference/select.html),
[filter](https://dplyr.tidyverse.org/reference/filter.html),
[mutate](https://dplyr.tidyverse.org/reference/mutate.html),
[group_by](https://dplyr.tidyverse.org/reference/group_by.html),
[summarise](https://dplyr.tidyverse.org/reference/summarise.html), etc.
In the end of any sequence of commands you will need to add
[collect](https://dplyr.tidyverse.org/reference/compute.html) to execute
the whole chain of data manipulations and load the results into memory
in an R `data.frame`/`tibble`. For more in-depth usage of such data,
please refer to DuckDB documentation and examples at
<https://duckdb.org/docs/api/r#dbplyr> . Some more useful examples can
be found here
<https://arrow-user2022.netlify.app/data-wrangling#combining-arrow-with-duckdb>
. You may also use `arrow` package to work with parquet files
<https://arrow.apache.org/docs/r/>.

For detailed data descriptions, see package vignettes using
[`spod_codebook(ver = 1)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and
[`spod_codebook(ver = 2)`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md)
and official methodology documents in **References** section.

## Usage

``` r
spod_convert(
  type = c("od", "origin-destination", "os", "overnight_stays", "nt", "number_of_trips"),
  zones = c("districts", "dist", "distr", "distritos", "municipalities", "muni",
    "municip", "municipios"),
  dates = NULL,
  save_format = "duckdb",
  save_path = NULL,
  overwrite = FALSE,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  max_mem_gb = NULL,
  max_n_cpu = max(1, parallelly::availableCores() - 1),
  max_download_size_gb = 1,
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

  - For the
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
    and `spod_convert()` functions, the `dates` can be set to
    "cached_v1" or "cached_v2" to request data from cached (already
    previously downloaded) v1 (2020-2021) or v2 (2022 onwards) data. In
    this case, the function will identify and use all data files that
    have been downloaded and cached locally, (e.g. using an explicit run
    of
    [`spod_download()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_download.md),
    or any data requests made using the
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
    or `spod_convert()` functions).

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

- save_format:

  A `character` vector of length 1 with values "duckdb" or "parquet".
  Defaults to "duckdb". If `NULL` automatically inferred from the
  `save_path` argument. If only `save_format` is provided, `save_path`
  will be set to the default location set in `SPANISH_OD_DATA_DIR`
  environment variable using
  [spod_set_data_dir](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md)`(path = 'path/to/your/cache/dir')`.
  So for v1 data that path would be
  `<data_dir>/clean_data/v1/tabular/duckdb/` or
  `<data_dir>/clean_data/v1/tabular/parquet/`.

  You can also set `save_path`. If it ends with ".duckdb", will save to
  `DuckDB` database format, if `save_path` does not end with ".duckdb",
  will save to `parquet` format and will treat the `save_path` as a path
  to a folder, not a file, will create necessary hive-style
  subdirectories in that folder. Hive style looks like
  `year=2020/month=2/day=14` and inside each such directory there will
  be a `data_0.parquet` file that contains the data for that day.

- save_path:

  A `character` vector of length 1. The full (not relative) path to a
  `DuckDB` database file or `parquet` folder.

  - If `save_path` ends with `.duckdb`, it will be saved as a DuckDB
    database file. The format argument will be automatically set to
    `save_format='duckdb'`.

  - If `save_path` ends with a folder name (e.g.
    `/data_dir/clean_data/v1/tabular/parquet/od_distr` for
    origin-destination data for district level), the data will be saved
    as a collection of `parquet` files in a hive-style directory
    structure. So the subfolders of `od_distr` will be
    `year=2020/month=2/day=14` and inside each of these folders a single
    `parquet` file will be placed containing the data for that day.

  - If `NULL`, uses the default location in `data_dir` (set by the
    `SPANISH_OD_DATA_DIR` environment variable using
    [spod_set_data_dir](https://rOpenSpain.github.io/spanishoddata/reference/spod_set_data_dir.md)`(path = 'path/to/your/cache/dir')`.
    Therefore, the default relative path for `DuckDB` is
    `<data_dir>/clean_data/v1/tabular/duckdb/<type>_<zones>.duckdb` and
    for `parquet` files is
    `<data_dir>/clean_data/v1/tabular/parquet/<type>_<zones>/`, where
    `type` is the type of data (e.g. 'od', 'os', 'nt', that correspoind
    to 'origin-destination', 'overnight-stays', 'number-of-trips', etc.)
    and `zones` is the name of the geographic zones (e.g. 'distr',
    'muni', etc.). See the details below in the function arguments
    description.

- overwrite:

  A `logical` or a `character` vector of length 1. If `TRUE`, overwrites
  existing `DuckDB` or `parquet` files. Defaults to `FALSE`. For parquet
  files can also be set to 'update', so that only parquet files are only
  created for the dates that have not yet been converted.

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

- ignore_missing_dates:

  Logical. If `TRUE`, the function will not raise an error if the some
  of the specified dates are missing. Any dates that are missing will be
  skipped, however the data for any valid dates will be acquired.
  Defaults to `FALSE`.

## Value

Path to saved `DuckDB` database file or to a folder with `parquet` files
in hive-style directory structure.

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

# download and convert data
dates_1 <- c(start = "2020-02-17", end = "2020-02-18")
db_2 <- spod_convert(
  type = "number_of_trips",
  zones = "distr",
  dates = dates_1,
  overwrite = TRUE
)

# now connect to the converted data
my_od_data_2 <- spod_connect(db_2)

# disconnect from the database
spod_disconnect(my_od_data_2)
# }
}
```
