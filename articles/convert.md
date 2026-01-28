# Download and convert mobility datasets

## 1 Introduction

**TL;DR (too long, didn’t read): For analysing more than 1 week of data,
use
[`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
to convert the data into `DuckDB` and
[`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md)
to connect to it for analysis using
[dplyr](https://dplyr.tidyverse.org). Skip to the [section about
it](#duckdb).**

The main focus of this vignette is to show how to get long periods of
origin-destination data for analysis. First, we describe and compare the
two ways to get the mobility data using origin-destination data as an
example. The package functions and overall approaches are the same for
working with other types of data available through the package, such as
the number of trips, overnight stays and any other data. Then we show
how to get a few days of origin-destination data with
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md).
Finally, we show how to download and convert multiple weeks, months or
even years of origin-destination data into analysis-ready formats. See
description of datasets in the [Codebook and cookbook for v1 (2020-2021)
Spanish mobility
data](https://rOpenSpain.github.io/spanishoddata/articles/v1-2020-2021-mitma-data-codebook.md)
and in the [Codebook and cookbook for v2 (2022 onwards) Spanish mobility
data](https://rOpenSpain.github.io/spanishoddata/articles/v2-2022-onwards-mitma-data-codebook.md).

## 2 Two ways to get the data

There are two main ways to import the datasets:

1.  as an in-memory object with
    [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md);

2.  as a connection to DuckDB or Parquet files on disk with
    [`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md) +
    [`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md).
    The latter is recommended for large datasets (more than 1 week), as
    it is much faster and more memory efficient, as we demonstarte
    below.

[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
returns objects that are only appropriate for small datasets
representing a few days of the national origin-destination flows. We
recommend converting the data into analysis-ready formats (`DuckDB` or
`Parquet`) using
[`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md) +
[`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md).
This will allow you to work with much longer time periods (months and
years) on a consumer laptop (with 8-16 GB of memory). See the section
below for more details.

## 3 Analysing large datasets

The mobility datasets available through `{spanishiddata}` are very
large. Particularly the origin-destination data, which contains millions
of rows. These data sets may not fit into the memory of your computer,
especially if you plan to run the analysis over multiple days, weeks,
months, or even years.

To work with these datasets, we highly recommend using `DuckDB` and
`Parquet`. These are systems for efficiently processing
larger-than-memory datasets, while being user-firendly by presenting the
data in a familiar `data.frame`/`tibble` object (almost). For a great
intoroduction to both, we recommend materials by Danielle Navarro,
Jonathan Keane, and Stephanie Hazlitt:
[website](https://arrow-user2022.netlify.app/),
[slides](https://arrow-user2022.netlify.app/slides), and [the video
tutorial](https://www.youtube.com/watch?v=YZMuFavEgA4). You can also
find examples of aggregating origin-destination data for flows analysis
and visualisation in our vignettes on
[static](https://rOpenSpain.github.io/spanishoddata/articles/flowmaps-static.html)
and
[interactive](https://rOpenSpain.github.io/spanishoddata/articles/flowmaps-interactive.html)
flows visualisation.

Learning to use `DuckDB` and `Parquet` is easy for anyone who have ever
worked with [dplyr](https://dplyr.tidyverse.org) functions such as
[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
etc. However, since there is some learning curve to master these new
tools, we provide some helper functions for novices to get started and
easily open the datasets from `DuckDB` and `Parquet`. Please read the
relevant sections below, where we first show how to convert the data,
and then how to use it.

### 3.1 How to choose between DuckDB, Parquet, and CSV

The main considerations to make when choosing between `DuckDB` and
`Parquet` (that you can get with
[`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md) +
[`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md)),
as well as `CSV.gz` (that you can get with
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md))
are analysis speed, convenience of data analysis, and the specific
approach you prefer when getting the data. We discuss all three below.

#### 3.1.1 Analysis Speed

The data format you choose may dramatically impact the speed of analysis
(e.g. filtering by dates, calculating number of trips per hour, per
week, per month, per origin-destination pair, and any other data
aggregation or manipulation).

In our tests (see [Figure 1](#fig-csv-duckdb-parquet-speed)), we found
that conducting an analysis using `DuckDB` database provided a
significant speed advantage over using `Parquet` and, more importantly,
raw `CSV.gz` files. Specifically, when comparing a query to determine
the mean hourly trips over 18 months for each zone pair, we observed
that using `DuckDB` database was **up to 5 times** faster than using
`Parquet` files and **up to 8 times** faster than using `CSV.gz` files.

![](media/duckdb-parquet-csv-speed-mean-hourly-v1.svg)

Figure 1: Data processing speed comparison: DuckDB engine running on
CSV.gz files vs DuckDB database vs a folder of Parquet files

You can see the query we used for measuring the speed here

For reference, here is a simple query we used for speed comparison in
[Figure 1](#fig-csv-duckdb-parquet-speed):

``` r
# data represents either CSV files acquired from `spod_get()`, a `DuckDB` database or a folder of Parquet files connected with `spod_connect()`
data |>
  group_by(id_origin, id_destination, hour) |> 
  summarise(mean_hourly_trips = mean(n_trips, na.rm = TRUE),
    .groups = "drop")
```

[Figure 1](#fig-csv-duckdb-parquet-speed) also shows that `DuckDB`
format will give you the best performance even on low-end systems with
limited memory and number of processor cores, conditional on a fast SSD
storage. Also note, that if you do choose to work with long time periods
using CSV.gz files via
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md),
you will need to balance the amount of memory and processor cores via
the `max_n_cpu` and `max_mem_gb` arguments, otherwise the analysis may
fail (see the grey area in the figure), when there are too many parallel
processes running at the same time with limited memory.

#### 3.1.2 Convenience of data analysis

Regardless of the data format (`DuckDB`, `Parquet`, or `CSV.gz`), the
functions you will need for data manipulation and analysis are the same.
This is because the analysis is actually performed by the `DuckDB`
([Mühleisen and Raasveldt 2024](#ref-duckdb-r)) engine, which presents
the data as if it were a regular `data.frame`/`tibble` object in R
(almost). So from that point of view, there is no difference between the
data formats. You can manipulate the data using
[dplyr](https://dplyr.tidyverse.org) functions such as
[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
etc. In the end of any sequence of commands you will need to add
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) to
execute the whole chain of data manipulations and load the results into
memory in an R `data.frame`/`tibble`. We provide examples in the
following sections. Please refer to the recommended external tutorials
and our own vignettes in the [Analysing large
datasets](#analysing-large-datasets) section.

#### 3.1.3 Scenarios of getting the data

The choice between converting to `DuckDB` and `Parquet` could also be
made based on how you plan to work with the data. Specifically whether
you want to just download long periods or even all available data, or if
you want to get the data gradually, as you progress through with the
analysis.

- If you plan to work with long time periods, we recommend `DuckDB`, as
  it is one big file and it is easier to update it completely. For
  example you may be working with all 2020 data. Later you decide to add
  all of 2021 data. In this case it would be better to delete the
  database and create it from scratch.

- If you only want certain dates, analyse them and add additional dates
  later, `Parquet` may be better, as each day is saved in a separate
  file, just like the original CSV files. Therefore updating a folder of
  `Parquet` files is as easy as just creating a new file only for the
  missing date.

- If you only work with a few individual days, you may not notice the
  advantages of the `DuckDB` or `Parquet` formats. In this case, you can
  keep using the `CSV.gz` format for the analysis using the
  [`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
  function. This is also useful for quick tutorials, where you only need
  one or two days of data for demonstration purposes.

## 4 Setup

Make sure you have loaded the package:

``` r
library(spanishoddata)
```

## 5 Set the data directory

Choose where
[spanishoddata](https://rOpenSpain.github.io/spanishoddata/) should
download (and convert) the data by setting the data directory following
command:

``` r
spod_set_data_dir(data_dir = "~/spanish_od_data")
```

The function above will also ensure that the directory is created and
that you have sufficient permissions to write to it.

Setting data directory for advanced users

You can also set the data directory with an environment variable:

``` r
Sys.setenv(SPANISH_OD_DATA_DIR = "~/spanish_od_data")
```

The package will create this directory if it does not exist on the first
run of any function that downloads the data.

To permanently set the directory for all projects, you can specify the
data directory globally by setting the `SPANISH_OD_DATA_DIR` environment
variable, e.g. with the following command:

``` r
usethis::edit_r_environ()
# Then set the data directory globally, by typing this line in the file:
```

    SPANISH_OD_DATA_DIR = "~/spanish_od_data"

You can also set the data directory locally, just for the current
project. Set the ‘envar’ in the working directory by editing `.Renviron`
file in the root of the project:

``` r
file.edit(".Renviron")
```

## 6 Getting a single day with `spod_get()`

As you might have seen in the codebooks for
[v1](https://rOpenSpain.github.io/spanishoddata/articles/v1-2020-2021-mitma-data-codebook.md)
and
[v2](https://rOpenSpain.github.io/spanishoddata/articles/v2-2022-onwards-mitma-data-codebook.md)
data, you can get a single day’s worth of data as an in-memory object
with
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md):

``` r
dates <- c("2024-03-01")
d_1 <- spod_get(type = "od", zones = "distr", dates = dates)
class(d_1)
```

The output should look like this:

    # Source:   table<od_csv_clean_filtered> [?? x 19]
    # Database: DuckDB v1.0.0 [... 6.5.0-45-generic:R 4.4.1/:memory:]
       date       hour id_origin id_destination distance activity_origin
       <date>         <int> <fct>     <fct>          <fct>    <fct>
     1 2024-03-01        19 01009_AM  01001          0.5-2    frequent_activity
     2 2024-03-01        15 01002     01001          10-50    frequent_activity

Note that this is a lazily-evaluated in-memory object (note the
`:memory:` in the database path). This means that the data is not loaded
into memory until you call
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) on it.
While this is useful for quick exploration of the data, we do not
recommended this for large datasets, as we have demonstrated
[above](#speed-comparison).

## 7 Analysing the data using `DuckDB` database

Please make sure you did all the steps in the [Setup](#setup) section
above.

### 7.1 Convert to `DuckDB`

You can download and convert the data into `DuckDB` database in two
steps. For example, you select a few dates, and download the data
manually (note: we use `dates_2` to refer to the fact that we are using
v2 data):

``` r
dates_2 <- c(start = "2023-02-14", end = "2023-02-17")
spod_download(type = "od", zones = "distr", dates = dates_2)
```

After that, you can convert any downloaded data (including the files
that might have been downloaded previosly by running
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
or
[`spod_download()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_download.md)
with other dates or date intervals) into `DuckDB` like so
(`dates = "cached_v2"` means use *all* downloaded files):

``` r
db_2 <- spod_convert(type = "od", zones = "distr", dates = "cached_v2", save_format = "duckdb", overwrite = TRUE)
db_2 # check the path to the saved `DuckDB` database
```

The `dates = "cached_v2"` (which can also be `dates = "cached_v1"` for
v1 data) argument instructs the function to only work with
already-downloaded files. By default this resulting `DuckDB` database
for v2 origin-destination data for districts will be saved in the
`SPANISH_OD_DATA_DIR` directory under `v2/tabular/duckdb/` with filename
`od_distritos.duckdb` (you can change this file path with the
`save_path` argument). The function returns the full path to the
database file, which we save into `db_2` variable. You can also any
desired save location with the `save_path` argument of
[`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md).

You can also convert any dates range or dates list to `DuckDB`:

``` r
dates_1 <- c(start = "2020-02-17", end = "2020-02-19")
db_2 <- spod_convert(type = "od", zones = "distr", dates = dates_1, overwrite = TRUE)
```

In this case, any missing data that has not yet been downloaded will be
automatically downloaded, while 2020-02-17 will not be redownloaded, as
we already requsted it when creating `db_1`. Then the requested dates
will be converted into `DuckDB`, overwriting the file with `db_1`. Once
again, we save the path to the output `DuckDB` database file into `db_2`
variable.

### 7.2 Load the converted `DuckDB`

You can read the introductory information on how to connect to `DuckDB`
files [here](https://duckdb.org/docs/api/r), however to simplify things
for you we created a helper function. So to connect to the data stored
in at path `db_1` and `db_2` you can do the following:

``` r
my_od_data_2 <- spod_connect(db_2)
```

Just like before, with
[`spod_get()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get.md)
funciton that we used to download raw CSV.gz files and analyse them
without any conversion, the resulting object `my_od_data_2` is also a
`tbl_duckdb_connection`. So, you can treat it as regular `data.frame` or
`tibble` and use [dplyr](https://dplyr.tidyverse.org) functions such as
[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
etc. For analysis, please refer to the recommended external tutorials
and our own vignettes in the [Analysing large
datasets](#analysing-large-datasets) section.

After finishing working with `my_od_data_2` we advise that you
“disconnect” this data using:

``` r
spod_disconnect(my_od_data_2)
```

This is useful to free-up memory and is neccessary if you would like to
run
[`spod_convert()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_convert.md)
again and save the data to the same location. Otherwise, it is also
helpful to avoid unnecessary possible warnings in terminal for garbage
collected connections.

## 8 Analysing the data using `Parquet`

Please make sure you did all the steps in the [Setup](#setup) section
above.

### 8.1 Convert to `Parquet`

The process is exactly the same as for `DuckDB` above. The only
difference is that the data is converted to `parquet` format and stored
in `SPANISH_OD_DATA_DIR` under `v1/clean_data/tabular/parquet/`
directory for v1 data (change this with the `save_path` argument), and
the subfolders are in hive-style format like `year=2020/month=2/day=14`
and inside each of these folders a single `parquet` file will be placed
containing the data for that day.

The advantage of this format is that you can “update” it quickly. For
example, if you first downloaded the data for March and April 2020,
converted this period into `parquet` format, and then downloaded the
data for May and June 2020, when you run the convertion function again,
it will only convert the data for May and June 2020 and add it to the
existing `parquet` files. So you will save time and will not have to
wait for March and April 2020 to be converted again.

Let us convert a few dates to `parquet` format:

``` r
type <- "od"
zones <- "distr"
dates <- c(start = "2020-02-14", end = "2020-02-17")
od_parquet <- spod_convert(type = type, zones = zones, dates = dates, save_format = "parquet")
```

If we now request additional dates that overlap with the already
converted data like so and specifiy argument `overwrite = 'update'` we
will update the existing `parquet` files with the new data:

``` r
dates <- c(start = "2020-02-16", end = "2020-02-19")
od_parquet <- spod_convert(type = type, zones = zones, dates = dates, save_format = "parquet", overwrite = 'update')
```

That is, 16 and 17 Feboruary will not be converted again. Only the new
data, that was not converted (18 and 19 February) will be converted, and
these will be added to the existing folder structure of`parquet` files
stored at the default `save_path` location, which is
`<data_dir>/clean_data/v1/tabular/parquet/od_distritos`. Alternatively,
you can set any other save location by setting the `save_path` argument.

### 8.2 Load the converted `Parquet`

Working with these `parquet` files is exactly the same as with `DuckDB`
and `Arrow` files. Just like before, you can use the same helper
function
[`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md)
to connect to the `parquet` files:

``` r
my_od_data_3 <- spod_connect(od_parquet)
```

Mind you though, because we have first converted the data for a period
between 14 and 17 February 2020, and then converted the data for a
period between 16 and 19 February 2020 into the save default location,
the `od_parquet` contains the path to all this data, and therefore
`my_od_data_3` will connect you to all data.

You can check this like so:

``` r
my_od_data_3 |> 
  dplyr::distinct(date) |>
  dplyr::arrange(date)
```

For analysis, please refer to the recommended external tutorials and our
own vignettes in the [Analysing large
datasets](#analysing-large-datasets) section.

## 9 Download all available data

To prepare origin-destination data v1 (2020-2021) for analysis over the
whole period of data availability, please follow the steps below:

``` r
dates_v1 <- spod_get_valid_dates(ver = 1)
dates_v2 <- spod_get_valid_dates(ver = 2)
```

> **Warning**
>
> Due to mobile network outages, the data on certain dates is missing.
> Kindly keep this in mind when calculating mean monthly or weekly
> flows.
>
>   
>
> Please check the [original data
> page](https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad)
> for currently known missing dates. At the time of writing, the
> following dates are missing: 26, 27, 30, 31 October; 1, 2 and 3
> November 2023; 4, 18, 19 April 2024, 10 and 11 November 2024. You can
> use
> [`spod_get_valid_dates()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_get_valid_dates.md)
> function to get all available dates.

### 9.1 Download all data

Here the example is for origin-destination on district level for v1
data. You can change the `type` to “number_of_trips” and the `zones` to
“municipalities” for v1 data. For v2 data, just use `dates` starting
with 2022-01-01 or the `dates_v2` from above. Use all other function
arguments for v2 in the same way as shown for v1, but also consult the
[v2 data
codebook](https://rOpenSpain.github.io/spanishoddata/articles/v2-2022-onwards-mitma-data-codebook.md),
as it has many more datasets in addition to “origin-destination” and
“number_of_trips”.

``` r
type <- "origin-destination"
zones <- "districts"
spod_download(
  type = type,
  zones = zones,
  dates = dates_v1,
  return_local_file_paths = FALSE, # to avoid getting all downloaded file paths printed to console
  max_download_size_gb = 50 # in Gb, this should be well over the actual download size for v1 data
)
```

### 9.2 Convert all data into analysis ready format

``` r
save_format <- "duckdb"

analysis_data_storage <- spod_convert_data(
  type = type,
  zones = zones,
  dates = "cached_v1", # to just convert all data that was previously downloaded, no need to specify dates here
  save_format = save_format,
  overwrite = TRUE
)
```

This will convert all downloaded data to `DuckDB` format for lightning
fast analysis. You can change the `save_format` to `parquet` if you want
to save the data in `Parquet` format. For comparison overview of the two
formats please see the [Converting the data to DuckDB/Parquet for faster
analysis](https://ropenspain.github.io/spanishoddata/articles/convert.html).

By default, `spod_convert_data()` will save the converted data in the
`SPANISH_OD_DATA_DIR` directory. You can change the `save_path` argument
of `spod_convert_data()` if you want to save the data in a different
location.

For this conversion, 4 GB or operating memory should be enough, the
speed of the process depends on the number of processor cores and the
speed of your disk storage. SSD is preferred. By default, the
`spod_convert_data()` will use all except one processor cores on your
computer. You can adjust this with the `max_n_cpu` argument of
`spod_convert_data()`. You can also increase the maximum amount of
memory used with the `max_mem_gb` argument, but this makes more
difference during the analysis stage.

Finally, `analysis_data_storage` will simply store the path to the
converted data. Either a path to the `DuckDB` database file or a path to
the folder with `Parquet` files.

### 9.3 Conversion speed

For reference, converting the whole v1 origin-destination data to
`DuckDB` takes about 20 minutes with 4 GB of memory and 3 processor
cores. The final size of the `DuckDB` database is about 18 GB, in
`Parquet` format - 26 GB. The raw CSV files in gzip archives are about
20GB. v2 data is much larger, with origin-destination tables for 2022 -
mid-2024 taking up 150+ GB in raw CSV.gz format.

### 9.4 Connecting to and analysing the converted datasets

You can pass the `analysis_data_storage` path to
[`spod_connect()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_connect.md)
function, whether it is `DuckDB` or `Parquet`. The function will
determine the data type automatically and give you back a
`tbl_duckdb_connection`[¹](#fn1).

``` r
my_data <- spod_connect(
  data_path = analysis_data_storage, 
  max_mem_gb = 16
)
```

Here we set `max_mem_gb` to 16 GB. Generally, if you have more, feel
free to increase it, but also consult the
[Figure 1](#fig-csv-duckdb-parquet-speed) with our speed testing results
in the [Speed](#speed-comparison) section. You can try other
combinations of `max_mem_gb` and `max_n_cpu` arguments for your needs

Compared to conversion process, you might want to increase the available
memory for the analysis step. The more, the better. You can control that
with the `max_mem_gb` argument.

You can manipulate `my_data` using [dplyr](https://dplyr.tidyverse.org)
functions such as
[`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
etc. In the end of any sequence of commands you will need to add
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) to
execute the whole chain of data manipulations and load the results into
memory in an R `data.frame`/`tibble`. For analysis, please refer to the
recommended external tutorials and our own vignettes in the [Analysing
large datasets](#analysing-large-datasets) section.

After finishing working with `my_data` we advise that you “disconnect”
to free up memory:

``` r
spod_disconnect(my_data)
```

Mühleisen, Hannes, and Mark Raasveldt. 2024. *Duckdb: DBI Package for
the DuckDB Database Management System*.
<https://doi.org/10.32614/CRAN.package.duckdb>.

------------------------------------------------------------------------

1.  For reference: this object also has classes: `tbl_dbi` ,`tbl_sql`,
    `tbl_lazy` ,and `tbl` .
