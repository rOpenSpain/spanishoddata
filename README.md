

<!-- badges: start -->

[![R-CMD-check](https://github.com/Robinlovelace/spanish_od_data/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Robinlovelace/spanish_od_data/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repo demonstrates how to download and use OD data from Spain,
published by
[transportes.gob.es](https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad)

The data is provided as follows:

- Estudios basicos
  - Por disitritos
    - Personas (population)
    - Pernoctaciones (overnight stays)
    - Viajes
      - ficheros-diarios
      - meses-completos

The package is designed to save people time by providing the data in
analyis-ready formats. Automating the process of downloading, cleaning
and importing the data can also reduce the risk of errors in the
laborious process of data preparation.

The datasets are large, so the package aims to reduce computational
resources, by using computationally efficient packages behind the
scenes. If you want to use many of the data files, it’s recommended you
set a data directory where the package will look for the data, only
downloading the files that are not already present.

# Installation

Install the package as follows:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("Robinlovelace/spanishoddata")
```

Load it as follows:

``` r
library(spanishoddata)
```

Local development: to load the package locally, clone it and navigate to
the root of the package in the terminal, e.g. with the following:

``` bash
gh repo clone Robinlovelace/spanishoddata
code spanishoddata
```

then run the following command from the R console:

``` r
devtools::load_all()
```

# Setting the data directory

You can specify the data directory globally by setting the
`SPANISH_OD_DATA_DIR` environment variable, e.g. with the following
command:

``` r
usethis::edit_r_environ()
# Then set the data directory globally, by typing this line in the file:
```

    SPANISH_OD_DATA_DIR = "/path/to/data"

You can also set the data directory locally or on a per session basis as
described below.

<details>

Set the ‘envar’ in the working directory by editing `.Renviron` file in
the root of the project:

``` r
file.edit(".Renviron")
```

Finally, you can set the data directory in the current R session as
follows:

``` r
Sys.setenv(SPANISH_OD_DATA_DIR = "/path/to/data")
```

</details>

# Using the package

<div id="fig-overall-flow">

<img src="vignettes/media/package-functions-overview.png"
style="width:120.0%;height:120.0%" />


Figure 1: The overview of how to use the pacakge functions to get the
data

</div>

To run the code in this README we will use the following setup:

``` r
library(tidyverse)
theme_set(theme_minimal())
sf::sf_use_s2(FALSE)
```

Get metadata for the datasets as follows (we are using version 2 data
covering years 2022 and onwards):

``` r
metadata <- spod_available_data(ver = 2) # for version 2 of the data
metadata
```

    # A tibble: 9,442 × 7
       target_url           pub_ts              file_extension data_ym data_ymd  
       <chr>                <dttm>              <chr>          <date>  <date>    
     1 https://movilidad-o… 2024-07-30 10:54:08 gz             NA      2022-10-23
     2 https://movilidad-o… 2024-07-30 10:51:07 gz             NA      2022-10-22
     3 https://movilidad-o… 2024-07-30 10:47:52 gz             NA      2022-10-20
     4 https://movilidad-o… 2024-07-30 10:14:55 gz             NA      2022-10-18
     5 https://movilidad-o… 2024-07-30 10:11:58 gz             NA      2022-10-17
     6 https://movilidad-o… 2024-07-30 10:09:03 gz             NA      2022-10-12
     7 https://movilidad-o… 2024-07-30 10:05:57 gz             NA      2022-10-07
     8 https://movilidad-o… 2024-07-30 10:02:12 gz             NA      2022-08-07
     9 https://movilidad-o… 2024-07-30 09:58:34 gz             NA      2022-08-06
    10 https://movilidad-o… 2024-07-30 09:54:30 gz             NA      2022-08-05
    # ℹ 9,432 more rows
    # ℹ 2 more variables: local_path <chr>, remote_file_size_mb <dbl>

## Zones

Zones can be downloaded as follows:

``` r
distritos <- spod_get_zones("distritos", ver = 2)
distritos_wgs84 <- distritos |> sf::st_simplify(dTolerance = 200) |> sf::st_transform(4326)
plot(sf::st_geometry(distritos_wgs84))
```

![](man/figures/README-distritos-1.png)

## Estudios basicos

``` r
od_db <- spod_get(
  type = "origin-destination",
  zones = "districts",
  dates = c(start = "2024-03-01", end = "2024-03-07")
)
class(od_db)
```

    [1] "tbl_duckdb_connection" "tbl_dbi"               "tbl_sql"              
    [4] "tbl_lazy"              "tbl"                  

``` r
colnames(od_db)
```

     [1] "date"                        "time_slot"                  
     [3] "id_origin"                   "id_destination"             
     [5] "distance"                    "activity_origin"            
     [7] "activity_destination"        "study_possible_origin"      
     [9] "study_possible_destination"  "residence_province_ine_code"
    [11] "residence_province"          "income"                     
    [13] "age"                         "sex"                        
    [15] "n_trips"                     "trips_total_length_km"      
    [17] "year"                        "month"                      
    [19] "day"                        

The result is an R database interface object (`tbl_dbi`) that can be
used with dplyr functions and SQL queries ‘lazily’, meaning that the
data is not loaded into memory until it is needed. Let’s do an
aggregation to find the total number trips per hour over the 7 days:

``` r
n_per_hour <- od_db |>
  group_by(date, time_slot) |>
  summarise(n = n(), Trips = sum(n_trips)) |>
  collect() |>
  mutate(Time = lubridate::ymd_h(paste0(date, time_slot, sep = " "))) |>
  mutate(Day = lubridate::wday(Time, label = TRUE))
n_per_hour |>
  ggplot(aes(x = Time, y = Trips)) +
  geom_line(aes(colour = Day)) +
  labs(title = "Number of trips per hour over 7 days")
```

![](man/figures/README-trips-per-hour-1.png)

The figure above summarises 925,874,012 trips over the 7 days associated
with 135,866,524 records.

To highlight the benefits of the package, here is how you would do this
manually:

- download the [xml](https://movilidad-opendata.mitma.es/RSS.xml) file
  with the download links

- parse this xml to extract the download links

- write a script to download the files and locate them on disk in a
  logical manner

- figure out the data structure of the downloaded files, read the
  codebook

- translate the data (columns and values) into English, if you are not
  familiar with Spanish

- write a script to load the data into the database or figure out a way
  to claculate summaries on multiple files

- and much more…

We did all of that for you and present you with a few simple functions
that get you straight to the data in one line of code, and you are ready
to run any analysis on it.

# Desire lines

We’ll use the same input data to pick-out the most important flows in
Spain, with a focus on longer trips for visualisation:

``` r
od_national_aggregated <- od_db |>
  group_by(id_origin, id_destination) |>
  summarise(Trips = sum(n_trips), .groups = "drop") |>
  filter(Trips > 500) |>
  collect() |>
  arrange(desc(Trips))
od_national_aggregated
```

    # A tibble: 96,404 × 3
       id_origin id_destination    Trips
       <fct>     <fct>             <dbl>
     1 2807908   2807908        2441404.
     2 0801910   0801910        2112188.
     3 0801902   0801902        2013618.
     4 2807916   2807916        1821504.
     5 2807911   2807911        1785981.
     6 04902     04902          1690606.
     7 2807913   2807913        1504484.
     8 2807910   2807910        1299586.
     9 0704004   0704004        1287122.
    10 28106     28106          1286058.
    # ℹ 96,394 more rows

The results show that the largest flows are intra-zonal. Let’s keep only
the inter-zonal flows:

``` r
od_national_interzonal <- od_national_aggregated |>
  filter(id_origin != id_destination)
```

We can convert these to geographic data with the {od} package:

``` r
od_national_sf <- od::od_to_sf(
  od_national_interzonal,
  z = distritos_wgs84
)
distritos_wgs84 |>
  ggplot() +
  geom_sf(aes(fill = population)) +
  geom_sf(data = spData::world, fill = NA, colour = "black") +
  geom_sf(aes(size = Trips), colour = "blue", data = od_national_sf) +
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45)) +
  theme_void()
```

![](man/figures/README-desire-lines-1.png)

Let’s focus on trips in and around a particular area (Salamanca):

``` r
salamanca_zones <- zonebuilder::zb_zone("Salamanca")
distritos_salamanca <- distritos_wgs84[salamanca_zones, ]
plot(distritos_salamanca)
```

![](man/figures/README-salamanca-zones-1.png)

We will use this information to subset the rows, to capture all movement
within the study area:

``` r
ids_salamanca <- distritos_salamanca$id
od_salamanca <- od_national_sf |>
  filter(id_origin %in% ids_salamanca) |>
  filter(id_destination %in% ids_salamanca) |>
  arrange(Trips)
```

Let’s plot the results:

``` r
od_salamanca_sf <- od::od_to_sf(
  od_salamanca,
  z = distritos_salamanca
)
ggplot() +
  geom_sf(fill = "grey", data = distritos_salamanca) +
  geom_sf(aes(colour = Trips), size = 1, data = od_salamanca_sf) +
  scale_colour_viridis_c() +
  theme_void()
```

![](man/figures/README-salamanca-plot-1.png)

# Disaggregating desire lines

For this you’ll need some additional dependencies:

``` r
library(sf)
library(tmap)
```

We’ll get the road network from OSM:

``` r
salamanca_boundary <- sf::st_union(distritos_salamanca)
osm_full <- osmactive::get_travel_network(salamanca_boundary)
```

``` r
osm <- osm_full[salamanca_boundary, ]
drive_net <- osmactive::get_driving_network(osm)
drive_net_major <- osmactive::get_driving_network_major(osm)
cycle_net <- osmactive::get_cycling_network(osm)
cycle_net <- osmactive::distance_to_road(cycle_net, drive_net_major)
cycle_net <- osmactive::classify_cycle_infrastructure(cycle_net)
map_net <- osmactive::plot_osm_tmap(cycle_net)
map_net
```

![](man/figures/README-osm-1.png)

We can use the road network to disaggregate the desire lines:

``` r
od_jittered <- odjitter::jitter(
  od_salamanca_sf,
  zones = distritos_salamanca,
  subpoints = drive_net,
  disaggregation_threshold = 1000,
  disaggregation_key = "Trips"
)
```

Let’s plot the disaggregated desire lines:

``` r
od_jittered |>
  arrange(Trips) |>
  ggplot() +
  geom_sf(aes(colour = Trips), size = 1) +
  scale_colour_viridis_c() +
  geom_sf(data = drive_net_major, colour = "black") +
  theme_void()
```

![](man/figures/README-disaggregated-1.png)
