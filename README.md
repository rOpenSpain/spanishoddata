

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_cran("duckdb")
```

    Skipping install of 'duckdb' from a cran remote, the SHA1 (0.10.2) has not changed since last install.
      Use `force = TRUE` to force installation

``` r
library(duckdb)
```

    Loading required package: DBI

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ✔ purrr     1.0.2     

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ purrr::%||%()   masks base::%||%()
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

This repo demonstrates how to download and use OD data from Spain,
published by
[transportes.gob.es](https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad)

``` r
u = "https://movilidad-opendata.mitma.es/estudios_completos/por-distritos/movilidad_obligada/meses-completos/202201_Movilidad_obligada_distritos.csv.gz"
f = basename(u)
if (!file.exists(f)) {
  download.file(u, f)
}
drv = duckdb::duckdb("movilidad.duckdb")
con = DBI::dbConnect(drv)
od1 = duckdb::tbl_file(con, f)
od1_head = od1 |>
  head() |>
  collect()
# close connection:
DBI::dbDisconnect(con)
```

``` r
od1_head |>
  knitr::kable()
```

|    mes | origen | destino  | edad  | sexo   | residencia | recurrencia | personas |
|-------:|:-------|:---------|:------|:-------|:-----------|:------------|---------:|
| 202201 | 01001  | 01002    | NA    | NA     | 01         | 1-2         |    4.406 |
| 202201 | 01001  | 01002    | 0-25  | hombre | 01         | 5-7         |   11.000 |
| 202201 | 01001  | 01004_AM | NA    | NA     | 01         | 1-2         |    4.388 |
| 202201 | 01001  | 01009_AM | 0-25  | mujer  | 01         | 1-2         |  144.513 |
| 202201 | 01001  | 01009_AM | 25-45 | mujer  | 01         | 1-2         |  119.816 |
| 202201 | 01001  | 01009_AM | 45-65 | mujer  | 01         | 1-2         |  118.621 |
