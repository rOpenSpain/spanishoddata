

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

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_cran("duckdb")
```

``` r
library(duckdb)
library(tidyverse)
```

## Zones

Zones can be downloaded as follows:

``` r
#  zonificacion_distritos.cpg   2023-04-19 10:57:02 5 Bytes
# zonificacion_distritos.dbf    2023-04-19 10:57:02 309 KB
# zonificacion_distritos.prj    2023-04-19 10:57:02 393 Bytes
# zonificacion_distritos.qmd    2022-12-20 15:25:26 657 Bytes
# zonificacion_distritos.qpj    2023-04-19 10:57:02 618 Bytes
# zonificacion_distritos.shp    2023-04-19 10:57:02 22 MB
# zonificacion_distritos.shx
shp_extensions = c("cpg", "dbf", "prj", "qmd", "qpj", "shp", "shx")
base_url = "https://movilidad-opendata.mitma.es/zonificacion/zonificacion_distritos/zonificacion_distritos"
dir.create("zonificacion_distritos", showWarnings = FALSE)
for (ext in shp_extensions) {
  u = paste0(base_url, ".", ext)
  f = file.path("zonificacion_distritos", paste0("zonificacion_distritos.", ext))
  if (!file.exists(f)) {
    download.file(u, f)
  }
}
```

## Estudios basicos

Each day in the `ficheros-diarios` folder contains a file with the
following columns:

``` r
# set timeout for downloads
options(timeout = 600) # 10 minutes
u1 = "https://movilidad-opendata.mitma.es/estudios_basicos/por-distritos/viajes/ficheros-diarios/2024-03/20240301_Viajes_distritos.csv.gz"
f1 = basename(u1)
if (!file.exists(f1)) {
  download.file(u1, f1)
}
drv = duckdb::duckdb("daily.duckdb")
con = DBI::dbConnect(drv)
od1 = duckdb::tbl_file(con, f1)
# colnames(od1)
#  [1] "fecha"                   "periodo"                
#  [3] "origen"                  "destino"                
#  [5] "distancia"               "actividad_origen"       
#  [7] "actividad_destino"       "estudio_origen_posible" 
#  [9] "estudio_destino_posible" "residencia"             
# [11] "renta"                   "edad"                   
# [13] "sexo"                    "viajes"                 
# [15] "viajes_km"
od1_head = od1 |>
  head() |>
  collect()
od1_head |>
  knitr::kable()
```

|    fecha | periodo | origen   | destino | distancia | actividad_origen | actividad_destino | estudio_origen_posible | estudio_destino_posible | residencia | renta | edad | sexo | viajes | viajes_km |
|---------:|:--------|:---------|:--------|:----------|:-----------------|:------------------|:-----------------------|:------------------------|:-----------|:------|:-----|:-----|-------:|----------:|
| 20240301 | 19      | 01009_AM | 01001   | 0.5-2     | frecuente        | casa              | no                     | no                      | 01         | 10-15 | NA   | NA   |  5.124 |     6.120 |
| 20240301 | 15      | 01002    | 01001   | 10-50     | frecuente        | casa              | no                     | no                      | 01         | 10-15 | NA   | NA   |  2.360 |   100.036 |
| 20240301 | 00      | 01009_AM | 01001   | 10-50     | frecuente        | casa              | no                     | no                      | 01         | 10-15 | NA   | NA   |  1.743 |    22.293 |
| 20240301 | 05      | 01009_AM | 01001   | 10-50     | frecuente        | casa              | no                     | no                      | 01         | 10-15 | NA   | NA   |  2.404 |    24.659 |
| 20240301 | 06      | 01009_AM | 01001   | 10-50     | frecuente        | casa              | no                     | no                      | 01         | 10-15 | NA   | NA   |  5.124 |    80.118 |
| 20240301 | 09      | 01009_AM | 01001   | 10-50     | frecuente        | casa              | no                     | no                      | 01         | 10-15 | NA   | NA   |  7.019 |    93.938 |

``` r
DBI::dbDisconnect(con)
```
