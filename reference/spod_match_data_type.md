# Match data types for normalisation

Match data types for normalisation

## Usage

``` r
spod_match_data_type(
  type = c("od", "origin-destination", "viajes", "os", "overnight_stays",
    "pernoctaciones", "nt", "number_of_trips", "personas")
)
```

## Arguments

- type:

  The type of data to match. Can be "od", "origin-destination", "os",
  "overnight_stays", or "nt", "number_of_trips".

## Value

A `character` string with the folder name for the specified data type.
Or `NULL` if the type is not recognized.
