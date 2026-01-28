# Match data types to folders

Match data types to folders

## Usage

``` r
spod_match_data_type_for_local_folders(
  type = c("od", "origin-destination", "os", "overnight_stays", "nt", "number_of_trips"),
  ver = c(1, 2)
)
```

## Arguments

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

## Value

A `character` string with the folder name for the specified data type.
Or `NULL` if the data type is not recognized.
