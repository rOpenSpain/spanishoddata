# Get raw data cache subfolder name

Change subfolder name in the code of this function for raw data cache
here to apply globally, as all functions in the package should use this
function to get the raw data cache path.

## Usage

``` r
spod_subfolder_raw_data_cache(ver = 1)
```

## Arguments

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

## Value

A `character` string with the subfolder name for the raw data cache.
