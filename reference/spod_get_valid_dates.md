# Get valid dates for the specified data version

**\[stable\]**

Get all metadata for requested data version and identify all dates
available for download.

## Usage

``` r
spod_get_valid_dates(ver = NULL)
```

## Arguments

- ver:

  Integer. Can be 1 or 2. The version of the data to use. v1 spans
  2020-2021, v2 covers 2022 and onwards. See more details in codebooks
  with
  [`spod_codebook()`](https://rOpenSpain.github.io/spanishoddata/reference/spod_codebook.md).

## Value

A vector of type `Date` with all possible valid dates for the specified
data version (v1 for 2020-2021 and v2 for 2020 onwards).

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
# Get all valid dates for v1 (2020-2021) data
spod_get_valid_dates(ver = 1)

# Get all valid dates for v2 (2020 onwards) data
spod_get_valid_dates(ver = 2)
# }
}
```
