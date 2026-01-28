# View codebooks for v1 and v2 open mobility data

**\[stable\]**

Opens relevant vignette with a codebook for v1 (2020-2021) and v2 (2022
onwards) data or provide a webpage if vignette is missing.

## Usage

``` r
spod_codebook(ver = 1)
```

## Arguments

- ver:

  An `integer` or `numeric` value. The version of the data. Defaults
  to 1. Can be `1` for v1 (2020-2021) data and 2 for v2 (2022 onwards)
  data.

## Value

Nothing, opens vignette if it is installed. If vignette is missing,
prints a message with a link to a webpage with the codebook.

## Examples

``` r
# View codebook for v1 (2020-2021) data
spod_codebook(ver = 1)
#> Warning: vignette ‘v1-2020-2021-mitma-data-codebook’ not found
#> For some reason the codebook was not installed with the package. Please refer to the online version at: https://ropenspain.github.io/spanishoddata/articles/v1-2020-2021-mitma-data-codebook.html

# View codebook for v2 (2022 onwards) data
spod_codebook(ver = 2)
#> Warning: vignette ‘v2-2022-onwards-mitma-data-codebook’ not found
#> For some reason the codebook was not installed with the package. Please refer to the online version at: https://ropenspain.github.io/spanishoddata/articles/v2-2022-onwards-mitma-data-codebook.html
```
