# Get files sizes for remote files of v1 and v2 data and save them into a csv.gz file in the inst/extdata folder.

Get files sizes for remote files of v1 and v2 data and save them into a
csv.gz file in the inst/extdata folder.

## Usage

``` r
spod_files_sizes(ver = 2)
```

## Arguments

- ver:

  The version of the data (1 or 2). Can be both. Defaults to 2, as v1
  data is not being updated since 2021.

## Value

Nothing. Only saves a csv.gz file with up to date file sizes in the
inst/extdata folder.
