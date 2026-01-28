# Compute ETag for a file

Compute ETag for a file

## Usage

``` r
spod_compute_s3_etag(file_path, part_size = 8 * 1024^2)
```

## Arguments

- file_path:

  Character. The path to the file.

- part_size:

  Numeric. The size of each part in bytes. Do not change, as this is a
  default for S3 Etag.

## Value

Character. The ETag for the file.
