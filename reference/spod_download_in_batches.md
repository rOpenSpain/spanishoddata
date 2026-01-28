# Download multiple files with progress bar in parallel

Download multiple files with a progress bar. Retries failed downloads up
to 3 times. Downloads are in parallel and in batches to show progress.
First 10 Mb of a file is downloaded to check the speed.

## Usage

``` r
spod_download_in_batches(
  files_to_download,
  batch_size = 5,
  bar_width = 20,
  chunk_size = 1024 * 1024,
  test_size = 10 * 1024 * 1024,
  max_retries = 3L,
  timeout = 900,
  show_progress = interactive() && !isTRUE(getOption("knitr.in.progress"))
)
```

## Arguments

- files_to_download:

  A data frame with columns `target_url`, `local_path` and
  `file_size_bytes`.

- batch_size:

  Numeric. Number of files to download at a time.

- bar_width:

  Numeric. Width of the progress bar.

- chunk_size:

  Numeric. Number of bytes to download at a time for speed test.

- max_retries:

  Integer. Maximum number of retries for failed downloads.

- timeout:

  Numeric. Timeout in seconds for each download.

- show_progress:

  Logical. Whether to show the progress bar.

## Value

A data frame with columns `target_url`, `local_path`, `file_size_bytes`
and `local_file_size`.
