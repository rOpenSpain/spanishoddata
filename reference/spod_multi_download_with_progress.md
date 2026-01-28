# Download multiple files with progress bar sequentially

Download multiple files with a progress bar. Retries failed downloads up
to 3 times.

## Usage

``` r
spod_multi_download_with_progress(
  files_to_download,
  chunk_size = 1024 * 1024,
  bar_width = 20,
  show_progress = interactive() && !isTRUE(getOption("knitr.in.progress"))
)
```

## Arguments

- files_to_download:

  A data frame with columns `target_url`, `local_path` and
  `file_size_bytes`.

- chunk_size:

  Number of bytes to download at a time.

- bar_width:

  Width of the progress bar.

- show_progress:

  Whether to show the progress bar.

## Value

A data frame with columns `target_url`, `local_path`, `file_size_bytes`
and `local_file_size`.
