#' Get available data list from Amazon S3 storage
#'
#' @description
#'
#' Get a table with links to available data files for the specified data version from Amazon S3 storage.
#'
#' @inheritParams spod_available_data
#' @inheritParams global_quiet_param
#' @return A tibble with links, release dates of files in the data, dates of data coverage, local paths to files, and the download status.
#'
#' @export
spod_available_data_s3 <- function(
  ver = c(1, 2),
  force = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir()
) {
  ver <- as.character(ver)
  ver <- match.arg(ver)
  metadata_folder <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}")

  # if forcing, evict the in-session cache now
  if (isTRUE(force)) {
    memoise::forget(spod_available_data_s3_memoised)
  }

  # shortcut: if we already have it memoised, return immediately
  if (!force && memoise::has_cache(spod_available_data_s3_memoised)(ver)) {
    return(spod_available_data_s3_memoised(ver))
  }

  # no in-session data, check your on-disk RDS pool
  pattern <- glue::glue("metadata_s3_v{ver}_\\d{{4}}-\\d{{2}}-\\d{{2}}\\.rds$")
  rds_files <- fs::dir_ls(
    path = metadata_folder,
    type = "file",
    regexp = pattern
  ) |>
    sort()

  latest_file <- utils::tail(rds_files, 1)
  latest_date <- if (length(latest_file) == 1) {
    stringr::str_extract(basename(latest_file), "\\d{4}-\\d{2}-\\d{2}") |>
      as.Date()
  } else {
    NA
  }

  needs_update <- isTRUE(force) ||
    length(rds_files) == 0 ||
    (!is.na(latest_date) && latest_date < Sys.Date())

  if (!needs_update) {
    if (!quiet) message("Using existing disk cache: ", latest_file)
    return(readRDS(latest_file))
  }

  # if forcing, also clear old disk files
  if (isTRUE(force) && length(rds_files) > 0) {
    fs::file_delete(rds_files)
  }

  # fetch via the memoised function (this will re-hit S3 if we forgot it)
  if (!quiet) message("Fetching latest data from S3 (v", ver, ")…")
  dat <- spod_available_data_s3_memoised(ver)

  # write a new RDS stamped with today’s date
  file_date <- format(Sys.Date(), "%Y-%m-%d")
  out_path <- file.path(
    metadata_folder,
    glue::glue("metadata_s3_v{ver}_{file_date}.rds")
  )
  saveRDS(dat, out_path)
  if (!quiet) message("Cached new data to: ", out_path)

  dat
}


spod_available_data_s3_function <- function(
  ver = c(1, 2)
) {
  ver <- as.character(ver)
  ver <- match.arg(ver)

  bucket <- paste0("mitma-movilidad-v", ver)

  original_aws_region <- Sys.getenv("AWS_DEFAULT_REGION")
  original_aws_url_style <- Sys.getenv("AWS_S3_URL_STYLE")
  on.exit({
    Sys.setenv(
      AWS_DEFAULT_REGION = original_aws_region,
      AWS_S3_URL_STYLE = original_aws_url_style
    )
  })
  Sys.setenv(
    AWS_DEFAULT_REGION = "eu-west-1",
    AWS_S3_URL_STYLE = "virtual"
  )

  if (ver == 1) {
    url_prefix <- "https://opendata-movilidad.mitma.es/"
  } else {
    url_prefix <- "https://movilidad-opendata.mitma.es/"
  }

  all_objects <- aws.s3::get_bucket_df(
    bucket = bucket,
    prefix = "", # root of bucket
    max = Inf # fetch beyond the default 1000
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      target_url = paste0(url_prefix, .data$Key),
      pub_ts = as.POSIXct(
        .data$LastModified,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ),
      file_size_bytes = as.numeric(.data$Size),
      etag = gsub('\\"', '', .data$ETag)
    ) |>
    dplyr::select(
      .data$target_url,
      .data$pub_ts,
      .data$file_size_bytes,
      .data$etag
    )

  return(all_objects)
}

spod_available_data_s3_memoised <- memoise::memoise(
  spod_available_data_s3_function
)
