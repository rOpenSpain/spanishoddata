#' Get available data list from Amazon S3 storage
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
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
  s3_force_update = FALSE
) {
  if (s3_force_update) {
    memoise::forget(spod_available_data_s3_memoised)
  }

  spod_available_data_s3_memoised(ver = ver)
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
      target_url = paste0(url_prefix, Key),
      pub_ts = as.POSIXct(
        LastModified,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ),
      file_size_bytes = as.integer(Size),
      etag = gsub('\\"', '', ETag)
    ) |>
    dplyr::select(
      target_url,
      pub_ts,
      file_size_bytes,
      etag
    )

  return(all_objects)
}

spod_available_data_s3_memoised <- memoise::memoise(
  spod_available_data_s3_function
)
