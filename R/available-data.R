#' Get available data list
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Get a table with links to available data files for the specified data version. Optionally check (see arguments) the file size and availability of data files previously downloaded into the cache directory specified with SPANISH_OD_DATA_DIR environment variable (set by [spod_set_data_dir()]) or a custom path specified with `data_dir` argument. By default the data is fetched from Amazon S3 bucket where the data is stored. If that fails, the function falls back to downloading an XML file from the Spanish Ministry of Transport website. You can also control this behaviour with `use_s3` argument.
#'
#' For detailed data descriptions, see package vignettes using [`spod_codebook(ver = 1)`][spod_codebook] and [`spod_codebook(ver = 2)`][spod_codebook] and official methodology documents in **References** section.
#'
#' @template references
#'
#' @param ver Integer. Can be 1 or 2. The version of the data to use. v1 spans 2020-2021, v2 covers 2022 and onwards. See more details in codebooks with [spod_codebook()].
#' @param check_local_files Logical. Whether to check if the local files exist and get the file size. Defaults to `FALSE`.
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param use_s3 `r lifecycle::badge("experimental")` Logical. If `TRUE`, use Amazon S3 to get available data list, which does not require downloading the XML file and caching it locally, which may be a bit faster. If `FALSE`, use the XML file to get available data list.
#' @param force Logical. If `TRUE`, force re-download of metadata. For Amazon S3 this queries the S3 bucket for the XML file it re-downloads. If `FALSE`, only update the available data list if it is older than 1 day.
#' @inheritParams spod_available_data_v1
#' @inheritParams global_quiet_param
#' @return A tibble with links, release dates of files in the data, dates of data coverage, local paths to files, and the download status.
#' \describe{
#'   \item{target_url}{\code{character}. The URL link to the data file.}
#'   \item{pub_ts}{\code{POSIXct}. The timestamp of when the file was published.}
#'   \item{file_extension}{\code{character}. The file extension of the data file (e.g., 'tar', 'gz').}
#'   \item{data_ym}{\code{Date}. The year and month of the data coverage, if available.}
#'   \item{data_ymd}{\code{Date}. The specific date of the data coverage, if available.}
#' \item{study}{\code{factor}. Study category derived from the URL (e.g., 'basic', 'complete', 'routes').}
#'   \item{type}{\code{factor}. Data type category derived from the URL (e.g., 'number_of_trips', 'origin-destination', 'overnight_stays', 'data_quality', 'metadata').}
#'   \item{period}{\code{factor}. Temporal granularity category derived from the URL (e.g., 'day', 'month').}
#'   \item{zones}{\code{factor}. Geographic zone classification derived from the URL (e.g., 'districts', 'municipalities', 'large_urban_areas').}
#'   \item{local_path}{\code{character}. The local file path where the data is (or going to be) stored.}
#'   \item{downloaded}{\code{logical}. Indicator of whether the data file has been downloaded locally. This is only available if `check_local_files` is `TRUE`.}
#' }
#' @export
#' @examplesIf interactive()
#' \donttest{
#'
#' # Set data dir for file downloads
#' spod_set_data_dir(tempdir())
#'
#' # Get available data list for v1 (2020-2021) data
#' spod_available_data(ver = 1)
#'
#' # Get available data list for v2 (2022 onwards) data
#' spod_available_data(ver = 2)
#'
#' # Get available data list for v2 (2022 onwards) data
#' # while also checking for local files that are already downloaded
#' spod_available_data(ver = 2, check_local_files = TRUE)
#' }
#'
spod_available_data <- function(
  ver = 2,
  check_local_files = FALSE,
  quiet = FALSE,
  data_dir = spod_get_data_dir(),
  use_s3 = TRUE,
  force = FALSE
) {
  # Validate input
  checkmate::assertIntegerish(ver, max.len = 1)
  if (!ver %in% c(1, 2)) {
    stop(
      "Invalid version number. Must be 1 (for v1 2020-2021 data) or 2 (for v2 2022 onwards)."
    )
  }
  checkmate::assert_flag(check_local_files)
  checkmate::assert_flag(quiet)
  checkmate::assert_directory_exists(data_dir, access = "rw")

  if (ver == 1) {
    available_data <- spod_available_data_v1(
      data_dir = data_dir,
      check_local_files = check_local_files,
      quiet = quiet,
      use_s3 = use_s3,
      force = force
    )
  } else if (ver == 2) {
    available_data <- spod_available_data_v2(
      data_dir = data_dir,
      check_local_files = check_local_files,
      quiet = quiet,
      use_s3 = use_s3,
      force = force
    )
  }

  return(available_data)
}

#' Get latest file list from the XML for MITMA open mobility data v1 (2020-2021)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://opendata-movilidad.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @keywords internal
spod_get_latest_v1_file_list <- function(
  data_dir = spod_get_data_dir(),
  xml_url = "https://opendata-movilidad.mitma.es/RSS.xml"
) {
  if (!dir.exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue(
    "{data_dir}/{spod_subfolder_metadata_cache()}/data_links_v1_{current_date}.xml"
  )

  # ensure dir exists
  if (!dir.exists(dirname(current_filename))) {
    fs::dir_create(dirname(current_filename), recurse = TRUE)
  }

  message("Saving the file to: ", current_filename)
  utils::download.file(xml_url, current_filename, mode = "wb")
  # disable curl::multi_download() for now
  # xml_requested <- curl::multi_download(
  #   urls = xml_url,
  #   destfiles = current_filename
  # )
  if (!fs::file_exists(current_filename)) {
    stop("Failed to download XML file.")
  }
  return(current_filename)
}

#' Get the available v1 data list
#'
#' This function provides a table of the available data list of MITMA v1 (2020-2021), both remote and local.
#'
#' @inheritParams spod_available_data
#' @inheritParams global_quiet_param
#' @inherit spod_available_data return
#' @importFrom rlang .data
#' @keywords internal
spod_available_data_v1 <- function(
  data_dir = spod_get_data_dir(),
  # check_local_files (below) is FALSE by default to avoid excessive filesystem access, perhaps should be TRUE. Download functions use it to load the xml file, but we probably do not want the script to check all local cache directories every time we run a get data function. Perhaps it is better to offload this check to a separate function and have a csv file or some other way to keep track of the files that were downloaded and cached. An output of curl::multi_download() could be used for this purpose.
  check_local_files = FALSE,
  use_s3 = TRUE,
  force = FALSE,
  quiet = FALSE
) {
  metadata_folder <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}")
  if (!dir.exists(metadata_folder)) {
    fs::dir_create(metadata_folder)
  }

  s3_successful <- FALSE

  if (use_s3) {
    files_table <- tryCatch(
      {
        files_table_s3 <- spod_available_data_s3(
          ver = 1,
          force = force,
          quiet = quiet
        )
        s3_successful <- TRUE
        files_table_s3
      },
      error = function(e) {
        message(
          "S3 fetch failed (",
          e$message,
          "); falling back to XML sequence."
        )
        read_data_links_xml(
          metadata_folder = metadata_folder,
          data_dir = data_dir,
          force = force,
          quiet = quiet,
          latest_file_function = spod_get_latest_v1_file_list
        )
      }
    )
  } else {
    files_table <- read_data_links_xml(
      metadata_folder = metadata_folder,
      data_dir = data_dir,
      force = force,
      quiet = quiet,
      latest_file_function = spod_get_latest_v1_file_list
    )
  }

  files_table$file_extension <- tools::file_ext(files_table$target_url)
  files_table <- files_table[files_table$file_extension != "", ]

  files_table$data_ym <- lubridate::ym(stringr::str_extract(
    files_table$target_url,
    "[0-9]{4}-[0-9]{2}"
  ))
  files_table$data_ymd <- lubridate::ymd(stringr::str_extract(
    files_table$target_url,
    "[0-9]{8}"
  ))
  # order by pub_ts
  files_table <- files_table[order(files_table$pub_ts, decreasing = TRUE), ]
  files_table$local_path <- file.path(
    data_dir,
    stringr::str_replace(
      files_table$target_url,
      ".*mitma.es/",
      spod_subfolder_raw_data_cache(ver = 1)
    )
  )

  files_table$local_path <- stringr::str_replace_all(
    files_table$local_path,
    "\\/\\/\\/|\\/\\/",
    "/"
  )

  # change path for daily data files to be in hive-style format
  files_table$local_path <- gsub(
    "([0-9]{4})-([0-9]{2})\\/[0-9]{6}([0-9]{2})_",
    "year=\\1\\/month=\\2\\/day=\\3\\/",
    files_table$local_path
  )

  # fix paths for files that are in '0000-referencia' folder
  files_table$local_path <- gsub(
    "0000-referencia\\/([0-9]{4})([0-9]{2})([0-9]{2})_",
    "year=\\1\\/month=\\2\\/day=\\3\\/",
    files_table$local_path
  )

  # replace 2 digit month with 1 digit month
  files_table$local_path <- gsub(
    "month=0([1-9])",
    "month=\\1",
    files_table$local_path
  )

  # replace 2 digit day with 1 digit day
  files_table$local_path <- gsub(
    "day=0([1-9])",
    "day=\\1",
    files_table$local_path
  )

  # change txt.gz to csv.gz
  files_table$local_path <- gsub(
    "\\.txt\\.gz",
    "\\.csv\\.gz",
    files_table$local_path
  )

  # replace all municipal data download links with districts links
  # this is to address the bugs described in detail in:
  # http://www.ekotov.pro/mitma-data-issues/issues/011-v1-tpp-mismatch-zone-ids-in-table-and-spatial-data.html
  # http://www.ekotov.pro/mitma-data-issues/issues/012-v1-tpp-district-files-in-municipality-folders.html
  # the decision was to use distrcit data and aggregate it to replicate municipal data
  files_table$target_url <- gsub(
    "mitma-municipios",
    "mitma-distritos",
    files_table$target_url
  )
  files_table$target_url <- gsub(
    "mitma_municipio",
    "mitma_distrito",
    files_table$target_url
  )

  files_table <- files_table |>
    dplyr::mutate(
      study = factor(
        dplyr::case_when(
          grepl("maestra", .data$target_url) ~ "basic",
          TRUE ~ NA_character_
        ),
        levels = c("basic")
      ),

      type = factor(
        dplyr::case_when(
          grepl("maestra2", .data$target_url) ~ "number_of_trips",
          grepl("maestra1", .data$target_url) ~ "origin-destination",
          grepl("RSS\\.xml", .data$target_url) ~ "metadata",
          grepl("zonificacion", .data$target_url) ~ "zones",
          grepl("relacion", .data$target_url) ~ "relations",
          grepl("index\\.html", .data$target_url) ~ "index",
          grepl("\\.pdf", .data$target_url) ~ "documentation",
          TRUE ~ NA_character_
        ),
        levels = c(
          "number_of_trips",
          "origin-destination",
          "metadata",
          "zones",
          "relations",
          "index",
          "documentation"
        )
      ),

      period = factor(
        dplyr::case_when(
          grepl("ficheros-diarios", .data$target_url) ~ "day",
          grepl("meses-completos|mensual", .data$target_url) ~ "month",
          TRUE ~ NA_character_
        ),
        levels = c("day", "month")
      ),

      zones = factor(
        dplyr::case_when(
          grepl("distrito", .data$target_url) ~ "districts",
          grepl("municipio", .data$target_url) ~ "municipalities",
          TRUE ~ NA_character_
        ),
        levels = c("districts", "municipalities")
      )
    )

  # add known file sizes from cached data
  if (s3_successful) {
    # replace remote file sizes for v1
    replacement_file_sizes_distr <- files_table |>
      dplyr::filter(grepl("mitma-distr", .data$local_path)) |>
      dplyr::select(.data$target_url, .data$file_size_bytes)
    replaced_file_sizes_municip <- files_table |>
      dplyr::filter(grepl("mitma-municip", .data$local_path)) |>
      dplyr::select(-"file_size_bytes") |>
      dplyr::left_join(replacement_file_sizes_distr, by = "target_url")
    files_table_replaced_file_sizes <- files_table |>
      dplyr::filter(!grepl("mitma-municip", .data$local_path)) |>
      dplyr::bind_rows(replaced_file_sizes_municip) |>
      dplyr::arrange(dplyr::desc(.data$pub_ts))
    files_table <- files_table_replaced_file_sizes

    files_table$remote_file_size_mb <- round(
      files_table$file_size_bytes / 1024^2,
      2
    )

    file_sizes <- readRDS(
      system.file(
        "extdata",
        "available_data_v1.rds",
        package = "spanishoddata"
      )
    )
    files_table <- dplyr::left_join(
      files_table |> dplyr::select(-"file_size_bytes"),
      file_sizes |>
        dplyr::select(
          "target_url",
          "etag",
          "true_etag",
          file_size_bytes = "true_remote_file_size_bytes"
        ),
      by = c("target_url", "etag")
    ) |>
      dplyr::relocate("file_size_bytes", .after = "pub_ts") |>
      dplyr::mutate(
        etag = dplyr::if_else(
          condition = !is.na(.data$true_etag),
          true = .data$true_etag,
          false = .data$etag
        )
      ) |>
      dplyr::select(-"true_etag")
  } else {
    file_sizes <- readRDS(
      system.file(
        "extdata",
        "available_data_v1.rds",
        package = "spanishoddata"
      )
    )
    files_table <- dplyr::left_join(
      files_table |> dplyr::select(-"file_size_bytes"),
      file_sizes |>
        dplyr::select(
          "target_url",
          "etag",
          "true_etag",
          file_size_bytes = "true_remote_file_size_bytes"
        ),
      by = c("target_url", "etag")
    ) |>
      dplyr::relocate("file_size_bytes", .after = "pub_ts") |>
      dplyr::mutate(
        etag = dplyr::if_else(
          condition = !is.na(.data$true_etag),
          true = .data$true_etag,
          false = .data$etag
        )
      ) |>
      dplyr::select(-"true_etag")

    # if there are files with missing sizes, impute them
    if (any(is.na(files_table$remote_file_size_mb))) {
      # impute uknown file sizes
      # primitive file categorisation
      # Extract file category from the target URL
      files_table <- files_table |>
        dplyr::mutate(
          file_category = stringr::str_extract(
            .data$target_url,
            "\\/maestra(\\d)-mitma-(distritos|municipios)\\/(ficheros-diarios|meses-completos)\\/"
          )
        )

      # Set other category for non-categorized files
      files_table$file_category[is.na(files_table$file_category)] <- "other"

      # Calculate mean file sizes by category
      size_by_file_category <- files_table |>
        dplyr::group_by(.data$file_category) |>
        dplyr::summarise(
          mean_file_size_mb = mean(.data$remote_file_size_mb, na.rm = TRUE)
        )

      # Impute missing file sizes
      files_table <- files_table |>
        dplyr::left_join(size_by_file_category, by = "file_category")
      files_table$remote_file_size_mb[is.na(
        files_table$remote_file_size_mb
      )] <- mean(files_table$mean_file_size_mb)

      # Clean up temporary columns
      files_table <- files_table |>
        dplyr::select(-"mean_file_size_mb", -"file_category")
    }
  }

  # check file sizes
  if (check_local_files == TRUE) {
    files_table <- files_table |>
      dplyr::mutate(
        local_file_size = fs::file_size(.data$local_path)
      ) |>
      dplyr::mutate(
        downloaded = dplyr::if_else(
          condition = is.na(.data$local_file_size),
          true = FALSE,
          false = TRUE
        )
      )
  }

  return(files_table)
}

#' Get latest file list from the XML for MITMA open mobility data v2 (2022 onwards)
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @param xml_url The URL of the XML file to download. Defaults to "https://movilidad-opendata.mitma.es/RSS.xml".
#'
#' @return The path to the downloaded XML file.
#' @keywords internal
spod_get_latest_v2_file_list <- function(
  data_dir = spod_get_data_dir(),
  xml_url = "https://movilidad-opendata.mitma.es/RSS.xml"
) {
  if (!dir.exists(data_dir)) {
    fs::dir_create(data_dir)
  }

  current_date <- format(Sys.Date(), format = "%Y-%m-%d")
  current_filename <- glue::glue(
    "{data_dir}/{spod_subfolder_metadata_cache()}/data_links_v2_{current_date}.xml"
  )

  # ensure dir exists
  if (!dir.exists(dirname(current_filename))) {
    fs::dir_create(dirname(current_filename), recurse = TRUE)
  }

  message("Saving the file to: ", current_filename)
  utils::download.file(xml_url, current_filename, mode = "wb")
  # disable curl::multi_download() for now
  # xml_requested <- curl::multi_download(
  #   urls = xml_url,
  #   destfiles = current_filename
  # )
  if (!fs::file_exists(current_filename)) {
    stop("Failed to download the XML file.")
  }

  return(current_filename)
}

#' Get the data dictionary
#'
#' This function retrieves the data dictionary for the specified data directory.
#'
#' @param data_dir The directory where the data is stored. Defaults to the value returned by `spod_get_data_dir()`.
#' @inheritParams spod_available_data_v1
#' @inheritParams global_quiet_param
#' @inherit spod_available_data return
#' @importFrom rlang .data
#' @keywords internal
spod_available_data_v2 <- function(
  data_dir = spod_get_data_dir(),
  check_local_files = FALSE,
  use_s3 = TRUE,
  force = FALSE,
  quiet = FALSE
) {
  metadata_folder <- glue::glue("{data_dir}/{spod_subfolder_metadata_cache()}")
  if (!dir.exists(metadata_folder)) {
    fs::dir_create(metadata_folder)
  }

  s3_successful <- FALSE

  if (use_s3) {
    files_table <- tryCatch(
      {
        files_table_s3 <- spod_available_data_s3(
          ver = 2,
          force = force,
          quiet = quiet
        )
        s3_successful <- TRUE
        files_table_s3
      },
      error = function(e) {
        message(
          "S3 fetch failed (",
          e$message,
          "); falling back to XML sequence."
        )
        read_data_links_memoised(
          metadata_folder = metadata_folder,
          data_dir = data_dir,
          force = force,
          quiet = quiet,
          latest_file_function = spod_get_latest_v2_file_list
        )
      }
    )
  } else {
    files_table <- read_data_links_memoised(
      metadata_folder = metadata_folder,
      data_dir = data_dir,
      force = force,
      quiet = quiet,
      latest_file_function = spod_get_latest_v2_file_list
    )
  }

  files_table$file_extension <- tools::file_ext(files_table$target_url)
  files_table <- files_table[files_table$file_extension != "", ]

  files_table$data_ym <- lubridate::ym(stringr::str_extract(
    files_table$target_url,
    "[0-9]{4}-[0-9]{2}"
  ))
  files_table$data_ymd <- lubridate::ymd(stringr::str_extract(
    files_table$target_url,
    "[0-9]{8}"
  ))
  # order by pub_ts
  files_table <- files_table[order(files_table$pub_ts, decreasing = TRUE), ]
  files_table$local_path <- file.path(
    data_dir,
    stringr::str_replace(
      files_table$target_url,
      ".*mitma.es/",
      spod_subfolder_raw_data_cache(ver = 2)
    )
  )
  files_table$local_path <- stringr::str_replace_all(
    files_table$local_path,
    "\\/\\/\\/|\\/\\/",
    "/"
  )

  # change path for daily data files to be in hive-style format
  # TODO: check if this is needed for estudios completo and rutas
  files_table$local_path <- gsub(
    "([0-9]{4})-([0-9]{2})\\/[0-9]{6}([0-9]{2})_",
    "year=\\1\\/month=\\2\\/day=\\3\\/",
    files_table$local_path
  )

  # replace 2 digit month with 1 digit month
  files_table$local_path <- gsub(
    "month=0([1-9])",
    "month=\\1",
    files_table$local_path
  )

  # replace 2 digit day with 1 digit day
  files_table$local_path <- gsub(
    "day=0([1-9])",
    "day=\\1",
    files_table$local_path
  )

  # lowercase GAU to avoid problems with case-sensitive matching
  files_table$local_path <- gsub("GAU", "gau", files_table$local_path)

  files_table <- files_table |>
    dplyr::mutate(
      study = factor(
        dplyr::case_when(
          grepl("estudios_basicos", .data$target_url) ~ "basic",
          grepl("estudios_completos", .data$target_url) ~ "complete",
          grepl("rutas", .data$target_url) ~ "routes",
          TRUE ~ NA_character_
        ),
        levels = c("basic", "complete", "routes")
      ),

      type = factor(
        dplyr::case_when(
          grepl("personas", .data$target_url) ~ "number_of_trips",
          grepl("viajes", .data$target_url) ~ "origin-destination",
          grepl("pernoctaciones", .data$target_url) ~ "overnight_stays",
          grepl("calidad", .data$target_url) ~ "data_quality",
          grepl("RSS\\.xml", .data$target_url) ~ "metadata",
          TRUE ~ NA_character_
        ),
        levels = c(
          "origin-destination",
          "number_of_trips",
          "overnight_stays",
          "data_quality",
          "metadata"
        )
      ),

      period = factor(
        dplyr::case_when(
          grepl("ficheros-diarios", .data$target_url) ~ "day",
          grepl("meses-completos|mensual", .data$target_url) ~ "month",
          TRUE ~ NA_character_
        ),
        levels = c("day", "month")
      ),

      zones = factor(
        dplyr::case_when(
          grepl("distritos", .data$target_url) ~ "districts",
          grepl("municipios", .data$target_url) ~ "municipalities",
          grepl("GAU", .data$target_url) ~ "large_urban_areas",
          TRUE ~ NA_character_
        ),
        levels = c("districts", "municipalities", "large_urban_areas")
      )
    )

  # add known file sizes from cached data
  if (s3_successful) {
    files_table$remote_file_size_mb <- round(
      files_table$file_size_bytes / 1024^2,
      2
    )
  } else {
    file_sizes <- readr::read_csv(
      system.file(
        "extdata",
        "url_file_sizes_v2.txt.gz",
        package = "spanishoddata"
      ),
      show_col_types = FALSE
    )
    files_table <- dplyr::left_join(files_table, file_sizes, by = "target_url")

    # if there are files with missing sizes, impute them
    if (any(is.na(files_table$remote_file_size_mb))) {
      # impute uknown file sizes
      # primitive file categorisation
      files_table <- files_table |>
        dplyr::mutate(
          cleaned_url = stringr::str_remove_all(
            .data$target_url,
            "/[0-9]{4}[-_][0-9]{2}[-_][0-9]{2}|/[0-9]{6,8}"
          ) |>
            stringr::str_remove("/[^/]+$"),
          file_category = dplyr::case_when(
            stringr::str_detect(.data$cleaned_url, "calidad") ~ "quality",
            stringr::str_detect(.data$cleaned_url, "rutas") ~ "routes",
            stringr::str_detect(.data$cleaned_url, "estudios_basicos") ~
              paste0(
                "basic_studies_",
                dplyr::case_when(
                  stringr::str_detect(.data$cleaned_url, "por-distritos") ~
                    "district_",
                  stringr::str_detect(.data$cleaned_url, "por-municipios") ~
                    "municipal_",
                  stringr::str_detect(.data$cleaned_url, "por-GAU") ~ "GAU_",
                  TRUE ~ "unknown_"
                ),
                dplyr::case_when(
                  stringr::str_detect(.data$cleaned_url, "viajes") ~ "trips_",
                  stringr::str_detect(.data$cleaned_url, "personas") ~
                    "people_",
                  stringr::str_detect(.data$cleaned_url, "pernoctaciones") ~
                    "overnight_",
                  TRUE ~ "unknown_"
                ),
                ifelse(
                  stringr::str_detect(.data$cleaned_url, "ficheros-diarios"),
                  "daily",
                  "monthly"
                )
              ),
            TRUE ~ "other"
          )
        ) |>
        dplyr::select(-"cleaned_url")

      # Calculate mean file sizes by category
      size_by_file_category <- files_table |>
        dplyr::group_by(.data$file_category) |>
        dplyr::summarise(
          mean_file_size_mb = mean(.data$remote_file_size_mb, na.rm = TRUE)
        )

      # Impute missing file sizes
      files_table <- dplyr::left_join(
        files_table,
        size_by_file_category,
        by = "file_category"
      )
      files_table <- files_table |>
        dplyr::mutate(
          size_imputed = ifelse(is.na(.data$remote_file_size_mb), TRUE, FALSE)
        )
      if (
        length(files_table$remote_file_size_mb[is.na(
          files_table$remote_file_size_mb
        )]) >
          0
      ) {
        files_table <- files_table |>
          dplyr::mutate(
            remote_file_size_mb = ifelse(
              is.na(.data$remote_file_size_mb),
              .data$mean_file_size_mb,
              .data$remote_file_size_mb
            )
          )
      }
      files_table$mean_file_size_mb <- NULL
      files_table$file_category <- NULL
    } else {
      files_table$size_imputed <- FALSE
    }
  }

  # check file sizes
  if (check_local_files == TRUE) {
    files_table <- files_table |>
      dplyr::mutate(
        local_file_size = fs::file_size(.data$local_path)
      ) |>
      dplyr::mutate(
        downloaded = dplyr::if_else(
          condition = is.na(.data$local_file_size),
          true = FALSE,
          false = TRUE
        )
      )
  }

  return(files_table)
}

read_data_links_xml <- function(
  metadata_folder,
  data_dir,
  force = FALSE,
  quiet = FALSE,
  latest_file_function
) {
  xml_files_list <- fs::dir_ls(
    metadata_folder,
    type = "file",
    regexp = "data_links_v1"
  ) |>
    sort()
  latest_file <- utils::tail(xml_files_list, 1)

  needs_update <- isTRUE(force) ||
    length(xml_files_list) == 0 ||
    as.Date(
      stringr::str_extract(latest_file, "\\d{4}-\\d{2}-\\d{2}")
    ) <
      Sys.Date()

  if (needs_update) {
    if (!quiet) {
      message("Fetching latest data links xml")
    }
    latest_data_links_xml_path <- latest_file_function(
      data_dir = data_dir
    )
  } else {
    if (!quiet) {
      message("Using existing data links xml: ", latest_file)
    }
    latest_data_links_xml_path <- latest_file
  }

  x_xml <- xml2::read_xml(latest_data_links_xml_path)
  files_table <- tibble::tibble(
    target_url = xml2::xml_find_all(x_xml, "//link") |> xml2::xml_text(),
    pub_date = xml2::xml_find_all(x_xml, "//pubDate") |> xml2::xml_text()
  )
  files_table$pub_ts <- lubridate::dmy_hms(files_table$pub_date)
  files_table$pub_date <- NULL

  files_table
}

read_data_links_memoised <- memoise::memoise(read_data_links_xml)
