#' Convert data from plain text to duckdb or parquet format
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Converts data for faster analysis into either `DuckDB` file or into `parquet` files in a hive-style directory structure. Running analysis on these files is sometimes 100x times faster than working with raw CSV files, espetially when these are in gzip archives. To connect to converted data, please use 'mydata <- \link{spod_connect}(data_path = path_returned_by_spod_convert)' passing the path to where the data was saved. The connected `mydata` can be analysed using `dplyr` functions such as \link[dplyr]{select}, \link[dplyr]{filter}, \link[dplyr]{mutate}, \link[dplyr]{group_by}, \link[dplyr]{summarise}, etc. In the end of any sequence of commands you will need to add \link[dplyr]{collect} to execute the whole chain of data manipulations and load the results into memory in an R `data.frame`/`tibble`. For more in-depth usage of such data, please refer to DuckDB documentation and examples at [https://duckdb.org/docs/api/r#dbplyr](https://duckdb.org/docs/api/r#dbplyr) . Some more useful examples can be found here [https://arrow-user2022.netlify.app/data-wrangling#combining-arrow-with-duckdb](https://arrow-user2022.netlify.app/data-wrangling#combining-arrow-with-duckdb) . You may also use `arrow` package to work with parquet files [https://arrow.apache.org/docs/r/](https://arrow.apache.org/docs/r/).
#'
#' @param save_format A `character` vector of length 1 with values "duckdb" or "parquet". Defaults to "duckdb". If `NULL` automatically inferred from the `save_path` argument. If only `save_format` is provided, `save_path` will be set to the default location set in `SPANISH_OD_DATA_DIR` environment variable using \link{spod_set_data_dir}`(path = 'path/to/your/cache/dir')`. So for v1 data that path would be `<data_dir>/clean_data/v1/tabular/duckdb/` or `<data_dir>/clean_data/v1/tabular/parquet/`.
#'
#' You can also set `save_path`. If it ends with ".duckdb", will save to `DuckDB` database format, if `save_path` does not end with ".duckdb", will save to `parquet` format and will treat the `save_path` as a path to a folder, not a file, will create necessary hive-style subdirectories in that folder. Hive style looks like `year=2020/month=2/day=14` and inside each such directory there will be a `data_0.parquet` file that contains the data for that day.
#'
#' @param save_path A `character` vector of length 1. The full (not relative) path to a `DuckDB` database file or `parquet` folder.
#'
#' * If `save_path` ends with `.duckdb`, it will be saved as a DuckDB database file. The format argument will be automatically set to `save_format='duckdb'`.
#'
#' * If `save_path` ends with a folder name (e.g. `/data_dir/clean_data/v1/tabular/parquet/od_distr` for origin-destination data for district level), the data will be saved as a collection of `parquet` files in a hive-style directory structure. So the subfolders of `od_distr` will be `year=2020/month=2/day=14` and inside each of these folders a single `parquet` file will be placed containing the data for that day.
#'
#' * If `NULL`, uses the default location in `data_dir` (set by the `SPANISH_OD_DATA_DIR` environment variable using \link{spod_set_data_dir}`(path = 'path/to/your/cache/dir')`. Therefore, the default relative path for `DuckDB` is `<data_dir>/clean_data/v1/tabular/duckdb/<type>_<zones>.duckdb` and for `parquet` files is `<data_dir>/clean_data/v1/tabular/parquet/<type>_<zones>/`, where `type` is the type of data (e.g. 'od', 'os', 'nt', that correspoind to 'origin-destination', 'overnight-stays', 'number-of-trips', etc.) and `zones` is the name of the geographic zones (e.g. 'distr', 'muni', etc.). See the details below in the function arguments description.
#'
#' @inheritParams spod_get_zones
#' @inheritParams spod_download
#' @inheritParams spod_get
#' @inheritParams spod_duckdb_limit_resources
#' @param overwrite A `logical` or a `character` vector of length 1. If `TRUE`, overwrites existing `DuckDB` or `parquet` files. Defaults to `FALSE`. For parquet files can also be set to 'update', so that only parquet files are only created for the dates that have not yet been converted.
#' @inheritParams global_quiet_param
#' @return Path to saved `DuckDB` database file or to a folder with `parquet` files in hive-style directory structure.
#' @export
#'
#' @examplesIf interactive()
#' \donttest{
#' # Set data dir for file downloads
#' spod_set_data_dir(tempdir())
#'
#' # download and convert data
#' dates_1 <- c(start = "2020-02-17", end = "2020-02-18")
#' db_2 <- spod_convert(
#'  type = "number_of_trips",
#'  zones = "distr",
#'  dates = dates_1,
#'  overwrite = TRUE
#' )
#'
#' # now connect to the converted data
#' my_od_data_2 <- spod_connect(db_2)
#'
#' # disconnect from the database
#' spod_disconnect(my_od_data_2)
#' }
#'
spod_convert <- function(
  type = c(
    "od",
    "origin-destination",
    "os",
    "overnight_stays",
    "nt",
    "number_of_trips"
  ),
  zones = c(
    "districts",
    "dist",
    "distr",
    "distritos",
    "municipalities",
    "muni",
    "municip",
    "municipios"
  ),
  dates = NULL,
  save_format = "duckdb",
  save_path = NULL,
  overwrite = FALSE,
  data_dir = spod_get_data_dir(),
  quiet = FALSE,
  max_mem_gb = NULL,
  max_n_cpu = max(1, parallelly::availableCores() - 1),
  max_download_size_gb = 1,
  ignore_missing_dates = FALSE
) {
  # Validate inputs
  checkmate::assert_choice(
    type,
    choices = c(
      "od",
      "origin-destination",
      "os",
      "overnight_stays",
      "nt",
      "number_of_trips"
    )
  )
  checkmate::assert_choice(
    zones,
    choices = c(
      "districts",
      "dist",
      "distr",
      "distritos",
      "municipalities",
      "muni",
      "municip",
      "municipios",
      "lua",
      "large_urban_areas",
      "gau",
      "grandes_areas_urbanas"
    )
  )
  checkmate::assert_choice(
    save_format,
    choices = c("duckdb", "parquet"),
    null.ok = TRUE
  )
  checkmate::assert_character(save_path, len = 1, null.ok = TRUE)
  checkmate::assert_flag(overwrite)
  checkmate::assert_directory_exists(data_dir, access = "rw")
  checkmate::assert_flag(quiet)
  checkmate::assert_number(max_mem_gb, lower = 0.1, null.ok = TRUE)
  checkmate::assert_integerish(max_n_cpu, lower = 1)
  checkmate::assert_number(max_download_size_gb, lower = 0.1)
  checkmate::assert_flag(ignore_missing_dates)

  # simple null check is enough here, as spod_dates_arugument_to_dates_seq will do additional checks anyway
  if (is.null(dates)) {
    message(
      "No period specified in the `dates` argument. Please set `dates='cached_v1'` or `dates='cached_v2'` to convert all data that was previously downloaded. Alternatively, specify at least one date between 2020-02-14 and 2021-05-09 (for v1 data) or between 2022-01-01 onwards (for v2). Any missing data will be downloaded before conversion. For more details on the dates argument, see ?spod_convert."
    )
  }

  dates <- spod_dates_argument_to_dates_seq(dates = dates)
  # check if user is requesting to just get all cached data
  cached_data_requested <- length(dates) == 1 &&
    all(as.character(dates) %in% c("cached_v1", "cached_v2"))

  ver <- spod_infer_data_v_from_dates(
    dates = dates,
    ignore_missing_dates = ignore_missing_dates
  )

  # normalise type
  type <- spod_match_data_type(type = type)

  # normalise zones
  zones <- spod_zone_names_en2es(zones)

  # check format
  if (length(save_format) > 1) {
    stop(
      "Invalid save_format. Please select 'duckdb' or 'parquet', or leave empty to use the default, which is 'duckdb'."
    )
  }
  # if (is.null(save_format) & is.null(save_path)) {
  #   save_format <- "duckdb"
  # } # maybe don't need this, see line with
  # infer format from `save_path`
  # below
  if (!save_format %in% c("duckdb", "parquet")) {
    stop(
      "Invalid save_format. Please set `save_format` to 'duckdb' or 'parquet', or leave empty to use the default, which is 'duckdb'."
    )
  }

  # check for inconsistencies between save_format and save_path
  if (!is.null(save_format) & !is.null(save_path)) {
    if (!grepl("duckdb$", save_path) & save_format == "duckdb") {
      stop(
        "`save_path` is set to a folder, while `save_format` is set to 'duckdb'. Please make sure that `save_format` also ends with '.duckdb' when setting format to 'duckdb', or leave `save_format` empty, so that it is inferred from `save_path`. Perhaps you wanted to save to `parquet` files? Then please set `save_format` to 'parquet', or leave `save_format` empty."
      )
    }
  }

  # `save_path` is NULL, format 'duckdb'
  if (is.null(save_path) & save_format == "duckdb") {
    save_path <- fs::path(
      data_dir,
      spod_subfolder_clean_data_cache(ver = ver),
      "tabular/duckdb/",
      glue::glue("{type}_{zones}.duckdb")
    )
    if (isFALSE(quiet)) {
      message("Using default save_path: ", save_path)
    }
  }

  # `save_path` is NULL, format 'parquet'
  if (is.null(save_path) & save_format == "parquet") {
    save_path <- fs::path(
      data_dir,
      spod_subfolder_clean_data_cache(ver = ver),
      "tabular/parquet/",
      glue::glue("{type}_{zones}/")
    )
    if (isFALSE(quiet)) {
      message("Using default save_path: ", save_path)
    }
  }

  # infer format from `save_path`
  if (is.null(save_format)) {
    if (grepl("\\.duckdb$", save_path)) {
      save_format <- "duckdb"
      save_dir <- fs::path_dir(save_path)
      duckdb_target <- save_path
      # else check if it is a path to a folder
      # basically check that the path does not end with any other file extension other than .duckdb and then consider it as path to folder
    } else if (!grepl('\\.[a-zA-Z0-9]+$', save_path)) {
      save_format <- "parquet"
    }
  }

  # make sure there is no trailing slash for parquet
  if (save_format == "parquet") {
    save_dir <- gsub("\\/$", "", save_path)
    duckdb_target <- ":memory:"
  } else if (save_format == "duckdb") {
    save_dir <- fs::path_dir(save_path)
    duckdb_target <- save_path
  }

  if (!dir.exists(save_dir)) {
    fs::dir_create(save_dir, recurse = TRUE)
  }

  # check if overwrite update conflicts with `save_format`
  if (overwrite == "update" & save_format == "duckdb") {
    stop(
      "You are trying to save to a duckdb file with `overwrite = 'update'`. This is not supported. Please set `overwrite=TRUE` to remove the existing duckdb file and create a new one with the requested data."
    )
  }

  # check if duckdb file already exists
  # for conversion to duckdb we need to initialise a writeable database file
  # therefore  this check is before we create a database with `spod_get()` below
  if (save_format == "duckdb" && fs::file_exists(save_path)) {
    if (!overwrite) {
      message("Duckdb file already exists: ", save_path)
      response <- readline(prompt = "Overwrite existing duckdb file? (yes/no) ")
      overwrite <- tolower(response) %in% c("y", "yes", "Yes")

      if (!overwrite) {
        message(glue::glue(
          "Exiting without overwriting existing duckdb file. You may delete it from {save_path} manually and rerun the function. Or rerun the function with `overwrite = TRUE`."
        ))
        return()
      }
    }
    if (overwrite) {
      if (isFALSE(quiet)) {
        message(glue::glue("Overwriting existing duckdb file: ", save_path))
      }
      fs::file_delete(save_path)
    }
  }

  # create duckdb view with a file target
  con <- spod_get(
    type = type,
    zones = zones,
    dates = dates,
    data_dir = data_dir,
    quiet = quiet,
    max_mem_gb = max_mem_gb,
    max_n_cpu = max_n_cpu,
    max_download_size_gb = max_download_size_gb,
    duckdb_target = duckdb_target,
    ignore_missing_dates = ignore_missing_dates
  )

  # resolve the actual database connection from the returned table
  db_con <- con$src$con

  if (isTRUE(cached_data_requested)) {
    table_to_convert <- glue::glue("{type}_csv_clean")
  } else {
    table_to_convert <- glue::glue("{type}_csv_clean_filtered")
  }

  if (isFALSE(quiet)) {
    if (!is.null(max_mem_gb)) {
      message(glue::glue(
        "Using {max_mem_gb} GB of memory and {max_n_cpu} threads. You may adjust this using the function arguments `max_mem_gb` and `max_n_cpu`."
      ))
    } else {
      max_mem_duckdb <- DBI::dbGetQuery(
        db_con,
        "SELECT current_setting('memory_limit');"
      )[
        1,
      ]
      message(glue::glue(
        "Using DuckDB's default of 80% of operating memory (currently detected as {max_mem_duckdb}) and {max_n_cpu} threads. You may adjust this using the function arguments `max_mem_gb` and `max_n_cpu`."
      ))
    }

    message(glue::glue(
      "Converting v{ver} {type} data for {zones} to {save_format}: {save_path} \n..."
    ))
  }
  # add some indication on how long it may take from empirical experimentation
  # hopefully, the progress_bar feature will be implemented in duckdb R package soon, bug filed here https://github.com/duckdb/duckdb-r/issues/199
  # if I change the code to import data in batches it slows down significantly

  if (save_format == "duckdb") {
    # convert to ducdkb
    # prepare SQL query for converting to duckdb
    target_table_name <- gsub("\\..*", "", basename(save_path)) # experimental
    sql_import_query <- dplyr::sql(
      # glue::glue("CREATE TABLE {type} AS SELECT * FROM {table_to_convert} ;") should be standard
      glue::glue(
        'CREATE TABLE "{target_table_name}" AS SELECT * FROM {table_to_convert} ;'
      ) # experimental - for the user friendly duckdb connection wrapper so that it can guess the table name from the file name, hopefully the user will not rename it
    )

    # import view of CSV files into duckdb
    DBI::dbExecute(
      db_con,
      statement = sql_import_query
    )
    # drop the views to csv files
    tables_list <- DBI::dbListTables(db_con)
    # find all views that contain csv in any case
    tables_list <- tables_list[stringr::str_detect(tables_list, "csv")]
    if (length(tables_list) > 0) {
      for (i in 1:length(tables_list)) {
        sql_drop_view <- dplyr::sql(
          glue::glue(
            "DROP VIEW IF EXISTS {tables_list[i]};"
          )
        )
        DBI::dbExecute(
          db_con,
          sql_drop_view
        )
      }
    }
  }

  # if save_format is parquet
  need_to_return_parquet_files <- FALSE # will change in the if below if needed
  if (save_format == "parquet") {
    if (overwrite == FALSE | overwrite == 'update') {
      # check if there are any existing parquet files in the save_path
      parquet_files <- fs::dir_ls(
        save_path,
        regexp = "\\.parquet$",
        recurse = TRUE
      )
      if (overwrite == FALSE & length(parquet_files) > 0) {
        message(
          "You have set `overwrite = FALSE`, but there are already parquet files in ",
          save_path
        )
        response <- readline(
          "What should be done? [D]elete all existing files in the target folder and convert all requested data from scratch? [U]pdate the folder with any new data while keeping the existing files? [C]ancel and quit? (D/U/C): "
        )
      }
      if (overwrite == 'update' & length(parquet_files) > 0) {
        response <- overwrite
      }
      if (
        (overwrite == 'update' | overwrite == FALSE) &
          length(parquet_files) == 0
      ) {
        response <- 'skip'
        overwrite <- TRUE
      }

      if (tolower(response) %in% c("c", "cancel")) {
        message(
          "Cancelled by the user. Exiting without overwriting existing parquet files. You may delete them from ",
          save_path,
          " manually and rerun the function. Or rerun the function with `overwrite = TRUE` or `overwrite = 'update'`."
        )
        return(invisible(NULL))
      }

      if (tolower(response) %in% c("d", "delete")) {
        # set overwrite true, so that DuckDB to parquet export will overwrite the existing files, but also just to be on the safe side delete the directory recursively
        overwrite <- TRUE
        fs::file_delete(save_path)
      }

      if (tolower(response) %in% c("u", "update")) {
        # find dates to filter out from the conversion
        file_list <- fs::dir_ls(save_path, recurse = TRUE, type = "file")
        parquet_skip_dates <- stringr::str_extract(
          file_list,
          "(?<=year=)\\d{4}(?:/month=\\d{1,2})(?:/day=\\d{1,2})"
        ) |>
          stringr::str_replace_all("year=|/month=|/day=", "-") |>
          lubridate::ymd()
        requested_dates <- spod_dates_argument_to_dates_seq(dates)
        filter_to_dates <- requested_dates[
          !(requested_dates %in% parquet_skip_dates)
        ]
        # rename/move the folder temporarily, we will then have to restore it once the DuckDB export to parquet is done
        temp_path <- paste0(save_path, "_temp")
        fs::file_move(save_path, temp_path)
        # set overwrite fales, so that if the move failed, DuckDB will fail if it sees existing directory
        overwrite <- FALSE
        need_to_return_parquet_files <- TRUE # to remember to return the temporary moved existing parquet files\
        # note: if we used `arrow` package instead to convert to parquet, it would be easier to add new parquet files without overwriting the existing ones, however, it would mean (1) we through away all the SQL and DuckDB code that cleans up the data, (2) we would not be able to limit memory usage, as `arrow` has no such setting and just consumes all available memory, and (3) we would not be able to limit the number of threads, as `arrow` has no such setting.
      }
    }

    # check if parquet_skip_dates exists and not empty note that object may not extist at all but the check should not fail
    if (exists("filter_to_dates") == TRUE) {
      if (length(filter_to_dates) > 0) {
        # reset the table_to_convert to the all view, as all missing files must have been downloaded by the call to `spod_get()` above and it is better to filter from the main view to avoid filtering on filtered view
        table_to_convert <- glue::glue("{type}_csv_clean")
        table_to_convert_update <- paste0(table_to_convert, "_update")
        db_con <- spod_duckdb_filter_by_dates(
          con = db_con,
          source_view_name = table_to_convert,
          new_view_name = table_to_convert_update,
          dates = filter_to_dates
        )
        table_to_convert <- table_to_convert_update
      }
    }

    # prepare sql query with `table_to_convert` as source table name
    sql_save_parquet_query <- dplyr::sql(
      glue::glue(
        "
      COPY {table_to_convert} TO '{save_path}'
        (
          FORMAT PARQUET,
          OVERWRITE_OR_IGNORE {overwrite},
          PARTITION_BY (year, month, day),
          -- PER_THREAD_OUTPUT true,
          COMPRESSION snappy,
          FILENAME_PATTERN 'data_{{i}}' -- this prevents the user from saving several differnt file formats into the same folder structure
        ) ;
      "
      )
    )

    DBI::dbExecute(
      db_con,
      statement = sql_save_parquet_query
    )
  }

  DBI::dbDisconnect(db_con, shutdown = TRUE)
  # duckdb::duckdb_shutdown(drv)

  # time to move back the pre-existing parquet files if we moved them to temp folder
  if (need_to_return_parquet_files) {
    # we cannot simply merge the folders, we will have to list the files and move them one by one
    file_list <- fs::dir_ls(temp_path, recurse = TRUE, type = "file")
    new_paths <- gsub(temp_path, save_path, file_list)
    fs::dir_create(unique(fs::path_dir(new_paths)), recurse = TRUE)
    fs::file_move(file_list, new_paths)
    fs::dir_delete(temp_path)
  }

  message("Data imported into ", save_format, " at: ", save_path)

  # a few instructions on how to use the duckdb file
  if (isFALSE(quiet)) {
    message(
      "You can start working with the imported data by runining:\n mydata <- spod_connect(data_path='",
      save_path,
      "')"
    )
    message(
      "You can then manipulate `mydata` using `dplyr` functions such as `select()`, `filter()`, `mutate()`, `group_by()`, `summarise()`, etc. In the end of any sequence of commands you will need to add `collect()` to execute the whole chain and load the results into memory in an R `data.frame`/`tibble`."
    )
    if (save_format == "duckdb") {
      message(
        "For more in-depth usage of such data, please refer to DuckDB documentation and examples at https://duckdb.org/docs/api/r#dbplyr . Some more useful examples can be found here https://arrow-user2022.netlify.app/data-wrangling#combining-arrow-with-duckdb ."
      )
    } else if (save_format == "parquet") {
      message(
        "For more in-depth usage of such data, please refer to DuckDB documentation and examples at https://duckdb.org/docs/api/r#dbplyr . Some more useful examples can be found here https://arrow-user2022.netlify.app/data-wrangling#combining-arrow-with-duckdb . You may also use `arrow` package to work with parquet files https://arrow.apache.org/docs/r/ ."
      )
    }
  }

  return(save_path)
}
