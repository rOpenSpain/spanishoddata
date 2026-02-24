library(testthat)
library(spanishoddata)

# Use setup_test_data_dir helper instead of mocks
# This ensures we test against the real file structure and logic

test_that("spod_duckdb_od validates version", {
  expect_error(spod_duckdb_od(ver = 3), "Invalid version number")
})

test_that("spod_duckdb_od handles gau zones in v1", {
  expect_error(spod_duckdb_od(zones = "gau", ver = 1), "not available in v1 data")
})

test_that("spod_duckdb_number_of_trips handles gau zones in v1", {
  expect_error(spod_duckdb_number_of_trips(zones = "gau", ver = 1), "not available in v1 data")
})

test_that("spod_duckdb_od works with real fixtures (v2 dist)", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  res <- spod_duckdb_od(con = con, ver = 2, zones = "dist")
  
  expect_identical(res, con)
  
  views <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views()")$view_name
  expect_true("od_csv_raw" %in% views)
  expect_true("od_csv_clean" %in% views)
  
  count <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM od_csv_raw")$n
  expect_true(count >= 0)
})

test_that("spod_duckdb_number_of_trips works with real fixtures (v2 dist)", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  nt_path <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/personas/ficheros-diarios/year=2022/month=2/day=1")
  dir.create(nt_path, recursive = TRUE)
  
  gz_file <- file.path(nt_path, "dummy.csv.gz")
  gz_con <- gzfile(gz_file, "w")
  writeLines("fecha|zona_pernoctacion|edad|sexo|numero_viajes|personas\n20220201|01|1|1|2|10.5", gz_con)
  close(gz_con)
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  res <- spod_duckdb_number_of_trips(con = con, ver = 2, zones = "dist")
  
  expect_identical(res, con)
  
  views <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views()")$view_name
  # The view names might be different for NT
  expect_true(any(grepl("_csv_raw", views)))
  expect_true(any(grepl("_csv_clean", views)))
  
  # Verify schema of the clean view
  clean_view <- views[grepl("_csv_clean", views)][1]
  schema <- DBI::dbGetQuery(con, paste("DESCRIBE", clean_view))
  expect_true("n_trips" %in% schema$column_name)
})

test_that("spod_duckdb_overnight_stays fails for v1", {
  expect_error(spod_duckdb_overnight_stays(ver = 1), "only available in v2")
})

test_that("spod_duckdb_overnight_stays works with real fixtures (v2 dist)", {
  test_dir <- setup_test_data_dir()
  withr::defer(unlink(test_dir, recursive = TRUE))
  withr::local_envvar(c("SPANISH_OD_DATA_DIR" = test_dir))
  
  # Create Hive directory structure for 'pernoctaciones'
  os_path <- file.path(test_dir, "raw_data_cache/v2/estudios_basicos/por-distritos/pernoctaciones/ficheros-diarios/year=2022/month=2/day=1")
  dir.create(os_path, recursive = TRUE)
  
  # Create valid GZIP file
  gz_file <- file.path(os_path, "dummy.csv.gz")
  gz_con <- gzfile(gz_file, "w")
  writeLines("fecha|zona_residencia|zona_pernoctacion|personas\n20220201|01|02|10", gz_con)
  close(gz_con)
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  res <- spod_duckdb_overnight_stays(con = con, ver = 2, zones = "dist")
  expect_identical(res, con)
  
  views <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views()")$view_name
  expect_true(any(grepl("_csv_raw", views)))
})

test_that("spod_duckdb_filter_by_dates logic works with real SQL generation", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  DBI::dbExecute(con, "CREATE VIEW raw_view AS SELECT 2020 as year, 1 as month, 1 as day, 'a' as val")
  DBI::dbExecute(con, "CREATE VIEW source_view AS SELECT * FROM raw_view")
  
  dates <- as.Date("2020-01-01")
  
  res <- spod_duckdb_filter_by_dates(
    con = con,
    source_view_name = "source_view",
    new_view_name = "filtered_view",
    dates = dates
  )
  
  expect_identical(res, con)
  
  count <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM filtered_view")$n
  expect_equal(count, 1)
  
  dates_exclude <- as.Date("2021-01-01")
  res <- spod_duckdb_filter_by_dates(
    con = con,
    source_view_name = "source_view",
    new_view_name = "filtered_view_empty",
    dates = dates_exclude
  )
  count_empty <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM filtered_view_empty")$n
  expect_equal(count_empty, 0)
})

test_that("spod_sql_where_dates generates correct WHERE clause", {
  dates <- as.Date(c("2020-01-01", "2020-01-02", "2020-02-15"))
  res <- spanishoddata:::spod_sql_where_dates(dates)
  
  expect_match(res, "WHERE")
  expect_match(res, "year = 2020 AND month = 01 AND day IN \\(01, 02\\)")
  expect_match(res, "year = 2020 AND month = 02 AND day IN \\(15\\)")
})

test_that("spod_duckdb_limit_resources executes valid SQL", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  spod_duckdb_limit_resources(con, max_mem_gb = 1, max_n_cpu = 1)
  
  threads <- DBI::dbGetQuery(con, "SELECT value FROM duckdb_settings() WHERE name = 'threads'")$value
  expect_equal(as.integer(threads), 1)
})

test_that("spod_duckdb_set_temp executes valid SQL", {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  
  tmp <- tempdir()
  spod_duckdb_set_temp(con, temp_path = tmp)
  
  res <- DBI::dbGetQuery(con, "SELECT value FROM duckdb_settings() WHERE name = 'temp_directory'")$value
  expect_true(grepl(basename(tmp), res))
})
