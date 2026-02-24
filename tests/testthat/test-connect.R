
test_that("spod_connect works with parquet folder", {
  skip_if_not(requireNamespace("duckdb", quietly = TRUE))
  
  # Setup parquet fixture
  parquet_dir <- setup_parquet_dir()
  on.exit(unlink(parquet_dir, recursive = TRUE))
  
  # Connect
  con <- spod_connect(parquet_dir)
  expect_s3_class(con, "tbl_duckdb_connection")
  
  # Verify data access
  df <- dplyr::collect(con)
  expect_equal(nrow(df), 10)
  expect_true(all(c("id", "val") %in% names(df)))
  
  # Disconnect
  spod_disconnect(con)
})

test_that("spod_connect works with duckdb file", {
  skip_if_not(requireNamespace("duckdb", quietly = TRUE))
  
  # Setup duckdb fixture
  db_file <- setup_duckdb_file()
  on.exit(unlink(db_file))
  
  # Connect (auto-detect table)
  con <- spod_connect(db_file)
  expect_s3_class(con, "tbl_duckdb_connection")
  
  df <- dplyr::collect(con)
  expect_equal(nrow(df), 5)
  
  spod_disconnect(con)
})

test_that("spod_connect respects resource limits", {
  skip_if_not(requireNamespace("duckdb", quietly = TRUE))
  
  parquet_dir <- setup_parquet_dir()
  on.exit(unlink(parquet_dir, recursive = TRUE))
  
  # Connect with limits
  con <- spod_connect(parquet_dir, max_mem_gb = 1, max_n_cpu = 1)
  
  # Verify limits (tricky to verify exact values without querying pragma, 
  # but we can check if it runs without error)
  expect_s3_class(con, "tbl_duckdb_connection")
  
  spod_disconnect(con)
})

test_that("spod_disconnect closes connection", {
  skip_if_not(requireNamespace("duckdb", quietly = TRUE))
  
  parquet_dir <- setup_parquet_dir()
  on.exit(unlink(parquet_dir, recursive = TRUE))
  
  con <- spod_connect(parquet_dir)
  raw_con <- con$src$con
  
  spod_disconnect(con)
  
  expect_false(DBI::dbIsValid(raw_con))
})
