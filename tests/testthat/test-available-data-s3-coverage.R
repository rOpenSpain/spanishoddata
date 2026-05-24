# Behavioural tests for R/available-data-s3.R
#
# These tests mock the network/S3 layer (the un-memoised inner function, or
# paws.storage::paginate) and then verify the cache layer's behaviour by
# observing call counts, file-system side effects, and returned values.
# We avoid asserting on informational message strings, which would couple
# the tests to wording rather than behaviour.

# Helpers ---------------------------------------------------------------

#' A tibble shaped like the real S3 metadata. `tag` distinguishes mock instances
#' (e.g. to verify "this RDS came from the disk cache, not from the mock").
make_s3_meta <- function(tag = "mock") {
  tibble::tibble(
    target_url = paste0(
      "https://movilidad-opendata.mitma.es/",
      tag,
      "/file.csv.gz"
    ),
    pub_ts = as.POSIXct("2022-01-01", tz = "UTC"),
    file_size_bytes = 1234,
    etag = tag
  )
}

#' Build a temp data dir with the metadata-cache subfolder already in place.
fresh_data_dir <- function(envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  dir.create(
    file.path(d, spod_subfolder_metadata_cache()),
    recursive = TRUE
  )
  d
}

# Cache-layer behaviour --------------------------------------------------

test_that("memoise cache prevents a second inner call within the session", {
  data_dir <- fresh_data_dir()

  call_count <- 0
  testthat::local_mocked_bindings(
    spod_available_data_s3_memoised = memoise::memoise(function(ver) {
      call_count <<- call_count + 1
      make_s3_meta()
    })
  )

  res1 <- spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE)
  res2 <- spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE)

  expect_equal(call_count, 1)
  expect_equal(res1, res2)
})

test_that("fresh on-disk RDS is used and inner function is not invoked", {
  data_dir <- fresh_data_dir()
  on_disk <- make_s3_meta(tag = "from-disk")
  fresh_path <- file.path(
    data_dir,
    spod_subfolder_metadata_cache(),
    paste0("metadata_s3_v2_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
  )
  saveRDS(on_disk, fresh_path)

  testthat::local_mocked_bindings(
    spod_available_data_s3_memoised = memoise::memoise(function(ver) {
      stop("inner function should not be called when fresh disk cache exists")
    })
  )

  res <- spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE)
  expect_identical(res, on_disk)
})

test_that("stale on-disk RDS triggers fetch; new RDS is written; stale kept", {
  data_dir <- fresh_data_dir()
  stale_path <- file.path(
    data_dir,
    spod_subfolder_metadata_cache(),
    paste0("metadata_s3_v2_", Sys.Date() - 1, ".rds")
  )
  saveRDS(make_s3_meta(tag = "stale"), stale_path)

  call_count <- 0
  testthat::local_mocked_bindings(
    spod_available_data_s3_memoised = memoise::memoise(function(ver) {
      call_count <<- call_count + 1
      make_s3_meta(tag = "fresh")
    })
  )

  res <- spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE)

  expect_equal(call_count, 1)
  # Returned data comes from the new fetch, not the stale RDS
  expect_equal(res$etag, "fresh")
  # New dated RDS written
  new_path <- file.path(
    data_dir,
    spod_subfolder_metadata_cache(),
    paste0("metadata_s3_v2_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
  )
  expect_true(file.exists(new_path))
  # Without force, stale file is kept (the code deletes only on force=TRUE)
  expect_true(file.exists(stale_path))
})

test_that("force=TRUE deletes old RDS files, evicts memoise, refetches", {
  data_dir <- fresh_data_dir()
  meta_dir <- file.path(data_dir, spod_subfolder_metadata_cache())

  # Pre-populate two old RDS files
  old1 <- file.path(meta_dir, paste0("metadata_s3_v2_", Sys.Date() - 5, ".rds"))
  old2 <- file.path(meta_dir, paste0("metadata_s3_v2_", Sys.Date() - 2, ".rds"))
  saveRDS(make_s3_meta(tag = "old1"), old1)
  saveRDS(make_s3_meta(tag = "old2"), old2)

  call_count <- 0
  testthat::local_mocked_bindings(
    spod_available_data_s3_memoised = memoise::memoise(function(ver) {
      call_count <<- call_count + 1
      make_s3_meta(tag = "refetched")
    })
  )

  # First, prime the memoise cache so we can verify force=TRUE evicts it
  spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE)
  expect_equal(call_count, 1)

  # Now force=TRUE: should delete old1/old2, evict memoise, call inner again
  res <- spod_available_data_s3(
    ver = 2,
    data_dir = data_dir,
    force = TRUE,
    quiet = TRUE
  )

  expect_equal(call_count, 2)
  expect_equal(res$etag, "refetched")
  expect_false(file.exists(old1))
  expect_false(file.exists(old2))
  # New RDS written for today
  expect_true(file.exists(file.path(
    meta_dir,
    paste0("metadata_s3_v2_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
  )))
})

test_that("quiet=TRUE silences the function; quiet=FALSE emits messages on each cache branch", {
  # This test asserts the quiet/verbose contract across all three cache
  # branches: memoise hit, fresh disk read, fetch+write. We don't grep
  # message strings — we only verify the binary contract.
  data_dir <- fresh_data_dir()
  testthat::local_mocked_bindings(
    spod_available_data_s3_memoised = memoise::memoise(function(ver) {
      make_s3_meta()
    })
  )

  # Branch A: fetch path (no cache yet)
  expect_silent(spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE))
  data_dir2 <- fresh_data_dir()
  expect_message(spod_available_data_s3(ver = 2, data_dir = data_dir2, quiet = FALSE))

  # Branch B: memoise hit (same session, same data_dir)
  expect_silent(spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE))
  expect_message(spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = FALSE))

  # Branch C: fresh disk cache, fresh memoise. Forget memoise so it has
  # to fall back to disk.
  memoise::forget(spod_available_data_s3_memoised)
  expect_silent(spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE))
  memoise::forget(spod_available_data_s3_memoised)
  expect_message(spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = FALSE))
})

test_that("v1 and v2 caches are independent", {
  data_dir <- fresh_data_dir()
  meta_dir <- file.path(data_dir, spod_subfolder_metadata_cache())

  v1_disk <- make_s3_meta(tag = "v1-on-disk")
  v2_disk <- make_s3_meta(tag = "v2-on-disk")
  saveRDS(
    v1_disk,
    file.path(meta_dir, paste0("metadata_s3_v1_", format(Sys.Date(), "%Y-%m-%d"), ".rds"))
  )
  saveRDS(
    v2_disk,
    file.path(meta_dir, paste0("metadata_s3_v2_", format(Sys.Date(), "%Y-%m-%d"), ".rds"))
  )

  testthat::local_mocked_bindings(
    spod_available_data_s3_memoised = memoise::memoise(function(ver) {
      stop("inner should not be called — fresh disk cache exists for both versions")
    })
  )

  expect_identical(
    spod_available_data_s3(ver = 1, data_dir = data_dir, quiet = TRUE),
    v1_disk
  )
  expect_identical(
    spod_available_data_s3(ver = 2, data_dir = data_dir, quiet = TRUE),
    v2_disk
  )
})

# Data-transformation behaviour -----------------------------------------

test_that("spod_available_data_s3_function builds correct URLs and strips ETag quotes per version", {
  # Mock paws.storage::paginate so we exercise BOTH spod_available_data_s3_function
  # AND list_objects_v2_all end-to-end (rather than mocking out list_objects_v2_all).
  # The mock also captures the bucket name passed in, so we can verify versioning.
  captured_bucket <- character()
  fake_pages <- list(list(Contents = list(
    list(
      Key = "path/to/file1.csv.gz",
      LastModified = as.POSIXct("2022-01-01T00:00:00Z", tz = "UTC"),
      Size = 100,
      ETag = "\"etag-one\""
    ),
    list(
      Key = "path/to/file2.csv.gz",
      LastModified = as.POSIXct("2022-01-02T12:00:00Z", tz = "UTC"),
      Size = 200,
      ETag = "\"etag-two\""
    )
  )))

  testthat::local_mocked_bindings(
    paginate = function(operation, ...) {
      # Force the unevaluated operation arg so the spy on list_objects_v2
      # actually fires (mimics the real paginate, which evaluates it).
      force(operation)
      fake_pages
    },
    .package = "paws.storage"
  )

  # Spy on the S3 client so we can verify which bucket was requested
  fake_s3 <- list(list_objects_v2 = function(Bucket, ...) {
    captured_bucket <<- c(captured_bucket, Bucket)
    NULL
  })
  testthat::local_mocked_bindings(
    s3 = function(...) fake_s3,
    .package = "paws.storage"
  )

  res_v1 <- spod_available_data_s3_function(ver = 1)
  res_v2 <- spod_available_data_s3_function(ver = 2)

  # Schema
  expect_named(res_v1, c("target_url", "pub_ts", "file_size_bytes", "etag"))
  expect_equal(nrow(res_v1), 2)

  # Version-dependent URL prefix
  expect_true(all(startsWith(res_v1$target_url, "https://opendata-movilidad.mitma.es/")))
  expect_true(all(startsWith(res_v2$target_url, "https://movilidad-opendata.mitma.es/")))

  # Key is concatenated onto the prefix verbatim
  expect_equal(
    res_v1$target_url,
    c(
      "https://opendata-movilidad.mitma.es/path/to/file1.csv.gz",
      "https://opendata-movilidad.mitma.es/path/to/file2.csv.gz"
    )
  )

  # ETag quotes stripped
  expect_equal(res_v1$etag, c("etag-one", "etag-two"))

  # Sizes coerced to numeric
  expect_equal(res_v1$file_size_bytes, c(100, 200))
  expect_type(res_v1$file_size_bytes, "double")

  # Bucket name is version-specific
  expect_equal(captured_bucket, c("mitma-movilidad-v1", "mitma-movilidad-v2"))
})

test_that("list_objects_v2_all flattens multi-page Contents into a single tibble", {
  # Two pages, three objects total
  fake_pages <- list(
    list(Contents = list(
      list(Key = "a.csv", LastModified = 1640995200, Size = 100, ETag = "\"x\""),
      list(Key = "b.csv", LastModified = 1641081600, Size = 200, ETag = "\"y\"")
    )),
    list(Contents = list(
      list(Key = "c.csv", LastModified = 1641168000, Size = 300, ETag = "\"z\"")
    ))
  )

  captured_args <- list()
  testthat::local_mocked_bindings(
    paginate = function(operation, PageSize, ...) {
      captured_args[["PageSize"]] <<- PageSize
      fake_pages
    },
    .package = "paws.storage"
  )

  # NOTE: We don't force(operation) here because list_objects_v2_all is being
  # called with s3 = NULL, so forcing would crash. The test exercises the
  # tibble construction path; PageSize forwarding is verified via the spy.

  # The s3 argument is never actually used because paginate is mocked,
  # but we pass a sentinel to make sure list_objects_v2_all forwards it.
  res <- list_objects_v2_all(s3 = NULL, bucket = "test-bucket", max_keys = 7777)

  # Output schema and shape
  expect_named(res, c("Key", "LastModified", "Size", "ETag"))
  expect_equal(nrow(res), 3)
  expect_equal(res$Key, c("a.csv", "b.csv", "c.csv"))
  expect_equal(res$Size, c(100, 200, 300))
  expect_s3_class(res$LastModified, "POSIXct")
  expect_equal(attr(res$LastModified, "tzone"), "UTC")

  # max_keys was forwarded to paginate's PageSize
  expect_equal(captured_args[["PageSize"]], 7777)
})
