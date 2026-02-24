
test_that("spod_codebook works for v1 and matches vignette topic", {
  
  # Mock utils::vignette to return a fake vignette object or NULL
  local_mock_vignette <- function(topic_to_find, env = parent.frame()) {
    mock_vignette <- function(topic, package) {
      if (topic == topic_to_find && package == "spanishoddata") {
        # Return a structure that mimics a vignette object
        structure(list(topic = topic, package = package), class = "vignette")
      } else {
        # Return something that doesn't inherit "vignette" (e.g. NULL or empty list)
        return(NULL)
      }
    }
    
    # We use local_mocked_bindings for utils::vignette if possible.
    # But since utils is a base package, testthat might restrict it.
    # However, testthat::local_mocked_bindings supports package namespaces.
    # Let's try mocking in the 'spanishoddata' namespace where it's imported/used.
    # Since spod_codebook uses `vignette(...)`, it's resolved from `utils` unless imported.
    # The file has `@importFrom utils vignette`, so it is in the package imports.
    # We can mock it in the package namespace.
    
    testthat::local_mocked_bindings(
      vignette = mock_vignette,
      .package = "spanishoddata",
      .env = env
    )
  }
  
  local_mock_vignette("v1-2020-2021-mitma-data-codebook")
  
  # Success case
  res <- spod_codebook(ver = 1)
  expect_s3_class(res, "vignette")
  expect_equal(res$topic, "v1-2020-2021-mitma-data-codebook")
  
  # Fallback case (mock failure for v2 in this scope)
  # This relies on the mock NOT matching v2
  expect_message(
    spod_codebook(ver = 2),
    "codebook was not installed.*https://ropenspain.github.io/spanishoddata/articles/v2"
  )
})

test_that("spod_codebook works for v2", {
  local_mock_vignette <- function(topic_to_find, env = parent.frame()) {
    mock_vignette <- function(topic, package) {
      if (topic == topic_to_find && package == "spanishoddata") {
        structure(list(topic = topic, package = package), class = "vignette")
      } else {
        return(NULL)
      }
    }
    testthat::local_mocked_bindings(
      vignette = mock_vignette,
      .package = "spanishoddata",
      .env = env
    )
  }
  
  local_mock_vignette("v2-2022-onwards-mitma-data-codebook")
  
  res <- spod_codebook(ver = 2)
  expect_s3_class(res, "vignette")
  expect_equal(res$topic, "v2-2022-onwards-mitma-data-codebook")
})

test_that("spod_codebook handles invalid inputs", {
  expect_error(spod_codebook(ver = 3), "Invalid version number")
  expect_error(spod_codebook(ver = "a"), "Must be of type 'integerish'")
})
