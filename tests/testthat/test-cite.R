# Load testthat
library(testthat)

# --- Tests for spod_cite function ---

test_that("spod_cite with default arguments prints all citations in all formats", {
  # Capture the output of a single function call
  output <- capture.output(spod_cite())
  output_str <- paste(output, collapse = "\n")

  # Run all checks on the captured string
  expect_true(grepl("Plain text citations:", output_str))
  expect_true(grepl("Markdown citations:", output_str))
  expect_true(grepl("BibTeX citations:", output_str))
  expect_true(grepl("To cite the spanishoddata package", output_str))
  expect_true(grepl(
    "To cite the Ministry's mobility study website",
    output_str
  ))
  expect_true(grepl("To cite the methodology for 2020-2021 data", output_str))
  expect_true(grepl(
    "To cite the methodology for 2022 and onwards data",
    output_str
  ))
})

test_that("spod_cite with specific 'what' and 'format' arguments works", {
  # Capture the specific output to a string for inspection
  output <- capture.output(spod_cite(what = "package", format = "bibtex"))
  output_str <- paste(output, collapse = "\n")

  # Check for expected content
  expect_true(grepl("BibTeX citations:", output_str))
  expect_true(grepl("%% To cite the spanishoddata package", output_str))
  expect_true(grepl("@Manual", output_str))

  # Check that other content is NOT present
  expect_false(grepl("Plain text citations:", output_str))
  expect_false(grepl("Markdown citations:", output_str))
  expect_false(grepl(
    "To cite the Ministry's mobility study website",
    output_str
  ))
})

test_that("spod_cite with multiple 'what' arguments works", {
  output <- capture.output(spod_cite(
    what = c("methodology_v1", "data"),
    format = "text"
  ))
  output_str <- paste(output, collapse = "\n")

  # Check for expected content
  expect_true(grepl("Plain text citations:", output_str))
  expect_true(grepl(
    "To cite the Ministry's mobility study website:",
    output_str
  ))
  expect_true(grepl("To cite the methodology for 2020-2021 data:", output_str))

  # Check that other content is NOT present
  expect_false(grepl("To cite the spanishoddata package", output_str))
  expect_false(grepl("BibTeX citations:", output_str))
})

test_that("spod_cite handles invalid inputs gracefully", {
  # The function uses checkmate, which will throw an error on invalid input
  expect_error(spod_cite(what = "invalid_input"))
  expect_error(spod_cite(format = "invalid_format"))
})


test_that("spod_cite includes the special note for methodology_v2", {
  # Text format
  output_text <- capture.output(spod_cite(
    what = "methodology_v2",
    format = "text"
  ))
  expect_true(any(grepl(
    "Note: A more up-to-date methodology document may be available",
    output_text
  )))

  # Markdown format
  output_md <- capture.output(spod_cite(
    what = "methodology_v2",
    format = "markdown"
  ))
  expect_true(any(grepl("> \\*\\*Note:\\*\\*", output_md)))

  # BibTeX format
  output_bib <- capture.output(spod_cite(
    what = "methodology_v2",
    format = "bibtex"
  ))
  expect_true(any(grepl(
    "%% Note: A more up-to-date methodology document may be available",
    output_bib
  )))
})

test_that("spod_cite expands 'all' argument correctly", {
  # Test 'all' in 'what'
  output_what <- capture.output(spod_cite(
    what = c("package", "all"),
    format = "text"
  ))
  output_what_str <- paste(output_what, collapse = "\n")
  expect_true(grepl("To cite the spanishoddata package", output_what_str))
  expect_true(grepl(
    "To cite the Ministry's mobility study website",
    output_what_str
  ))

  # Test 'all' in 'format'
  output_format <- capture.output(spod_cite(
    what = "package",
    format = c("text", "all")
  ))
  output_format_str <- paste(output_format, collapse = "\n")
  expect_true(grepl("Plain text citations:", output_format_str))
  expect_true(grepl("Markdown citations:", output_format_str))
  expect_true(grepl("BibTeX citations:", output_format_str))
})
