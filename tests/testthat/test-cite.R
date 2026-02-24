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
  
  # Check for case-insensitive article type (Rdpack uses @Article)
  expect_true(grepl("@article", output_str, ignore.case = TRUE))

  # CRITICAL: Check for "Last, First" author format with BRACES
  # "Kotov" should be simple
  expect_true(grepl("Kotov, Egor", output_str))
  
  # "Vidal-Tortosa" should have braces because of hyphen
  expect_true(grepl("\\{Vidal-Tortosa\\}, Eugeni", output_str))
  
  # "Cantú-Ros" should have braces (assuming backslash makes it complex, even if not visible in grepl, 
  # we check for the brace wrapper)
  expect_true(grepl("\\{Cant.*\\-Ros\\}", output_str)) 

  # Check that plain output does NOT appear (e.g. Vidal-Tortosa without braces)
  # Actually, simple grepl for "{Vidal-Tortosa}" confirms braces are present.
  
  # Check that other content is NOT present
  expect_false(grepl("Plain text citations:", output_str))
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
  expect_error(spod_cite(what = "invalid_input"))
  expect_error(spod_cite(format = "invalid_format"))
})

test_that("spod_cite includes the special note for methodology_v2", {
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
  output_what <- capture.output(spod_cite(
    what = c("package", "all"),
    format = "text"
  ))
  expect_true(grepl("To cite the spanishoddata package", paste(output_what, collapse="\n")))
})
