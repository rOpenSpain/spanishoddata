#' Cite the package and the data
#'
#' @param what Character vector specifying what to cite.
#'   Can include "package", "data", "methodology_v1", "methodology_v2", or "all".
#'   Default is "all".
#' @param format Character vector specifying output format(s).
#'   Can include "text", "markdown", "bibtex", or "all".
#'   Default is "all".
#' @return Nothing. Prints citation in plain text, markdown, BibTeX, or all formats at once to console.
#' @export
#'
#' @examples
#' # Cite everything in all formats
#' \dontrun{
#'  spod_cite()
#' }
#'
#' # Cite just the package in BibTeX format
#' \dontrun{
#'  spod_cite(what = "package", format = "bibtex")
#' }
#'
#' # Cite both methodologies in plain text
#' \dontrun{
#'  spod_cite(what = c("methodology_v1", "methodology_v2"), format = "text")
#' }
spod_cite <- function(
  what = "all",
  format = "all"
) {
  # 1. Define valid inputs
  valid_what   <- c("all", "package", "data", "methodology_v1", "methodology_v2")
  valid_format <- c("all", "text", "markdown", "bibtex")
  
  # 2. Use checkmate to validate arguments
  checkmate::assertCharacter(what, any.missing = FALSE, min.len = 1)
  checkmate::assertCharacter(format, any.missing = FALSE, min.len = 1)
  checkmate::assertSubset(what, choices = valid_what)
  checkmate::assertSubset(format, choices = valid_format)
  
  # 3. Expand "all" options
  # If "all" is included in what, use all sources except the "all" string itself
  if ("all" %in% what) {
    what <- unique(c(what, valid_what[valid_what != "all"]))
  }
  # If "all" is included in format, use all formats except the "all" string itself
  if ("all" %in% format) {
    format <- unique(c(format, valid_format[valid_format != "all"]))
  }
  
  # Now remove the literal "all" from each to avoid confusion
  what   <- setdiff(what, "all")
  format <- setdiff(format, "all")
  
  # 4. Get the citation object
  cit <- citation("spanishoddata")
  
  # 5. Function to get citation by key
  get_citation_by_key <- function(key) {
    idx <- which(sapply(cit, function(x) x$key == key))
    if (length(idx) > 0) return(cit[idx])
    return(NULL)
  }
  
  # 6. Map what options to citation keys
  citation_keys <- list(
    package        = "r-spanishoddata",
    data           = "mitms_mobility_web",
    methodology_v1 = "mitma_methodology_2020_v3",
    methodology_v2 = "mitms_methodology_2022_v8"
  )
  
  # 7. Collect the requested citations
  citations_to_show <- list()
  for (w in what) {
    key <- citation_keys[[w]]
    cit_entry <- get_citation_by_key(key)
    if (!is.null(cit_entry)) {
      citations_to_show[[w]] <- cit_entry
    }
  }
  
  # If nothing was found (e.g., user gave an empty vector)
  if (length(citations_to_show) == 0) {
    message("No valid citations found for the requested 'what'.")
    return(invisible(NULL))
  }
  
  # 8. Helper functions for formatting output
  
  # Plain text
  format_text <- function(citation) {
    text <- format(citation, style = "text")
    # remove asterisks or underscores used for emphasis in the default text
    text <- gsub("\\*([^*]*)\\*", "\\1", text)
    text <- gsub("_([^_]*)_", "\\1", text)
    # Clean up URLs (remove angle brackets)
    text <- gsub("<(http[^>]*)>", "\\1", text)
    paste(text, collapse = "\n")
  }
  
  # Markdown
  format_markdown <- function(citation) {
    text <- format(citation, style = "text")
    # minimal transformation to markdown: italicize text within asterisks
    text <- gsub("\\*([^*]*)\\*", "_\\1_", text)
    # remove angle brackets around URLs
    text <- gsub("<(http[^>]*)>", "\\1", text)
    paste(text, collapse = "\n")
  }
  
  # 9. Print the citations in requested formats
  for (f in format) {
    if (f == "text") {
      cat("\nPlain text citations:\n---------------------\n")
      for (cit_item in citations_to_show) {
        cat(format_text(cit_item), "\n\n")
      }
    } else if (f == "markdown") {
      cat("\nMarkdown citations:\n-------------------\n")
      for (cit_item in citations_to_show) {
        cat(format_markdown(cit_item), "\n\n")
      }
    } else if (f == "bibtex") {
      cat("\nBibTeX citations:\n-----------------\n")
      for (cit_item in citations_to_show) {
        print(toBibtex(cit_item))
        cat("\n")
      }
    }
  }
  
  invisible(NULL)
}
