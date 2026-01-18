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
#' spod_cite()
#' }
#'
#' # Cite just the package in BibTeX format
#' \dontrun{
#' spod_cite(what = "package", format = "bibtex")
#' }
#'
#' # Cite both methodologies in plain text
#' \dontrun{
#' spod_cite(what = c("methodology_v1", "methodology_v2"), format = "text")
#' }
spod_cite <- function(
  what = "all",
  format = "all"
) {
  # 1. Define valid inputs
  valid_what <- c("all", "package", "data", "methodology_v1", "methodology_v2")
  valid_format <- c("all", "text", "markdown", "bibtex")

  # 2. Use checkmate to validate arguments
  checkmate::assertCharacter(what, any.missing = FALSE, min.len = 1)
  checkmate::assertCharacter(format, any.missing = FALSE, min.len = 1)
  checkmate::assertSubset(what, choices = valid_what)
  checkmate::assertSubset(format, choices = valid_format)

  # 3. Expand "all" options
  if ("all" %in% what) {
    what <- unique(c(what, valid_what[valid_what != "all"]))
  }
  if ("all" %in% format) {
    format <- unique(c(format, valid_format[valid_format != "all"]))
  }

  # Remove the literal "all" from each
  what <- setdiff(what, "all")
  format <- setdiff(format, "all")

  # 4. Map what options to BibTeX keys in REFERENCES.bib
  citation_keys <- list(
    package = "spanishoddata-r-pkg",
    data = "mitms_mobility_web",
    methodology_v1 = "mitma_methodology_2020_v3",
    methodology_v2 = "mitms_methodology_2022_v8"
  )

  # 4.1. Human-readable labels for each "what"
  citation_labels <- list(
    package = "To cite the spanishoddata package",
    data = "To cite the Ministry's mobility study website",
    methodology_v1 = "To cite the methodology for 2020-2021 data",
    methodology_v2 = "To cite the methodology for 2022 and onwards data"
  )

  # 5. Read REFERENCES.bib using Rdpack
  bibs <- Rdpack::get_bibentries(package = "spanishoddata")

  # 6. Collect the requested citations
  citations_to_show <- list()
  for (w in what) {
    key <- citation_keys[[w]]
    if (key %in% names(bibs)) {
      citations_to_show[[w]] <- bibs[key]
    }
  }

  # If nothing was found
  if (length(citations_to_show) == 0) {
    message("No valid citations found for the requested 'what'.")
    return(invisible(NULL))
  }

  # 7. Print the citations in requested formats
  for (f in format) {
    if (f == "text") {
      cat("\nPlain text citations:\n---------------------\n")
      for (w in names(citations_to_show)) {
        cit_entry <- citations_to_show[[w]]
        cat(paste0(citation_labels[[w]], ":\n"))
        cat(format(cit_entry, style = "text"), "\n\n")
        # Note for methodology_v2
        if (w == "methodology_v2") {
          cat(
            "Note: A more up-to-date methodology document may be available at https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/metodologia-del-estudio-de-movilidad-con-bigdata\n\n"
          )
        }
      }
    } else if (f == "markdown") {
      cat("\nMarkdown citations:\n-------------------\n")
      for (w in names(citations_to_show)) {
        cit_entry <- citations_to_show[[w]]
        cat(paste0("**", citation_labels[[w]], ":**\n"))
        cat(format(cit_entry, style = "text"), "\n\n")
        # Note for methodology_v2
        if (w == "methodology_v2") {
          cat(
            "> **Note:** A more up-to-date methodology document may be available at https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/metodologia-del-estudio-de-movilidad-con-bigdata\n\n"
          )
        }
      }
    } else if (f == "bibtex") {
      cat("\nBibTeX citations:\n-----------------\n")
      for (w in names(citations_to_show)) {
        cit_entry <- citations_to_show[[w]]
        cat(paste0("%% ", citation_labels[[w]], "\n"))
        
        # Capture standard BibTeX output
        bib_lines <- capture.output(print(cit_entry, style = "bibtex"))

        # CUSTOM FORMATTING: Ensure "Last, First" author format with strict brace preservation
        # 1. Generate text string for authors in "Last, First" format
        auth_obj <- cit_entry$author
        if (is.null(auth_obj)) {
          # Fallback via unclass if direct access fails (unlikely for bibentry)
          auth_obj <- unclass(cit_entry)[[1]]$author
        }

        custom_author_str <- NULL
        if (!is.null(auth_obj) && length(auth_obj) > 0) {
          custom_author_str <- paste(
            sapply(auth_obj, function(p) {
              fam <- paste(p$family, collapse = " ")
              giv <- paste(p$given, collapse = " ")
              
              # Heuristic: If family name contains hyphens, spaces, or backslashes (latex),
              # preserve it wrapped in curly braces.
              # Note: Standard names like "Kotov" (alphanumeric) are left as is.
              if (grepl("[-\\\\\\s]", fam)) {
                 fam <- paste0("{", fam, "}")
              }
              
              if (length(p$given) == 0 || nchar(giv) == 0) {
                # Organization or Mononym
                # Even if simple, organization names often benefit from braces if not broken down
                # But our heuristic handles spaces, so "Ministry of ..." gets braces.
                return(fam) 
              } else {
                return(paste0(fam, ", ", giv))
              }
            }),
            collapse = " and "
          )
        }

        # 2. Replace the 'author = { ... },' block in bib_lines
        start_idx <- grep("^\\s*author\\s*=", bib_lines)
        
        if (length(start_idx) == 1 && !is.null(custom_author_str)) {
          # Find end of the author block by brace balancing
          current_line_idx <- start_idx
          brace_balance <- 0
          
          # Iterate to find the closing brace
          for (i in start_idx:length(bib_lines)) {
            line <- bib_lines[i]
            # Simple count of { and }
            # Note: R's toBibtex output is regular and clean
            n_open <- stringr::str_count(line, "\\{")
            n_close <- stringr::str_count(line, "\\}")
            brace_balance <- brace_balance + n_open - n_close
            
            if (brace_balance == 0) {
              current_line_idx <- i
              break
            }
          }
          end_idx <- current_line_idx
          
          # Replace the block with the single formatted line
          new_line <- paste0("  author = {", custom_author_str, "},")
          
          # Construct new lines vector
          pre_block <- if (start_idx > 1) bib_lines[1:(start_idx - 1)] else character(0)
          post_block <- if (end_idx < length(bib_lines)) bib_lines[(end_idx + 1):length(bib_lines)] else character(0)
          
          bib_lines <- c(pre_block, new_line, post_block)
        }
        
        cat(bib_lines, sep = "\n")
        cat("\n\n")
        
        # Note for methodology_v2
        if (w == "methodology_v2") {
          cat(
            "%% Note: A more up-to-date methodology document may be available at https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/metodologia-del-estudio-de-movilidad-con-bigdata\n\n"
          )
        }
      }
    }
  }

  invisible(NULL)
}
