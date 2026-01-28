# Cite the package and the data

Cite the package and the data

## Usage

``` r
spod_cite(what = "all", format = "all")
```

## Arguments

- what:

  Character vector specifying what to cite. Can include "package",
  "data", "methodology_v1", "methodology_v2", or "all". Default is
  "all".

- format:

  Character vector specifying output format(s). Can include "text",
  "markdown", "bibtex", or "all". Default is "all".

## Value

Nothing. Prints citation in plain text, markdown, BibTeX, or all formats
at once to console.

## Examples

``` r
# Cite everything in all formats
if (FALSE) { # \dontrun{
spod_cite()
} # }

# Cite just the package in BibTeX format
if (FALSE) { # \dontrun{
spod_cite(what = "package", format = "bibtex")
} # }

# Cite both methodologies in plain text
if (FALSE) { # \dontrun{
spod_cite(what = c("methodology_v1", "methodology_v2"), format = "text")
} # }
```
