# Ensure necessary libraries are installed and loaded
if (!require(httr2)) {
  install.packages("httr2")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(jsonlite)) {
  install.packages("jsonlite")
}
library(httr2)
library(dplyr)
library(jsonlite)


#' Parses a simple filter string (e.g., "pob_llega<1000").
#' (This helper function is unchanged)
parse_where_string <- function(str) {
  op_map <- c(
    "<" = "$lt",
    "<=" = "$lte",
    ">" = "$gt",
    ">=" = "$gte",
    "!=" = "$ne",
    "==" = "EQUAL",
    "=" = "EQUAL"
  )
  op_regex <- paste(names(op_map), collapse = "|")
  match <- regexpr(op_regex, str)
  if (match == -1) {
    stop(paste("Invalid filter string. No operator found in:", str))
  }

  operator <- regmatches(str, match)
  parts <- strsplit(str, operator)[[1]]
  field <- trimws(parts[1])
  value_str <- trimws(parts[2])
  value <- utils::type.convert(value_str, as.is = TRUE)
  api_op <- op_map[operator]

  if (api_op == "EQUAL") {
    setNames(list(value), field)
  } else {
    inner_list <- setNames(list(value), api_op)
    setNames(list(inner_list), field)
  }
}


#' Fetches and processes paginated data from an API endpoint in parallel,
#' with a highly flexible and user-friendly filtering system.
#'
#' @param api_endpoint The name of the API endpoint.
#' @param where A list that can contain a mix of named elements for equality
#'   (e.g., `id_grupo = "007M"`) and unnamed strings for complex comparisons
#'   (e.g., `"pob_llega < 1000"`).
#' @param ... Other arguments like projection, sort, limit, max_results_per_page.
#'
get_api_data <- function(api_endpoint, where = NULL, ...) {
  # --- NEW: Hybrid 'where' clause processor ---
  if (!is.null(where) && is.list(where)) {
    processed_list <- list()
    where_names <- names(where)

    for (i in seq_along(where)) {
      name <- where_names[i]
      item <- where[[i]]

      # If the element is named, it's a simple equality filter
      if (!is.null(name) && name != "") {
        processed_list[[name]] <- item
      } else if (is.character(item)) {
        # If it's an unnamed string, parse it for complex operators
        parsed_part <- parse_where_string(item)
        processed_list <- c(processed_list, parsed_part)
      } else {
        # Otherwise, the format is invalid
        stop(paste(
          "Invalid element in 'where' list at position",
          i,
          "Elements must be named (e.g., id_grupo = '007M') or unnamed strings (e.g., 'pob_llega < 1000')."
        ))
      }
    }
    # Use the fully processed list for the API call
    where <- processed_list
  }

  # The rest of the function proceeds as before
  .get_api_data_core(api_endpoint, where = where, ...)
}


#' Core logic for fetching API data (internal function).
#' (This function is unchanged)
.get_api_data_core <- function(
  api_endpoint,
  where = NULL,
  projection = NULL,
  sort = NULL,
  limit = NULL,
  max_results_per_page = 1000
) {
  api_base_url <- "https://flowmaps.life.bsc.es/api/"
  req_url <- paste0(api_base_url, api_endpoint)

  base_query_params <- list()
  if (!is.null(where)) {
    base_query_params$where <- toJSON(where, auto_unbox = TRUE)
  }
  if (!is.null(projection)) {
    base_query_params$projection <- toJSON(projection, auto_unbox = TRUE)
  }
  if (!is.null(sort)) {
    base_query_params$sort <- sort
  }

  cat("Performing initial request to get metadata...\n")
  initial_query_params <- c(base_query_params, list(page = 1, max_results = 1))

  initial_resp <- request(req_url) |>
    req_url_query(!!!initial_query_params) |>
    req_perform()
  initial_data <- resp_body_json(initial_resp, simplifyVector = TRUE)

  if (is.null(initial_data$`_meta`) || initial_data$`_meta`$total == 0) {
    cat("Query returned 0 results. Returning an empty data frame.\n")
    return(data.frame())
  }

  total_items_on_server <- initial_data$`_meta`$total
  items_to_fetch <- if (is.null(limit)) {
    total_items_on_server
  } else {
    min(total_items_on_server, limit)
  }

  if (items_to_fetch == 0) {
    cat("Number of items to fetch is 0. Returning an empty data frame.\n")
    return(data.frame())
  }

  total_pages <- ceiling(items_to_fetch / max_results_per_page)
  cat(paste(
    "Query matches",
    total_items_on_server,
    "items. Will fetch",
    items_to_fetch,
    "items across",
    total_pages,
    "pages.\n"
  ))

  requests <- lapply(1:total_pages, function(page_num) {
    current_max_results <- if (page_num == total_pages) {
      last_page_size <- items_to_fetch %% max_results_per_page
      if (last_page_size == 0) max_results_per_page else last_page_size
    } else {
      max_results_per_page
    }
    page_query_params <- c(
      base_query_params,
      list(page = page_num, max_results = current_max_results)
    )
    request(req_url) |> req_url_query(!!!page_query_params)
  })

  cat("Starting parallel fetch... (This may take a moment)\n")
  responses <- req_perform_parallel(requests)

  cat("All pages fetched. Processing and combining data...\n")
  all_items <- lapply(responses, function(resp) {
    if (resp_is_error(resp)) {
      warning(paste("Request failed:", resp_status(resp)))
      return(NULL)
    }
    resp_body_json(resp, simplifyVector = TRUE)$`_items`
  })
  all_items <- all_items[!sapply(all_items, is.null)]
  all_items_consistent <- lapply(all_items, function(df) {
    mutate(df, across(everything(), as.character))
  })
  final_data <- bind_rows(all_items_consistent)

  cat("Done.\n")
  return(final_data)
}


# --- THE BEST OF BOTH WORLDS: MIX-AND-MATCH FILTERING ---

# Define a single, readable list that combines both styles
hybrid_filter <- list(
  id_grupo = "007M", # Easy, named element for equality
  "pob_llega < 700", # Unnamed string for complex comparison
  "pob_sale >= 500" # Another one
)

# The function call is now extremely intuitive
final_data <- get_api_data(
  api_endpoint = "ine_mov.areas_diarias",
  where = hybrid_filter,
  # projection = list(id_grupo = 1, pob_sale = 1, pob_llega = 1),
  sort = "-pob_sale",
  limit = 500
)

glimpse(final_data)

# just get all data
final_data_all <- get_api_data(
  api_endpoint = "ine_mov.areas_diarias"
)


# This will generate the final 'where' clause for the API:
# {
#   "id_grupo": "007M",
#   "pob_llega": { "$lt": 1000 },
#   "pob_sale": { "$gte": 500 }
# }

date_range_filter <- list(
  date = list(
    "$gte" = "2020-02-22",
    "$lte" = "2020-03-02"
  )
)

# Call our existing function with the new endpoint and the new filter structure
daily_mobility_data <- get_api_data(
  api_endpoint = "mitma_mov.daily_mobility_matrix", # Use the correct endpoint
  where = date_range_filter,
  limit = 500
)

glimpse(daily_mobility_data)
