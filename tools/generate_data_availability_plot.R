#!/usr/bin/env Rscript
# Generate a data availability plot for the README
#
# This script is called by the data-availability-plot GitHub Actions workflow.
# It queries the Spanish mobility data source for available files and creates
# a timeline chart showing which dates have origin-destination data available.
#
# Usage:
#   Rscript tools/generate_data_availability_plot.R [output_path]
#
# If output_path is not provided, defaults to "data_availability.png".

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(spanishoddata)
}
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# --- Configuration ---
args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[1] else "data_availability.png"

cat("Generating data availability plot...\n")
cat("Output path:", output_path, "\n")

# --- Fetch available data ---
cat("Fetching v1 (2020-2021) metadata...\n")

# In GitHub Actions, MITMS blocks the IP, causing the XML download to fail.
# We mock the download function to fetch from the Internet Archive instead.
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  orig_fn <- spanishoddata:::spod_get_latest_v1_file_list
  assignInNamespace(
    "spod_get_latest_v1_file_list",
    function(data_dir = spanishoddata:::spod_get_data_dir(), xml_url = "https://opendata-movilidad.mitma.es/RSS.xml", quiet = FALSE) {
      orig_fn(data_dir, "https://web.archive.org/web/20240703091554id_/https://opendata-movilidad.mitma.es/RSS.xml", quiet)
    },
    ns = "spanishoddata"
  )
}

v1 <- tryCatch(
  spod_available_data(ver = 1, use_s3 = FALSE, quiet = TRUE),
  error = function(e) {
    cat("Warning: Could not fetch v1 data:", e$message, "\n")
    NULL
  }
)

cat("Fetching v2 (2022 onwards) metadata...\n")
v2 <- tryCatch(
  spod_available_data(ver = 2, quiet = TRUE),
  error = function(e) {
    cat("Warning: Could not fetch v2 data:", e$message, "\n")
    NULL
  }
)

if (is.null(v1) && is.null(v2)) {
  stop("Could not fetch data availability for either v1 or v2. Aborting.")
}

# --- Process data ---
# Focus on origin-destination, daily data
process_version <- function(data, ver_label, ver_num) {
  if (is.null(data)) return(tibble())
  
  df_filtered <- data |>
    filter(
      type == "origin-destination",
      !is.na(data_ymd)
    ) |>
    mutate(
      version = ver_label,
      zones = as.character(zones)
    ) |>
    select(date = data_ymd, version, zones) |>
    distinct()
  
  # For v1, municipalities are derived from districts and have identical availability,
  # but are not listed as separate entries in spod_available_data(ver = 1).
  # We explicitly duplicate the districts rows as municipalities for v1.
  if (ver_num == 1 && nrow(df_filtered) > 0) {
    muni_df <- df_filtered |>
      filter(zones == "districts") |>
      mutate(zones = "municipalities")
    df_filtered <- bind_rows(df_filtered, muni_df)
  }
  
  return(df_filtered)
}

v1_od <- process_version(v1, "v1 (2020\u20132021)", 1)
v2_od <- process_version(v2, "v2 (2022 onwards)", 2)

combined <- bind_rows(v1_od, v2_od) |>
  mutate(
    # Clean factor labels for the zones
    zones = factor(
      zones,
      levels = c("large_urban_areas", "municipalities", "districts"),
      labels = c("Large Urban Areas", "Municipalities", "Districts")
    )
  ) |>
  filter(!is.na(zones))

if (nrow(combined) == 0) {
  stop("No origin-destination daily data found. Aborting.")
}

# --- Calculate Midpoints of Longest Contiguous Segments for Gap-Free Labels ---
label_data <- combined |>
  group_by(version, zones) |>
  do({
    df_sub <- .
    df_sub <- df_sub |> arrange(date)
    # Identify gaps > 1 day to separate segments
    df_sub <- df_sub |>
      mutate(
        is_new_seq = if_else(is.na(lag(date)) | as.numeric(date - lag(date)) > 1, 1, 0),
        seq_id = cumsum(is_new_seq)
      )
    # Find the longest segment
    longest_seg <- df_sub |>
      group_by(seq_id) |>
      summarise(
        start = min(date),
        end = max(date),
        len = as.numeric(max(date) - min(date)),
        .groups = "drop"
      ) |>
      filter(len == max(len)) |>
      slice(1)
    
    # Calculate midpoint
    midpoint <- longest_seg$start + (longest_seg$end - longest_seg$start) / 2
    tibble(midpoint = midpoint)
  }) |>
  ungroup() |>
  mutate(
    label = if_else(grepl("v1", version), "v1", "v2")
  )

# --- Determine Latest & Start Available Dates ---
v1_dates <- combined |> filter(version == "v1 (2020\u20132021)") |> pull(date)
v2_dates <- combined |> filter(version == "v2 (2022 onwards)") |> pull(date)

# Format dates into a highly compact two-line format (e.g., "14 Feb\n2020")
format_date_label <- function(d) {
  if (length(d) == 0 || is.na(d)) return("N/A")
  paste0(format(d, "%d %b"), "\n", format(d, "%Y"))
}

date_extent <- function(dates) {
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(list(start = as.Date(NA), latest = as.Date(NA)))
  }

  list(start = min(dates), latest = max(dates))
}

v1_extent <- date_extent(v1_dates)
v2_extent <- date_extent(v2_dates)

start_v1_label  <- format_date_label(v1_extent$start)
latest_v1_label <- format_date_label(v1_extent$latest)
start_v2_label  <- format_date_label(v2_extent$start)
latest_v2_label <- format_date_label(v2_extent$latest)

# Write Shields.io JSON for dynamic badges
output_dir <- dirname(output_path)

write_latest_json <- function(latest_date, path, label, color) {
  available <- !is.na(latest_date)
  json <- list(
    schemaVersion = 1,
    label = label,
    message = if (available) as.character(latest_date) else "unavailable",
    color = if (available) color else "lightgrey"
  )

  jsonlite::write_json(json, path, auto_unbox = TRUE)
}

write_latest_json(
  v1_extent$latest,
  file.path(output_dir, "latest_v1.json"),
  "latest v1 data",
  "D35252"
)
write_latest_json(
  v2_extent$latest,
  file.path(output_dir, "latest_v2.json"),
  "latest v2 data",
  "9E1B1B"
)

# --- Determine Timeline Event Dates for Vertical Lines ---
# We place v1 date labels (2020-2021) in the blank space under the Municipalities bar (y = 1.3),
# and v2 date labels (2022 onwards) above the Districts bar (y = 3.4).
# To keep a consistent visual rhythm and prevent vertical lines from intersecting text,
# all date labels sit neatly to the left of their respective vertical lines (hjust = 1.15).
vline_data <- tibble(
  date = c(v1_extent$start, v1_extent$latest, v2_extent$start, v2_extent$latest),
  label = c(start_v1_label, latest_v1_label, start_v2_label, latest_v2_label),
  y_pos = c(1.3, 1.3, 3.4, 3.4),
  hjust = 1.15
) |>
  filter(!is.na(date))

# We draw a clean, continuous vertical segment for each timeline date behind/adjacent to the text.
segment_data <- tibble(
  date = vline_data$date,
  y = 0.5,
  yend = 3.25
)

# --- Create Plot ---
last_updated <- format(Sys.Date(), "%B %Y")
subtitle_text <- paste0(
  "Daily origin-destination flow matrices \u2022 Last checked: ", last_updated
)

p <- ggplot() +
  # Draw vertical timeline dashed line segments behind the text
  geom_segment(
    data = segment_data,
    aes(x = date, xend = date, y = y, yend = yend),
    color = "#475569", # Slate-600 for high visibility and clean look
    linetype = "dashed",
    linewidth = 1.0 # Prominent lines
  ) +
  # Draw data availability bars
  geom_tile(
    data = combined,
    aes(x = date, y = zones, fill = version),
    height = 0.5,
    linewidth = 0
  ) +
  # Place version labels inside the bars, in gap-free areas.
  # Since both dusty coral and deep crimson are dark reds, white text provides excellent contrast!
  geom_text(
    data = label_data,
    aes(x = midpoint, y = zones, label = label),
    color = "white",
    fontface = "bold",
    size = 7,
    vjust = 0.5
  ) +
  # Place subtle date labels to the side of the lines (ensuring no lines run through them)
  geom_text(
    data = vline_data,
    aes(x = date, y = y_pos, label = label, hjust = hjust),
    color = "#334155", # Darker charcoal slate for contrast
    fontface = "bold",
    size = 4.8,
    vjust = 0.5 # Perfectly centered vertically
  ) +
  # Customize scales
  scale_y_discrete(
    expand = expansion(add = c(0.5, 0.6)) # Expanded top margin just enough for the labels above the top bar without clipping
  ) +
  scale_x_date(
    date_breaks = "1 year",
    minor_breaks = "6 months",
    date_labels = "%Y",
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  # Highly premium custom color palette: Dusty Coral and Deep Crimson
  scale_fill_manual(
    values = c(
      "v1 (2020\u20132021)" = "#D35252",
      "v2 (2022 onwards)" = "#9E1B1B"
    )
  ) +
  labs(
    title = "Spanish Mobility Data (MITMS) \u2014 Data Availability", # Replaced MITMA with MITMS
    subtitle = subtitle_text,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 19) + # Increased base font size from 16 to 19
  theme(
    plot.title = element_text(face = "bold", size = 21, margin = margin(b = 6), color = "#1E293B"), # Increased from 18 to 21
    plot.subtitle = element_text(colour = "#64748B", size = 15, margin = margin(b = 15)), # Increased from 13 to 15
    axis.text.x = element_text(face = "bold", color = "#475569", size = 16), # Increased from 13 to 16
    axis.text.y = element_text(face = "bold", color = "#475569", size = 16), # Increased from 13 to 16
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E2E8F0", linewidth = 0.5),
    panel.grid.minor.x = element_line(color = "#F1F5F9", linewidth = 0.25),
    plot.margin = margin(15, 20, 15, 15)
  )

# --- Save Plot (Square-ish layout) ---
plot_width <- 10
plot_height <- 6.2

ggsave(
  filename = output_path,
  plot = p,
  width = plot_width,
  height = plot_height,
  dpi = 150,
  bg = "white"
)

cat("\nPlot saved to:", output_path, "\n")
cat("Dimensions:", plot_width, "x", plot_height, "inches at 150 DPI\n")

# --- Save Interactive Plot (HTML) if ggiraph is available ---
if (requireNamespace("ggiraph", quietly = TRUE) && requireNamespace("htmlwidgets", quietly = TRUE)) {
  cat("Generating interactive ggiraph HTML plot...\n")
  
  # Create interactive version of ggplot
  p_interactive <- ggplot() +
    # Draw vertical timeline dashed line segments behind the text
    geom_segment(
      data = segment_data,
      aes(x = date, xend = date, y = y, yend = yend),
      color = "#475569",
      linetype = "dashed",
      linewidth = 1.0
    ) +
    # Draw data availability bars with interactive tooltips and hover
    ggiraph::geom_tile_interactive(
      data = combined,
      aes(
        x = date,
        y = zones,
        fill = version,
        tooltip = paste0(
          "<strong>Version:</strong> ", version, "<br/>",
          "<strong>Zone:</strong> ", zones, "<br/>",
          "<strong>Date:</strong> ", format(date, "%d %b %Y")
        ),
        data_id = paste0(version, "_", zones)
      ),
      height = 0.5,
      linewidth = 0
    ) +
    # Place version labels inside the bars
    geom_text(
      data = label_data,
      aes(x = midpoint, y = zones, label = label),
      color = "white",
      fontface = "bold",
      size = 7,
      vjust = 0.5
    ) +
    # Place subtle date labels to the side of the lines (ensuring no overlap)
    geom_text(
      data = vline_data,
      aes(x = date, y = y_pos, label = label, hjust = hjust),
      color = "#334155",
      fontface = "bold",
      size = 4.8,
      vjust = 0.5
    ) +
    # Customize scales
    scale_y_discrete(
      expand = expansion(add = c(0.5, 0.6))
    ) +
    scale_x_date(
      date_breaks = "1 year",
      minor_breaks = "6 months",
      date_labels = "%Y",
      expand = expansion(mult = c(0.12, 0.12))
    ) +
    scale_fill_manual(
      values = c(
        "v1 (2020\u20132021)" = "#D35252",
        "v2 (2022 onwards)" = "#9E1B1B"
      )
    ) +
    labs(
      title = "Spanish Mobility Data (MITMS) \u2014 Data Availability",
      subtitle = subtitle_text,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 19) +
    theme(
      plot.title = element_text(face = "bold", size = 21, margin = margin(b = 6), color = "#1E293B"),
      plot.subtitle = element_text(colour = "#64748B", size = 15, margin = margin(b = 15)),
      axis.text.x = element_text(face = "bold", color = "#475569", size = 16),
      axis.text.y = element_text(face = "bold", color = "#475569", size = 16),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E2E8F0", linewidth = 0.5),
      panel.grid.minor.x = element_line(color = "#F1F5F9", linewidth = 0.25),
      plot.margin = margin(15, 20, 15, 15)
    )
    
  # Render girafe widget
  x <- ggiraph::girafe(
    ggobj = p_interactive,
    width_svg = plot_width,
    height_svg = plot_height,
    options = list(
      ggiraph::opts_hover(css = "fill-opacity:0.85;stroke:#1E293B;stroke-width:1.5px;"),
      ggiraph::opts_tooltip(
        css = "background-color:#1E293B;color:#F8FAFC;font-family:sans-serif;padding:8px;border-radius:4px;font-size:14px;border:none;",
        use_cursor_pos = TRUE
      )
    )
  )
  
  # Save to standalone interactive HTML
  html_output_path <- file.path(output_dir, "data_availability.html")
  htmlwidgets::saveWidget(x, html_output_path, selfcontained = TRUE)
  cat("Interactive plot saved to:", html_output_path, "\n")
}
