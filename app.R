# Reef Persistence Tool ----

# Adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center
# from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Lauren T. Toth (USGS) and John T. Morris (NOAA).

# Call Packages ----
library(sf)
library(DT)
library(png)
library(jpeg)
library(maps)
library(here)
library(bslib)
library(dplyr)
library(later)
library(tidyr)
library(RCurl)
library(plotly)
library(readxl)
library(writexl)
library(stringr)
library(ggplot2)
library(ggforce)
library(leaflet)
library(leaflegend)
library(tidyverse)
library(magrittr)
library(reshape2)
library(rsconnect)
library(geojsonio)
library(jsonlite)
library(RColorBrewer)

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)

# Enable automatic reloading of the app when code changes are detected
options(shiny.autoreload = TRUE)

# Quiet Excel reader ----
# readxl auto-repairs blank/duplicate headers and prints "New names: `` -> `...2`"
# to the console. .name_repair = "unique_quiet" suppresses that for every read.
read_excel_quiet <- function(path, ...) {
  readxl::read_excel(path, .name_repair = "unique_quiet", ...)
}

# ---- Global log buffer + writer ----
# log_msg() is callable from ANY scope (top-level model functions included),
# because it lives in the global environment. It appends to a plain-environment
# buffer; the server drains that buffer into a reactiveVal for the log panel.
# A version counter lets the server know when new lines have arrived.
.LOG_ENV <- new.env(parent = emptyenv())
.LOG_ENV$lines   <- character(0)
.LOG_ENV$version <- 0L

log_msg <- function(..., console = FALSE) {
  msg <- paste0(...)
  # stamped <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
  .LOG_ENV$lines   <- c(.LOG_ENV$lines, msg) #stamped)
  # Cap to the most recent 2000 lines to bound memory over long sessions.
  if (length(.LOG_ENV$lines) > 2000) {
    .LOG_ENV$lines <- utils::tail(.LOG_ENV$lines, 2000)
  }
  .LOG_ENV$version <- .LOG_ENV$version + 1L
  if (console) cat(msg, "\n")
  invisible(NULL)
}

log_clear_buffer <- function() {
  .LOG_ENV$lines   <- character(0)
  .LOG_ENV$version <- .LOG_ENV$version + 1L
  invisible(NULL)
}

# Ingest data ----
# World map data for leaflet map
world_data   <- ggplot2::map_data("world")
worldcountry <- fortify(world_data)

# Observational data to be fed into the growth model:
# Assemblage-specific porosity
porosity     <- read.csv(here("data", "Porosity.csv"))
# Species-specific growth rates (linear extension and planar growth)
growth_rates <- read.csv(here("data", "growth_rates_ReefBudget_NCRMP_Aug2026.csv"))
# Species-specific calcification rates
calc_rates   <- read.csv(here("data", "Calcification_Rates_Courtney_et_al_2024.csv"))
# Species-specific average colony diameters
diams        <- read.csv(here("data", "NCRMP_Colony_Diam_Florida.csv"))
# Region- and habitat-specific bioerosion rates
bioerosion   <- read.csv(here("data", "Bioerosion_Rates_Regional.csv"))
# Species / morphology-specific Year 1 outplant mortality
mortality_outplant <- read.csv(here("data", "Mortality_Rates_Outplant_3cm_Mote.csv"))
# Morph-specific chronic partial mortality rates (for annual growth reduction)
mortality_partial <- read.csv(here("data", "Mortality_Rates_Partial_Browne_et_al_2026.csv"))
# Morph-specific chronic whole-colony mortality rates (for annual colony loss)
# (Rate is typically negligible but could make an impact if simulating thousands of colonies)
mortality_whole <- read.csv(here("data", "Mortality_Rates_WholeColony_Browne_et_al_2026.csv"))
# Species- and region-specific degree-heating-week-driven percent-cover loss relationships
sp_dhw_slopes <- read.csv(here("data", "Species_DHW_Slopes_Webb.csv"))

# Species-specific bioerosion rate lookups (Monitoring tab) ----
# Three sheets: "Parrotfish", "Urchins", "Sponges". Read at startup so the
# observed-bioerosion pipeline can join per-taxon rates. Missing file / sheet
# is tolerated (returns NULL and the pipeline falls back to regional rates).
read_sheet_safe <- function(path, sheet) {
  tryCatch(
    read_excel_quiet(path, sheet = sheet),
    error = function(e) NULL
  )
}

# Function to find the nearest number in a vector to a given value
nearest_value <- function(vec, target) {
  # Input validation
  if (!is.numeric(vec) || !is.numeric(target)) {
    stop("Both 'vec' and 'target' must be numeric.")
  }
  if (length(vec) == 0) {
    stop("The input vector is empty.")
  }

  # Remove NA values
  vec <- vec[!is.na(vec)]
  if (length(vec) == 0) {
    stop("The vector contains only NA values.")
  }

  # Find index of the closest value
  idx <- which.min(abs(vec - target))

  # Return the closest value
  vec[idx]
}

# Change any null/NA value to 0.
# Optionally, Change any positive value to 0
null_to_zero <- function(v, positive_null = FALSE) {
  if (positive_null) {
    if (!is.null(v) && !is.na(v) && v > 0) return(0)
  }
  if (is.null(v) || is.na(v)) 0 else v
}

coral_icon <- makeIcon(
  iconUrl = here("www", "coral_icon.svg"),
  iconWidth = 40,            # Width in pixels
  iconHeight = 40,           # Height in pixels
  iconAnchorX = 0,          # Anchor point X (center)
  iconAnchorY = 0           # Anchor point Y (bottom))
)

# Species-specific bioerosion rates
species_bioerosion_path <- here("data", "Bioerosion_Rates_Species.xlsx")
sp_erosion_parrotfish <- read_sheet_safe(species_bioerosion_path, "Parrotfish")
sp_erosion_urchins    <- read_sheet_safe(species_bioerosion_path, "Urchins")
sp_erosion_sponges    <- read_sheet_safe(species_bioerosion_path, "Sponges")

# NCRMP carbonate budget survey data
df <- read.csv(here("data", "NCRMP_CarbonateBudgets_2014_to_2024.csv"))

# Create unique site IDs in case PRIMARY_SAMPLE_UNIT is reused/not unique
df$site_id <- paste(df$YEAR, df$SUB_REGION, df$PRIMARY_SAMPLE_UNIT, sep = "_")

sites <- sort(df$site_id)

# Ingest regions polygon shapefile
regions_sf <- sf::st_read(here("data", "regions", "regions.shp"), quiet = TRUE)
# Ensure geographic CRS (WGS84) so it aligns with the leaflet basemap
regions_sf <- sf::st_transform(regions_sf, 4326)

# Ingest named-reef point shapefile (labels in the "Location" field).
# Tolerated if missing -> NULL, and the layer/checkbox simply draws nothing.
named_reefs_sf <- tryCatch(
  sf::st_transform(
    sf::st_read(here("data", "named_reefs", "named_reefs.shp"), quiet = TRUE),
    4326
  ),
  error = function(e) NULL
)

# Additional named reefs from FKNMS
named_reefs_sf_fknms <- tryCatch(
  sf::st_transform(
    sf::st_read(here("data", "named_reefs", "named_reefs_fknms.shp"), quiet = TRUE),
    4326
  ),
  error = function(e) NULL
)

# Pastel palette keyed to the Region field
region_levels <- sort(unique(regions_sf$Region))
pastel_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))(length(region_levels))
region_pal <- colorFactor(pastel_colors, domain = region_levels)

# Ingest baseline cover data template to retrieve list of taxa.
# Sort alphabetically.
taxa <- read_excel_quiet(here("www", "Baseline_Cover_TEMPLATE.xlsx"), sheet = "Taxa")
taxa <- sort(taxa$Taxon)

suggested_taxa <- c(
  "Acropora cervicornis",
  "Acropora palmata",
  "Orbicella faveolata",
  "Orbicella annularis",
  "Montastraea cavernosa",
  "Pseudodiploria strigosa",
  "Pseudodiploria clivosa",
  "Porites astreoides",
  "Stephanocoenia intersepta",
  "Siderastrea siderea",
  "Diploria labryinthinformis",
  "Orbicella franksi",
  "Dichocoenia stokesii",
  "Solenastrea bournoni")

# Remove the suggested taxa from their original indices and place them at the top of the list
taxa <- c(suggested_taxa, setdiff(taxa, suggested_taxa))

# Ingest NASA Interagency sea-level projections (PSMSL id 1701 (Vaca Key), "Total" sheet)
slr_raw <- read_excel_quiet(
  here("data", "sl_taskforce_scenarios_psmsl_id_1701.xlsx"),
  sheet = "Total"
)

# Keep only the median (quantile 50), medium-confidence rows
slr_med <- slr_raw[slr_raw$quantile == 50, , drop = FALSE]
# & slr_raw$confidence == "medium", # this filter is only necessary for NASA IPCC data, not Interagency data.

# Identify year columns (headers that are purely 4-digit numeric)
slr_year_cols <- names(slr_med)[grepl("^[0-9]{4}$", names(slr_med))]
slr_years_all <- as.integer(slr_year_cols)

# Build a per-scenario lookup: scenario -> named numeric vector (year -> m)
slr_scenarios <- c("Low", "IntLow", "Int", "IntHigh", "High")

slr_by_scenario <- lapply(slr_scenarios, function(scn) {
  row <- slr_med[slr_med$scenario == scn, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  vals_cm <- as.numeric(unlist(row[1, slr_year_cols])) / 1000 # convert mm to cm
  setNames(vals_cm, slr_years_all) # values in cm
})
names(slr_by_scenario) <- slr_scenarios
slr_by_scenario <- slr_by_scenario[!vapply(slr_by_scenario, is.null, logical(1))]

# Given a projection start year and horizon (n_years), return a data.frame of
# the SLR RATE (mm/yr) for every scenario across the whole simulation range.
# Unlike the earlier +/- decade version, this interpolates over the ENTIRE
# dataset (all available year columns) so it supports long horizons (10..150
# years). Reported as an annual rate so it shares units (mm/yr) with Reef
# Accretion Potential.
build_slr_timeline <- function(start_year, n_years = 10) {
  sim_years <- start_year + (0:n_years)

  out <- lapply(names(slr_by_scenario), function(scn) {
    vec <- slr_by_scenario[[scn]]
    ax  <- as.integer(names(vec))       # every year column in the dataset
    if (length(ax) < 2) return(NULL)

    ay_m <- as.numeric(vec)             # m at each dataset year
    # Smooth (monotone spline) cumulative SLR so the year-over-year rate below
    # is a continuous slope rather than the piecewise-constant steps that a
    # piecewise-linear fit would produce.
    fit <- splinefun(ax, ay_m, method = "monoH.FC")

    slr_m   <- fit(sim_years)
    slr_mm  <- slr_m * 1000             # m -> mm
    # Year-to-year rate (mm/yr). Year 0 uses the same rate as Year 1 so the
    # series has a defined starting rate rather than 0.
    slr_rate <- c(NA, diff(slr_mm))
    slr_rate[1] <- slr_rate[2]

    data.frame(
      Year     = 0:n_years,
      SLR      = slr_rate,
      Scenario = scn,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out[!vapply(out, is.null, logical(1))])
}

# Int annual SLR RATE (mm/yr) at a specific calendar year. Computed as the
# year-over-year difference of the interpolated cumulative SLR, matching the
# rate convention used by build_slr_timeline. Used for the Scenario Comparison
# RAP barplot reference lines (2030 / 2050 / 2070).
Int_rate_at <- function(cal_year) {
  vec <- slr_by_scenario[["Int"]]
  if (is.null(vec)) return(NA_real_)
  ax <- as.integer(names(vec))
  if (length(ax) < 2) return(NA_real_)
  fit <- approxfun(ax, as.numeric(vec), rule = 2)   # cumulative SLR (m) vs year
  (fit(cal_year) - fit(cal_year - 1)) * 1000        # m/yr -> mm/yr
}

# Calculate reef accretion potential
df$rap <- df$net_G / 2.9 / (1 - 0.6265)
df$current_state <- ifelse(df$rap > 0.5, "growth", ifelse(df$rap < -0.5, "erosion", "stasis"))

# RAP percentile helper ----
# Rank a RAP value against the NCRMP baseline distribution (df$rap).
rap_percentile <- function(rap_value) {
  vals <- df$rap[is.finite(df$rap)]
  if (length(vals) == 0) return(NA_real_)
  if (length(rap_value) != 1 || !is.finite(rap_value)) return(NA_real_)
  mean(vals < rap_value, na.rm = TRUE) * 100
}

# Color a percentile: dark green >75, green >50, orange >25, else red.
percentile_color <- function(pct) {
  if (is.na(pct)) return("#777777")
  if (pct > 75) "#1a7a1a" else if (pct > 50) "#4caf50" else if (pct > 25) "#e69500" else "#d9534f"
}

# Linear regression: RAP ~ % Cover ----
# Used on the Home tab to translate a target percent-cover increase into a
# projected ("restored") RAP for each site.
fit <- lm(rap ~ hardCoral_PrctCvr, data = df)
# summary(fit)

# Quadratic model
# df$hardCoral_PrctCvr_2 <- df$hardCoral_PrctCvr ^ 2
# fit_q <- lm(rap ~ hardCoral_PrctCvr + hardCoral_PrctCvr_2, data = df)
# summary(fit_q)

# # smoothScatter(df$hardCoral_PrctCvr, df$rap) # Another way to plot the data

# # Plot the model
# (Code retained for posterity; does not need to run every time.)
# png(filename = here("cache", "RAP_LM.png"))
# plot(rap ~ hardCoral_PrctCvr, data = df, pch = 20)
# abline(fit) # Add linear model to plot

# # Generate prediction with quadratic model
# cover_vals <- seq(0, max(df$hardCoral_PrctCvr), 0.1)
# rap_predict <- predict(fit_q, list(hardCoral_PrctCvr = cover_vals, hardCoral_PrctCvr_2 = cover_vals^2))
# lines(cover_vals, rap_predict, col = "blue") # Add quadratic model to plot

# dev.off()

cover_rap_slope <- unname(coef(fit)["hardCoral_PrctCvr"])

# Directory for saved restoration scenarios (Scenario Comparison tab)
scenario_dir <- here("scenarios")
if (!dir.exists(scenario_dir)) dir.create(scenario_dir, showWarnings = FALSE)

# Cache dir for the most-recent baseline .xlsx (auto-reload on launch) ----
cache_dir <- here("cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir, showWarnings = FALSE)
cached_baseline_path <- file.path(cache_dir, "last_baseline.xlsx")

# Cache paths for the Monitoring-tab uploads (auto-reload on launch) ----
# Extensions are resolved at load time (either .csv or .xlsx may be present).
cached_cover_stub      <- file.path(cache_dir, "last_monitoring_cover")
cached_bioerosion_stub <- file.path(cache_dir, "last_monitoring_bioerosion")

# Find an existing cached file for a stub, trying .xlsx then .csv.
find_cached <- function(stub) {
  for (ext in c("xlsx", "csv")) {
    p <- paste0(stub, ".", ext)
    if (file.exists(p)) return(p)
  }
  NULL
}

# Canonical field order for saved scenarios (used to sanitize read + write) ----
scenario_fields <- c(
  "project", "scenario", "site", "subregion", "habitat", "site_area_m2",
  "total_coral_pct_cvr", "baseline_cover", "restored_cover",
  "baseline_budget", "restored_budget",
  "baseline_rap", "restored_rap", "outplants",
  "dhw", "bleach_events", "rest_horizon", "sim_duration",
  "cost", "roi", "elev_gain_10yr", "saved"
)

# Coerce a parsed scenario (list from fromJSON) into a clean one-row data.frame.
# Guards against length-0 / NULL / multi-length fields that break as.data.frame.
scenario_to_row <- function(s) {
  if (is.null(s) || !length(s)) return(NULL)
  vals <- lapply(scenario_fields, function(f) {
    v <- s[[f]]
    if (is.null(v) || length(v) == 0) return(NA)
    # Collapse any accidental multi-length field to a single scalar
    if (length(v) > 1) v <- paste(v, collapse = "; ")
    v
  })
  names(vals) <- scenario_fields
  as.data.frame(vals, stringsAsFactors = FALSE)
}

# Subregion code -> full label remap ----
# (used for the Subregion dropdown)
subregion_labels <- c(
  "SEFCRI" = "SoutheastFlorida",
  "BISC"   = "Biscayne",
  "UK"     = "UpperKeys",
  "MK"     = "MiddleKeys",
  "LK"     = "LowerKeys",
  "DRTO"   = "DryTortugas"
)

# Abbreviate a species name to "G. species" unless it ends with "spp."
abbrev_species <- function(s) {
  if (grepl("spp\\.$", s)) return(s)          # leave "Genus spp." intact
  parts <- stringr::str_split(s, " ")[[1]]
  if (length(parts) != 2) return(s)            # Nonstandard (e.g. CCA); return as-is
  paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
}

# Abbreviate a species name to "Gspe" (1 genus letter + 3 species letters).
# Leaves "Genus spp." intact.
abbrev_species_code <- function(s) {
  if (grepl("spp\\.$", s)) return(s)
  parts <- stringr::str_split(s, " ")[[1]]
  if (length(parts) != 2) return(s)
  paste0(substr(parts[1], 1, 1), substr(parts[2], 1, 3))
}

# Two-line species label for target-cover inputs: genus on line 1, remainder on line 2.
make_species_label <- function(s, split_line = TRUE) {
  parts <- stringr::str_split(s, " ")[[1]]
  if (length(parts) < 2) return(HTML(s))
  if (split_line) {
    HTML(paste0(parts[1], "<br>", paste(parts[-1], collapse = " ")))
  } else {
    paste(parts[1], parts[-1])
  }
}

# Shared y-axis floor rule ----
# If the data minimum sits above -2, pin the lower limit to -1 (the default
# floor) so the yellow / red status bands are always visible; otherwise expand
# to the data minimum.
rap_axis_min <- function(data_min) {
  if (!is.finite(data_min)) return(-1)
  if (data_min > -2) -1 else data_min
}

# Shared y-axis break rule ----
# Breaks are even numbers upward. When the floor is the -1 default, -1 is forced
# as the FIRST break (odd, special-cased) followed by evens: -1, 0, 2, 4, ...
# When the floor is <= -2 (data-driven), the -1 tick is unnecessary and omitted;
# breaks are pure evens from the nearest even at/below the floor up to the top.
rap_axis_breaks <- function(y_lo, y_hi) {
  hi_even <- ceiling(y_hi / 2) * 2
  step <- if (hi_even <= 30) 2 else if (hi_even <= 50) 5 else 10
  min_val <- if (step <= 5) -1 else 0
  if (y_lo <= -2) {
    lo_even <- floor(y_lo / 2) * 2
    seq(lo_even, hi_even, by = step)
  } else {
    c(min_val, seq(0, hi_even, by = step))
  }
}

# Status-band data frame builder ----
# Two-row df (erosion + stasis) carrying a `label` column that rides through
# ggplotly's tooltip="text" channel, so bands hover as "erosion"/"stasis"
# instead of "trace 0"/"trace 1". Drawn with geom_rect (annotate() can't carry
# a tooltip aesthetic).
status_bands_df <- function(xmin, xmax, y_lo) {
  data.frame(
    xmin = c(xmin, xmin), xmax = c(xmax, xmax),
    ymin = c(y_lo, -0.5),  ymax = c(-0.5, 0.5),
    fill = c("red", "yellow"), label = c("erosion", "stasis"),
    stringsAsFactors = FALSE
  )
}

# Insert interpolated crossing points at RAP == threshold so a clamped ribbon
# terminates exactly where the line crosses, with no fill slivers. `df` needs
# an x column (named by `xcol`) and a `RAP` column; extra columns are carried
# through (NA at synthetic rows). Adds a `ribbon_max` column = pmax(threshold, RAP).
insert_threshold_crossings <- function(df, xcol = "Year", threshold = 0.5) {
  df <- df[order(df[[xcol]]), , drop = FALSE]
  if (nrow(df) < 2) {
    df$ribbon_max <- pmax(threshold, df$RAP)
    return(df)
  }
  out <- df[0, , drop = FALSE]
  for (i in seq_len(nrow(df) - 1)) {
    out <- rbind(out, df[i, , drop = FALSE])
    y1 <- df$RAP[i]
    y2 <- df$RAP[i + 1]
    # Straddles the threshold (strictly on opposite sides)?
    if (is.finite(y1) && is.finite(y2) &&
        ((y1 < threshold && y2 > threshold) || (y1 > threshold && y2 < threshold))) {
      x1 <- df[[xcol]][i]
      x2 <- df[[xcol]][i + 1]
      frac <- (threshold - y1) / (y2 - y1)
      xc <- x1 + frac * (x2 - x1)
      cross <- df[i, , drop = FALSE]        # template row (carries other cols)
      cross[] <- NA                          # blank all fields
      cross[[xcol]] <- xc
      cross$RAP <- threshold
      out <- rbind(out, cross)
    }
  }
  out <- rbind(out, df[nrow(df), , drop = FALSE])
  rownames(out) <- NULL
  out$ribbon_max <- pmax(threshold, out$RAP)
  out
}

# Bleaching lookup ----
# Percent reduction in cover per degree-heating week
dhw_slope_lookup_fk <- tibble::tribble(
  ~taxon,           ~slope_pct_per_dhw,
  "Acropora",       2.00,  # Conservative estimate
  "Agaricia",       1.00,  # Based on morph similarity to Porites
  "Orbicella",      0.85,  # (6.8% / 8)
  "Montastraea",    1.56,  # (12.5% / 8)
  "Colpophyllia",   0.89,  # (7.1% / 8)
  "Porites",        1.37,  # (11.0% / 8)
  "Siderastrea",    0.21,  # (1.7% / 8)
  "Pseudodiploria", 0.80,  # Generic brain coral estimate
  "Diploria",       0.80,
  "Stephanocoenia", 0.50,
  "Madracis",       0.50,
  "Millepora",      0.50
)

# Retrieve the appropriate region from the provided subregion.
# Update this function as more regional data is added.
region_from_subregion <- function(subregion) {
  if (grepl("Keys", subregion, fixed = TRUE)) {
    region <- "Florida Keys"
  } else if (subregion == "DryTortugas") {
    region <- "Dry Tortugas"
  } else {
    region <- "?"
  }
}

# Retrieve degree-heating-week percent-loss slope from Webb's data.
# Handle missing species and regions.
get_dhw_intrvl <- function(s, subregion) {

  region <- region_from_subregion(subregion)

  sp_dhw_slopes_region <- subset(sp_dhw_slopes, Region == region)
  if (length(sp_dhw_slopes_region) == 0) { # Use region-generic rate if region not present.
    sp_dhw_slopes_region <- subset(sp_dhw_slopes, Region == "Cross-region")
  }

  sp_dhw_slope <- subset(sp_dhw_slopes_region, Species == s)$Slope
  # NOTE: In this dataset, the lower bound designates a more severe DHW response.
  sp_dhw_lower <- subset(sp_dhw_slopes_region, Species == s)$CI_Lower
  sp_dhw_upper <- subset(sp_dhw_slopes_region, Species == s)$CI_Upper

  # If species not found, look for a species-generic rate
  if (is.null(sp_dhw_slope) || length(sp_dhw_slope) == 0) {
    genus        <- stringr::str_split(s, " ")[[1]][1]
    g_spp        <- paste(genus, "spp.")
    sp_dhw_slope <- subset(sp_dhw_slopes_region, Species == g_spp)$Slope
    sp_dhw_lower <- subset(sp_dhw_slopes_region, Species == g_spp)$CI_Lower
    sp_dhw_upper <- subset(sp_dhw_slopes_region, Species == g_spp)$CI_Upper
  }

  if (is.null(sp_dhw_slope) || length(sp_dhw_slope) == 0) { # If still not found, use mean rate
    sp_dhw_slope <- sp_dhw_slopes_region$mean_slope[1]
    # Only Florida Keys and Dry Tortugas have real regional confidence intervals.
    # Other regions repeat their mean values.
    sp_dhw_lower <- sp_dhw_slopes_region$mean_slope_lower[1]
    sp_dhw_upper <- sp_dhw_slopes_region$mean_slope_upper[1]
  }

  # Set to 0 if slope is initially positive/NA.
  # Flip the sign of the result so values are ultimately positive for later handling.
  sp_dhw_slope <- null_to_zero(sp_dhw_slope, positive_null = TRUE) * -1
  sp_dhw_lower <- null_to_zero(sp_dhw_lower, positive_null = TRUE) * -1
  sp_dhw_upper <- null_to_zero(sp_dhw_upper, positive_null = TRUE) * -1

  # Enusre zeroes are not portrayed as negative (just a formatting thing)
  sp_dhw_slope <- if (sp_dhw_slope == 0) 0 else sp_dhw_slope
  sp_dhw_lower <- if (sp_dhw_lower == 0) 0 else sp_dhw_lower
  sp_dhw_upper <- if (sp_dhw_upper == 0) 0 else sp_dhw_upper

  c(sp_dhw_lower, sp_dhw_slope, sp_dhw_upper) # Flip all negative slopes to positive for later handling.
}

# Post-bleaching production-loss vector:
# Reductions applied the 1st .. 4th years after bleaching occurs
pbr <- c(0.60, 0.35, 0.15, 0.05)

# Species vectors for porosity and partial mortality calculations:
all_massive_species <- c(
  "Colpophyllia natans",
  "Diploria labyrinthiformis",
  "Favia fragum",
  "Favia spp.",
  "Isophyllia rigida",
  "Isophyllia sinuosa",
  "Isophyllia spp.",
  "Meandrina meandrites",
  "Meandrina spp.",
  "Montastraea cavernosa",
  "Mycetophyllia aliciae",
  "Mycetophyllia ferox",
  "Mycetophyllia lamarckiana",
  "Mycetophyllia spp.",
  "Orbicella annularis",
  "Orbicella faveolata",
  "Orbicella franksi",
  "Pseudodiploria clivosa",
  "Pseudodiploria strigosa",
  "Scolymia cubensis",
  "Scolymia lacera",
  "Scolymia spp.",
  "Siderastrea radians",
  "Siderastrea siderea",
  "Solenastrea bournoni",
  "Solenastrea hyades",
  "Hard Coral (massive)"
)

all_branching_species <- c(
  "Acropora cervicornis",
  "Acropora palmata",
  "Acropora prolifera",
  "Acropora spp.",
  "Cladocora arbuscula",
  "Madracis auretenra",
  "Madracis carmabi",
  "Millepora spp.",
  "Millepora alcicornis",
  "Millepora squarrosa",
  "Oculina spp.",
  "Oculina diffusa",
  "Porites divaricata",
  "Porites furcata",
  "Porites porites",
  "Stylaster roseus",
  "Hard Coral (branching)"
)

# Outplant defaults (used to auto-fill per-species cells when a target is set) ----
OUTPLANT_DIAM_DEFAULT <- 5    # cm
OUTPLANT_COST_DEFAULT <- 100  # $

# Special pseudo-taxa (not grown as corals) ----
UC_TAXON  <- "REQUIRED Unconsolidated substrate"
OLB_TAXON <- "REQUIRED Other living benthos"
CCA_TAXON <- "Crustose coralline algae"

# Is this taxon a non-growing reserved/benthos pseudo-taxon?
is_reserved_taxon <- function(s) s %in% c(UC_TAXON, OLB_TAXON)

# Morphology class for a species (branching / massive / weedy-other) ----
# Hoisted from build_calcifier_table so the Restoration Mix grid can reuse it.
morph_class <- function(s) {
  if (s %in% all_branching_species) "Branching"
  else if (s %in% all_massive_species) "Massive"
  else "Weedy / Other"
}

# Compiled per-species calcifier data table (Calcifier Data tab) ----
# Joins the species-keyed reference tables (calcification, growth, diameter) by
# species name, then attaches morphology-class values (outplant + chronic
# mortality) using the same branching/massive/weedy classification the model
# uses. Built once at startup; displayed as a sortable DT table.
build_calcifier_table <- function(subregion) {

  # Union of all species appearing in any species-keyed table.
  sp_all <- sort(unique(c(
    calc_rates$Taxon,
    growth_rates$name,
    diams$name
  )))
  sp_all <- sp_all[!is.na(sp_all) & nzchar(sp_all)]

  # Outplant mortality group key used by mortality_outplant.
  outplant_group <- function(s) {
    if (s %in% mortality_outplant$Species) s
    else if (s %in% all_branching_species) "Acropora"
    else if (s %in% all_massive_species) "Massives"
    else "Weedy"
  }

  get1 <- function(df, key_col, key, val_col) {
    v <- df[[val_col]][df[[key_col]] == key]
    if (length(v) == 0 || all(is.na(v))) NA_real_ else suppressWarnings(as.numeric(v[1]))
  }

  rows <- lapply(sp_all, function(s) {
    grp <- outplant_group(s)
    om  <- get1(mortality_outplant, "Species", grp, "Mortality")
    ose <- get1(mortality_outplant, "Species", grp, "SE")
    genus        <- stringr::str_split(s, " ")[[1]][1]
    sp_dhw_intrvl <- get_dhw_intrvl(s, subregion)
    # sp_dhw_slope <- dhw_slope_lookup_fk$slope_pct_per_dhw[dhw_slope_lookup_fk$taxon == genus]
    # if (length(sp_dhw_slope) == 0) sp_dhw_slope <- 0.85 # generic fallback

    data.frame(
      Species             = s,
      Morphology          = morph_class(s),
      Calc_rate_kg_m2_yr  = get1(calc_rates, "Taxon", s, "rate"),
      Calc_calc_rate_lo        = if ("lower_bound" %in% names(calc_rates)) get1(calc_rates, "Taxon", s, "lower_bound") else NA_real_,
      Calc_calc_rate_hi        = if ("upper_bound" %in% names(calc_rates)) get1(calc_rates, "Taxon", s, "upper_bound") else NA_real_,
      Planar_growth_mm_yr = get1(growth_rates, "name", s, "planar_mean"),
      Planar_growth_lo    = get1(growth_rates, "name", s, "planar_lwr"),
      Planar_growth_hi    = get1(growth_rates, "name", s, "planar_upr"),
      Avg_colony_diam_cm  = get1(diams, "name", s, "length_mean"),
      dhw_loss_intrvl     = sprintf("%.1f : %.1f : %.1f",  sp_dhw_intrvl[[1]], sp_dhw_intrvl[[2]], sp_dhw_intrvl[[3]]),
      Outplant_mort_pct   = om,
      Outplant_mort_SE    = ose,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # Round numeric columns for display.
  num_cols <- vapply(out, is.numeric, logical(1))
  # Pretty column names
  col_names <- c(
    "Species",
    "Morphology",
    "Calc. Rate\n(kg/m²/yr)",
    "Calc. Rate\n(Low)",
    "Calc. Rate\n(High)",
    "Planar growth\n(mm/yr)",
    "Planar growth\n(Low)",
    "Planar growth\n(High)",
    "Avg. Colony Diam.\n(cm)",
    "Mortality per DHW\n(%)",
    "Outplant Mortality\n(%)",
    "Outplant Mortality\nSE (%)"
  )
  colnames(out) <- col_names
  out[num_cols] <- lapply(out[num_cols], function(x) round(x, 3))
  out
}

# Mortality by colony size & species:
outplant_mortality_by_size <- function(size, sp) {
  # Use generic fallback if species-specific outplant dieoff data is not available
  # ...Are some species classified differently for mortality than they are for porosity?
  # NOTE: Pseudodiploria strigosa and Porites astreoides lack legitimate SD/SE.
  # An arbitrary 5% SD/SE has been imposed for these species.
  if (!(sp %in% mortality_outplant$Species)) {
    if (sp %in% all_branching_species) {
      sp <- "Acropora"
    } else if (sp %in% all_massive_species) {
      sp <- "Massives"
    } else {
      sp <- "Weedy"
    }
  }
  mort <- mortality_outplant[mortality_outplant$Species == sp, "Mortality"][[1]] / 100 # percent to proportion
  mort_se <- mortality_outplant[mortality_outplant$Species == sp, "SE"][[1]] / 100
  mort_lo <- (mort - mort_se)
  mort_hi <- (mort + mort_se)

  mort_intrvl <- c(mort_lo, mort, mort_hi)

  bin <- round(floor(size / 5)) # Determine how many 5-cm bins above the 0-5cm bin this colony's size is
  # Reduce mortality by 5% of the original value for each bin above 0-5cm
  reduc <- bin * 0.05
  mort_intrvl <- mort_intrvl * (1 - reduc)
}

# Assemblage-porosity selector ----
# Chooses Acropora / Massive / Mixed porosity (as a proportion) from a
# cover_df with columns `taxon` and a numeric cover column named by cover_col.
assemblage_porosity <- function(cover_df, cover_col) {
  # Empty input -> Mixed-assemblage porosity (proportion), no classification
  if (is.null(cover_df) || nrow(cover_df) == 0) {
    return(porosity$Porosity[porosity$Assemblage == "Mixed"] / 100)
  }
  # Collapse any duplicate taxa (repeated .xlsx rows, auto + manual adds) so each
  # taxon contributes a single scalar. Without this, a taxon matching multiple
  # rows makes the `&&` classification below receive a length>1 logical.
  cover_df <- stats::aggregate(
    stats::as.formula(paste(cover_col, "~ taxon")),
    data = cover_df, FUN = function(x) sum(x, na.rm = TRUE)
  )

  total_pct <- sum(cover_df[[cover_col]], na.rm = TRUE)
  massive_pct <- 0
  for (s in all_massive_species) {
    if (s %in% cover_df$taxon) {
      massive_pct <- massive_pct + cover_df[cover_df$taxon == s, cover_col]
    }
  }

  # Determine total branching percentage
  branch_pct <- 0
  for (sp in all_branching_species) {
    sp_pct <- if (sp %in% cover_df$taxon) cover_df[cover_df$taxon == sp, cover_col] else 0
    branch_pct <- branch_pct + sp_pct
  }

  if (total_pct > 0 && branch_pct > total_pct * 0.75) {
    por <- porosity$Porosity[porosity$Assemblage == "Acropora"]
  } else if (total_pct > 0 && massive_pct > total_pct * 0.75) {
    por <- porosity$Porosity[porosity$Assemblage == "Massive"]
  } else {
    por <- porosity$Porosity[porosity$Assemblage == "Mixed"]
  }
  por / 100 # proportion
}

baseline_bioerosion_RAP <- function(bg_df, site_area, uc_pct, be_micro_rate, macrobioerosion, bp,
                                    be_sd_lo = NA_real_, be_sd_hi = NA_real_) {
    # Apply bioerosion to baseline growth df:
    bg_df$consol_area_orig <- site_area - (site_area * uc_pct / 100) - (site_area * bg_df$pct_cvr_orig / 100)
    bg_df$microbioerosion  <- (bg_df$consol_area_orig / site_area) * be_micro_rate # Multiply the general microbioerosion rate by the proportion of available consolidated sediment
    bg_df$carb_budg_orig   <- bg_df$carb_budg_orig - bg_df$microbioerosion - macrobioerosion
    bg_df$RAP_orig         <- bg_df$carb_budg_orig / 2.9 / (1 - bp)

    # Bounded RAP. min = low calcification + HIGH erosion (macro + +1 SD);
    # max = high calcification + LOW erosion (macro + -1 SD). When bounds/SDs
    # are unavailable the min/max columns mirror the mean.
    if ("carb_budg_orig_min" %in% names(bg_df) && "carb_budg_orig_max" %in% names(bg_df)) {
      macro_hi <- macrobioerosion + (if (is.finite(be_sd_hi)) be_sd_hi else 0)
      macro_lo <- macrobioerosion + (if (is.finite(be_sd_lo)) be_sd_lo else 0)
      bg_df$carb_budg_orig_min <- bg_df$carb_budg_orig_min - bg_df$microbioerosion - macro_hi
      bg_df$carb_budg_orig_max <- bg_df$carb_budg_orig_max - bg_df$microbioerosion - macro_lo
      bg_df$RAP_orig_min <- bg_df$carb_budg_orig_min / 2.9 / (1 - bp)
      bg_df$RAP_orig_max <- bg_df$carb_budg_orig_max / 2.9 / (1 - bp)
    }

    bg_df
}

# ----------------------------------------------------------------------------
# growth simulation ----
# Applicable to original colonies and new outplants, and reused by both the
# restoration model and the baseline-growth reactive. Performs its own per-
# species lookups (growth rate, DHW slope) from `species` + `bleaching_severity`.
# Applies Year-1 outplant mortality (group == "outplant" only), per-event
# bleaching dieoff, post-bleaching growth reduction, and bioerosion.
# Returns a per-year data.frame (area, carb_accr, carb_budg, RAP, pct_cvr)
# plus the final colony count and final area.
#
# UNITS NOTE: `carb_accr` is a whole-patch flux (kg CaCO3/yr). RAP normalizes it to
# a per-m2 basis by dividing by site_area before the /2.9/(1-por) conversion.
# ----------------------------------------------------------------------------
simulate_growth <- function(group, subregion, species, colony_count, colony_diam, duration,
                            site_area, uc_pct, macrobioerosion,
                            bleaching_severity, bleaching_frequency,
                            opc = NA_real_, check_sanity = FALSE
                            ) {

  # ---- Cluster geometry ----
  # When Outplants-per-cluster (opc) is supplied, treat each CLUSTER as one
  # colony. Count = round(colony_count / opc); each cluster's starting diameter
  # packs opc plants in the longest linear run with a 0.5 cm inter-plant gap.
  if (!is.null(opc) && is.finite(opc) && opc > 0) {
    plants_in_cluster_diam <- ceiling(sqrt(opc))
    cluster_diam <- (plants_in_cluster_diam * (colony_diam * 100)) +
                    (0.5 * (plants_in_cluster_diam - 1))   # cm
    colony_diam  <- cluster_diam / 100                     # back to m
    colony_count <- round(colony_count / opc)
  }
  # Per-species lookups
  # Species-specific slope of the relationship between degree-heating weeks and percent mortality.
  # sp_dhw_slope <- dhw_slope_lookup_fk$slope_pct_per_dhw[dhw_slope_lookup_fk$taxon == genus]
  # if (length(sp_dhw_slope) == 0) sp_dhw_slope <- 0.85 # generic fallback
  sp_dhw_intrvl <- get_dhw_intrvl(species, subregion)

  # Unpack degree-heating-week percent-cover-loss interval
  sp_dhw_lower <- sp_dhw_intrvl[[1]]
  sp_dhw_slope <- sp_dhw_intrvl[[2]]
  sp_dhw_upper <- sp_dhw_intrvl[[3]]

  # Total DHW-driven percent loss converted to proportion. Cap at 1 == 100%.
  sp_dhw_loss_lo  <- min(sp_dhw_lower * bleaching_severity / 100, 1)
  sp_dhw_loss     <- min(sp_dhw_slope * bleaching_severity / 100, 1)
  sp_dhw_loss_hi  <- min(sp_dhw_upper * bleaching_severity / 100, 1)

  # 30% of the cover loss applied as whole-colony mortality
  # (generalized default, see about species-specific values later)
  sp_dhw_mortality  <- 0.30

  # Retrieve growth rate interval
  sp_growth_rate    <- subset(growth_rates, growth_rates["name"] == species)["planar_mean"][, 1] / 1000 # convert from mm to m
  sp_growth_rate_lo <- subset(growth_rates, growth_rates["name"] == species)["planar_lwr"][,  1] / 1000
  sp_growth_rate_hi <- subset(growth_rates, growth_rates["name"] == species)["planar_upr"][,  1] / 1000

  # Retrieve and unpack outplant mortality interval
  mortality_interval <- outplant_mortality_by_size(colony_diam, species) # c(low, mean, high) interval
  mort_lo <- mortality_interval[[1]]
  mort    <- mortality_interval[[2]]
  mort_hi <- mortality_interval[[3]]
  # Sanity checks for simulation inputs
  if (check_sanity) {
    if (bleaching_frequency == 5) log_msg(" ===== ANNUAL BLEACHING ===== ")
    else if (bleaching_frequency == 0) log_msg(" ======= NO BLEACHING ======= ")
    else log_msg(" ============================ ")
    log_msg(
        "\t", abbrev_species(species),
        # "\n\t", str_pad("Baseline cover:", side="right", width = 28), signif(current_sp_m, 3), "m2",
        "\n\t", str_pad("Initial count:",           width = 32, side = "right"), sprintf("  %d colonies", colony_count),
        "\n\t", str_pad("Initial colony diameter:", width = 32, side = "right"), sprintf("  %.1f cm", colony_diam * 100), # Readout in centimeters
        "\n\t", str_pad("Initial species area:",    width = 32, side = "right"), sprintf("  %.4f m²", colony_count * (colony_diam / 2) ^ 2 * pi)
    )
    if (bleaching_frequency > 0) {
      log_msg(
          "\t",   str_pad("Bleaching severity:",            width = 32, side = "right"), sprintf("  %d DHW", bleaching_severity),
          "\n\t", str_pad("Combined bleaching mortality:",  width = 32, side = "right"),
            str_pad(sprintf("  %.1f", sp_dhw_loss_lo * 100), width = 8, side = "both"), " | ", # Readout as percent
            str_pad(sprintf("  %.1f", sp_dhw_loss    * 100), width = 8, side = "both"), " | ",
            str_pad(sprintf("  %.1f", sp_dhw_loss_hi * 100), width = 8, side = "both"), " %",
          "\n\t\t", str_pad("Partial:",                     width = 24, side = "right"),
            str_pad(sprintf("  %.1f", (sp_dhw_loss_lo - (sp_dhw_loss_lo * sp_dhw_mortality)) * 100), width = 8, side = "both"), " | ",
            str_pad(sprintf("  %.1f", (sp_dhw_loss    - (sp_dhw_loss    * sp_dhw_mortality)) * 100), width = 8, side = "both"), " | ",
            str_pad(sprintf("  %.1f", (sp_dhw_loss_hi - (sp_dhw_loss_hi * sp_dhw_mortality)) * 100), width = 8, side = "both"), " %",
          "\n\t\t", str_pad("Whole-colony:",                width = 24, side = "right"),
            str_pad(sprintf("  %.1f", (sp_dhw_loss_lo * sp_dhw_mortality) * 100), width = 8, side = "both"), " | ",
            str_pad(sprintf("  %.1f", (sp_dhw_loss    * sp_dhw_mortality) * 100), width = 8, side = "both"), " | ",
            str_pad(sprintf("  %.1f", (sp_dhw_loss_hi * sp_dhw_mortality) * 100), width = 8, side = "both"), " %"
      )
    }
    log_msg("\t", str_pad("Planar growth rate interval:", width = 32, side = "right"),
              str_pad(sprintf("  %.2f", sp_growth_rate_lo * 1000), width = 8, side = "both"), " | ", # Readout in mm/yr
              str_pad(sprintf("  %.2f", sp_growth_rate    * 1000), width = 8, side = "both"), " | ",
              str_pad(sprintf("  %.2f", sp_growth_rate_hi * 1000), width = 8, side = "both"), " mm/yr"
    )
    if (group == "outplant") {
      log_msg(
        "\t",
        str_pad("Outplant mortality interval:", width = 32, side = "right"),
        str_pad(sprintf("%.1f", mort_lo * 100), width = 8, side = "both"), " | ",
        str_pad(sprintf("%.1f", mort    * 100), width = 8, side = "both"), " | ",
        str_pad(sprintf("%.1f", mort_hi * 100), width = 8, side = "both"), "%"
      )
    }
  }

  # Per-species calcification-rate bounds (kg CaCO3/m2/yr). NA-safe: when
  # unavailable, min/max budget columns mirror the average.
  cr_bounds   <- calc_rate_bounds(species)
  calc_rate_lo     <- cr_bounds[1]
  calc_rate_hi     <- cr_bounds[2]

  out_df     <- data.frame()
  out_df_min <- data.frame()
  out_df_max <- data.frame()
  new_size             <- colony_diam  # Working colony diameter for this run
  new_size_lo          <- colony_diam  # Initialize size bands
  new_size_hi          <- colony_diam
  colony_count_thisrun    <- colony_count # working colony count for this run
  colony_count_thisrun_lo <- colony_count
  colony_count_thisrun_hi <- colony_count
  last_bleach_year     <- 1            # placeholder

  for (i in 1:duration) { # R starts counting at 1 so "Year 0" = Year 1; "Year 10" = Year 11

    # Incorporate Mote outplant mortality observations during Year 0 = "Year 1".
    # Apply before growth calculation.
    # Colony numbers rounded at every step.
    if (group == "outplant" && i == 1) {
      outplant_dieoff    <- round(colony_count_thisrun * mort)
      outplant_dieoff_lo <- round(colony_count_thisrun * mort_lo)
      outplant_dieoff_hi <- round(colony_count_thisrun * mort_hi)
      colony_count_thisrun_lo <- colony_count_thisrun_lo - outplant_dieoff_hi # Apply high mortality to yield low-range colony count
      colony_count_thisrun_hi <- colony_count_thisrun_hi - outplant_dieoff_lo # Vice-versa
      colony_count_thisrun    <- colony_count_thisrun    - outplant_dieoff
      if (check_sanity) {
        log_msg(
          "\t",
          str_pad("Outplant dieoff interval:", width = 32, side = "right"),
          str_pad(sprintf("-%d", outplant_dieoff_lo), width = 8, side = "both"), " | ",
          str_pad(sprintf("-%d", outplant_dieoff),    width = 8, side = "both"), " | ",
          str_pad(sprintf("-%d", outplant_dieoff_hi), width = 8, side = "both"), "colonies"
        )
      }
    }

    if (check_sanity && i == 1) {
      # Initialize printout table headers here:
      log_msg("\n\t", "| ",
          str_pad("Population",  width = 23), " |",
          # str_pad("Site-wide", width = 24), " |",
          str_pad("Colony size", width = 27), " |",
          str_pad("Area",        width = 27),
          # Linebreak
          "\n\t", "| ",
          str_pad("interval",  width = 23), " |",
          # str_pad("budget contrib.",  width = 24), " |",
          str_pad("interval",  width = 27), " |",
          str_pad("interval",  width = 27),
          # Units
          "\n\t", "| ",
          str_pad("(colonies)", width = 23), " |",
          # str_pad("(kg/m²/yr)", width = 24), " |",
          str_pad("(cm)",       width = 27), " |",
          str_pad("(m²)",       width = 27),
          "\n",
          str_pad("-", pad = "-",  width = 90, side = "both")
      )
    }

    # Determine post-bleaching growth reduction due to the most recent bleaching event (if applicable)
    years_since_last_bleach <- i - last_bleach_year
    if (years_since_last_bleach <= 4 && years_since_last_bleach > 0) {
        # Apply post-bleaching production losses
        post_bleach_growth_reduction <- pbr[years_since_last_bleach]
    } else {
        post_bleach_growth_reduction <- 0
    }

    # Will bleaching occur this year?:
    bleaching <- FALSE
    bleach_dieoff <- 0
    if ((bleaching_frequency == 1 && i %% 4 == 0)    # Every 4th year
     || (bleaching_frequency == 2 && i %% 2 == 0)    # Even years
     || (bleaching_frequency == 5 && i %% 1 == 0)) { # Every year
        # If so, kill colonies before growth if bleaching occurs
        # Apply species-specific dieoff proportion to the colony count:
        bleaching <- TRUE
        # Apply lower bound (more intense impact) to get high bleaching dieoff interval
        bleach_dieoff_hi <- round(colony_count_thisrun * (sp_dhw_loss_lo * sp_dhw_mortality))
        bleach_dieoff_lo <- round(colony_count_thisrun * (sp_dhw_loss_hi * sp_dhw_mortality))
        bleach_dieoff    <- round(colony_count_thisrun * (sp_dhw_loss    * sp_dhw_mortality))

        # if (check_sanity && bleaching_frequency != 5) {
        #   bleach_dieoff <- round(colony_count_thisrun * (sp_dhw_loss * sp_dhw_mortality))
        #   if (bleach_dieoff == 0) {
        #     readout <- " BLEACHING: NO MORTALITY "
        #   } else {
        #     readout <- paste0(" BLEACHING DIEOFF: -", bleach_dieoff, " COLONIES ")
        #   }
        #   log_msg("\t", str_pad(readout, width = 70, side = "both", pad = "="))
        # }
        last_bleach_year <- i
        # Higher (less severe) DHW interval -> lower dieoff interval -> higher count interval.
        colony_count_thisrun_hi <- colony_count_thisrun - bleach_dieoff_lo
        colony_count_thisrun_lo <- colony_count_thisrun - bleach_dieoff_hi
        colony_count_thisrun    <- colony_count_thisrun - bleach_dieoff

    }

    # Kill colonies due to chronic whole-colony mortality (Brown et al. 2026)
    # Determine size bin
    this_bin <- round(floor(new_size / 5) * 5) # Round size down to nearest 5 cm bin
    this_bin <- ifelse(this_bin < 5, 5, this_bin) # Ensure minimum bin is 5 cm
    # Determine morphology
    this_morph <- if (species %in% all_branching_species) "branching" else "other"
    this_mortality_whole <- subset(mortality_whole, mortality_whole["size_bin_cm"] == this_bin)[this_morph][, 1]
    this_mortality_whole <- this_mortality_whole / 100 # Convert from percent to proportion
    # Reduce colony count
    colony_count_thisrun_lo <- round(colony_count_thisrun_lo * (1 - this_mortality_whole))
    colony_count_thisrun_hi <- round(colony_count_thisrun_hi * (1 - this_mortality_whole))
    colony_count_thisrun    <- round(colony_count_thisrun    * (1 - this_mortality_whole))

    # Shrink colonies due to chronic partial mortality (Brown et al. 2026)
    # Calculate starting area
    new_area    <- (new_size    / 2) ^ 2 * pi
    new_area_lo <- (new_size_lo / 2) ^ 2 * pi
    new_area_hi <- (new_size_hi / 2) ^ 2 * pi
    # Apply chronic partial mortality to surviving colonies' starting area.
    this_mortality_partial <- subset(mortality_partial, mortality_partial["size_bin_cm"] == this_bin)[this_morph][, 1]
    this_mortality_partial <- this_mortality_partial / 100 # Convert from percent to proportion
    new_area    <- new_area    * (1 - this_mortality_partial)
    new_area_lo <- new_area_lo * (1 - this_mortality_partial)
    new_area_hi <- new_area_hi * (1 - this_mortality_partial)

    # Shrink colonies due to DHW-driven partial mortality:
    # If bleaching occurs this year, apply the remaining bleaching stress
    # that did not cause whole-colony mortality as a reduction to the remaining colonies' area:
    if (bleaching) {
      new_area    <- new_area    * (1 - sp_dhw_loss    * (1 - sp_dhw_mortality))
      # Low DHW interval is more severe than high DHW, so calculate area_lo with loss_lo
      new_area_lo <- new_area_lo * (1 - sp_dhw_loss_lo * (1 - sp_dhw_mortality))
      new_area_hi <- new_area_hi * (1 - sp_dhw_loss_hi * (1 - sp_dhw_mortality))
    }

    # Recalculate post-mortality colony diameter from reduced new_area:
    new_size    <- 2 * sqrt(new_area    / pi)
    new_size_lo <- 2 * sqrt(new_area_lo / pi)
    new_size_hi <- 2 * sqrt(new_area_hi / pi)

    # Grow the surviving colonies
    # Apply post-bleaching growth reduction to planar growth rate.
    new_size    <- new_size    + sp_growth_rate    * (1 - post_bleach_growth_reduction)
    new_size_lo <- new_size_lo + sp_growth_rate_lo * (1 - post_bleach_growth_reduction)
    new_size_hi <- new_size_hi + sp_growth_rate_hi * (1 - post_bleach_growth_reduction)

    # Recalculate the post-growth per-colony area:
    new_area    <- (new_size    / 2) ^ 2 * pi
    new_area_lo <- (new_size_lo / 2) ^ 2 * pi
    new_area_hi <- (new_size_hi / 2) ^ 2 * pi
    # Multiply by colony count to calculate per-species area:
    sp_area    <- new_area    * colony_count_thisrun
    sp_area_lo <- new_area_lo * colony_count_thisrun_lo
    sp_area_hi <- new_area_hi * colony_count_thisrun_hi


    # Calculate the carbonate budget contribution from this species for this year
    sp_rate <- calc_rates$rate[calc_rates$Taxon == species]
    carb_accr <- sp_rate * sp_area
    carb_budg  <- carb_accr / site_area

    # Bounded budgets (same geometry, bounded calcification rate). Fall back to
    # the average rate when a bound is missing so min/max mirror the mean.
    cr_lo <- if (is.finite(calc_rate_lo)) calc_rate_lo else if (length(sp_rate)) sp_rate else NA_real_
    cr_hi <- if (is.finite(calc_rate_hi)) calc_rate_hi else if (length(sp_rate)) sp_rate else NA_real_
    budget_lo <- (cr_lo * sp_area_lo) / site_area
    budget_hi <- (cr_hi * sp_area_hi) / site_area

    # Sanity check: growth table printout
    if (check_sanity) {
      log_msg(
        str_pad(paste0("Y", i - 1),                     width = 7), " |",
        str_pad(sprintf("%d", colony_count_thisrun_lo), width = 6), " :",
        str_pad(sprintf("%d", colony_count_thisrun),    width = 6), " :",
        str_pad(sprintf("%d", colony_count_thisrun_hi), width = 6), "   |",
        "   ", # Spacer
        # str_pad(sprintf("%.3f", carb_budg),             width = 24), " |",
        str_pad(sprintf("%.2f", new_size_lo * 100),     width = 6), " :",
        str_pad(sprintf("%.2f", new_size * 100),        width = 6), " :",
        str_pad(sprintf("%.2f", new_size_hi * 100),     width = 6), "   |",
        "   ",
        str_pad(sprintf("%.2f", sp_area_lo),         width = 7), " : ",
        str_pad(sprintf("%.2f", sp_area),            width = 7), " : ",
        str_pad(sprintf("%.2f", sp_area_hi),         width = 7)
      )
    }

    # Populate the output dataframe with total values as of this year:
    # Exclude CCA from coral cover
    if (str_detect(species, "algae")) {
      pct_cvr    <- 0
      pct_cvcr_lo <- 0
      pct_cvcr_hi <- 0
    } else {
      pct_cvr     <- sp_area    / site_area * 100
      pct_cvcr_lo <- sp_area_lo / site_area * 100
      pct_cvcr_hi <- sp_area_hi / site_area * 100
    }

    out_df[i, "area"]      <- sp_area   # Calcifier area
    out_df[i, "carb_accr"] <- carb_accr # Site-wide carbonate accretion contribution (kg CaCO3 / yr)
    out_df[i, "carb_budg"] <- carb_budg # Calcifier carbonate budget (kg CaCO3 / m2 / yr)
    out_df[i, "pct_cvr"]   <- pct_cvr   # Hard coral percent cover

    out_df_min[i, "area"]      <- sp_area_lo
    out_df_min[i, "carb_accr"] <- cr_lo * sp_area_lo
    out_df_min[i, "carb_budg"] <- budget_lo
    out_df_min[i, "pct_cvr"]   <- pct_cvcr_lo

    out_df_max[i, "area"]      <- sp_area_hi
    out_df_max[i, "carb_accr"] <- cr_hi * sp_area_hi
    out_df_max[i, "carb_budg"] <- budget_hi
    out_df_max[i, "pct_cvr"]   <- pct_cvcr_hi
  }

  if (check_sanity) log_msg("\n")

  list(df = out_df, final_count = colony_count_thisrun, final_area = new_area,
       df_min = out_df_min, df_max = out_df_max)
}

# Resolve habitat-specific macrobioerosion ----
# (kg CaCO3/m2/yr)
resolve_regional_bioerosion <- function(subregion, habitat) {
  be_sub <- bioerosion[bioerosion$SUB_REGION == subregion, ]
  be_hab <- be_sub[be_sub$HABITAT_TYPE == habitat, ]
  be_pfish  <- if (nrow(be_hab)) be_hab$AVG_PARROTFISH[1] else 0
  be_urchin <- if (nrow(be_hab)) be_hab$AVG_URCHIN[1] else 0
  be_sponge  <- if (nrow(be_hab)) be_hab$AVG_MACROBIOEROSION[1] else 0
  sum(be_pfish, be_urchin, be_sponge, na.rm = TRUE)
}

# Resolve habitat-specific bioerosion, split by taxon type ----
# Returns a named list (parrotfish / urchin / sponge) so the Monitoring pipeline
# can fall back per-taxon-type when a given observed sheet is empty. Sponge maps
# to the regional macrobioerosion term.
resolve_species_bioerosion <- function(subregion, habitat) {
  be_sub <- bioerosion[bioerosion$SUB_REGION == subregion, ]
  be_hab <- be_sub[be_sub$HABITAT_TYPE == habitat, ]
  list(
    parrotfish = if (nrow(be_hab)) .safe0(be_hab$AVG_PARROTFISH[1]) else 0,
    urchin     = if (nrow(be_hab)) .safe0(be_hab$AVG_URCHIN[1]) else 0,
    sponge     = if (nrow(be_hab)) .safe0(be_hab$AVG_MACROBIOEROSION[1]) else 0
  )
}

# Small NA/NULL -> 0 helper (module scope; server has its own .safe_num)
.safe0 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) 0 else as.numeric(x)

# Generalized Caribbean microbioerosion rate: 0.24 kg CaCO3/m2/yr
be_micro_rate <- 0.24

# Uncertainty-column availability ----
# Calcification bounds present + non-empty?
calc_uncert_available <- all(c("lower_bound", "upper_bound") %in% names(calc_rates)) &&
  any(is.finite(suppressWarnings(as.numeric(calc_rates[["lower_bound"]])))) &&
  any(is.finite(suppressWarnings(as.numeric(calc_rates[["upper_bound"]]))))

# Bioerosion STDEV columns present + non-empty?
bioerosion_uncert_available <- all(
  c("STDEV_PARROTFISH", "STDEV_URCHIN", "STDEV_MACROBIOEROSION") %in% names(bioerosion)
) && any(is.finite(suppressWarnings(as.numeric(bioerosion$STDEV_MACROBIOEROSION))))

# Per-species calcification-rate bounds lookup (kg CaCO3/m2/yr).
# Returns c(lo, hi); NA when unavailable for that taxon.
calc_rate_bounds <- function(species) {
  if (!calc_uncert_available) {
    return(c(NA_real_, NA_real_))
  }
  row <- calc_rates[calc_rates$Taxon == species, , drop = FALSE]
  if (nrow(row) == 0) return(c(NA_real_, NA_real_))
  lo <- suppressWarnings(as.numeric(row[["lower_bound"]][1]))
  hi <- suppressWarnings(as.numeric(row[["upper_bound"]][1]))
  c(lo, hi)
}

# +/-1 STDEV bioerosion band (kg CaCO3/m2/yr) for a subregion/habitat.
# Returns c(minus1sd_total, plus1sd_total) added to the AVG macrobioerosion.
# NA when unavailable.
bioerosion_stdev <- function(subregion, habitat) {
  if (!bioerosion_uncert_available) {
    return(c(NA_real_, NA_real_))
  }
  be_sub <- bioerosion[bioerosion$SUB_REGION == subregion, ]
  be_hab <- be_sub[be_sub$HABITAT_TYPE == habitat, ]
  if (nrow(be_hab) == 0) return(c(NA_real_, NA_real_))
  sd_p <- .safe0(be_hab$STDEV_PARROTFISH[1])
  sd_u <- .safe0(be_hab$STDEV_URCHIN[1])
  sd_m <- .safe0(be_hab$STDEV_MACROBIOEROSION[1])
  sd_tot <- sd_p + sd_u + sd_m
  c(-sd_tot, sd_tot)
}

# Baseline-only original growth ----
# Thin wrapper over simulate_growth: loops the selected species (originals
# only) and sums the per-year RAP + % cover + budget. Porosity from the
# baseline assemblage.
run_baseline_growth <- function(subregion, site_area, uc_pct, sim_duration,
                                bleaching_severity, bleaching_frequency,
                                baseline_cover_df, progress_cb = NULL) {

  n <- sim_duration + 1
  total_rap  <- rep(0, n)
  total_cvr  <- rep(0, n)
  total_cvr_min  <- rep(0, n)
  total_cvr_max  <- rep(0, n)
  total_budg <- rep(0, n)
  total_budg_min <- rep(0, n)
  total_budg_max <- rep(0, n)
  end_cover_by_species <- numeric(0)  # per-species end-of-duration cover (%)

  log_msg(str_pad(" Baseline assemblage growth simulation ", side = "both", width = 90, pad = "="), "\n\n")
  # print(baseline_cover_df)

  for (row_i in seq_len(nrow(baseline_cover_df))) {
    species        <- baseline_cover_df$taxon[row_i]
    current_sp_pct <- baseline_cover_df$current_cvr_pct[row_i]
    if (is.na(current_sp_pct) || current_sp_pct <= 0) next

    current_sp_m <- site_area * (current_sp_pct / 100)

    # Average colony diameter for this species, converted from cm to m
    sp_diam <- subset(diams, diams["name"] == species)["length_mean"][, 1] / 100
    # Skip species without an initial diameter
    if (length(sp_diam) == 0 || is.na(sp_diam)) next
    # Round to nearest colony. Does this introduce too much rounding error?
    orig_colonies <- round(current_sp_m / ((sp_diam / 2) ^ 2 * pi))

    log_msg(" ============================ ")
    log_msg(" Simulating baseline growth: ", species, "...")
    if (is.function(progress_cb)) {
      progress_cb(paste0("Simulating baseline ", abbrev_species(species), "..."))
    }

    sim <- simulate_growth(group = "original", subregion = subregion, species = species,
                           colony_count = orig_colonies,
                           colony_diam = sp_diam, duration = n,
                           site_area = site_area, uc_pct = uc_pct,
                           bleaching_severity = bleaching_severity,
                           bleaching_frequency = bleaching_frequency,
                           check_sanity = TRUE)

    #total_rap  <- total_rap  + sim$df$RAP
    total_cvr      <- total_cvr      + sim$df$pct_cvr
    total_cvr_min  <- total_cvr_min  + sim$df_min$pct_cvr
    total_cvr_max  <- total_cvr_max  + sim$df_max$pct_cvr
    total_budg     <- total_budg     + sim$df$carb_budg
    total_budg_min <- total_budg_min + sim$df_min$carb_budg
    total_budg_max <- total_budg_max + sim$df_max$carb_budg
    # Per-species end-of-duration cover (last simulated year). CCA's pct_cvr is
    # forced to 0 inside simulate_growth (excluded from coral cover), so recover
    # its cover from grown AREA instead; corals use pct_cvr as before.
    prev <- if (species %in% names(end_cover_by_species)) end_cover_by_species[[species]] else 0
    end_val <- if (str_detect(species, "algae")) {
      (sim$df$area[nrow(sim$df)] / site_area) * 100
    } else {
      sim$df$pct_cvr[nrow(sim$df)]
    }
    end_cover_by_species[species] <- prev + end_val
  }

  log_msg(str_pad(" Baseline growth simulation complete ", side = "both", width = 90, pad = "="), "\n")

  df <- data.frame(Year = 0:sim_duration, #RAP_orig = total_rap,
             pct_cvr_orig = total_cvr,
             pct_cvr_orig_min = total_cvr_min,
             pct_cvr_orig_max = total_cvr_max,
             carb_budg_orig = total_budg,
             carb_budg_orig_min = total_budg_min,
             carb_budg_orig_max = total_budg_max)
  attr(df, "end_cover_by_species") <- end_cover_by_species
  df
}

# ----------------------------------------------------------------------------
# Restoration model ----
# (adapted from acer_model_mockup.R)
# Originals for EVERY baseline species with cover > 0 grow over the full
# sim_duration and sum into the RAP_orig / total baseline (they persist and
# grow whether or not they are targeted for restoration).
# For species with target > current, a two-phase outplant solve adds new growth:
#   (1) Solve the outplant count to hit target % cover by the RESTORATION
#       HORIZON (rest_horizon).
#   (2) Run that solved count for the FULL sim_duration and add its contribution.
# Returns the summed budget_df across species, a per-species outplant vector,
# and total cost.
# ----------------------------------------------------------------------------
run_restoration_model <- function(habitat, subregion, site_area, uc_pct,
                                  sim_duration, rest_horizon,
                                  bleaching_severity, bleaching_frequency,
                                  target_cover_df, extra_years_df = NULL,
                                  olb_pct = 0, progress_cb = NULL) {

  # ---- Reserved space: Other living benthos (OLB) ----
  # OLB is permanently reserved and never grown. The competable area that all
  # growing populations (corals + CCA) may occupy is reduced by OLB up front.
  olb_pct <- if (is.null(olb_pct) || is.na(olb_pct)) NA_real_ else as.numeric(olb_pct)
  if (!is.finite(olb_pct) || olb_pct < 0) {
    log_msg("---- Error: 'REQUIRED Other living benthos' must be a number >= 0. ----")
    showNotification("Error: 'REQUIRED Other living benthos' cover (>= 0) is required.",
                     type = "error")
    return(NULL)
  }
  olb_pct        <- min(olb_pct, 100)
  competable_area <- site_area * (1 - olb_pct / 100)   # coral + CCA ceiling
  if (competable_area <= 0) {
    log_msg("---- Error: Other living benthos reserves all available space. ----")
    showNotification("Error: Other living benthos leaves no space for growth.",
                     type = "error")
    return(NULL)
  }

  # Guard: refuse if total target cover exceeds the competable (post-OLB) space.
  total_target_pct <- sum(target_cover_df$target_cvr_pct, na.rm = TRUE)
  cap_pct <- 100 - olb_pct
  if (is.finite(total_target_pct) && total_target_pct > cap_pct) {
    log_msg(sprintf("---- Error: Target cover cannot exceed %.1f%% (100%% - OLB). ----", cap_pct))
    showNotification(sprintf("Error: Target cover cannot exceed %.1f%% (100%% minus Other living benthos).", cap_pct),
                     type = "error")
    return(NULL)
    # return(list(budget_df = data.frame(),
    #             outplants_by_species = c(),
    #             outplants = 0, cost = 0,
    #             error = paste0("Total target cover (", round(total_target_pct, 1),
    #                            "%) exceeds 100%.")))
  }

  log_msg(str_pad(" Restoration scenario simulation ", side = "both", width = 90, pad = "="), "\n\n")
  # print(subset(target_cover_df, target_cvr_pct - target_cover_df$current_cvr_pct > 0))

  # Subregion/habitat-specific non-microbioerosion + generalized microbioerosion
  macrobioerosion <- resolve_regional_bioerosion(subregion, habitat)
  be_micro_rate   <- 0.24 # kg CaCO3/m2 consolidated substrate/yr

  # Assign porosity by target assemblage
  por <- assemblage_porosity(target_cover_df, "target_cvr_pct")

  n <- sim_duration + 1

  # ---- Accumulators ----
  # Originals (all baseline species) and new outplant growth (restored only).
  area_orig       <- rep(0, n)
  calc_accr_orig  <- rep(0, n)
  carb_budg_orig  <- rep(0, n)
  carb_budg_orig_min <- rep(0, n)
  carb_budg_orig_max <- rep(0, n)
  # RAP_orig        <- rep(0, n)
  pct_cvr_orig    <- rep(0, n)
  area_new        <- rep(0, n)
  calc_accr_new   <- rep(0, n)
  carb_budg_new   <- rep(0, n)
  carb_budg_new_min  <- rep(0, n)
  carb_budg_new_max  <- rep(0, n)
  # RAP_new         <- rep(0, n)
  pct_cvr_new     <- rep(0, n)
  pct_cvr_min     <- rep(0, n)
  pct_cvr_max     <- rep(0, n)

  # ---- Per-interval coral AREA accumulators (m2), all populations summed ----
  # These drive the per-year, per-interval overgrowth cap. Kept
  # separate from the budget accumulators so the cap can rescale area and the
  # budget is recomputed from capped area afterward.
  coral_area_orig     <- rep(0, n)   # originals, mean
  coral_area_new      <- rep(0, n)   # outplants (all pops), mean
  coral_area_new_lo   <- rep(0, n)
  coral_area_new_hi   <- rep(0, n)
  coral_area_orig_lo  <- rep(0, n)
  coral_area_orig_hi  <- rep(0, n)

  # Per-species summed population AREA (m2) across Y0 + additional-year efforts.
  # Converted to capped end-of-duration cover after the overgrowth cap.
  species_area_by_sp <- list()   # named: species -> numeric vector length n

  # CCA grown-area series (m2, soft ceiling). NULL until a CCA row is grown.
  cca_grown_area <- NULL

  outplants_by_species <- c()   # named: species -> outplant count
  achieved_cover_by_species <- c()  # named: species -> total % cover at horizon
  total_cost <- 0
  any_growth <- FALSE           # did any species produce output?

  # ---- Phase 0: grow ORIGINALS for every baseline species with cover > 0 ----
  # These persist and grow alongside restored species regardless of targeting.
  for (row_i in seq_len(nrow(target_cover_df))) {
    species        <- target_cover_df$taxon[row_i]
    current_sp_pct <- target_cover_df$current_cvr_pct[row_i]
    if (is.na(current_sp_pct) || current_sp_pct <= 0) next

    sp_diam <- subset(diams, diams["name"] == species)["length_mean"][, 1] / 100
    if (length(sp_diam) == 0 || is.na(sp_diam)) next
    sp_growth_rate    <- subset(growth_rates, growth_rates["name"] == species)["planar_mean"][, 1] / 1000
    if (length(sp_growth_rate) == 0 || is.na(sp_growth_rate)) next

    current_sp_m  <- site_area * (current_sp_pct / 100)
    orig_colonies <- round(current_sp_m / ((sp_diam / 2) ^ 2 * pi))

    orig_list <- simulate_growth(group = "original", subregion = subregion, species = species,
                                 colony_count = orig_colonies, colony_diam = sp_diam,
                                 duration = n,
                                 site_area = site_area, uc_pct = uc_pct,
                                 bleaching_severity = bleaching_severity,
                                 bleaching_frequency = bleaching_frequency)
    od <- orig_list[[1]] # original df
    area_orig       <- area_orig       + od$area
    calc_accr_orig  <- calc_accr_orig  + od$carb_accr
    carb_budg_orig  <- carb_budg_orig  + od$carb_budg
    carb_budg_orig_min <- carb_budg_orig_min + orig_list$df_min$carb_budg
    carb_budg_orig_max <- carb_budg_orig_max + orig_list$df_max$carb_budg
    # RAP_orig        <- RAP_orig        + od$RAP
    pct_cvr_orig    <- pct_cvr_orig    + od$pct_cvr

    # Per-interval area (CCA excluded from the coral cap; its grown area is
    # captured separately as a soft maximum that coral may overgrow).
    if (str_detect(species, "algae")) {
      # Accumulate the CCA population's GROWN area per year (soft ceiling).
      cca_grown_area <- (if (is.null(cca_grown_area)) rep(0, n) else cca_grown_area) + od$area
      # CCA still contributes to species_area_by_sp so its Final cover reports.
      species_area_by_sp[[species]] <-
        (if (is.null(species_area_by_sp[[species]])) 0 else species_area_by_sp[[species]]) + od$area
    } else {
      coral_area_orig    <- coral_area_orig    + od$area
      coral_area_orig_lo <- coral_area_orig_lo + orig_list$df_min$area
      coral_area_orig_hi <- coral_area_orig_hi + orig_list$df_max$area
      species_area_by_sp[[species]] <-
        (if (is.null(species_area_by_sp[[species]])) 0 else species_area_by_sp[[species]]) + od$area
    }
    any_growth <- TRUE
  }

  # ---- Phase 1 + 2: outplant solve + full-duration growth (restored only) ----
  for (row_i in seq_len(nrow(target_cover_df))) {
    species        <- target_cover_df$taxon[row_i]
    target_sp_pct  <- target_cover_df$target_cvr_pct[row_i]
    current_sp_pct <- target_cover_df$current_cvr_pct[row_i]

    # Two solve modes:
    #   count-driven  -> outplant_count given (>0); simulate it, report cover.
    #   target-driven -> target > current;       solve the count to hit target.
    row_count <- target_cover_df$outplant_count[row_i]
    count_driven <- !is.na(row_count) && row_count > 0

    sp_to_grow_pct <- target_sp_pct - current_sp_pct
    if (!count_driven && (is.na(sp_to_grow_pct) || sp_to_grow_pct <= 0)) next
    log_msg(" ============================ ")
    if (count_driven) {
      log_msg(" Simulating ", species, " with ", row_count, " outplants...")
    } else {
      log_msg(" Simulating ", species, " growth to ", target_sp_pct, "% cover...")
    }
    log_msg(" ============================ ")

    # Per-species outplant geometry/cost (blank -> don't outplant this species).
    row_diam_cm  <- target_cover_df$outplant_diam_cm[row_i]
    row_cost     <- target_cover_df$outplant_cost[row_i]
    if (is.na(row_diam_cm) || row_diam_cm <= 0 || is.na(row_cost)) {
      log_msg(" Skipping ", species, ": no outplant diameter/cost set.")
      next
    }
    outplant_diam <- row_diam_cm / 100  # cm -> m (local to this species)

    # Baseline colony-count seeding needs the species diameter (also used by the sim)
    sp_diam <- subset(diams, diams["name"] == species)["length_mean"][, 1] / 100
    # Use placeholder average diameter instead of skipping:
    if (length(sp_diam) == 0 || is.na(sp_diam)) {# next
      sp_diam <- 10
    }
    # # Additional sanity checks to track down missing data
    # log_msg(" Average adult sp_diam", sp_diam)

    # # Skip species with no growth-rate record (sim would produce NA)
    sp_growth_rate <- subset(growth_rates, growth_rates["name"] == species)["planar_mean"][, 1] / 1000
    # log_msg(" sp_growth_rate", sp_growth_rate)
    if (length(sp_growth_rate) == 0 || is.na(sp_growth_rate)) next

    current_sp_m <- site_area * (current_sp_pct / 100)
    # log_msg(" current_sp_m", current_sp_m)
    target_sp_m  <- site_area * (target_sp_pct / 100)
    # log_msg(" target_sp_m", target_sp_m)

    # Remaining target size to grow, after original growth to the HORIZON.
    # Grow this species' originals once to read its area at the horizon year.
    orig_colonies <- round(current_sp_m / ((sp_diam / 2) ^ 2 * pi))
    orig_only <- simulate_growth(group = "original", subregion = subregion, species = species,
                                colony_count = orig_colonies, colony_diam = sp_diam,
                                duration = n,
                                site_area = site_area, uc_pct = uc_pct,
                                bleaching_severity = bleaching_severity,
                                bleaching_frequency = bleaching_frequency)
    orig_area_at_horizon <- orig_only[[1]][["area"]][min(rest_horizon + 1, n)]
    sp_to_grow_m <- target_sp_m - orig_area_at_horizon

    # ---- PHASE 1: determine outplant count ----
    if (count_driven) {
      # Count given directly; no solve.
      outplant_guess <- round(row_count)
      guard <- 0
    } else if (rest_horizon == 0) {
      # Immediate coverage: enough outplants of the given size to hit the target
      # % cover at Year 0.
      outplant_area <- (outplant_diam / 2) ^ 2 * pi
      outplant_guess <- if (outplant_area > 0) {
        max(0, ceiling(sp_to_grow_m / outplant_area))
      } else {
        0
      }
    } else {
      # Iterative solve: find the count that reaches target by the HORIZON year.
      outplant_guess <- 50

      trial <- simulate_growth(group = "outplant", subregion = subregion, species = species,
                               colony_count = outplant_guess, colony_diam = outplant_diam,
                               duration = rest_horizon + 1,
                               site_area = site_area, uc_pct = uc_pct,
                               bleaching_severity = bleaching_severity,
                               bleaching_frequency = bleaching_frequency)
      per_colony_area <- trial[[3]]

      if (is.finite(per_colony_area) && per_colony_area > 0) {
        outplant_guess <- max(0, ceiling(sp_to_grow_m / per_colony_area))
      }

      log_msg("Initial outplant guess: ", outplant_guess)

      reiterate <- TRUE
      guard <- 0
      step_schedule <- c(1000, 100, 10, 1)
      step_i <- if (outplant_guess > 2000) 1 else if (outplant_guess > 200) 2 else if (outplant_guess > 40) 3 else 4
      step <- step_schedule[step_i]
      prev_sign <- 0

      while (reiterate) {
        guard <- guard + 1
        if (guard > 5000) break

        starting_outplant_area <- outplant_guess * (outplant_diam / 2) ^ 2 * pi
        needed_outplant_growth <- sp_to_grow_m - starting_outplant_area

        search_list <- simulate_growth(group = "outplant", subregion = subregion, species = species,
                                       colony_count = outplant_guess, colony_diam = outplant_diam,
                                       duration = rest_horizon + 1,
                                       site_area = site_area, uc_pct = uc_pct,
                                       bleaching_severity = bleaching_severity,
                                       bleaching_frequency = bleaching_frequency)

        new_area <- search_list[[3]] * outplant_guess
        diff <- needed_outplant_growth - new_area
        cur_sign <- if (diff < -0.01) -1 else if (diff > 0.01) 1 else 0

        if (cur_sign == 0) {
          reiterate <- FALSE
        } else {
          if (prev_sign != 0 && cur_sign != prev_sign && step > 1) {
            step_i <- step_i + 1
            step <- step_schedule[step_i]
          }
          if (cur_sign > 0) {
            outplant_guess <- outplant_guess + step
          } else {
            outplant_guess <- outplant_guess - step
            if (outplant_guess < 0) {
              outplant_guess <- 0
              reiterate <- FALSE
            }
          }
          prev_sign <- cur_sign
        }
      }

      horizon_abs_err <- function(n) {
        if (n < 0) return(Inf)
        starting_area <- n * (outplant_diam / 2) ^ 2 * pi
        needed        <- sp_to_grow_m - starting_area
        sl <- simulate_growth(group = "outplant", subregion = subregion, species = species,
                              colony_count = n, colony_diam = outplant_diam,
                              duration = rest_horizon + 1,
                              site_area = site_area, uc_pct = uc_pct,
                              bleaching_severity = bleaching_severity,
                              bleaching_frequency = bleaching_frequency)
        achieved <- sl[[3]] * n
        abs(needed - achieved)
      }
      if (prev_sign != 0) {
        neighbor <- if (prev_sign > 0) outplant_guess + 1 else outplant_guess - 1
        if (neighbor >= 0 && horizon_abs_err(neighbor) < horizon_abs_err(outplant_guess)) {
          outplant_guess <- neighbor
        }
      }
    }

    if (!count_driven && rest_horizon > 0) log_msg("Outplant count solved in ", guard, " iterations.")
    if (is.function(progress_cb)) {
      iters <- if (rest_horizon > 0) guard else 0L
      progress_cb(paste0("Simulating restored ", abbrev_species(species),
                         " (", iters, " iterations)..."))
    }

    log_msg("Simulating outplanting solution...")

    # ---- PHASE 2: run the solved outplant count for the FULL simulation duration ----
    row_opc <- target_cover_df$opc[row_i]
    new_list <- simulate_growth(group = "outplant", subregion = subregion, species = species,
                                colony_count = outplant_guess, colony_diam = outplant_diam,
                                duration = n,
                                site_area = site_area, uc_pct = uc_pct,
                                bleaching_severity = bleaching_severity,
                                bleaching_frequency = bleaching_frequency,
                                opc = row_opc,
                                check_sanity = TRUE)
    nd     <- new_list[[1]]
    nd_max <- new_list$df_max
    nd_min <- new_list$df_min

    area_new       <- area_new       + nd$area
    calc_accr_new  <- calc_accr_new  + nd$carb_accr
    carb_budg_new  <- carb_budg_new  + nd$carb_budg
    carb_budg_new_min <- carb_budg_new_min + nd_min$carb_budg
    carb_budg_new_max <- carb_budg_new_max + nd_max$carb_budg
    # RAP_new        <- RAP_new        + nd$RAP
    pct_cvr_new    <- pct_cvr_new    + nd$pct_cvr
    pct_cvr_min    <- pct_cvr_min    + nd_min$pct_cvr
    pct_cvr_max    <- pct_cvr_max    + nd_max$pct_cvr

    if (!str_detect(species, "algae")) {
      coral_area_new    <- coral_area_new    + nd$area
      coral_area_new_lo <- coral_area_new_lo + nd_min$area
      coral_area_new_hi <- coral_area_new_hi + nd_max$area
      species_area_by_sp[[species]] <-
        (if (is.null(species_area_by_sp[[species]])) 0 else species_area_by_sp[[species]]) + nd$area
    }

    outplants_by_species[species] <- outplant_guess
    # Achieved NEW cover for this species at the restoration horizon (the value
    # the count produced). Baseline/original cover for the species is added so
    # the reported Target reflects total species cover, matching the input's
    # meaning (target = total % cover for that species).
    dr_idx <- nrow(nd) # min(rest_horizon + 1, nrow(nd)) # duration row index
    orig_dr <- if (!is.null(orig_only) && nrow(orig_only[[1]]) >= dr_idx) {
      orig_only[[1]][["pct_cvr"]][dr_idx]
    } else {
      0
    }
    achieved_cover_by_species[species] <- .safe0(nd$pct_cvr[dr_idx]) + .safe0(orig_dr)
    total_cost <- total_cost + outplant_guess * row_cost
    any_growth <- TRUE
  }

  # ---- Phase 2.5: additional-year outplant efforts ----
  # Each (species, plant_year) is an independent population. It contributes 0
  # area/budget before its plant_year, then grows for the remaining duration.
  # Populations are offset in time by prepending plant_year zero-rows.
  if (!is.null(extra_years_df) && nrow(extra_years_df) > 0) {
    for (er in seq_len(nrow(extra_years_df))) {
      sp_e   <- extra_years_df$taxon[er]
      py     <- extra_years_df$plant_year[er]
      cnt_e  <- extra_years_df$outplant_count[er]
      diam_e <- extra_years_df$outplant_diam_cm[er]
      cost_e <- extra_years_df$outplant_cost[er]
      opc_e  <- extra_years_df$opc[er]
      if (is.na(cnt_e) || cnt_e <= 0 || is.na(diam_e) || diam_e <= 0 ||
          is.na(cost_e) || py < 1 || py > sim_duration) next
      if (str_detect(sp_e, "algae")) next

      log_msg(" ============================ ")
      log_msg(" Additional effort: ", sp_e, " @ Year ", py,
              " (", cnt_e, " outplants)...")

      run_years <- (n - py)  # grow for remaining sim after plant_year
      if (run_years < 1) next
      el <- simulate_growth(group = "outplant", subregion = subregion, species = sp_e,
                            colony_count = round(cnt_e), colony_diam = diam_e / 100,
                            duration = run_years,
                            site_area = site_area, uc_pct = uc_pct,
                            bleaching_severity = bleaching_severity,
                            bleaching_frequency = bleaching_frequency,
                            opc = opc_e, check_sanity = TRUE)

      # Offset: py leading zero rows, then the population's run_years rows.
      pad <- function(v) c(rep(0, py), v)[seq_len(n)]

      area_new          <- area_new          + pad(el$df$area)
      calc_accr_new     <- calc_accr_new     + pad(el$df$carb_accr)
      carb_budg_new     <- carb_budg_new     + pad(el$df$carb_budg)
      carb_budg_new_min <- carb_budg_new_min + pad(el$df_min$carb_budg)
      carb_budg_new_max <- carb_budg_new_max + pad(el$df_max$carb_budg)
      pct_cvr_new       <- pct_cvr_new       + pad(el$df$pct_cvr)
      pct_cvr_min       <- pct_cvr_min       + pad(el$df_min$pct_cvr)
      pct_cvr_max       <- pct_cvr_max       + pad(el$df_max$pct_cvr)
      coral_area_new    <- coral_area_new    + pad(el$df$area)
      coral_area_new_lo <- coral_area_new_lo + pad(el$df_min$area)
      coral_area_new_hi <- coral_area_new_hi + pad(el$df_max$area)
      species_area_by_sp[[sp_e]] <-
        (if (is.null(species_area_by_sp[[sp_e]])) 0 else species_area_by_sp[[sp_e]]) + pad(el$df$area)

      total_cost <- total_cost + round(cnt_e) * cost_e
      any_growth <- TRUE
    }
  }

  if (!any_growth) {
    return(list(budget_df = data.frame(),
                outplants_by_species = outplants_by_species,
                achieved_cover_by_species = achieved_cover_by_species,
                end_cover_by_species = numeric(0),
                outplants = 0, cost = 0))
  }

  budget_df <- data.frame(
    area_orig = area_orig, calc_accr_orig = calc_accr_orig,
    carb_budg_orig = carb_budg_orig,
    pct_cvr_orig = pct_cvr_orig,
    area_new = area_new, calc_accr_new = calc_accr_new,
    carb_budg_new = carb_budg_new,
    pct_cvr_new = pct_cvr_new,
    pct_cvr_min = pct_cvr_min,
    pct_cvr_max = pct_cvr_max
  )

  budget_df$area_total       <- budget_df$area_orig       + budget_df$area_new
  budget_df$calc_accr_total  <- budget_df$calc_accr_orig  + budget_df$calc_accr_new
  budget_df$carb_budg_total  <- budget_df$carb_budg_orig  + budget_df$carb_budg_new
  budget_df$pct_cvr_total    <- budget_df$pct_cvr_orig    + budget_df$pct_cvr_new

  budget_df$pct_cvr_total_min    <- budget_df$pct_cvr_orig    + budget_df$pct_cvr_min
  budget_df$pct_cvr_total_max    <- budget_df$pct_cvr_orig    + budget_df$pct_cvr_max


  budget_df$carb_budg_total_min <- carb_budg_orig_min + carb_budg_new_min
  budget_df$carb_budg_total_max <- carb_budg_orig_max + carb_budg_new_max

  # ---- Per-interval overgrowth cap ----
  # Total coral area may overgrow UC + CCA but never exceed site_area. When the
  # summed coral area (per interval) would exceed site_area, scale that
  # interval's coral contributions down by the same factor. Independent scale
  # factors per interval keep low/mean/high from collapsing to one value.
  coral_area_mean <- coral_area_orig    + coral_area_new
  coral_area_lo   <- coral_area_orig_lo + coral_area_new_lo
  coral_area_hi   <- coral_area_orig_hi + coral_area_new_hi

  # Coral may overgrow UC + CCA but never exceed the COMPETABLE area
  # (site_area minus permanently-reserved OLB).
  cap_factor <- function(area_vec) {
    ifelse(area_vec > competable_area & area_vec > 0, competable_area / area_vec, 1)
  }
  f_mean <- cap_factor(coral_area_mean)
  f_lo   <- cap_factor(coral_area_lo)
  f_hi   <- cap_factor(coral_area_hi)

  cap_year <- which(f_mean < 1)
  if (length(cap_year)) {
    log_msg(sprintf(
      "Coral cover reached the competable cap (%.1f%% of site) at year %d; capping.",
      cap_pct, cap_year[1] - 1))
  }

  # Apply cap factors (mean to mean, bound factors to bounds).
  budget_df$carb_budg_orig  <- budget_df$carb_budg_orig  * f_mean
  budget_df$carb_budg_new   <- budget_df$carb_budg_new   * f_mean
  budget_df$carb_budg_total <- budget_df$carb_budg_total * f_mean
  budget_df$pct_cvr_orig    <- budget_df$pct_cvr_orig    * f_mean
  budget_df$pct_cvr_new     <- budget_df$pct_cvr_new     * f_mean
  budget_df$pct_cvr_total   <- budget_df$pct_cvr_total   * f_mean
  budget_df$pct_cvr_total_min   <- budget_df$pct_cvr_total_min   * f_lo
  budget_df$pct_cvr_total_max   <- budget_df$pct_cvr_total_max   * f_hi
  budget_df$carb_budg_total_min <- budget_df$carb_budg_total_min * f_lo
  budget_df$carb_budg_total_max <- budget_df$carb_budg_total_max * f_hi

  capped_coral_mean <- coral_area_mean * f_mean

  # End-of-duration cover (% of whole site). Coral species get the mean cap
  # factor. CCA is special: its reported cover is the UNCOVERED grown CCA area
  # (computed below as cca_area_yr), not its raw grown area, so it reflects
  # overgrowth. Filled in after cca_area_yr is known.
  end_cover_by_species <- vapply(names(species_area_by_sp), function(sp_nm) {
    if (grepl("algae", sp_nm, fixed = TRUE)) return(NA_real_)   # set after cca_area_yr
    av <- species_area_by_sp[[sp_nm]]
    (av[n] * f_mean[n]) / site_area * 100
  }, numeric(1))
  names(end_cover_by_species) <- names(species_area_by_sp)

  # ---- CCA (growing, overgrowable) + per-step microbioerosion ----
  # CCA grows (Phase 0) up to a per-year soft maximum = its grown area. Coral
  # may overgrow CCA (and UC). The uncovered CCA area each year is the smaller
  # of (grown CCA area) and (space left after coral). CCA calcifies over that
  # uncovered area. Microbioerosion is recomputed from remaining UC area.
  # All space terms are bounded by competable_area (OLB is reserved).
  uc_area0  <- site_area * uc_pct / 100
  cca_rows  <- target_cover_df[str_detect(target_cover_df$taxon, "algae"), , drop = FALSE]

  be_sd <- bioerosion_stdev(subregion, habitat)
  macro_hi <- macrobioerosion + (if (is.finite(be_sd[2])) be_sd[2] else 0)
  macro_lo <- macrobioerosion + (if (is.finite(be_sd[1])) be_sd[1] else 0)

  # Space available to CCA + UC after coral: competable area minus capped coral.
  # (capped_coral_mean never exceeds competable_area by construction.)
  noncoral_area <- pmax(0, competable_area - capped_coral_mean)

  # CCA soft ceiling: the grown CCA area per year (0 when no CCA present).
  cca_ceiling <- if (is.null(cca_grown_area)) rep(0, n) else cca_grown_area
  # Uncovered CCA = min(grown CCA, space left after coral).
  cca_area_yr <- pmin(cca_ceiling, noncoral_area)
  # Remaining UC after coral + uncovered CCA.
  uc_area_yr  <- pmax(0, noncoral_area - cca_area_yr)

  budget_df$microbioerosion <- (uc_area_yr / site_area) * be_micro_rate

  # CCA calcification over the uncovered (overgrowth-limited) grown CCA area.
  if (nrow(cca_rows)) {
    cca_sp   <- cca_rows$taxon[1]
    cca_rate <- calc_rates$rate[calc_rates$Taxon == cca_sp]
    cca_rate <- if (length(cca_rate) && is.finite(cca_rate[1])) cca_rate[1] else 0
    cca_budg <- (cca_area_yr * cca_rate) / site_area
    budget_df$carb_budg_total     <- budget_df$carb_budg_total     + cca_budg
    budget_df$carb_budg_orig      <- budget_df$carb_budg_orig      + cca_budg
    budget_df$carb_budg_total_min <- budget_df$carb_budg_total_min + cca_budg
    budget_df$carb_budg_total_max <- budget_df$carb_budg_total_max + cca_budg

    # Report CCA final cover as the uncovered CCA area at the final year.
    end_cover_by_species[cca_sp] <- (cca_area_yr[n] / site_area) * 100
  }

  # Drop any leftover NA CCA placeholders (e.g., a CCA row with no grown area).
  end_cover_by_species <- end_cover_by_species[!is.na(end_cover_by_species)]

  por <- assemblage_porosity(target_cover_df, "target_cvr_pct")

  # Net budgets: subtract per-year micro + macro bioerosion.
  budget_df$carb_budg_total <- budget_df$carb_budg_total - budget_df$microbioerosion - macrobioerosion
  budget_df$RAP_total       <- budget_df$carb_budg_total / 2.9 / (1 - por)
  budget_df$carb_budg_total_min <- budget_df$carb_budg_total_min - budget_df$microbioerosion - macro_hi
  budget_df$carb_budg_total_max <- budget_df$carb_budg_total_max - budget_df$microbioerosion - macro_lo
  budget_df$RAP_total_min <- budget_df$carb_budg_total_min / 2.9 / (1 - por)
  budget_df$RAP_total_max <- budget_df$carb_budg_total_max / 2.9 / (1 - por)

  budget_df$carb_budg_orig <- budget_df$carb_budg_orig - budget_df$microbioerosion - macrobioerosion
  budget_df$RAP_orig       <- budget_df$carb_budg_orig / 2.9 / (1 - por)
  budget_df$carb_budg_orig_min <- carb_budg_orig_min - budget_df$microbioerosion - macro_hi
  budget_df$carb_budg_orig_max <- carb_budg_orig_max - budget_df$microbioerosion - macro_lo
  budget_df$RAP_orig_min <- budget_df$carb_budg_orig_min / 2.9 / (1 - por)
  budget_df$RAP_orig_max <- budget_df$carb_budg_orig_max / 2.9 / (1 - por)

  log_msg(str_pad(" Restoration simulation complete ", side = "both", width = 90, pad = "="), "\n\n")

  list(
    budget_df = budget_df,
    outplants_by_species = outplants_by_species,
    achieved_cover_by_species = achieved_cover_by_species,
    end_cover_by_species = end_cover_by_species,
    outplants = sum(outplants_by_species),
    cost      = total_cost,
    olb_pct   = olb_pct,
    competable_area = competable_area
  )
}

# ============================================================================
# Observed-bioerosion metabolism (Restoration Monitoring tab) ----
# Converts the three observed sheets (Parrotfish / Urchins / Sponges) into a
# total bioerosion rate (kg CaCO3/m2/yr) per Years_Post_Restoration, joined to
# the species-specific rate lookups read at startup. Per-taxon-type fallback to
# regional rates when a given observed sheet is empty (headers only).
# ============================================================================

# Detect "headers only" (a data frame with zero rows).
sheet_is_empty <- function(x) is.null(x) || nrow(x) == 0

# Urchin test-diameter string class -> urchin-rate df Test_Size value.
# String-based, exhaustive classes.
urchin_test_size <- function(diam_str) {
  switch(as.character(diam_str),
    "0-20"   = 10,
    "20-40"  = 30,
    "40-60"  = 50,
    "60-80"  = 70,
    "80-100" = 90,
    NA_real_
  )
}

# Sponge bioerosion (kg CaCO3/m2/yr) for one year's rows.
#   area_m2 = Area_cm2 / 1e4; contribution = area_m2 * rate / Survey_Area_Sponges_m2
compute_sponge_erosion <- function(rows, rate_df) {
  if (sheet_is_empty(rows) || is.null(rate_df)) return(0)
  total <- 0
  for (i in seq_len(nrow(rows))) {
    tx   <- rows$Taxon[i]
    rate <- rate_df$Bioerosion_Rate[rate_df$Taxon == tx]
    if (length(rate) == 0 || is.na(rate[1])) next
    area_m2   <- .safe0(rows$Area_cm2[i]) / 1e4
    survey_m2 <- .safe0(rows$Survey_Area_Sponges_m2[i])
    if (survey_m2 <= 0) next
    total <- total + (area_m2 * rate[1]) / survey_m2
  }
  total
}

# Urchin bioerosion (kg CaCO3/m2/yr) for one year's rows.
#   rate is g CaCO3/urchin/day -> /1000 * Count / Survey_Area_Urchins_m2 * 365
compute_urchin_erosion <- function(rows, rate_df) {
  if (sheet_is_empty(rows) || is.null(rate_df)) return(0)
  total <- 0
  for (i in seq_len(nrow(rows))) {
    tx    <- rows$Taxon[i]
    tsize <- urchin_test_size(rows$Test_Diameter_mm[i])
    if (is.na(tsize)) next
    rate <- rate_df$Bioerosion_Rate[rate_df$Taxon == tx & rate_df$Test_Size == tsize]
    if (length(rate) == 0 || is.na(rate[1])) next
    count     <- .safe0(rows$Count[i])
    survey_m2 <- .safe0(rows$Survey_Area_Urchins_m2[i])
    if (survey_m2 <= 0) next
    total <- total + ((rate[1] / 1000) * count / survey_m2) * 365
  }
  total
}

# Parrotfish bioerosion (kg CaCO3/m2/yr) for one year's rows.
#   phase_size = paste0(Life_phase, gsub("-","_",Fork_length_cm)); this maps to
#   a column name in the parrotfish rate df. Rate is kg CaCO3/fish/year.
#   Returns list(total, unobserved) where `unobserved` is a character vector of
#   "G. species XX-XX cm" strings for rows whose rate lookup returned NA.
compute_parrotfish_erosion <- function(rows, rate_df) {
  if (sheet_is_empty(rows) || is.null(rate_df)) {
    return(list(total = 0, unobserved = character(0)))
  }
  total <- 0
  unobserved <- character(0)
  for (i in seq_len(nrow(rows))) {
    tx        <- rows$Taxon[i]
    life      <- as.character(rows$Life_phase[i])
    fork      <- as.character(rows$Fork_length_cm[i])
    phase_col <- paste0(life, gsub("-", "_", fork), "cm")

    rate_row <- rate_df[rate_df$Taxon == tx, , drop = FALSE]
    rate_val <- if (nrow(rate_row) && phase_col %in% names(rate_row)) {
      suppressWarnings(as.numeric(rate_row[[phase_col]][1]))
    } else {
      NA_real_
    }

    if (is.na(rate_val)) {
      unobserved <- c(unobserved, paste0(abbrev_species(tx), " ", fork, " cm"))
      next
    }

    count     <- .safe0(rows$Count[i])
    survey_m2 <- .safe0(rows$Survey_Area_Parrotfish_m2[i])
    if (survey_m2 <= 0) next
    total <- total + (rate_val * count) / survey_m2
  }
  list(total = total, unobserved = unobserved)
}

# Filter parplor description end.

# Filter choices for the Home-tab map controls ----
year_choices <- sort(unique(df$YEAR))
habitat_choices <- sort(unique(df$HABITAT_TYPE))

# White-to-red palettes ----
# Used for the "Symbolize by" numeric options.
# Each clamped 0 -> field max.
# Use Jenks symbology:
make_wr_pal <- function(field, n = 7, rev = FALSE) {
  vals <- df[[field]]
  vals <- vals[is.finite(vals)]

  # Jenks natural-breaks classification
  brks <- classInt::classIntervals(vals, n = n, style = "fisher")$brks

  # Guard against duplicate breaks (can happen with skewed/zero-heavy data)
  brks <- unique(brks)

  colorBin(
    palette = colorRampPalette(c("white", "red"))(length(brks) - 1),
    domain  = vals,
    bins    = brks,
    reverse = rev
  )
}

# Color palette for RAP symbology
at <- c(-8, -6, -4, -2, -0.5, 0, 0.5, 2, 4, 6, 8)
colors <- c("darkred", "red", "orangered", "orange", "yellow", "white", "#0099FF", "#0033FF", "darkblue", "#000066", "midnightblue")
num_pal <- colorNumeric(colors, domain = at)

pal_rap        <- num_pal
pal_gross      <- make_wr_pal("grossE_G")

# Reversed palettes for legend displays
num_pal_rev        <- colorNumeric(colors, domain = at, reverse = TRUE)
pal_gross_rev      <- make_wr_pal("grossE_G", rev = TRUE)

# Reef State: original Blue / Yellow / Orange status colors
state_colors <- c("growth"  = "#0099FF",
                  "stasis"  = "#FFFF99",
                  "erosion" = "#FF6600")

num_pal_state <- colorFactor(
  palette = unname(state_colors),
  levels  = names(state_colors)
)

# Restoration Mix groupings ----
# Branching (tan) | Weedy/Other (lime) across the top; Massive (gray) below.
branching_species <- c("Acropora cervicornis", "Acropora palmata")
weedy_species <- c("Porites astreoides", "Porites porites")
mix_massive_species <- c(
  "Colpophyllia natans",
  "Diploria labyrinthiformis",
  "Montastraea cavernosa",
  "Orbicella faveolata",
  "Pseudodiploria spp.",
  "Siderastrea siderea",
  "Solenastrea bournoni",
  "Stephanocoenia intersepta"
)


# Pastel palette pool for scenarios ----
# Strong reds and greens are excluded so bar colors don't carry connotative
# (good/bad) meaning; the pool is blues/purples/oranges/teals/pinks/neutrals.
# Session-persistent assignment lives in the server (sc_color_map) so toggling
# a scenario does not reshuffle the colors.
scenario_pastel_pool <- c(
  "#AEC7E8", # light blue
  "#C5B0D5", # light purple
  "#FFBB78", # light orange
  "#9EDAE5", # light teal
  "#F7B6D2", # light pink
  "#C7C7C7", # light gray
  "#DBDB8D", # khaki (muted yellow-green, not a "growth" green)
  "#BCBD9A", # sage
  "#D5A6BD", # mauve
  "#A9CCE3"  # steel blue
)

# Fallback generator (used only for names not yet in the session color map).
scenario_palette <- function(scenario_names) {
  n <- length(scenario_names)
  if (n == 0) return(character(0))
  pool <- scenario_pastel_pool
  if (n > length(pool)) pool <- colorRampPalette(pool)(n)
  setNames(pool[seq_len(n)], scenario_names)
}

# Shared impact-summary HTML builder ----
# Reused by the Monitoring tab and the Scenario Comparison tab. Takes baseline
# and restored scalars and renders the same three-delta summary, now including
# a percentile delta (recomputed against the current df RAP distribution).
build_impact_summary <- function(label, b_cover, r_cover, b_budget, r_budget,
                                 b_rap, r_rap) {
  d_cover  <- r_cover - b_cover
  d_budget <- r_budget - b_budget
  d_rap    <- r_rap - b_rap

  b_pct <- rap_percentile(b_rap)
  r_pct <- rap_percentile(r_rap)
  d_pct <- if (is.na(b_pct) || is.na(r_pct)) NA_real_ else r_pct - b_pct

  arrow <- function(x) if (is.na(x)) "\u2013" else if (x > 0) "\u25B2" else if (x < 0) "\u25BC" else "\u2013"
  HTML(paste0(
    "<p><b>", label, "</b></p>",
    "<p>", arrow(d_cover), " Coral cover change: <b>",
    sprintf("%+.1f", d_cover), " %</b></p>",
    "<p>", arrow(d_budget), " Carbonate budget change: <b>",
    sprintf("%+.2f", d_budget), " kg/m²/yr</b></p>",
    "<p>", arrow(d_rap), " Reef accretion potential change: <b>",
    sprintf("%+.2f", d_rap), " mm/yr</b></p>",
    "<p>", arrow(d_pct), " RAP percentile change: <b>",
    if (is.na(d_pct)) "\u2013" else sprintf("%+.0f", d_pct), " points</b>",
    if (!is.na(b_pct) && !is.na(r_pct))
      paste0(" <span style='color:#777;'>(", round(b_pct), "% \u2192 ", round(r_pct), "%)</span>")
    else "",
    "</p>",
    "<hr>",
    "<p>",
    if (!is.na(r_rap) && r_rap >= 3.1 && r_rap < Int_rate_at(2030)) {
      "<span style='color:orangered;'>Restored accretion exceeds the geologic baseline, but sea-level rise will exceed restored accretion by 2030.</span>"
    } else if (!is.na(r_rap) && r_rap >= Int_rate_at(2030) && r_rap < Int_rate_at(2050)) {
      "<span style='color:orange;'>Restored accretion exceeds the geologic baseline, but sea-level rise will exceed restored accretion by 2050.</span>"
    } else if (!is.na(r_rap) && r_rap >= Int_rate_at(2050) && r_rap < Int_rate_at(2070)) {
      "<span style='color:yellow;'>Restored accretion exceeds the geologic baseline, but sea-level rise will exceed restored accretion by 2070.</span>"
    } else if (!is.na(r_rap) && r_rap >= Int_rate_at(2070)) {
      "<span style='color:green;'>Restored accretion exceeds the geologic baseline and will exceed sea-level rise at least until 2070.</span>"
    } else {
      "<span style='color:red;'>Restored accretion is still exceeded by the geological baseline and sea-level rise.</span>"
    }, "</p>"
  ))
}

# Shiny User Interface ----
# Converted from bootstrapPage/navbarPage to shinydashboard::dashboardPage

## Header ----
header <- dashboardHeader(
  title = "Reef Persistence Tool",
  titleWidth = 240,
  # Explanatory blurb
  tags$li(
    class = "dropdown",
    tags$div(
      class = "header-blurb",
      style = "padding: 5px; font-size: 14px; color: #333; text-align: right;",
      HTML("This tool is a collaboration between the
            <strong>National Oceanic and Atmospheric Administration (NOAA)</strong> (U.S. Department of Commerce)&nbsp<br/>
            and the <strong>U.S. Geological Survey (USGS)</strong> (U.S. Department of the  Interior).")
    )
  ),
  # Logos in the top ribbon (as right-aligned dropdown items)
  tags$li(class = "dropdown",
    tags$div(tags$img(src = "noaaLogo_noText.png", style = "height: 50px; padding: 2px 0 2px 0;"))
  ),
  tags$li(class = "dropdown",
    tags$div(tags$img(src = "usgsLogo.png", style = "height: 50px; padding: 2px 15px 2px 0;"))
  ),
  # Dark Mode toggle lives in the top ribbon (as a right-aligned "dropdown" item)
  tags$li(
    class = "dropdown",
    tags$div(
      class = "dark-mode-switch",
      style = "padding:0px 10px; margin-top:15px; margin-bottom:-15px; align-items: center; display: flex; justify-content: flex-end;",
      materialSwitch(
        inputId = "dark_mode",
        label = "Dark Mode",
        status = "primary",
        # value = TRUE,
        right = TRUE,
        inline = TRUE
      )
    )
  )
)

## Sidebar ----
sidebar <- dashboardSidebar(
  width = 240,
  sidebarMenu(
    id = "nav",
    menuItem("Reef Site Map", tabName = "home", icon = icon("map")),
    menuItem("Reef Site Projections", tabName = "projections", icon = icon("chart-line")),
    menuItem(("Management Interventions"), icon = icon("flask"),
      menuSubItem("Outplanting Scenarios", tabName = "outplanting", icon = icon("seedling")),
      menuSubItem("Scenario Comparison", tabName = "comparison", icon = icon("scale-balanced")),
      menuSubItem("Restoration Monitoring", tabName = "monitoring", icon = icon("chart-column")),
      menuSubItem("Calcifier Data", tabName = "calcifier", icon = icon("table"))
    ),
    menuItem("About this App", tabName = "about", icon = icon("circle-info"))
  )
)

## Body ----
body <- dashboardBody(
  useShinyjs(),
  tags$script(HTML("$('body').addClass('fixed');")), # Keep lock header and sidebar in-place when scrolling
  tags$head(
    includeHTML(here("gtag.html")),
    includeCSS(here("styles.css")),
    tags$style(HTML("
      /* Preserve custom background color */
      .content-wrapper, .right-side { background-color: #BFDADA; }
      .custom-absolute-panel { z-index: 9999; }
      /* .box { color: #000; } */
      /* Dark Mode switch label: black when OFF */
      .dark-mode-switch .control-label,
      .dark-mode-switch label { color: black; }
      /* Full-bleed map on the Home tab */
      .home-map-outer {
        position: absolute; top: 0; left: 0; right: 0; bottom: 0;
        overflow: hidden; padding: 0;
      }
      /* Map Controls floating panel */
      .map-controls-panel {
        position: absolute; top: 115px; right: 10px; z-index: 1000;
        width: 280px; background: rgba(255,255,255,0.92);
        border-radius: 8px; box-shadow: 0 1px 6px rgba(0,0,0,0.3);
      }
      .map-controls-header {
        cursor: pointer; padding: 8px 12px; font-weight: bold;
        background: #3c8dbc; color: white; border-radius: 8px 8px 0 0;
        display: flex; justify-content: space-between; align-items: center;
      }
      .map-controls-body { padding: 10px 12px; max-height: 60vh; overflow-y: auto; }
      .map-controls-body .form-group { margin-bottom: 10px; }
      /* Compact baseline species inputs: name + narrow box side-by-side */
      .baseline-species-row {
        display: flex; align-items: center; justify-content: space-between;
        gap: 6px; margin-bottom: 4px;
      }
      .baseline-species-row label { margin: 0; font-weight: normal; }
      .baseline-species-row .form-group { margin-bottom: 0; }
      .baseline-species-row .shiny-input-container { width: auto; margin-bottom: 0; }
      .baseline-species-name {
        flex: 1 1 auto; font-style: italic; font-size: 13px;
        /* white-space: nowrap; overflow: hidden; text-overflow: ellipsis; */
        overflow-wrap: break-word;
      }
      .baseline-species-input input {
        width: 10ch; min-width: 10ch; padding: 2px 4px; text-align: right;
      }
      /* Restoration mix: Italicize the species names on sliders */
      #restoration_sliders .shiny-input-container > label,
      #restoration_sliders .control-label {
        font-style: italic;
      }
      /* Restoration mix: per-species outplant caption */
      .rest-outplant-note {
        font-size: 11px; color: #2f4f2f; font-style: italic;
        margin: 1px 1px 1px -10px;
      }
      /* Restoration mix: morphology sub-boxes as compact fieldsets */
      .mix-fieldset {
        border-radius: 6px; padding: 0 5px; margin: 2px -4px 0 0;
      }
      .mix-fieldset > legend {
        width: auto; font-size: 13px; font-weight: bold;
        margin-bottom: 2px; padding: 0 4px; border: none;
      }
      .mix-branching > legend { color: #a97d3e; }
      .mix-massive   { border: 2px solid #9e9e9e; }   /* gray */
      .mix-massive   > legend { color: #6f6f6f; }
      .mix-weedy     { border: 2px solid #7bc043; margin-right: -10px }  /* lime */
      .mix-weedy     > legend { color: #5a9130; }
      
      /* Two-line, italic input labels in the mix sub-boxes */
      .mix-fieldset .control-label { font-style: italic; line-height: 1.2; }

      /* Active (user-supplied) mix input: blue border. The class may be
         applied to the numericInput's wrapper OR directly to the <input>
         depending on Shiny version, so target both the element itself when it
         is an input and any descendant input. */
      .mix-active-input input,
      input.mix-active-input {
        border: 2px solid #2C8CB9 !important;
        box-shadow: 0 0 0 1px #2C8CB933 !important;
      }
      body.dark-mode .mix-active-input input,
      body.dark-mode input.mix-active-input {
        border-color: #a06fd6 !important;
        box-shadow: 0 0 0 1px #a06fd644 !important;
      }

      /* Stacked species name above its numeric input in the Restoration Mix */
      .mix-species-stacked { margin-bottom: 12px; }
      .mix-species-stacked .mix-species-label {
        font-style: italic; font-size: 13px; line-height: 1.1;
        white-space: normal; margin-bottom: 3px; margin-top: 3px;
      }
      .mix-species-stacked .shiny-input-container { width: auto; margin-bottom: 0; }
      .mix-species-stacked input {
        width: 8ch; min-width: 8ch; padding: 4px 6px; text-align: right;
      }
      /* Tighten gutters between the three top-row boxes (~1/3 spacing) */
      .outplant-toprow > [class*='col-'] { padding-left: 5px; padding-right: 5px; }

      /* Outplant parameter inputs: Inline label + narrow box */
      .param-inline-row {
        display: flex; align-items: center; justify-content: space-between;
        gap: 6px; margin-bottom: 8px;
      }
      .param-inline-row .param-label {
        flex: 1 1 auto; font-weight: normal; font-size: 14px;
      }
      .param-inline-row .shiny-input-container { width: auto; margin-bottom: 0; }
      .param-inline-row .num-input {
        width: 18ch; min-width: 15ch; padding: 2px 0px; text-align: right; margin-right: -20px;
      }
      .param-inline-row .sel-input {
        width: 70%; min-width: 20ch; padding: 2px 0px; text-align: left;
      }
      .param-inline-row .param-unit {
        flex: 0 0 auto; font-weight: normal; font-size: 14px; width: 4ch;
      }

      /* Apply bolding and shadow to shinydashboard::box titles */
      .box-header .box-title {
        font-weight: bold;
        text-shadow: -1px -1px 0 black, 1px -1px 0 black,
                     -1px 1px 0 black, 1px 1px 0 black;
      }

      /* Shrink the main value text size and add a 1px black text shadow in the valueBox readouts */
      .small-box h3 {
        font-size: 30px; !important;
        text-shadow: -1px -1px 0 black, 1px -1px 0 black,
                          -1px 1px 0 black, 1px 1px 0 black;
      }
      .small-box .bordered-text {
      font-size: 18px; !important;
      text-shadow: -1px -1px 0 black, 1px -1px 0 black,
                          -1px 1px 0 black, 1px 1px 0 black;
      }

      /* Inline upload label + Download template button */
      .upload-label-row {
        display: flex; align-items: center; justify-content: space-between;
        gap: 8px; margin-bottom: 4px;
      }
      .upload-label-row .control-label { margin: 0; font-weight: bold; }

      /* ---- Right-side log panel ---- */
      .log-panel {
        position: fixed; top: 0; right: 0; height: 100vh; width: 650px;
        background: #f4f4f4; border-left: 2px solid #3c8dbc; z-index: 1200;
        box-shadow: -2px 0 6px rgba(0,0,0,0.2);
        transform: translateX(100%); transition: transform 0.25s ease;
        display: flex; flex-direction: column;
      }
      .log-panel.open { transform: translateX(0); }
      .log-panel-header {
        padding: 8px 12px; font-weight: bold; background: #3c8dbc; color: white;
        display: flex; justify-content: space-between; align-items: center;
      }
      .log-panel-body { padding: 8px 12px; overflow-y: auto; flex: 1 1 auto; }
      .log-toggle-tab {
        position: fixed; top: 120px; right: 0; z-index: 1201;
        background: #3c8dbc; color: white; cursor: pointer;
        padding: 8px 6px; border-radius: 6px 0 0 6px; writing-mode: vertical-rl;
        font-weight: bold; box-shadow: -1px 1px 4px rgba(0,0,0,0.3);
      }
      body.dark-mode .log-panel { background: #232a33; border-left-color: #8fb8d8; }
      body.dark-mode .log-panel-body { background: #232a33; }
      body.dark-mode .log-panel-body,
      body.dark-mode .log-panel-body pre { color: #e6e6e6; background: #232a33; }
      body.dark-mode .log-panel-header { background: #10141a; color: #e6e6e6; }

      /* ---- Full-height content area (fixes Home map + About cutoff) ----
         Under body.fixed + zoom, the content wrapper doesn't stretch to the
         zoom-adjusted viewport, so absolutely-positioned fills (the Home map)
         and tall blocks (About) get clipped partway down. Pin the wrapper to
         at least the full viewport height and let the map fill it. */
      .content-wrapper, .right-side {
        min-height: 100vh;
      }
      .home-map-outer {
        min-height: 100vh;
        height: 100%;
      }
      #mymap {
        height: 100vh !important;
      }
      .tab-content, .tab-pane.active {
        min-height: 100vh;
      }

      /* ---- Responsive uniform scaling for smaller screens ---- */
      /* Shrink the whole layout proportionally so a laptop looks like a
         zoomed-out desktop rather than a cramped reflow. `zoom` keeps
         click/leaflet coordinates correct (unlike transform: scale). */
      @media (max-width: 1600px) { body { zoom: 0.90; } }
      @media (max-width: 1440px) { body { zoom: 0.82; } }
      @media (max-width: 1366px) { body { zoom: 0.78; } }
      @media (max-width: 1280px) { body { zoom: 0.72; } }
      @media (max-width: 1200px) { body { zoom: 0.68; } }
      @media (max-width: 1024px) { body { zoom: 0.60; } }
      @media (max-width: 900px) { body { zoom: 0.55; } }
      @media (max-width: 800px) { body { zoom: 0.50; } }
      @media (max-width: 700px) { body { zoom: 0.45; } }
      @media (max-width: 600px) { body { zoom: 0.40; } }

      /* ---------------- DARK MODE (CSS-class toggle on <body>) ---------------- */
      body.dark-mode .content-wrapper,
      body.dark-mode .right-side { background-color: #1b2027 !important; }
      body.dark-mode .main-header .logo,
      body.dark-mode .main-header .navbar { background-color: #10141a !important; }
      body.dark-mode .main-sidebar { background-color: #141a21 !important; }
      /* Lighten ribbon text + menu (hamburger) icon */
      body.dark-mode .main-header .logo,
      body.dark-mode .main-header .navbar,
      body.dark-mode .main-header .sidebar-toggle,
      body.dark-mode .main-header .navbar .nav > li > a,
      body.dark-mode .dark-mode-switch .control-label,
      body.dark-mode .dark-mode-switch label { color: #e6e6e6 !important; }
      body.dark-mode .box {
        background-color: #232a33 !important;
        color: #e6e6e6 !important;
        border-top-color: #3a4552 !important;
      }
      body.dark-mode .box-title,
      body.dark-mode .box-body,
      body.dark-mode label,
      body.dark-mode .control-label,
      body.dark-mode h1, body.dark-mode h2, body.dark-mode h3,
      body.dark-mode h4, body.dark-mode .param-label,
      body.dark-mode .param-unit { color: #e6e6e6 !important; }
      body.dark-mode .form-control,
      body.dark-mode .selectize-input,
      body.dark-mode input[type='number'],
      body.dark-mode input[type='text'] {
        background-color: #2c353f !important; color: #e6e6e6 !important;
        border-color: #3a4552 !important;
      }
      body.dark-mode .selectize-dropdown { background-color: #2c353f !important; color: #e6e6e6 !important; }
      body.dark-mode .rest-outplant-note { color: #9fd08a !important; }
      body.dark-mode .impact-summary-box {
        background: #2c353f !important; border-color: #3a4552 !important; color: #e6e6e6 !important;
      }
      /* Darken the Map Controls widget */
      body.dark-mode .map-controls-panel { background: rgba(35,42,51,0.95) !important; color: #e6e6e6; }
      body.dark-mode .map-controls-panel .map-controls-body,
      body.dark-mode .map-controls-panel label,
      body.dark-mode .map-controls-panel .control-label,
      body.dark-mode .map-controls-panel strong,
      body.dark-mode .map-controls-panel .point-size-label { color: #e6e6e6 !important; }

      /* Header collaboration blurb */
      body.dark-mode .header-blurb,
      body.dark-mode .header-blurb strong { color: #e6e6e6 !important; }

      /* Filter-by (Habitat / Year) dropdownButton panels.
         shinyWidgets renders the panel as a Bootstrap .dropdown-menu; the
         visible background is on that container, not the label elements. */
      body.dark-mode .dropdown-menu,
      body.dark-mode .sw-dropdown-content,
      body.dark-mode .sw-dropdown-in,
      body.dark-mode .dropdown-menu .form-group,
      body.dark-mode .dropdown-menu .shiny-options-group {
        background-color: #2c353f !important;
        border-color: #3a4552 !important;
        color: #e6e6e6 !important;
      }
      body.dark-mode .dropdown-menu .checkbox label,
      body.dark-mode .dropdown-menu label,
      body.dark-mode .dropdown-menu .control-label {
        color: #e6e6e6 !important;
      }

      /* selectInput caret (project-name dropdown, etc.) */
      body.dark-mode .selectize-input:after { border-color: #e6e6e6 transparent transparent transparent !important; }
      body.dark-mode .selectize-control.single .selectize-input:after { border-top-color: #e6e6e6 !important; }

      /* Plotly axis tick labels + titles (RAP timelines) */
      body.dark-mode .js-plotly-plot .xtick text,
      body.dark-mode .js-plotly-plot .ytick text,
      body.dark-mode .js-plotly-plot .xtitle,
      body.dark-mode .js-plotly-plot .ytitle { fill: #e6e6e6 !important; }

      /* DataTables (Calcifier Data tab) dark mode: lighten text + controls */
      body.dark-mode .dataTables_wrapper,
      body.dark-mode table.dataTable,
      body.dark-mode table.dataTable thead th,
      body.dark-mode table.dataTable tbody td,
      body.dark-mode .dataTables_wrapper .dataTables_length,
      body.dark-mode .dataTables_wrapper .dataTables_filter,
      body.dark-mode .dataTables_wrapper .dataTables_info,
      body.dark-mode .dataTables_wrapper .dataTables_paginate,
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #e6e6e6 !important;
      }
      body.dark-mode table.dataTable tbody tr { background-color: #232a33 !important; }
      body.dark-mode table.dataTable tbody tr:hover { background-color: #2c353f !important; }
      body.dark-mode table.dataTable thead th { border-bottom-color: #3a4552 !important; }
      /* Column-filter search boxes injected by DT filter = 'top' */
      body.dark-mode .dataTables_wrapper input,
      body.dark-mode table.dataTable thead input {
        background-color: #2c353f !important; color: #e6e6e6 !important;
        border-color: #3a4552 !important;
      }
      /* Paginate buttons hover/current */
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      body.dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: #2c353f !important; color: #e6e6e6 !important;
        border-color: #3a4552 !important;
      }

    ")),
    # Toggle the body dark-mode class from the switch
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_dark', function(on) {
        document.body.classList.toggle('dark-mode', on);
      });
    ")),
    # Native-title tooltips keyed by input id. Re-applied on a short interval so
    # dynamically-rendered inputs (tabs, uiOutput) also receive their titles.
    tags$script(HTML("
      var RPT_TIPS = {
        'baseline_template_dl': 'Download a template baseline-input .xlsx file.',
        'baseline_upload': 'Upload an .xlsx file containing baseline reef site percent-cover data.',
        'baseline_load_example': 'Upload example baseline data.',
        'baseline_load_cache': 'Upload cached inputs from the most recent scenario.',
        'baseline_site': 'Select an uploaded survey site, or type a name to build a scenario from scratch.',
        'site_area_m2': 'Total planar area of the reef patch being modeled, in square meters.',
        'site_latitude': 'Site latitude in decimal degrees (optional; used for mapping).',
        'site_longitude': 'Site longitude in decimal degrees (optional; used for mapping).',
        'subregion_choice': 'Reef subregion, used to find region-specific bioerosion rates and bleaching-mortality relationships.',
        'habitat_choice': 'Habitat type within the subregion, used to refine bioerosion rates.',
        'baseline_save_dl': 'Save all this inputs for this scenario as an .xlsx file.',
        'baseline_delete_cache': 'Delete the cached baseline input. Does not affect the original input file.',
        'reset_mix': 'Clear all input cells in the Restoration Mix except Baseline Cover.',
        'additional_outplant_years': 'Comma-separated integer years (after Year 0) at which to place additional outplants. Each year becomes an extra row in count-driven species dropdowns.',
        'base_REQUIRED_Other_living_benthos': 'Percent cover of other living benthos (sponges, soft corals, etc.). This space is considered unavailable for calcifier growth. A value of 0 or greater is required.',
        'base_REQUIRED_Unconsolidated_substrate': 'Percent cover of unconsolidated substrate (sand, rubble, etc.). This value is used to determine the available non-coral consolidated substrate affected by microbioerosion. This area is considered available for calcifier growth.',
        'sim_duration': 'Number of years to project reef growth into the future.',
        'rest_horizon': 'Target year by which the desired coral cover should be reached through outplanting and and projected coral growth.',
        'dhw': 'Thermal-stress severity of each bleaching event, in degree-heating weeks.',
        'bleach_events': 'How often bleaching occurs, expressed as events per five-year period.',
        'scenario_project': 'A project name which groups related scenarios together for comparison.',
        'scenario_name': 'A label for this specific parameter combination.',
        'reactive_sim': 'When on, the projection recomputes automatically as inputs change.',
        'save_scenario': 'Save the results of this simulation to a .json file. Compare these outputs in the Scenario Comparison tab.',
        'run_sim': 'Run the growth simulation with the current scenario parameters.',
        'target_cover_increase': 'Hypothetical increase in coral cover, used to preview restored reef status on the map.',
        'symbolize_by': 'Choose which metric colors the site markers.',
        'show_named_reefs': 'Overlay labeled named-reef points and polygons on the map.',
        'filter_habitat_dd': 'Only display sites in the selected habitat(s).',
        'filter_year_dd': 'Only display sites surveyed in the selected year(s).',
        'show_slr': 'Overlay projected sea-level-rise rates on the timeline.',
        'sc_project': 'Select a project within which to compare scenarios.',
        'sc_scenarios': 'Select which scenarios to compare within the selected project.',
        'sc_show_slr': 'Overlay projected sea-level-rise reference rates on the bar chart.',
        'sc_refresh': 'Re-scan the scenarios folder and update the list of options.',
        'sc_download_csv': 'Download the Comparison Table as a .csv file.',
        'sc_show_slr': 'Overlay projected sea-level-rise reference rates on the bar chart.',
        'upload_cover': 'Upload an .xlsx file containing cover-monitoring timeseries data.',
        'monitoring_cover_template_dl': 'Download a template cover-monitoring-input .xlsx file.',
        'cover_load_example': 'Upload example cover-monitoring data.',
        'upload_bioerosion': 'Upload an .xlsx file containing bioerosion-monitoring timeseries data.',
        'monitoring_bioerosion_template_dl': 'Download a template bioerosion-monitoring-input .xlsx file.',
        'bioerosion_load_example': 'Upload example bioerosion data.',
        'monitoring_show_slr': 'Overlay projected sea-level-rise reference rates on the timeline.',
        'monitoring_selected_site': 'Choose a site to display observed post-restoration monitoring data.',
        'monitoring_download_report': 'Download a report of the monitoring simulation as a .csv file.',
        'monitoring_clear_cache': 'Delete cached cover- and bioerosion-monitoring data. The original files will be unaffected.',
      };
      function rptSetTitle(el, txt) {
        if (el) el.setAttribute('title', txt);
      }
      function rptApplyTips() {
        for (var id in RPT_TIPS) {
          var txt = RPT_TIPS[id];
          // The element carrying the id (often a hidden <input>/<select> for
          // sliders and selectize, or the visible <input> for numericInput).
          var el = document.getElementById(id);
          if (!el) continue;

          rptSetTitle(el, txt);

          // Walk up to the enclosing .form-group so the LABEL is covered too.
          var grp = el.closest ? el.closest('.form-group') : null;
          rptSetTitle(grp, txt);

          // Slider (ionRangeSlider): the visible widget is a sibling .irs block,
          // usually within the same .form-group. Title every piece so any hover
          // target works.
          var scope = grp || el.parentNode;
          if (scope) {
            scope.querySelectorAll('.irs, .irs-line, .irs-bar, .irs-handle, .irs-single, .js-irs-0')
                 .forEach(function(n){ rptSetTitle(n, txt); });
            // Selectize: the visible control replaces the hidden <select>.
            scope.querySelectorAll('.selectize-control, .selectize-input')
                 .forEach(function(n){ rptSetTitle(n, txt); });
            // materialSwitch / checkbox visible parts.
            scope.querySelectorAll('.bootstrap-switch, .material-switch, label')
                 .forEach(function(n){ rptSetTitle(n, txt); });
          }
        }
      }
      $(document).on('shiny:idle shiny:value', rptApplyTips);
      setInterval(rptApplyTips, 1500);
    "))
  ),

  tabItems(
    # Home Tab (Reef Site Map) ----
    tabItem(
      tabName = "home",
      div(
        class = "home-map-outer",
        leafletOutput("mymap", width = "100%", height = "100%"),
        # Right-aligned data caption overlaid on the top-right corner of the map
        tags$div(
          style = "position: absolute; top: 60px; right: 10px;
          z-index: 1000; display: flex; gap: 8px;",
          tags$li(
            class = "dropdown",
            tags$span(
              style = "color: white; line-height: 20px; margin-right: 15px; font-size: 18px;
                       text-shadow: -1px -1px 0 black, 1px -1px 0 black,
                                    -1px 1px 0 black, 1px 1px 0 black;
                                    ",
              HTML("Displaying National Coral Reef Monitoring Program (NCRMP) data (2014-2024).<br/>
                    The NCRMP is the world's largest coral-reef monitoring program, and is adminstered<br/>
                    by NOAA. Click a point for details.")
            )
          )
        ),

        # Map Controls: vertically-collapsible box below the logos
        tags$div(
          class = "map-controls-panel",
          tags$div(
            class = "map-controls-header",
            onclick = "var b=document.getElementById('map_controls_body'); b.style.display = (b.style.display==='none') ? 'block' : 'none';",
            tags$span("Map Controls"),
            tags$span(icon("chevron-down"))
          ),
          tags$div(
            id = "map_controls_body",
            class = "map-controls-body",

            # Target Percent-Cover Increase slider
            sliderInput("target_cover_increase", tags$strong("Target Percent-Cover Increase"),
              min = 0, max = 30, value = 0, step = 1, post = "%", width = "100%"
            ),

            # Filter group: Year + Habitat dropdown checkboxes
            tags$div(
              style = "display:flex; gap:8px; align-items:center;",
              tags$strong("Filter by:"),
              shinyWidgets::dropdownButton(
                inputId = "filter_habitat_dd",
                label = "Habitat",
                circle = FALSE, width = "100%", status = "default",
                checkboxGroupInput("filter_habitat", NULL,
                  choices = habitat_choices, selected = habitat_choices
                )
              ),
              shinyWidgets::dropdownButton(
                inputId = "filter_year_dd",
                label = "Year",
                circle = FALSE, width = "100%", status = "default",
                checkboxGroupInput("filter_year", NULL,
                  choices = year_choices, selected = year_choices
                )
              )
            ),

            tags$hr(),

            # Symbolize by: exclusive radio buttons
            radioButtons("symbolize_by", tags$strong("Symbolize by:"),
              choices = c(
                "Reef Accretion Potential (RAP)" = "rap",
                "Reef State"                     = "current_state",
                "Gross Bioerosion"               = "grossE_G"
              ),
              selected = "rap"
            ),

            tags$hr(),

            # Named-reef labels toggle
            checkboxInput("show_named_reefs", tags$strong("Show named reefs"), value = FALSE),

            tags$hr(),

            # Point-size stepper
            tags$div(
              class = "point-size-label",
              style = "font-size: 13px; margin-bottom: 4px; color: #333;",
              tags$strong("Point size")
            ),
            tags$div(
              style = "display: flex; align-items: center; gap: 8px;",
              actionButton("point_size_down", "\u2212", class = "btn-sm"),
              textOutput("point_size_label", inline = TRUE),
              actionButton("point_size_up", "+", class = "btn-sm")
            )
          )
        )
      )
    ),

    # Placeholder: Reef Site Projections ----
    tabItem(
      tabName = "projections",
      fluidRow(
        # column(10,
        #   tags$img(src = "persistenceExample.jpg", width = "1300px", height = "900px")
        # ),
        column(2,
          HTML("<span style = 'font-size: 48px;'><strong>Coming soon!</strong></span>")
        )
      )
    ),

    # Outplant Scenarios Tab ----
    tabItem(
      tabName = "outplanting",
      # Vertical layout: horizontal input row on top, timeline on the bottom
      # `outplant-toprow` class tightens the inter-box gutters.
      fluidRow(
        class = "outplant-toprow",
        # ---- Restoration Scenario (baseline cover + per-species mix) ----
        shinydashboard::box(
          title = "Restoration Scenario",
          width = 12, status = "primary", solidHeader = TRUE,
          column(width = 12,
            fluidRow(
              # Left: site controls
              column(
                width = 3,
                tags$div(
                  class = "upload-label-row",
                  tags$span(class = "control-label", tags$strong("Load .xlsx")),
                  tags$div(
                    style = "display: flex; gap: 6px;",
                    downloadButton("baseline_template_dl", "Template", class = "btn-sm"),
                  )
                ),
                fileInput("baseline_upload", NULL, accept = c(".xlsx")),
                tags$div(
                  style = "display:flex; margin-top:-15px; margin-bottom:20px; align-items:center; justify-content:space-between",
                  actionButton("baseline_load_example", "Example",
                              icon = icon("upload"), class = "btn-sm"),
                  actionButton("baseline_load_cache", "Cache",
                              icon = icon("upload"), class = "btn-sm")
                ),
                tags$hr(),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", tags$strong("Site")),
                  tags$span(
                    class = "sel-input",
                    selectizeInput(
                      "baseline_site",
                      label = NULL,
                      choices = c("\u2013 Select site \u2013" = ""),
                      selected = "",
                      options = list(create = TRUE, placeholder = "Select or type a site...")
                    )
                  )
                ),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", tags$strong("Subregion")),
                  tags$span(
                    class = "sel-input",
                    selectInput(
                      "subregion_choice",
                      label = NULL,
                      choices = c("\u2013 Select subregion \u2013" = "",
                                  unname(subregion_labels)),
                      selected = ""
                    )
                  )
                ),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", tags$strong("Habitat")),
                  tags$span(
                    class = "sel-input",
                    selectInput(
                      "habitat_choice",
                      label = NULL,
                      choices = c("\u2013 Select habitat \u2013" = ""),
                      selected = ""
                    )
                  )
                ),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", tags$strong("Site area")),
                  tags$span(
                    class = "num-input",
                    numericInput("site_area_m2", label = NULL,
                    value = 100, min = 1, max = 10000, step = 1
                    )
                  ),
                  tags$span(class = "param-unit", "m²")
                ),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", tags$strong("Latitude")),
                  tags$span(
                    class = "num-input",
                    numericInput("site_latitude", label = NULL,
                      value = NA, min = -90, max = 90, step = 0.00001
                    )
                  ),
                  tags$span(class = "param-unit", "\u00b0")
                ),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", tags$strong("Longitude")),
                  tags$span(
                    class = "num-input",
                    numericInput("site_longitude", label = NULL,
                      value = NA, min = -180, max = 180, step = 0.00001
                    )
                  ),
                  tags$span(class = "param-unit", "\u00b0")
                )
              ),

              # Right: 7-column Restoration mix grid
              column(
                width = 9,
                tags$div(class = "mix-grid",
                  style = "border: 2px solid #2C8CB9; border-radius: 8px; padding: 8px",
                  tags$div(style = "text-align:center;", tags$strong("Restoration Mix")),
                  # Column headers
                  tags$div(
                    class = "mix-grid-header",
                    style = "display:flex; align-items:flex-end; gap:6px;
                              font-weight:bold; font-size:12px; margin:4px 30px 0px 0;",
                    tags$div(style = "flex:0 0 auto; width:28px;", ""),
                    tags$div(style = "flex: 2 1 0;",
                      tags$div(style = "text-align:center;",
                        tags$div("Additional outplanting years"),
                        textInput("additional_outplant_years", label = NULL,
                                  value = "", placeholder = "e.g. 3, 5, 10", width = "100%")
                      )
                    ),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Current (baseline) percent cover for this species at the site.",
                      uiOutput("mix_baseline_header")),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Desired total percent cover for this species. Drives the simulation and solves for outplants when this is the active (blue) input.",
                      uiOutput("mix_target_header")),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Average starting diameter of each outplanted coral fragment, in centimeters.",
                      HTML("Avg. outplant<br/>diameter (cm)<br/><br/> ")),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Average cost per outplant, used to compute total project cost.",
                      HTML("Avg. outplant<br/>cost ($)<br/><br/> ")),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Number of coral fragments to outplant. Drives the simulation when this is the active (blue) input.",
                      HTML("Outplants<br/><br/><br/> ")),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Optional: fragments outplanted per 'cluster'. Clusters are constructed by packing this quantity of fragments into a modeled circle, with a 0.5 cm inter-fragment gap. Each cluster is simulated as a single colony.",
                      HTML("Outplants<br/>per cluster<br/>(optional)")),
                    tags$div(style = "flex: 1 1 0; text-align:center;",
                      title = "Read-only: this species' total projected percent cover at the end of the simulation duration.",
                      uiOutput("mix_final_header")
                    )
                  ),
                  div(
                    style = "overflow-y: scroll; height: 380px; padding: 5px; border: 1px solid #ccc",
                    uiOutput("restoration_mix_inputs")
                  )
                )
              ),

              # Bottom row: function buttons and scenario naming
              fluidRow(
                column(12,
                  tags$hr(),
                  column(3,
                    tags$div(
                      style = "display:flex; gap:30px; margin-top:25px; align-items:center; justify-content:flex-start",
                      downloadButton("baseline_save_dl", tags$strong("Save baseline"),
                        icon = icon("floppy-disk"), class = "btn-sm"),
                      actionButton("baseline_delete_cache", tags$strong("Clear cache"),
                        icon = icon("trash"), class = "btn-sm"),
                      actionButton("reset_mix", tags$strong("Reset targets"),
                        icon = icon("eraser"), class = "btn-sm")
                    )
                  ),
                  # Save and run scenario
                  column(9,
                    tags$div(style = "display:flex; gap:15px; align-items:center; justify-content:flex-end",
                      textInput("scenario_project", tags$strong("Project name"), value = ""),
                      textInput("scenario_name", tags$strong("Scenario name"), value = ""),
                      actionButton("save_scenario", tags$strong("Save result"), icon = icon("floppy-disk")),
                      materialSwitch("reactive_sim", HTML("<strong>Reactive<br/>simulation</strong>"),
                        value = FALSE, status = "primary", right = TRUE, inline = TRUE),
                      actionButton("run_sim", tags$strong("Simulate"), icon = icon("play"))
                    )
                  )
                )
              )
            )
          )
        ),

        # ---- Bottom row: Target Years / Bleaching ----
        # Target Years
        shinydashboard::box(
          title = "Target Years",
          width = 6, status = "success", solidHeader = TRUE,
          column(6,
            sliderInput(
              "sim_duration", tags$strong("Simulation duration (years)"),
              value = 10, min = 0, max = 30, step = 1
            )
          ),
          column(6,
            sliderInput("rest_horizon", tags$strong("Restoration horizon (years)"),
              value = 0, min = 0, max = 30, step = 1
            )
          )
        ),

        # Mortality factors
        shinydashboard::box(
          title = "Bleaching Scenario",
          width = 6, status = "warning", solidHeader = TRUE,
          column(6,
            sliderInput("dhw", tags$strong("Degree-Heating Weeks"),
              min = 8, max = 24, value = 8, step = 1
            )
          ),
          column(6,
            sliderTextInput(
              inputId = "bleach_events",
              label = tags$strong("Bleaching (Events / 5 years)"),
              choices = c(0, 1, 2, 5),
              selected = 0,
              grid = TRUE
            )
          )
        )

        # Temporarily removed placeholders:

        # shinydashboard::box(
        #   id = "mort_opt_box",
        #   title = "Additional Mortality (optional)",
        #   width = 12,
        #   status = "danger",
        #   collapsible = TRUE,
        #   collapsed = TRUE,
        #   solidHeader = TRUE,
        #   sliderInput("mort_adj", tags$strong("Chronic Mortality (%)"),
        #     min = -10, max = 10, value = 0, step = 1
        #   ),
        #   sliderInput("mort_adj", tags$strong("Episodic Mortality (%)"),
        #     min = -10, max = 10, value = 0, step = 1
        #   ),
        #   sliderTextInput(
        #     inputId = "mort_events",
        #     label = tags$strong("Episodic Mortality (Events / 5 years)"),
        #     choices = c(0, 1, 2, 5),
        #     selected = 0,
        #     grid = TRUE
        #   )
        # )
      # )
      ),

      # --- Timeline (bottom) ---
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            id = "restoration_timeline_box",
            title = "Projected Reef Accretion Potential (RAP)",
            width = 12, status = "info", solidHeader = TRUE,
            collapsible = TRUE, collapsed = FALSE,
            # Reactive surrounds (inline): baseline pctile | restored pctile | cost
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center;
                       gap:12px; padding:2px 6px; font-size:14px; font-weight:bold;",
              tags$div(style = "display:flex; gap:16px; align-items:center;",
                tags$div(style = "font-weight:normal;",
                  checkboxInput("show_slr", "Display SLR projections", value = FALSE)
                ),
                htmlOutput("rap_pctile_baseline", inline = TRUE),
                htmlOutput("rap_pctile_restored", inline = TRUE),
                tags$span(style = "color:#2f4f2f; display:flex; gap:16px; align-items:center;",
                htmlOutput("model_final_cost", inline = TRUE),
                uiOutput("target_cover_warning")
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
                plotly::plotlyOutput("restoration_timeline", height = "380px")
              ),
              column(
                width = 2,
                fluidRow(style = "height:320px; width:12.5vw; display:flex; flex-direction:column; vertical-align:top; justify-content:center;",
                tags$hr(),
                  tags$style("#rt_baseline_title { text-align: center; font-size: 16px; font-weight: bold; }"),
                  textOutput("rt_baseline_title"),
                  valueBoxOutput("rt_baseline_cover", width = NULL),
                  valueBoxOutput("rt_baseline_budget", width = NULL),
                  valueBoxOutput("rt_baseline_rap", width = NULL)
                )
              ),
              column(
                width = 2,
                fluidRow(style = "height:320px; width:12.5vw; display:flex; flex-direction:column; vertical-align:top; justify-content:center;",
                  tags$hr(),
                  tags$style("#rt_restored_title { text-align: center; font-size: 16px; font-weight: bold; }"),
                  textOutput("rt_restored_title"),
                  valueBoxOutput("rt_restored_cover", width = NULL),
                  valueBoxOutput("rt_restored_budget", width = NULL),
                  valueBoxOutput("rt_restored_rap", width = NULL)
                )
              )
            )
          )
        )
      )
    ),


    # Scenario Comparison Tab ----
    # Sidebar: project (single select), scenario (multi select from saved .json),
    #          download report (.csv), per-scenario Impact Summary table
    # Main:    cost bar, ROI bar, per-scenario RAP bar
    tabItem(
      tabName = "comparison",
      fluidRow(
      # Top row:
      # Left: Scenario selection
        column(
          width = 3,
          shinydashboard::box(
            title = "Scenario Selection", width = 12,
            status = "primary", solidHeader = TRUE,
            selectInput("sc_project", tags$strong("Project name"), choices = NULL),
            checkboxGroupInput("sc_scenarios", tags$strong("Scenarios"), choices = NULL),
            tags$div(
              style = "display:flex; gap:8px; align-items:center;",
              actionButton("sc_refresh", "Refresh list", icon = icon("rotate")),
              downloadButton("sc_download_csv", "Download report")
            )
          )
        ),
        # Right: Comparison DT table
        column(
          width = 9,
          shinydashboard::box(
            title = "Comparison Table", width = 12,
            status = "info", solidHeader = TRUE,
            div(style = "overflow-x: auto;",
              DT::DTOutput("sc_compare_dt")
            )
          )
        )
      ),
      # Bottom row: Project Cost + ROI, RAP by scenario
      fluidRow(
        # Left: cost
        column(
          width = 4,
          shinydashboard::box(
            title = "Project Cost", width = 12,
            status = "info", solidHeader = TRUE,
            plotOutput("sc_cost_bar"),# height = "300px")
          )
        ),
        # Middle: ROI
        column(
          width = 4,
          shinydashboard::box(
            title = "Return on Investment", width = 12,
            status = "primary", solidHeader = TRUE,
            plotOutput("sc_roi_bar"),# height = "300px")
          )
        ),
        # Right: RAP
        column(
          width = 4,
          shinydashboard::box(
            title = "Reef Accretion Potential (RAP) by Scenario", width = 12,
            status = "success", solidHeader = TRUE,
            plotly::plotlyOutput("sc_rap_bar"), # height = "350px"),
            tags$div(style = "font-weight:normal; margin-bottom:4px;",
              checkboxInput("sc_show_slr", "Display SLR projections", value = FALSE)
            )
          )
        )
      )
    ),

    # Restoration Monitoring Tab ----
    #   Sidebar: upload coral cover, upload bioerosion, select site, download report
    #   Main:    baseline vs restored impact (cover/budget/accretion + summary),
    #            timeline of RAP over 10 yrs with SLR reference lines
    tabItem(
      tabName = "monitoring",
      fluidRow(
        # Top row:
        # Input selection (left)
        column(
          width = 3,
          shinydashboard::box(
            title = "Inputs", width = 12, status = "primary", solidHeader = TRUE,
            tags$div(
              class = "upload-label-row",
              tags$span(class = "control-label", "Coral cover .xlsx"),
              tags$div(
                style = "display:flex; gap:6px;",
                downloadButton("monitoring_cover_template_dl", "Template",
                               class = "btn-sm"),
                actionButton("cover_load_example", "Example",
                             icon = icon("upload"), class = "btn-sm")
              )
            ),
            tags$div(style = "margin-bottom: -15px;",
              fileInput("upload_cover", NULL,
                accept = c(".csv", ".xlsx")
              )
            ),
            uiOutput("monitoring_cover_uc_warning"),
            tags$div(
              class = "upload-label-row",
              tags$span(class = "control-label", "Bioerosion .xlsx"),
              tags$div(
                style = "display:flex; gap:6px;",
                downloadButton("monitoring_bioerosion_template_dl", "Template",
                               class = "btn-sm"),
                actionButton("bioerosion_load_example", "Example",
                             icon = icon("upload"), class = "btn-sm")
              )
            ),
            tags$div(style = "margin-bottom: -15px;",
              fileInput("upload_bioerosion", NULL,
                accept = c(".csv", ".xlsx")
              )
            ),

            # Red warning for unobserved parrotfish size classes
            uiOutput("bioerosion_parrotfish_warning"),
            selectizeInput("monitoring_selected_site", tags$strong("Select site"),
              choices = NULL,
              options = list(placeholder = "Select a site...")
            ),
            br(),

            # Download" + "Clear cache" buttons top-and-bottom
            tags$div(
              style = "display:flex; gap:8px; align-items:center;",
              downloadButton("monitoring_download_report", "Download report"),
              actionButton("monitoring_clear_cache", "Clear cache", icon = icon("trash"))
            )
          )
        ),
        # Main content: restoration impact (right)
        column(
          width = 9,
          # Baseline vs restored impact
          shinydashboard::box(
            title = "Baseline vs. Restored Impact", width = 12,
            status = "info", solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                tags$h4("Baseline", style = "text-align:center; font-weight:bold;"),
                valueBoxOutput("monitoring_baseline_cover", width = NULL),
                valueBoxOutput("monitoring_baseline_budget", width = NULL),
                valueBoxOutput("monitoring_baseline_rap", width = NULL)
              ),
              column(
                width = 4,
                tags$h4(textOutput("monitoring_restored_title", inline = TRUE),
                        style = "text-align:center; font-weight:bold;"),
                valueBoxOutput("monitoring_restored_cover", width = NULL),
                valueBoxOutput("monitoring_restored_budget", width = NULL),
                valueBoxOutput("monitoring_restored_rap", width = NULL)
              ),
              column(
                width = 4,
                tags$h4("Impact summary", style = "text-align:center; font-weight:bold;"),
                div(
                  class = "impact-summary-box",
                  style = "background:#f7f7f7; border:1px solid #ddd;
                           border-radius:6px; padding:12px; min-height:180px;",
                  htmlOutput("monitoring_impact_summary")
                )
              )
            )
          )
        ),

        # Bottom row: timeline
        shinydashboard::box(
          title = "Reef Accretion Potential", width = 12,
          status = "info", solidHeader = TRUE,
          tags$div(style = "font-weight:normal; margin-bottom:4px;",
            checkboxInput("monitoring_show_slr", "Display SLR projections", value = FALSE)
          ),
          plotly::plotlyOutput("monitoring_timeline", height = "350px")
        )
      )
    ),

    # Calcifier Data Tab ----
    tabItem(
      tabName = "calcifier",
      shinydashboard::box(
        title = "Per-Species Calcifier Input Data", width = 12,
        status = "primary", solidHeader = TRUE,
        tags$p("All per-species reference data feeding the growth model. ",
               "Click any column header to sort. Species without a direct ",
               "record inherit genus- or morphology-level values where applicable."),
        tags$hr(),
        DT::DTOutput("calcifier_dt")
      )
    ),

    # "About this Site" Tab ----
    tabItem(
      tabName = "about",
      shinydashboard::box(
        width = 12, status = "primary", solidHeader = FALSE,          
        tags$div(
          tags$h4(tags$strong("Aim")),
          HTML("The aim of this application is to provide a predictive tool for decision makers to assess reef restoration efforts under future climate change 
            and bleaching scenarios. The modelling approach that is used to build projections in this interactive tool is described in a forthcoming journal publication."), tags$br(),
          tags$br(),
          tags$h4(tags$strong("Background")),
          HTML("For reef framework to persist, constructional processes by corals and other calcifers need 
          to outpace loss due to physical, chemical, and biological erosion. This balance is both delicate and 
          dynamic and is currently threatened by the effects of sea-level rise, ocean warming, and ocean acidifcation.
          
          Although the protection and recovery of ecosystem functions are at the center of most restoration 
          and conservation programs, decision makers are limited by the lack of predictive tools to forecast 
          reef accretion under different emission and bleaching scenarios."),
          tags$br(),
          tags$br(),
          HTML("The Reef Persistence Tool will enable decision makers to evaluate the impact of reef restoration decisions 
                in the context of climate change and a variety of bleaching scenarios."), tags$br(),
          tags$br(),
          tags$h4(tags$strong("Code")),
          "Code and input data used to generate this Shiny app are available on ", tags$a(href = "https://github.com/laurenttoth/CarbonateBudgetRestorationTool", "Github."), tags$br(),
          tags$br(),
          tags$h4(tags$strong("Disclaimer")),
          "This software is preliminary or provisional and is subject to revision. It is being
            provided to meet the need for timely best science. The software has not received final
            approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is
            made by the USGS or the U.S. Government as to the functionality of the software and
            related material nor shall the fact of release constitute any such warranty. The
            software is provided on the condition that neither the USGS nor the U.S. Government
            shall be held liable for any damages resulting from the authorized or unauthorized
            use of the software.", tags$br(),
          tags$br(),
          column(width = 8,
            tags$h4(tags$strong("Sources")),
            "Chronic coral mortality rates: Browne et al. (2026)", tags$br(),
            "Species-specific calcification rates: Courtney et al. (2024)", tags$br(),
            "Generalized Caribbean microbioerosion rate: Perry and Lange (2019)", tags$br(),
            "Sea-level rise projections: Sea Level Rise and Coastal Flood Hazard Scenarios and Tools Interagency Task Force (2022)", tags$br(),
            tags$br(),
            "Average colony diameter: ??", tags$br(),
            "Outplant mortality rates: ??", tags$br(),
            "Assemblage-based reef porosity: ??", tags$br(),
            "2014-2024 carbonate budget surveys: ??", tags$br(),
            "Regional bioerosion rates: ??", tags$br(),
            "Species-specific bioerosion rates: ??", tags$br(),
            "Species-specific planar growth rates: ??", tags$br(),
            tags$br(),
            tags$br(),
            tags$h4(tags$strong("Authors")),
            "Connor M. Jenkins, USGS St. Petersburg Coastal and Marine Science Center", tags$br(),
            "Lauren T. Toth, USGS St. Petersburg Coastal and Marine Science Center", tags$br(),
            "John Morris, NOAA Atlantic Oceanographic and Meteorological Laboratory", tags$br(),
            "Ian Enochs, NOAA Atlantic Oceanographic and Meteorological Laboratory", tags$br(),
            tags$br(),
            tags$h4(tags$strong("Contact")),
            "Lauren Toth: ", tags$a(href = "mailto:ltoth@usgs.gov", "ltoth@usgs.gov"), tags$br(),
            tags$br(),
            tags$br(),
            tags$h4(tags$strong("Acknowledgments")),
            "A special thanks to the participants of the Carbonate Budget Tool Workshop (St. Petersburg, Florida, September 1-3, 2026),", tags$br(),
            "who provided their time and expertise to test and critique the app:", tags$br(),
            tags$br(),
            tags$strong("Subject Matter Experts:"), tags$br(),
            "Simeon Yurek, USGS Wetland and Aquatic Research Center", tags$br(),
            "Jay Grove*, NOAA Southeast Fisheries Science Center", tags$br(),
            "Alice Webb*, University of Exeter", tags$br(),
            "Chris Perry*, University of Exeter", tags$br(),
            tags$br(),
            tags$strong("Stakeholders:"), tags$br(),
            "Sara Williams, MOTE Marine Laboratory", tags$br(),
            "Jason Spadaro, MOTE Marine Laboratory", tags$br(),
            "Lucas Skay, MOTE Marine Laboratory", tags$br(),
            "Nick Alcaraz*, Florida Fish and Wildlife Conservation Commission", tags$br(),
            "Christina Mallica, Florida Fish and Wildlife Conservation Commission", tags$br(),
            "Stephanie Schopmeyer, Florida Fish and Wildlife Conservation Commission", tags$br(),
            "Andy Bruckner, NOAA Florida Keys National Marine Sanctuary", tags$br(),
            "Alexandra Fine, NOAA Florida Keys National Marine Sanctuary", tags$br(),
            "Maurizio Martinelli, Florida Department of Environmental Protection", tags$br(),
            tags$br(),
            tags$strong("Facilitators:"), tags$br(),
            "David Gonzales, U.S. Fish and Wildlife Service", tags$br(),
            "William Hall, U.S. Department of the Interior", tags$br(),
            tags$br(),
            div(style = "font-style:italic", "* virtual attendee"), tags$br(),
            tags$br(),
            "and to Dr. Alice Webb and her team, who developed the ", tags$a(href = "https://github.com/alice35/ReefPersistence_app", "original Reef Persistence Tool"),
            ", which was the inspiration for this project:", tags$br(),
            tags$br(),
            "Dr. Alice Webb, Atlantic Oceanographic and Meteorological Laboratory, Ocean Chemistry and Ecosystem Division, NOAA, USA;", tags$br(),
            tags$p("Geography, College of Life and Environmental Sciences, University of Exeter, UK", style = "text-indent: 40px"),
            "Patrick Kiel, Atlantic Oceanographic and Meteorological Laboratory, Ocean Chemistry and Ecosystem Division, NOAA, Miami, Florida, USA;", tags$br(),
            tags$p("Cooperative Institute for Marine and Atmospheric Studies, University of Miami, USA", style = "text-indent: 40px"),
            "Mike Jankulak, Atlantic Oceanographic and Meteorological Laboratory, Ocean Chemistry and Ecosystem Division, NOAA, Miami, Florida, USA;", tags$br(),
            tags$p("Cooperative Institute for Marine and Atmospheric Studies, University of Miami, USA", style = "text-indent: 40px"),
            "Dr. Ian Enochs, Atlantic Oceanographic and Meteorological Laboratory, Ocean Chemistry and Ecosystem Division, NOAA, USA", tags$br(),
            tags$br(),
            "The paper describing the original Reef Persistence Tool is published in ", tags$a(href = "https://www.nature.com/articles/s41598-022-26930-4", "Scientific Reports"), ".", tags$br(),
            tags$br(),
            tags$br(),
            tags$strong("Coral icon credit:"), tags$br(),
            HTML("Reef ocean nature diving Icon by Lima Studio on <a href='https://icon-icons.com/authors/993-lima-studio'>Icon-Icons.com</a>")
          ),
          column(width = 4,
            # Add logo panel
            # absolutePanel(
            #   id = "absPanel",
            #   top = "62%",
            #   left = "72.5%",
            #   width = "30%",
            #   fixed = TRUE,
            tags$div(style = 'display:flex; justify-content:center; align-items:center; gap:40px; margin:20px',
              fluidRow(
                tags$img(src = "fknmsLogo.png", width = "450px", height = "200px"),
                tags$div(style = 'margin: 0px 10px 10px 10px',
                  tags$img(src = "moteLogo.png", width = "200px", height = "180px"),
                  tags$img(src = "noaaLogo.png", width = "200px", height = "200px")
                ),
                tags$div(style = 'margin-bottom: 20px',
                  tags$img(src = "fdepLogo.png", width = "220px", height = "200px"),
                  tags$img(src = "fwcLogo.png", width = "200px", height = "220px")
                ),
                tags$img(src = "usgsLogo.png", width = "450px", height = "150px")
              )
            )
          )
        )
      )
    )
  ),

  # ---- Right-side collapsible log panel (available on all tabs) ----
  tags$div(class = "log-toggle-tab",
    onclick = "document.getElementById('log_panel').classList.toggle('open');",
    "Log"
  ),
  tags$div(id = "log_panel", class = "log-panel",
    tags$div(class = "log-panel-header",
      tags$span("Console Log"),
      tags$div(
        actionButton("log_clear", "Clear", class = "btn-xs"),
        tags$span(style = "cursor:pointer; margin-left:8px;",
          onclick = "document.getElementById('log_panel').classList.remove('open');",
          icon("xmark"))
      )
    ),
    tags$div(class = "log-panel-body",
      uiOutput("log_panel_content")
    )
  )
)

ui <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body,
)

# Shiny Server ----
server <- function(input, output, session) {
  # define reactVal to store coordinates
  reef_name       <- reactiveVal()
  reef_year       <- reactiveVal(2019)
  initial_budget  <- reactiveVal(NULL)

  # ---- Log panel: drain the global buffer into a reactiveVal for display ----
  # A short poller watches the global version counter; when it changes, the
  # displayed lines refresh. This bridges the non-reactive global writer (usable
  # from top-level functions) to the reactive UI.
  log_lines <- reactiveVal(character(0))
  log_seen  <- reactiveVal(-1L)

  observe({
    invalidateLater(400, session)
    v <- .LOG_ENV$version
    if (!identical(isolate(log_seen()), v)) {
      log_seen(v)
      log_lines(.LOG_ENV$lines)
    }
  })

  observeEvent(input$log_clear, {
    log_clear_buffer()
    log_lines(character(0))
  })

  output$log_panel_content <- renderUI({
    lines <- log_lines()
    if (length(lines) == 0) {
      return(tags$em(style = "color:#888;", "No messages yet."))
    }
    tags$pre(
      style = "white-space: pre-wrap; word-break: break-word; font-size: 11px; margin: 0;",
      paste(lines, collapse = "\n")
    )
  })

  # Rebuild the calcifier table when subregion is changed to update the reported DHW slopes.
  observeEvent(input$subregion_choice, {
    # Calcifier Data tab: sortable reference table.
    calcifier_table <- build_calcifier_table(input$subregion_choice)

    output$calcifier_dt <- DT::renderDT({
      DT::datatable(
        calcifier_table,
        extensions = "FixedHeader",
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "60vh",
          scrollCollapse = TRUE,
          paging = FALSE,
          order = list(list(0, "asc"))
        )
      )
    })
  })

  # Condition-capture wrapper for reactives (errors/warnings -> log).
  with_logged_conditions <- function(expr) {
    withCallingHandlers(
      tryCatch(expr,
        error = function(e) { log_msg(conditionMessage(e)); NULL }),
      warning = function(w) {
        log_msg(conditionMessage(w)); invokeRestart("muffleWarning")
      }
    )
  }

  # Manual/reactive simulation gate ----
  # `sim_token` is the single dependency the projection reactives take. In
  # reactive mode it bumps on any relevant input change; in manual mode it
  # bumps only when "Run simulation" or "Save scenario" is clicked.
  # Projection reactives isolate() their real inputs so nothing
  # recomputes until the token advances.
  sim_token <- reactiveVal(0)

  # Set TRUE when the Site changes so the previous run's model_result() is
  # treated as stale (NULL) until the next explicit Simulate. Cleared when a
  # run advances sim_token.
  model_stale <- reactiveVal(FALSE)

  # Inputs that should trigger a recompute when in reactive mode. Listing them
  # explicitly (rather than depending on `input`) keeps the token from bumping
  # on unrelated UI (map controls, monitoring tab, etc.).
  observe({
    # Depend on every simulation-relevant input:
    input$site_area_m2
    input$subregion_choice
    input$habitat_choice
    input$rest_horizon
    input$sim_duration
    input$dhw
    input$bleach_events
    baseline_species_list()
    # Baseline covers + restoration-mix targets (dynamic ids)
    for (s in baseline_species_list()) {
      # s <- gsub("[^A-Za-z0-9]", "_", s)
      # # If last element is an underscore, replace it with "." to restore "spp_" to "spp."
      # if (substr(s, -1, -1) == "_") {
      #   n <- nchar(s)
      #   s <- str_sub(s, end = n - 1)
      #   s <- paste0(s, ".")
      # }
      # log_msg(" Input:", paste0("base_", gsub("[^A-Za-z0-9]", "_", s)))
      input[[paste0("base_", gsub("[^A-Za-z0-9]", "_", s))]]
    }
    for (s in mix_species()) {
      s_ <- gsub("[^A-Za-z0-9]", "_", s)
      input[[paste0("target_", s_)]]
      input[[paste0("diam_",   s_)]]
      input[[paste0("cost_",   s_)]]
      input[[paste0("count_",  s_)]]
    }
    input$base_REQUIRED_Unconsolidated_substrate
    input$base_REQUIRED_Other_living_benthos

    # Only advance automatically when reactive mode is ON.
    if (isTRUE(input$reactive_sim)) {
      model_stale(FALSE)
      sim_token(isolate(sim_token()) + 1)
    }
  })

  # Manual run: advance the token on click (works regardless of mode, but the
  # button is disabled while reactive mode is on).
  observeEvent(input$run_sim, {
    model_stale(FALSE)
    sim_token(sim_token() + 1)
    write_cached_baseline()
  })

  # Disable the Run button while reactive simulation is enabled.
  observe({
    if (isTRUE(input$reactive_sim)) {
      shinyjs::disable("run_sim")
    } else {
      shinyjs::enable("run_sim")
    }
  })

  .req_num <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) FALSE else as.numeric(x)
  }
  .safe_num <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) 0 else as.numeric(x)
  }
  .safe_num_chr <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
  }

  # Outplanting-tab readiness gate ----
  # Baseline/restoration projections require a nonzero unconsolidated-substrate
  # value AND at least one coral species with cover > 0. Returns TRUE only when
  # both hold, so reactives can req() on it and value boxes / the timeline show
  # a neutral placeholder instead of erroring while inputs are incomplete.
  outplanting_ready <- reactive({
    # UC and OLB must be a valid number >= 0 (0 allowed). NA/blank blocks the sim.
    uc <- .req_num(input$base_REQUIRED_Unconsolidated_substrate)
    if (isFALSE(uc)) {
      showNotification("Provde a value of at least 0 for 'Unconsolidated substrate'.", type = "error")
      return(FALSE)
    }
    olb <- .req_num(input$base_REQUIRED_Other_living_benthos)
    if (isFALSE(olb)) {
      showNotification("Provde a value of at least 0 for 'Other living benthos'.", type = "error")
      return(FALSE)
    }
    sp <- setdiff(baseline_species_list(), c(UC_TAXON, OLB_TAXON))
    if (length(sp) == 0) {
      showNotification("Submit at least one species to simulate.", type = "error")
      return(FALSE)
    }
    covers <- vapply(sp, function(s) {
      .safe_num(input[[paste0("base_", gsub("[^A-Za-z0-9]", "_", s))]])
    }, numeric(1))
    any(covers > 0)
  })

  # Dark Mode: toggle the body CSS class from the switch ----
  observeEvent(input$dark_mode, {
    session$sendCustomMessage("toggle_dark", isTRUE(input$dark_mode))
  }, ignoreInit = FALSE)

  # Point size state, adjusted by the +/- stepper (clamped 2-15)
  point_size <- reactiveVal(4)

  observeEvent(input$point_size_up, {
    point_size(min(point_size() + 1, 15))
  })
  observeEvent(input$point_size_down, {
    point_size(max(point_size() - 1, 2))
  })


  output$point_size_label <- renderText({
    point_size()
  })

  filtered_df <- reactive({
    df |>
      filter(site_id == input$selected_site)
  })

  # Sim-duration floor: snap any selection below 10 back to 10. The slider
  # shares the horizon's 0-30 domain (so ticks align natively), but a duration
  # under 10 years is not allowed.
  observeEvent(input$sim_duration, {
    if (.safe_num(input$sim_duration) < 10) {
      updateSliderInput(session, "sim_duration", value = 10)
    }
  }, ignoreInit = TRUE)

  ## Home map: filtered + restoration-projected data ----
  # Applies the Year/Habitat checkbox filters and computes restored_rap +
  # halo classification from the target percent-cover-increase slider.
  map_data_reactive <- reactive({
    d <- df

    # Year / Habitat filters (checkbox groups); empty selection => no sites
    yr_sel  <- input$filter_year
    hab_sel <- input$filter_habitat
    if (is.null(yr_sel))  yr_sel  <- character(0)
    if (is.null(hab_sel)) hab_sel <- character(0)
    d <- d[d$YEAR %in% yr_sel & d$HABITAT_TYPE %in% hab_sel, , drop = FALSE]
    if (nrow(d) == 0) return(d)

    # Projected RAP from the target cover increase, via the regression slope
    inc <- if (is.null(input$target_cover_increase)) 0 else input$target_cover_increase
    d$restored_rap <- d$rap + cover_rap_slope * inc
    d$restored_state <- ifelse(d$restored_rap > 0.5, "growth", ifelse(d$restored_rap < -0.5, "erosion", "stasis"))

    # Halo / fill classification (only meaningful when inc > 0)
    classify <- function(rap) {
        if (rap > 0.5) {
          "growth"
        } else if (rap < -0.5) {
          "erosion"
        } else {
          "stasis"
        }
    }
    d$halo <- mapply(classify, d$restored_rap)
    d
  })

  # Initialize leaflet map ----
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles(providers$Esri.WorldImagery,
        options = providerTileOptions(attribution = 'Map data &copy; <a href="https://www.esri.com/">Esri</a>')
      ) |>
      setView(lng = -81, lat = 25.5, zoom = 8) |>

      # Region polygons, pastel fill at 75% transparency
      addPolygons(
        data        = regions_sf,
        fillColor   = ~ region_pal(Region),
        fillOpacity = 0.45,
        color       = "white",
        weight      = 1,
        opacity     = 0.8,
        label       = ~Region
      ) |>

      # Dedicated low pane for the named-reef polygons so they sit above the
      # basemap/regions but BELOW the clickable site markers (default ~600).
      addMapPane("named_reefs_pane", zIndex = 410)
  })

  # Add / update NCRMP markers, halos, and legend ----
  observe({
    d <- map_data_reactive()
    field <- input$symbolize_by
    inc <- if (is.null(input$target_cover_increase)) 0 else input$target_cover_increase

    proxy <- leafletProxy("mymap") |>
      clearGroup("ncrmp") |>
      clearGroup("halo") |>
      clearControls()

    if (is.null(d) || nrow(d) == 0) {
      return(proxy)
    }

    # Choose fill color + legend per the selected symbolize-by field
    if (field == "current_state") {
      fill_cols <- num_pal_state(as.character(d$current_state))
    } else if (field == "rap") {
      fill_cols <- num_pal(d$rap)
    } else {
      pal <- switch(field,
        "grossE_G" = pal_gross
      )
      fill_cols <- pal(pmax(0, d[[field]]))
    }

    # In RAP mode, reduce "No Return" sites to 25% opacity
    # fill_opacity <- rep(0.85, nrow(d))
    # if (field == "rap" && inc > 0) {
    #   erod_idx <- which(d$halo == "erosion")
    #   fill_cols[erod_idx] <- "erosion"
    #   fill_opacity[erod_idx] <- 0.25
    # }

    # Draw halos first (underneath) when slider is active
    if (inc > 0) {
      halo_cols <- c("growth"  = "#002fff",
                     "stasis"  = "#ffff6d",
                     "erosion" = "#ff5500"
      )
      hd <- d[d$halo %in% names(halo_cols), , drop = FALSE]
      if (nrow(hd) > 0) {
        proxy <- proxy |>
          addCircleMarkers(
            data = hd,
            lng = ~LON_DEGREES, lat = ~LAT_DEGREES,
            radius = point_size() + 3,
            weight = 0,
            fillColor = unname(halo_cols[hd$halo]),
            fillOpacity = 0.9,
            stroke = FALSE,
            group = "halo"
          )
      }
    }

    # Main site markers
    proxy <- proxy |>
      addCircleMarkers(
        data = d,
        lng = ~LON_DEGREES, lat = ~LAT_DEGREES,
        radius = point_size(),
        weight = 1,
        color = "black",
        fillColor = fill_cols,
        fillOpacity = 100,
        stroke = TRUE,
        group = "ncrmp",
        layerId = ~site_id,
        popup = ~ paste0(
          "<span style='font-size: 20px; color: black;'>NCRMP Site: ", site_id, "</span>",
          "<table style='font-size: 14px; border-collapse: collapse; margin-top: 6px;'>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Habitat:</td>",
          "<td style='padding: 2px 0;'>", HABITAT_TYPE, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Survey year:</td>",
          "<td style='padding: 2px 0;'>", YEAR, "</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current coral cover:</td>",
          "<td style='padding: 2px 0;'>", round(hardCoral_PrctCvr, 1), "%</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Parrotfish bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(parrotfish_G, 2), "kg CaCO₃/m²/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Gross bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(grossE_G, 2), "kg CaCO₃/m²/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current RAP:</td>",
          "<td style='padding: 2px 0;'>", round(rap, 2), " mm/yr (", current_state, ")</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>RAP with restoration:</td>",
          "<td style='padding: 2px 0;'>", round(restored_rap, 2), " mm/yr (", restored_state, ")</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Water depth:</td>",
          "<td style='padding: 2px 0;'>", round(AVG_DEPTH, 1), " m</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Coordinates:</td>",
          "<td style='padding: 2px 0;'>", round(LON_DEGREES, 5), ", ", round(LAT_DEGREES, 5), "</td></tr>",
          "</table>"
        )
      )

    # Legend for Restoration Potential
    rest_colors = c("growth"  = "#002fff",
                    "stasis"  = "#ffff6d",
                    "erosion" = "#ff5500")

    proxy <- proxy |>
      addLegend("bottomleft",
        colors = unname(rest_colors),
        labels = names(rest_colors),
        title = HTML("Restored<br/>Reef Status"),
        opacity = 1
      )

    # Legend for the selected symbolize-by field
    if (field == "current_state") {
      proxy <- proxy |>
        addLegend("bottomleft",
          colors = unname(state_colors), labels = names(state_colors),
          title = HTML("Current<br/>Reef Status"), opacity = 1
        )
    } else if (field == "rap") {
      proxy <- proxy |>
        addLegendNumeric(
          pal = num_pal_rev,
          title = HTML("Reef<br/>accretion<br/>potential<br/>(mm/yr)"),
          shape = "stadium", values = at,
          fillOpacity = 1, decreasing = TRUE,
          position = "bottomleft"
        )
    } else {
      pal <- switch(field,
        "grossE_G" = pal_gross_rev
      )
      ttl <- switch(field,
        "grossE_G" = "Gross<br/>bioerosion<br/>(kg CaCO\u00b3/m²/yr)"
      )
      proxy <- proxy |>
        addLegend("bottomleft",
          pal = pal,
          values = c(0, max(df[[field]], na.rm = TRUE)),
          title = HTML(ttl), opacity = 1,
          labFormat = labelFormat(digits = 1,
                                  transform = function(x) sort(x, decreasing = TRUE))
        )
    }

    proxy
  })

  # ---- Baseline-upload site markers ----
  # Drawn in their own "baseline" group so they don't churn with the NCRMP
  # redraw. 50% larger than NCRMP points, on top; the selected site is a further
  # 25% larger and carries a restored-RAP halo when a model result exists.
  observe({
    bs <- baseline_map_sites()
    field <- input$symbolize_by
    sel_sid <- input$baseline_site

    proxy <- leafletProxy("mymap") |>
      clearGroup("baseline") |>
      clearGroup("baseline_halo")

    if (is.null(bs) || nrow(bs) == 0) return(proxy)

    # Restored RAP: active site uses the model's duration value; others default
    # to their baseline RAP (set in baseline_map_sites).
    sel_row <- which(bs$Unique_Site_ID == sel_sid)
    if (length(sel_row) == 1) {
      mr <- model_result()
      if (!is.null(mr) && nrow(mr$budget_df) > 0) {
        duration <- .safe_num(input$sim_duration)
        dr <- min(duration + 1, nrow(mr$budget_df))
        bs$restored_rap[sel_row] <- mr$budget_df$RAP_total[dr]
      }
    }
    bs$restored_state <- ifelse(bs$restored_rap > 0.5, "growth",
                         ifelse(bs$restored_rap < -0.5, "erosion", "stasis"))

    base_radius <- point_size() * 1.5           # 50% larger than NCRMP
    radii <- rep(base_radius, nrow(bs))
    sel_idx <- which(bs$Unique_Site_ID == sel_sid)
    if (length(sel_idx)) radii[sel_idx] <- base_radius * 1.25  # +25% for selected

    # Fill color: RAP or status only (no bioerosion option for these points)
    if (field == "current_state") {
      fill_cols <- num_pal_state(as.character(bs$state))
    } else {
      # default + rap both use the RAP palette
      fill_cols <- num_pal(bs$rap)
    }

    # Restored-RAP halo on the selected site (only if a model result exists)
    if (length(sel_idx) == 1) {
      mr <- model_result()
      if (!is.null(mr) && nrow(mr$budget_df) > 0) {
        horizon <- .safe_num(input$rest_horizon)
        hr <- min(horizon + 1, nrow(mr$budget_df))
        base_rap     <- bs$rap[sel_idx]
        restored_rap <- mr$budget_df$RAP_total[hr]
        halo_palette <- c("growth"  = "#002fff",
                          "stasis"  = "#ffff6d",
                          "erosion" = "#ff5500")
        halo_key <- if (restored_rap >= 0.5) {
          "growth"
        } else if (restored_rap <= -0.5) {
          "erosion"
        } else {
          "stasis"
        }
        halo_col <- unname(halo_palette[halo_key])
        proxy <- proxy |>
          addCircleMarkers(
            data = bs[sel_idx, , drop = FALSE],
            lng = ~lon, lat = ~lat,
            radius = radii[sel_idx] + 4,
            weight = 0, fillColor = halo_col, fillOpacity = 0.9,
            stroke = FALSE, group = "baseline_halo"
          )
      }
    }

    proxy |>
      addCircleMarkers(
        data = bs,
        lng = ~lon, lat = ~lat,
        radius = radii,
        weight = 2, color = "black",
        fillColor = fill_cols, fillOpacity = 0.95,
        stroke = TRUE, group = "baseline",
        layerId = ~paste0("baseline_", Unique_Site_ID),
        label = ~Unique_Site_ID,
        popup = ~paste0(
          "<span style='font-size: 20px; color: black;'>Baseline Site: ", Unique_Site_ID, "</span>",
          "<table style='font-size: 14px; border-collapse: collapse; margin-top: 6px;'>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current coral cover:</td>",
          "<td style='padding: 2px 0;'>", round(cover, 1), "%</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Parrotfish bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(pfish, 2), " kg CaCO\u00b3/m²/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Gross bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(gross_be, 2), " kg CaCO\u00b3/m²/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Current RAP:</td>",
          "<td style='padding: 2px 0;'>", round(rap, 2), " mm/yr (", state, ")</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>RAP with restoration:</td>",
          "<td style='padding: 2px 0;'>", round(restored_rap, 2), " mm/yr (", restored_state, ")</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Coordinates:</td>",
          "<td style='padding: 2px 0;'>", round(lon, 5), ", ", round(lat, 5), "</td></tr>",
          "</table>"
        )
      )
  })

  # ---- Named-reef labeled polygons ----
  # each permanently labeled by its "Location" attribute (white text, black
  # outline). Toggled by the "Show named reefs" checkbox; own group so it never
  # churns with the NCRMP / baseline redraws.
  observe({
    proxy <- leafletProxy("mymap") |>
      clearGroup("named_reefs")

    if (!isTRUE(input$show_named_reefs) || is.null(named_reefs_sf) ||
        nrow(named_reefs_sf) == 0) {
      return(proxy)
    }

    labs <- as.character(named_reefs_sf$Location)
    labs_fknms <- as.character(named_reefs_sf_fknms$Reef_Name)

    proxy |>
      addPolygons(
        data = named_reefs_sf,
        weight = 1, color = "#add8e6",
        fillColor = "#add8e6", fillOpacity = 0.3,
        stroke = TRUE, opacity = 0.6,
        group = "named_reefs",
        options = pathOptions(pane = "named_reefs_pane"),
        label = labs,
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textOnly = TRUE,
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "font-size" = "12px",
            "text-shadow" =
              "-1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000"
          )
        )
      ) |>
      addMarkers(
        data = named_reefs_sf_fknms,
        icon = coral_icon,
        popup = ~paste0(Reef_Name, "<br/>Designation: ", Sanctuary_),
        group = "named_reefs",
        options = pathOptions(pane = "named_reefs_pane"),
        label = labs_fknms,
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textOnly = TRUE, offset = c(0, -20),
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "font-size" = "12px",
            "text-shadow" =
              "-1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000"
          )
        )
      )
  })

  # Capture the selected reef from a marker click. The clicked site's id is
  # stored and (re)wired to the Monitoring "Select site" dropdown so the map and
  # dropdown stay in agreement (the Monitoring "Restored" panel keys off it when
  # no coral-cover file is uploaded).
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    sid <- click$id
    if (is.null(sid)) {
      sid <- df |>
        filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
        pull(site_id) |>
        unique()
    }
    reef_name(sid)

    # Keep map + monitoring dropdown in sync (only when no upload overrides it)
    if (is.null(uploaded_monitoring_cover())) {
      updateSelectizeInput(session, "monitoring_selected_site", selected = sid)
    }
  })

  ## ---------------------------------------------------------------------------
  ## Baseline cover ----
  ## dynamic per-species inputs + upload auto-populate + add-species dropdown
  ## ---------------------------------------------------------------------------

  # Full uploaded "Coral Cover input" sheet (all sites)
  baseline_upload_data <- reactiveVal(NULL)
  # Holds Taxon -> Percent_Cover for the CURRENTLY SELECTED site
  uploaded_covers <- reactiveVal(NULL)
  # The active list of baseline species (auto-populated + manually added)
  baseline_species_list <- reactiveVal(character(0))
  # Current carbonate budget computed at ingest time (for Year-0 pip / popup)
  ingested_current_budget <- reactiveVal(NULL)
  # Baseline RAP percentile computed at ingest (gray surround on the graph)
  ingested_baseline_pctile <- reactiveVal(NULL)

  # Per-Unique_Site_ID baseline points for the map. One row per site with
  # coordinates, bioerosion-inclusive baseline RAP/status, plus popup fields.
  baseline_map_sites <- reactive({
    up <- baseline_upload_data()
    if (is.null(up) || !all(c("Unique_Site_ID", "Latitude", "Longitude") %in% names(up))) {
      return(NULL)
    }
    ids <- unique(as.character(up$Unique_Site_ID[!is.na(up$Unique_Site_ID)]))
    if (length(ids) == 0) return(NULL)

    rows <- lapply(ids, function(sid) {
      sr <- up[as.character(up$Unique_Site_ID) == sid, , drop = FALSE]
      lat <- suppressWarnings(as.numeric(sr$Latitude[!is.na(sr$Latitude)][1]))
      lon <- suppressWarnings(as.numeric(sr$Longitude[!is.na(sr$Longitude)][1]))
      # Don't include sites with missing or zero coordinates for mapping
      if (is.na(lat) || is.na(lon) || lat == 0 || lon == 0) return(NULL)

      area_val <- if ("Site_Area_m2" %in% names(sr) && any(!is.na(sr$Site_Area_m2))) {
        .safe_num(sr$Site_Area_m2[!is.na(sr$Site_Area_m2)][1])
      } else {
        100
      }
      if (area_val <= 0) area_val <- 100

      # Per-site subregion / habitat for bioerosion resolution. Subregion may be
      # a full label or a code in the file; map to the code the data files use.
      raw_sub <- if ("Subregion" %in% names(sr) && any(!is.na(sr$Subregion))) {
        as.character(sr$Subregion[!is.na(sr$Subregion)][1])
      } else {
        NA_character_
      }
      # Data files use full names; expand a code to its label if one slips in.
      sub_full <- if (!is.na(raw_sub) && raw_sub %in% names(subregion_labels)) {
        unname(subregion_labels[raw_sub])
      } else {
        raw_sub
      }
      habitat <- if ("Habitat" %in% names(sr) && any(!is.na(sr$Habitat))) {
        as.character(sr$Habitat[!is.na(sr$Habitat)][1])
      } else {
        NA_character_
      }

      # Bioerosion terms (regional). parrotfish split out for the popup.
      be_split <- if (!is.na(sub_full) && !is.na(habitat)) {
        resolve_species_bioerosion(sub_full, habitat)
      } else {
        list(parrotfish = 0, urchin = 0, sponge = 0)
      }
      be_macro <- .safe0(be_split$parrotfish) + .safe0(be_split$urchin) + .safe0(be_split$sponge)

      # Unconsolidated substrate % from the special Taxon row (excluded from cover)
      uc_pct <- 0
      if (all(c("Taxon", "Percent_Cover") %in% names(sr))) {
        uc_row <- sr$Percent_Cover[sr$Taxon == "REQUIRED Unconsolidated substrate"]
        if (length(uc_row) && !is.na(uc_row[1])) uc_pct <- suppressWarnings(as.numeric(uc_row[1]))
      }

      # Area-occupied gross budget, total cover (skip the UC pseudo-taxon)
      patch <- 0
      total_cover <- 0
      if (all(c("Taxon", "Percent_Cover") %in% names(sr))) {
        for (i in seq_len(nrow(sr))) {
          s <- sr$Taxon[i]
          if (identical(s, "REQUIRED Unconsolidated substrate")) next
          cvr <- suppressWarnings(as.numeric(sr$Percent_Cover[i]))
          if (is.na(cvr)) next
          rate <- calc_rates$rate[calc_rates$Taxon == s]
          if (length(rate) == 0 || is.na(rate[1])) next
          patch <- patch + area_val * (cvr / 100) * rate[1]
          # Use CCA as a calcifying patch, but not for total cover summation.
          if (identical(s, CCA_TAXON)) next
          total_cover <- total_cover + cvr
        }
      }
      gross_budget <- patch / area_val

      # Microbioerosion on consolidated substrate (UC now read from the file)
      consol_area <- area_val - (area_val * uc_pct / 100) - (area_val * total_cover / 100)
      be_micro <- if (consol_area > 0) (consol_area / area_val) * be_micro_rate else 0

      net_budget <- gross_budget - be_micro - be_macro
      rap <- net_budget / 2.9 / (1 - 0.6265)
      state <- if (rap > 0.5) "growth" else if (rap < -0.5) "erosion" else "stasis"

      # Gross bioerosion figure for popup (whole-patch kg/yr, to echo NCRMP)
      gross_be <- (be_micro + be_macro)
      pfish    <- .safe0(be_split$parrotfish)

      data.frame(
        Unique_Site_ID = sid, lat = lat, lon = lon,
        rap = rap, state = state, restored_rap = rap,  # default: baseline
        cover = total_cover, pfish = pfish, gross_be = gross_be,
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  })

  # Habitat choices ----
  # respond to changes in the selected subregion
  observeEvent(input$subregion_choice, {
    up <- FALSE
    hab_val <- FALSE
    up <- baseline_upload_data()

    # If baseline data has been uploaded, automatically assign the habitat for the record.
    if (!isFALSE(up)) {
      site_rows <- up[as.character(up$Unique_Site_ID) == input$baseline_site, , drop = FALSE]
      if (!is.null(site_rows)) { # (nrow(site_rows) > 0) {
        if ("Habitat" %in% names(site_rows) && any(!is.na(site_rows$Habitat))) {
          hab_val <- as.character(site_rows$Habitat[!is.na(site_rows$Habitat)][1])
        }
      }
    }

    hab <- switch(input$subregion_choice,
      "UpperKeys"        = c("Inshore", "Offshore", "MidChannel"),
      "MiddleKeys"       = c("Inshore", "Offshore", "MidChannel"),
      "LowerKeys"        = c("Inshore", "Offshore", "MidChannel"),
      "DryTortugas"      = c("Bank", "Forereef", "Lagoon"),
      "Biscayne"         = c("Inshore", "Offshore", "MidChannel"),
      "SoutheastFlorida" = c("SEFCRI"),
      character(0)
    )
    updateSelectInput(session, "habitat_choice",
      choices = c("\u2013 Select habitat \u2013" = "", hab),
      selected = if (!isFALSE(hab_val)) hab_val else ""
    )
  }, ignoreInit = TRUE)

  # Shared baseline-ingest routine ----
  # Reused by both the fileInput upload and the cached-file auto-load so the
  # two paths behave identically. `path` points to an .xlsx on disk.
  ingest_baseline_file <- function(path) {
    up <- tryCatch(
      read_excel_quiet(path, sheet = "Coral Cover input"),
      error = function(e) {
        showNotification(paste("Could not read sheet:", e$message), type = "error")
        NULL
      }
    )
    if (is.null(up)) return(invisible(NULL))

    baseline_upload_data(up)

    # Populate the Site dropdown from Unique_Site_ID
    if ("Unique_Site_ID" %in% names(up)) {
      site_ids <- unique(as.character(up$Unique_Site_ID[!is.na(up$Unique_Site_ID)]))
      selectize_choices <- c("\u2013 Select site \u2013" = "", site_ids)
      updateSelectizeInput(session, "baseline_site",
        choices = selectize_choices,
        selected = selectize_choices[1]
      )
      # Refresh site selectize to trigger the site-change observer immediately (doesn't seem to work...)
      if (length(site_ids)) later::later(function() {
        updateSelectizeInput(session, "baseline_site", selected = site_ids[1])
      }, delay = 0.5)
    } else {
      showNotification("Upload has no 'Unique_Site_ID' column.", type = "warning")
    }
    invisible(TRUE)
  }

  # Baseline cover: load from uploaded .xlsx (Coral Cover input sheet).
  # Also caches a copy so it auto-loads on the next launch.
  observeEvent(input$baseline_upload, {
    req(input$baseline_upload)
    ingest_baseline_file(input$baseline_upload$datapath)
    # Cache a copy for next launch
    tryCatch(
      file.copy(input$baseline_upload$datapath, cached_baseline_path, overwrite = TRUE),
      error = function(e) NULL
    )
  })

  # load the cached baseline data
  # Baseline percentile still does not populate on the timeline automatically on first click...
  # but does populate on second "Cache" upload click. ?
  observeEvent(input$baseline_load_cache, {
    if (!file.exists(cached_baseline_path)) {
      showNotification("No cached baseline file to load.", type = "warning")
      return(invisible(NULL))
    }
    ingest_baseline_file(cached_baseline_path)
    # Force the site-change observer to fire even if the resolved site id equals
    # the currently-selected one (ingest sets the dropdown but an identical id
    # won't retrigger the observer, so params/covers wouldn't push).
    up <- baseline_upload_data()
    if (!is.null(up) && "Unique_Site_ID" %in% names(up)) {
      ids <- unique(as.character(up$Unique_Site_ID[!is.na(up$Unique_Site_ID)]))
      if (length(ids)) {
        updateSelectizeInput(session, "baseline_site", selected = "")
        later::later(function() {
          updateSelectizeInput(session, "baseline_site", selected = ids[1])
        }, delay = 0.3)
      }
    }
    showNotification("Loaded cached baseline.", type = "message")
  })

  # Load the bundled example baseline (.xlsx) via the same ingest + cache path.
  observeEvent(input$baseline_load_example, {
    ex <- here("www", "Baseline_Cover_EXAMPLE.xlsx")
    if (!file.exists(ex)) {
      showNotification("Baseline_Cover_EXAMPLE.xlsx not found in /www.", type = "error")
      return(invisible(NULL))
    }
    ingest_baseline_file(ex)
    tryCatch(file.copy(ex, cached_baseline_path, overwrite = TRUE),
             error = function(e) NULL)
  })

  # Load the bundled Monitoring example files by copying them into the cache
  # slots the monitoring reactives already read from, then bumping the token.
  observeEvent(input$cover_load_example, {
    ex <- here("www", "Restoration_Monitoring_Cover_EXAMPLE.xlsx")
    if (!file.exists(ex)) {
      showNotification("Restoration_Monitoring_Cover_EXAMPLE.xlsx not found in /www.", type = "error")
      return(invisible(NULL))
    }
    tryCatch(file.copy(ex, paste0(cached_cover_stub, ".xlsx"), overwrite = TRUE),
             error = function(e) NULL)
    monitoring_cache_token(monitoring_cache_token() + 1)
    showNotification("Loaded example coral-cover data.", type = "message")
  })

  observeEvent(input$bioerosion_load_example, {
    ex <- here("www", "Bioerosion_Data_EXAMPLE.xlsx")
    if (!file.exists(ex)) {
      showNotification("Bioerosion_Data_EXAMPLE.xlsx not found in /www.", type = "error")
      return(invisible(NULL))
    }
    tryCatch(file.copy(ex, paste0(cached_bioerosion_stub, ".xlsx"), overwrite = TRUE),
             error = function(e) NULL)
    monitoring_cache_token(monitoring_cache_token() + 1)
    showNotification("Loaded example bioerosion data.", type = "message")
  })

  # On launch: if a cached baseline exists, auto-load it via the same path.
  # observe({
  #   if (file.exists(cached_baseline_path)) {
  #     if (is.null(input$baseline_upload$datapath)) ingest_baseline_file(cached_baseline_path)
  #     }
  # })

  # Download the blank baseline-cover template (.xlsx) from www resource folder
  output$baseline_template_dl <- downloadHandler(
    filename = function() "Baseline_Cover_TEMPLATE.xlsx",
    content = function(file) {
      file.copy(here("www", "Baseline_Cover_TEMPLATE.xlsx"), file, overwrite = TRUE)
    }
  )

  # Delete the cached baseline-cover file.
  observeEvent(input$baseline_delete_cache, {
    if (file.exists(cached_baseline_path)) {
      ok <- isTRUE(file.remove(cached_baseline_path))
      showNotification(
        if (ok) "Deleted cached baseline file." else "Could not delete cached baseline file.",
        type = if (ok) "message" else "error"
      )
    } else {
      showNotification("No cached baseline file to delete.", type = "warning")
    }
  })

  # Build the current Restoration Scenario as a one-sheet baseline data.frame.
  # Per-Taxon columns: Target_Percent_Cover, Outplant_Size_cm, Outplant_Cost.
  # Scalar sim params repeated on every row. Shared by the download handler and
  # the cache writer (Simulate / Save scenario / Save baseline).
  build_baseline_df <- function() {
    sp <- setdiff(baseline_species_list(), c(UC_TAXON, OLB_TAXON))
    if (length(sp) == 0) return(NULL)
    extra_years <- additional_outplant_years()

    getn <- function(prefix, s) {
      v <- input[[paste0(prefix, "_", gsub("[^A-Za-z0-9]", "_", s))]]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
    }
    sub_lbl  <- input$subregion_choice
    sub_full <- if (sub_lbl %in% names(subregion_labels)) unname(subregion_labels[sub_lbl]) else sub_lbl
    site_id  <- if (nzchar(.safe_num_chr(input$baseline_site))) input$baseline_site else "SITE_1"

    mk_row <- function(taxon, year, pct, tgt, diam, cost, count, opc) {
      data.frame(
        Unique_Site_ID       = site_id,
        Subregion            = sub_full,
        Habitat              = input$habitat_choice,
        Site_Area_m2         = .safe_num(input$site_area_m2),
        Latitude             = .safe_num(input$site_latitude),
        Longitude            = .safe_num(input$site_longitude),
        Taxon                = taxon,
        Year                 = year,
        Percent_Cover        = pct,
        Target_Percent_Cover = tgt,
        Outplant_Size_cm     = diam,
        Outplant_Cost        = cost,
        Outplant_Count       = count,
        Outplants_Per_Cluster = opc,
        Sim_Duration         = .safe_num(input$sim_duration),
        Rest_Horizon         = .safe_num(input$rest_horizon),
        DHW                  = .safe_num(input$dhw),
        Bleach_Events        = .safe_num(input$bleach_events),
        stringsAsFactors = FALSE
      )
    }

    rows <- list()
    for (s in sp) {
      cvr <- getn("base",   s)
      tgt <- getn("target", s)
      dia <- getn("diam",   s)
      cst <- getn("cost",   s)
      cnt <- getn("count",  s)
      opc <- getn("opc",    s)
      rows[[length(rows) + 1]] <- mk_row(
        s, 0, if (is.na(cvr)) 0 else cvr, tgt, dia, cst, cnt, opc
      )
      restoring <- (!is.na(tgt) && !is.na(cvr) && tgt > cvr) || (!is.na(cnt) && cnt > 0)
      if (length(extra_years) > 0 && restoring) {
        s_ <- gsub("[^A-Za-z0-9]", "_", s)
        for (yr in extra_years) {
          gid <- function(p) {
            v <- input[[paste0(p, "_", s_, "_y", yr)]]
            if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
          }
          nv <- gid("count")
          if (is.na(nv) || nv <= 0) next
          rows[[length(rows) + 1]] <- mk_row(
            s, yr, NA_real_, NA_real_, gid("diam"), gid("cost"), nv, gid("opc")
          )
        }
      }
    }

    rows[[length(rows) + 1]] <- mk_row(
      UC_TAXON, 0,
      .safe_num(input$base_REQUIRED_Unconsolidated_substrate),
      NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
    )
    rows[[length(rows) + 1]] <- mk_row(
      OLB_TAXON, 0,
      .safe_num(input$base_REQUIRED_Other_living_benthos),
      NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
    )

    do.call(rbind, rows)
  }

  # Overwrite the cached baseline .xlsx from the current inputs.
  write_cached_baseline <- function() {
    out <- build_baseline_df()
    if (is.null(out)) return(invisible(NULL))
    tryCatch(
      writexl::write_xlsx(list("Coral Cover input" = out), path = cached_baseline_path),
      error = function(e) NULL
    )
    invisible(TRUE)
  }

  # Save the current Baseline Cover box contents as an .xlsx matching the
  # ingestion schema (Coral Cover input sheet). Column names are reconstructed
  # from the columns the ingestion path reads: Unique_Site_ID, Subregion,
  # Habitat, Site_Area_m2, Taxon, Percent_Cover.
  output$baseline_save_dl <- downloadHandler(
    filename = function() {
      site_tag <- if (nzchar(.safe_num_chr(input$baseline_site))) input$baseline_site else "baseline"
      paste0("Baseline_Cover_", gsub("[^A-Za-z0-9]", "_", site_tag), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      out <- build_baseline_df()
      if (is.null(out)) {
        showNotification("No species to save.", type = "warning")
        return(invisible(NULL))
      }
      writexl::write_xlsx(list("Coral Cover input" = out), path = file)
      write_cached_baseline()  # Save-baseline also refreshes the cache.
    }
  )

  # When the selected site changes, filter the upload to that site and push
  # subregion / habitat / area / species / covers into the inputs.
  observeEvent(input$baseline_site, {
    # New site selected: invalidate the prior run so value boxes / Final-cover
    # columns blank out until the user runs the sim again.
    model_stale(TRUE)
    rt_restored_hover(NULL)

    up <- baseline_upload_data()
    req(up, nzchar(input$baseline_site))

    site_rows <- up[as.character(up$Unique_Site_ID) == input$baseline_site, , drop = FALSE]
    req(nrow(site_rows) > 0)

    # Site area: default 100, overridden by Site_Area_m2 if present
    area_val <- if ("Site_Area_m2" %in% names(site_rows) && any(!is.na(site_rows$Site_Area_m2))) {
      site_rows$Site_Area_m2[!is.na(site_rows$Site_Area_m2)][1]
    } else {
      100
    }
    updateNumericInput(session, "site_area_m2", value = area_val)

    # Latitude / Longitude from the xlsx (if present)
    lat_val <- if ("Latitude" %in% names(site_rows) && any(!is.na(site_rows$Latitude))) {
      as.numeric(site_rows$Latitude[!is.na(site_rows$Latitude)][1])
    } else {
      NA
    }
    lon_val <- if ("Longitude" %in% names(site_rows) && any(!is.na(site_rows$Longitude))) {
      as.numeric(site_rows$Longitude[!is.na(site_rows$Longitude)][1])
    } else {
      NA
    }
    updateNumericInput(session, "site_latitude",  value = lat_val)
    updateNumericInput(session, "site_longitude", value = lon_val)

    # Subregion: read ['Subregion'] and remap the code to its full label
    if ("Subregion" %in% names(site_rows) && any(!is.na(site_rows$Subregion))) {
      raw_sub <- as.character(site_rows$Subregion[!is.na(site_rows$Subregion)][1])
      sub_label <- if (raw_sub %in% names(subregion_labels)) {
        unname(subregion_labels[raw_sub])
      } else if (raw_sub %in% unname(subregion_labels)) {
        raw_sub # already a full label
      } else {
        raw_sub
      }
      updateSelectInput(session, "subregion_choice", selected = sub_label)
    }

    # Habitat: read ['Habitat'] directly (deferred so the subregion-driven
    # habitat choices are in place before setting the selection)
    if ("Habitat" %in% names(site_rows) && any(!is.na(site_rows$Habitat))) {
      hab_val <- as.character(site_rows$Habitat[!is.na(site_rows$Habitat)][1])
      later::later(function() {
        updateSelectInput(session, "habitat_choice", selected = hab_val)
      }, delay = 0.5)
    }

    # Species + covers for this site only
    if ("Taxon" %in% names(site_rows)) {
      sp <- unique(site_rows$Taxon[!is.na(site_rows$Taxon)])
      baseline_species_list(sp)   # drives the dynamic per-species rows

      covers_vec <- setNames(
        round(as.numeric(site_rows$Percent_Cover[match(sp, site_rows$Taxon)]), 2),
        sp
      )
      uploaded_covers(covers_vec)

      # Push the new site's covers + per-species target/diameter/cost into the
      # grid inputs (NA where the column/value is absent). Suppress the mix
      # observers so these programmatic writes don't set active mode or clear
      # siblings; we seed active mode explicitly afterward.
      mix_suppress(TRUE)
      get_col <- function(col, s) {
        if (!(col %in% names(site_rows))) return(NA_real_)
        v <- suppressWarnings(as.numeric(site_rows[[col]][match(s, site_rows$Taxon)]))
        if (length(v) == 0) NA_real_ else v
      }

      # Restore the "Additional outplanting years" box from any Year>0 records.
      if ("Year" %in% names(site_rows)) {
        yrs_present <- suppressWarnings(as.integer(site_rows$Year))
        extra <- sort(unique(yrs_present[!is.na(yrs_present) & yrs_present > 0]))
        updateTextInput(session, "additional_outplant_years",
                        value = paste(extra, collapse = ", "))
      }

      # Year-0-scoped column getter (falls back to first row if no Year column).
      get_col0 <- function(col, s) {
        if (!(col %in% names(site_rows))) return(NA_real_)
        idx <- if ("Year" %in% names(site_rows)) {
          which(site_rows$Taxon == s &
                suppressWarnings(as.integer(site_rows$Year)) == 0)
        } else {
          which(site_rows$Taxon == s)
        }
        if (length(idx) == 0) return(NA_real_)
        suppressWarnings(as.numeric(site_rows[[col]][idx[1]]))
      }

      for (s in sp) {
        s_ <- gsub("[^A-Za-z0-9]", "_", s)
        v  <- covers_vec[[s]]
        updateNumericInput(session, paste0("base_", s_), value = if (is.na(v)) 0 else v)
        tv <- get_col0("Target_Percent_Cover", s)
        dv <- get_col0("Outplant_Size_cm",     s)
        cv <- get_col0("Outplant_Cost",        s)
        nv <- get_col0("Outplant_Count",       s)
        ov <- get_col0("Outplants_Per_Cluster", s)
        updateNumericInput(session, paste0("target_", s_), value = if (is.na(tv) || tv <= 0) NA else tv)
        updateNumericInput(session, paste0("diam_",   s_), value = if (is.na(dv)) NA else dv)
        updateNumericInput(session, paste0("cost_",   s_), value = if (is.na(cv)) NA else cv)
        updateNumericInput(session, paste0("count_",  s_), value = if (is.na(nv) || nv <= 0) NA else nv)
        updateNumericInput(session, paste0("opc_",    s_), value = if (is.na(ov)) NA else ov)

        # Seed per-year cells from Year>0 records for this species.
        if ("Year" %in% names(site_rows)) {
          syr <- suppressWarnings(as.integer(site_rows$Year))
          for (ridx in which(site_rows$Taxon == s & !is.na(syr) & syr > 0)) {
            yr <- syr[ridx]
            gc <- function(col) suppressWarnings(as.numeric(site_rows[[col]][ridx]))
            updateNumericInput(session, paste0("diam_",  s_, "_y", yr), value = gc("Outplant_Size_cm"))
            updateNumericInput(session, paste0("cost_",  s_, "_y", yr), value = gc("Outplant_Cost"))
            updateNumericInput(session, paste0("count_", s_, "_y", yr), value = gc("Outplant_Count"))
            updateNumericInput(session, paste0("opc_",   s_, "_y", yr),
                               value = if ("Outplants_Per_Cluster" %in% names(site_rows)) gc("Outplants_Per_Cluster") else NA)
          }
        }

        # Seed active mode from the loaded values: count if present (>0), else
        # target if present (>0), else none.
        if (!is.na(nv) && nv > 0) {
          mix_active_mode[[s_]] <- "count"
        } else if (!is.na(tv) && tv > 0) {
          mix_active_mode[[s_]] <- "target"
        } else {
          mix_active_mode[[s_]] <- NA_character_
        }
      }
      # Capture seeded modes now (reactive context) for the deferred DOM paint.
      seeded_modes <- setNames(lapply(sp, function(s) {
        isolate(mix_active_mode[[gsub("[^A-Za-z0-9]", "_", s)]])
      }), sp)

      # Paint borders once the new inputs are in the DOM (contextless is fine:
      # paint_mix_border takes the mode as an argument and only touches shinyjs).
      later::later(function() {
        shiny::withReactiveDomain(session, {
          for (s in sp) paint_mix_border(s, seeded_modes[[s]])
        })
      }, delay = 2)

      # Release suppression after the current reactive flush completes (this
      # callback runs in a valid context, so the reactive write is legal, and it
      # fires after the updateNumericInput messages have been queued).
      session$onFlushed(function() {
        mix_suppress(FALSE)
      }, once = TRUE)

      # When baseline cover data is ingested, calculate the current carbonate
      # budget by area occupied per species. For each species, convert its
      # percent cover to occupied area (m2), then multiply that area by the
      # species' calc_rates['rate'] (queried by Taxon). Subtract habitat
      # bioerosion so the Baseline display shows a net budget.
      area_val_num <- .safe_num(area_val)
      sp_budget <- 0
      for (s in sp) {
        cvr <- covers_vec[[s]]
        if (is.na(cvr)) next
        sp_area_m2 <- area_val_num * (cvr / 100)        # occupied area (m2)
        rate <- calc_rates$rate[calc_rates$Taxon == s]  # query by Taxon
        if (length(rate) == 0 || is.na(rate[1])) next
        sp_budget <- sp_budget + sp_area_m2 * rate[1]   # kg CaCO3/yr (patch)
      }
      hab_now <- if ("Habitat" %in% names(site_rows) && any(!is.na(site_rows$Habitat))) {
        as.character(site_rows$Habitat[!is.na(site_rows$Habitat)][1])
      } else {
        input$habitat_choice
      }

      unconsolidated_pct_cvr <- input$base_REQUIRED_Unconsolidated_substrate
      total_coral_pct_cvr <- sum(covers_vec, na.rm = TRUE)
      consolidated_cover_m2 <- area_val_num -
                            (area_val_num * unconsolidated_pct_cvr / 100) -
                            (area_val_num * total_coral_pct_cvr / 100)
      microbioerosion <- (consolidated_cover_m2 / area_val_num) * 0.24
      macrobioerosion <- resolve_regional_bioerosion(input$subregion_choice, input$habitat_choice)
      erosion <- microbioerosion + macrobioerosion

      # Normalize to per-m2 to keep budget in kg/m2/yr like the site formula
      cur_budget <- (sp_budget / area_val_num) - erosion
      ingested_current_budget(cur_budget)
      # Baseline RAP percentile vs. the NCRMP distribution (gray surround)
      ingested_baseline_pctile(rap_percentile(cur_budget / 2.9 / (1 - 0.6265)))
    }

    # ---- Load expanded restoration parameters if present in the file ----
    set_if_present <- function(col, input_id, updater = updateNumericInput) {
      if (col %in% names(site_rows)) {
        v <- suppressWarnings(as.numeric(site_rows[[col]][1]))
        if (!is.na(v)) updater(session, input_id, value = v)
      }
    }
    set_if_present("Sim_Duration",     "sim_duration", updateSliderInput)
    set_if_present("Rest_Horizon",     "rest_horizon", updateSliderInput)
    set_if_present("DHW",              "dhw", updateSliderInput)

    # Bleach_Events is a sliderTextInput (choices 0/1/2/5) -> use its updater.
    if ("Bleach_Events" %in% names(site_rows)) {
      be <- suppressWarnings(as.numeric(site_rows$Bleach_Events[1]))
      if (!is.na(be)) {
        shinyWidgets::updateSliderTextInput(session, "bleach_events", selected = be)
      }
    }

  })#, ignoreInit = TRUE)

  # Add-species dropdown: append the chosen species to the baseline list
  # (dropdown itself is rendered above the list in baseline_cover_inputs).
  observeEvent(input$add_baseline_species, {
    s <- input$add_baseline_species
    req(nzchar(s))
    cur <- baseline_species_list()
    if (!(s %in% cur)) baseline_species_list(c(s, cur))
    # Reset the picker so the same species can't stack + the placeholder returns
    updateSelectizeInput(session, "add_baseline_species", selected = "")
  }, ignoreInit = TRUE)

  # Remove-row buttons. Each row has an actionButton with id rm_<sanitized>.
  # A single observer watches all of them by rebuilding the mapping each time
  # the species list changes and binding one observeEvent per id.
  rm_bound <- reactiveVal(character(0))
  observe({
    sp <- baseline_species_list()
    already <- rm_bound()
    for (s in sp) {
      rmid <- paste0("rm_", gsub("[^A-Za-z0-9]", "_", s))
      if (!(rmid %in% already)) {
        local({
          sp_name <- s
          this_id <- rmid
          observeEvent(input[[this_id]], {
            cur <- baseline_species_list()
            baseline_species_list(cur[cur != sp_name])
          }, ignoreInit = TRUE)
        })
        already <- c(already, rmid)
      }
    }
    rm_bound(already)
  })

  # Dynamic per-species numericInputs: species:%cover, with the add-species
  # dropdown rendered BELOW the list (or as the sole component when empty).
  # Restoration Mix: one 4-column row per baseline species. Columns:
  #   base_<s>   Baseline cover (%)
  #   target_<s> Target cover (%)
  #   diam_<s>   Avg. outplant diameter (cm)   (blank = don't outplant)
  #   cost_<s>   Avg. outplant cost ($)        (blank = don't outplant)
  # UC row is a single plain cell (no morphology, no target/diam/cost).
output$restoration_mix_inputs <- renderUI({
    mix_repaint()                    # dependency: force re-render on demand
    sp <- baseline_species_list()
    covers <- uploaded_covers()
    rus <- UC_TAXON
    olb <- OLB_TAXON
    extra_years <- additional_outplant_years()

    # Order the two reserved pseudo-taxa first: UC, then OLB.
    reserved_present <- intersect(c(rus, olb), sp)
    if (length(reserved_present)) sp <- c(reserved_present, setdiff(sp, reserved_present))

    make_id <- function(prefix, s) paste0(prefix, "_", gsub("[^A-Za-z0-9]", "_", s))

    rows <- lapply(sp, function(s) {
      bid <- make_id("base",   s)
      tid <- make_id("target", s)
      did <- make_id("diam",   s)
      cid <- make_id("cost",   s)
      nid <- make_id("count",  s)
      oid <- make_id("opc",    s)
      rmid <- make_id("rm",    s)
      s_  <- gsub("[^A-Za-z0-9]", "_", s)

      live_base <- isolate(input[[bid]])
      base_val <- if (!is.null(live_base) && !is.na(live_base)) {
        live_base
      } else if (!is.null(covers) && s %in% names(covers) && !is.na(covers[[s]])) {
        covers[[s]]
      } else {
        0
      }

      remove_btn <- actionButton(rmid, "\u2212", class = "btn-xs mix-rm-btn",
                                 style = "padding:0 6px; line-height:1.2;")

      if (identical(s, rus)) {
        return(tags$div(
          class = "mix-grid-row",
          style = "display:flex; align-items:center; gap:6px; margin-bottom:4px;",
          tags$div(style = "flex:0 0 auto;", remove_btn),
          tags$div(style = "flex: 2 1 0;",
            tags$span(class = "baseline-species-name", title = s, s)),
          tags$div(style = "flex: 1 1 0;",
            numericInput(bid, label = NULL, value = base_val, min = 0, max = 100, step = 0.1)),
          tags$div(style = "flex: 6 1 0;", "")  # spans target/diam/cost/count/opc/final
        ))
      }

      # OLB (Other living benthos): Baseline cover only, reserved space. No
      # target/diam/cost/count/opc and no Final-cover column (never grown).
      if (identical(s, olb)) {
        return(tags$div(
          class = "mix-grid-row",
          style = "display:flex; align-items:center; gap:6px; margin-bottom:4px;",
          tags$div(style = "flex:0 0 auto;", remove_btn),
          tags$div(style = "flex: 2 1 0;",
            tags$span(class = "baseline-species-name", title = s, s)),
          tags$div(style = "flex: 1 1 0;",
            numericInput(bid, label = NULL, value = base_val, min = 0, max = 100, step = 0.1)),
          tags$div(style = "flex: 6 1 0;", "")  # spans the remaining columns
        ))
      }

      # CCA: Baseline cover + Final cover only. No target/diam/cost/count/opc.
      if (str_detect(s, "algae")) {
        return(tags$div(
          class = "mix-grid-row",
          style = "display:flex; align-items:center; gap:6px; margin-bottom:4px;",
          tags$div(style = "flex:0 0 auto;", remove_btn),
          tags$div(style = "flex: 2 1 0;",
            tags$div(class = "baseline-species-name", title = s, s),
            tags$div(class = "mix-morph-label",
                     style = "font-size:11px; color:#666; font-style:italic;",
                     "(crustose coralline algae)")),
          tags$div(style = "flex: 1 1 0;",
            numericInput(bid, label = NULL, value = base_val, min = 0, max = 100, step = 0.1)),
          # Spacers for target / diam / cost / count / opc (5 columns)
          tags$div(style = "flex: 5 1 0;", ""),
          # Retain the Final-cover readout column
          tags$div(style = "flex: 1 1 0; text-align:right; padding-top:6px; font-size:13px;",
            textOutput(paste0("final_cover_", s_), inline = TRUE))
        ))
      }

      live_t <- isolate(input[[tid]]); t_val <- if (!is.null(live_t)) live_t else NA
      live_d <- isolate(input[[did]]); d_val <- if (!is.null(live_d)) live_d else NA
      live_c <- isolate(input[[cid]]); c_val <- if (!is.null(live_c)) live_c else NA
      live_n <- isolate(input[[nid]]); n_val <- if (!is.null(live_n)) live_n else NA
      live_o <- isolate(input[[oid]]); o_val <- if (!is.null(live_o)) live_o else NA

      # Multi-year outplanting applies ONLY to count-driven species. A target-
      # driven species is assumed to be a single outplanting effort.
      is_count_mode <- identical(isolate(mix_active_mode[[s_]]), "count") &&
                       !is.na(n_val) && n_val > 0
      show_extra <- length(extra_years) > 0 && is_count_mode

      # Collapsible container id for the per-year sub-rows.
      sub_id <- paste0("mix_extra_", s_)

      # Chevron toggles the sub-panel (only rendered when extra years apply).
      chevron <- if (show_extra) {
        tags$span(
          style = "cursor:pointer; flex:0 0 auto; padding:4px 2px;",
          onclick = sprintf(
            "var b=document.getElementById('%s'); b.style.display=(b.style.display==='none')?'block':'none';",
            sub_id),
          icon("chevron-down")
        )
      } else {
        tags$span(style = "flex:0 0 auto; width:14px;", "")
      }

      # Y0 (top) row.
      y0_row <- tags$div(
        class = "mix-grid-row",
        style = "display:flex; align-items:flex-start; gap:6px; margin-bottom:2px;",
        tags$div(style = "flex:0 0 auto; padding-top:4px;", remove_btn),
        tags$div(style = "flex: 2 1 0;",
          tags$div(class = "baseline-species-name", title = s, s),
          tags$div(class = "mix-morph-label",
                   style = "font-size:11px; color:#666; font-style:italic;",
                   paste0("(", morph_class(s), ")"))
        ),
        chevron,
        tags$div(style = "flex: 1 1 0;",
          numericInput(bid, label = NULL, value = base_val, min = 0, max = 100, step = 0.1)),
        tags$div(style = "flex: 1 1 0;",
          numericInput(tid, label = NULL, value = t_val, min = 0, max = 100, step = 0.1)),
        tags$div(style = "flex: 1 1 0;",
          numericInput(did, label = NULL, value = d_val, min = 2, max = 100, step = 0.1)),
        tags$div(style = "flex: 1 1 0;",
          numericInput(cid, label = NULL, value = c_val, min = 1, max = 10000, step = 1)),
        tags$div(style = "flex: 1 1 0;",
          numericInput(nid, label = NULL, value = n_val, min = 0, max = 100000, step = 1)),
        tags$div(style = "flex: 1 1 0;",
          numericInput(oid, label = NULL, value = o_val, min = 1, max = 100000, step = 1)),
        tags$div(style = "flex: 1 1 0; text-align:right; padding-top:6px; font-size:13px;",
          textOutput(paste0("final_cover_", s_), inline = TRUE))
      )

      if (!show_extra) return(y0_row)

      # Per-year sub-rows (diam/cost/count/opc only), seeded from Y0 values.
      sub_rows <- lapply(extra_years, function(yr) {
        did_y <- paste0("diam_",  s_, "_y", yr)
        cid_y <- paste0("cost_",  s_, "_y", yr)
        nid_y <- paste0("count_", s_, "_y", yr)
        oid_y <- paste0("opc_",   s_, "_y", yr)

        seed <- function(id, fallback) {
          lv <- isolate(input[[id]])
          if (!is.null(lv) && !is.na(lv)) lv else fallback
        }
        dv <- seed(did_y, if (is.na(d_val)) NA else d_val)
        cv <- seed(cid_y, if (is.na(c_val)) NA else c_val)
        nv <- seed(nid_y, if (is.na(n_val)) NA else n_val)
        ov <- seed(oid_y, if (is.na(o_val)) NA else o_val)

        tags$div(
          class = "mix-grid-row mix-extra-row",
          style = "display:flex; align-items:flex-start; gap:6px; margin-bottom:2px;",
          tags$div(style = "flex:0 0 auto; width:20px;", ""),
          tags$div(style = "flex:0 0 auto; width:14px;", ""),
          tags$div(style = "flex: 2 1 0; font-size:12px; font-style:italic; color:#555; padding-top:6px;",
                   paste0("Year ", yr)),
          tags$div(style = "flex: 1 1 0;", ""),   # no baseline
          tags$div(style = "flex: 1 1 0;", ""),   # no target
          tags$div(style = "flex: 1 1 0;",
            numericInput(did_y, label = NULL, value = dv, min = 2, max = 100, step = 0.1)),
          tags$div(style = "flex: 1 1 0;",
            numericInput(cid_y, label = NULL, value = cv, min = 1, max = 10000, step = 1)),
          tags$div(style = "flex: 1 1 0;",
            numericInput(nid_y, label = NULL, value = nv, min = 0, max = 100000, step = 1)),
          tags$div(style = "flex: 1 1 0;",
            numericInput(oid_y, label = NULL, value = ov, min = 1, max = 100000, step = 1)),
          tags$div(style = "flex: 1 1 0;", "")   # no final-cover readout on sub-rows
        )
      })

      tagList(
        y0_row,
        tags$div(id = sub_id, style = "display:none;", tagList(sub_rows))
      )
    })

    remaining <- setdiff(unique(taxa), sp)
    reserved_remaining <- intersect(c(rus, olb), remaining)
    if (length(reserved_remaining)) {
      remaining <- c(reserved_remaining, setdiff(remaining, reserved_remaining))
    }
    picker <- selectizeInput(
      "add_baseline_species", label = NULL,
      choices = c("+ Add species..." = "", remaining),
      selected = "",
      options = list(placeholder = "+ Add species...                                   ")
    )

    tagList(tags$div(style = "margin-top: 6px; display:flex; justify-content:flex-start", picker), rows)
  })

  # Helper: species in the mix that are true corals (exclude UC).
  mix_species <- reactive({
    setdiff(baseline_species_list(), c(UC_TAXON, OLB_TAXON))
  })

  # Parse the "Additional outplanting years" text box into a clean integer
  # vector of years strictly after Year 0 and within the sim duration. Dedupes,
  # drops out-of-range / non-integer tokens, and toasts anything discarded.
  additional_outplant_years <- reactive({
    raw <- input$additional_outplant_years
    if (is.null(raw) || !nzchar(trimws(raw))) return(integer(0))
    dur <- .safe_num(input$sim_duration)
    toks <- trimws(strsplit(raw, ",")[[1]])
    toks <- toks[nzchar(toks)]
    nums <- suppressWarnings(as.numeric(toks))

    bad   <- toks[is.na(nums) | nums != round(nums)]
    vals  <- nums[!is.na(nums) & nums == round(nums)]
    vals  <- as.integer(vals)
    oor   <- vals[vals <= 0 | vals > dur]
    vals  <- sort(unique(vals[vals > 0 & vals <= dur]))

    dropped <- c(bad, as.character(oor))
    if (length(dropped)) {
      showNotification(
        paste0("Ignored invalid outplanting year(s): ",
               paste(unique(dropped), collapse = ", "),
               " (must be integers 1..", dur, ")."),
        type = "warning"
      )
    }
    vals
  })

  # Repaint active-input borders whenever the mix grid re-renders (species list
  # changes recreate the inputs, dropping their CSS classes). Deferred so the
  # new inputs exist in the DOM before shinyjs targets them.
  observeEvent(baseline_species_list(), {
    sp <- mix_species()
    # Capture modes NOW (reactive context); the deferred callback is contextless.
    modes <- setNames(lapply(sp, function(s) {
      isolate(mix_active_mode[[gsub("[^A-Za-z0-9]", "_", s)]])
    }), sp)
    later::later(function() {
      shiny::withReactiveDomain(session, {
        for (s in sp) paint_mix_border(s, modes[[s]])
      })
    }, delay = 2.0)
  }, ignoreInit = TRUE)

  # Set TRUE while the result-writer pushes solved Target/Outplants values, so
  # the count/target cross-clearing observers don't fire on those programmatic
  # updates and wipe what we just wrote.
  mix_suppress <- reactiveVal(FALSE)

  # Bumped to force the Restoration Mix grid to re-render (e.g., when a species
  # newly enters count-mode and needs its multi-year dropdown injected).
  mix_repaint <- reactiveVal(0)

  # Per-species active input mode: "target" | "count" | NA (nothing supplied).
  # Set only by USER edits (guarded by mix_suppress). The simulation reads this
  # to decide which cell to honor, so a result-populated cell never steals
  # priority on re-run.
  mix_active_mode <- reactiveValues()

  # Toggle the blue border to match a species' active mode. Called after every
  # user edit and on load-time seeding.
  # Toggle the blue border for one species. `mode` is passed in (read by the
  # caller inside a reactive context) so this is safe to call from deferred
  # (later::later) callbacks, which have no reactive context.
  paint_mix_border <- function(s, mode) {
    s_  <- gsub("[^A-Za-z0-9]", "_", s)
    tid <- paste0("target_", s_)
    nid <- paste0("count_",  s_)
    if (identical(mode, "target")) {
      shinyjs::addCssClass(id = tid, class = "mix-active-input")
      shinyjs::removeCssClass(id = nid, class = "mix-active-input")
    } else if (identical(mode, "count")) {
      shinyjs::addCssClass(id = nid, class = "mix-active-input")
      shinyjs::removeCssClass(id = tid, class = "mix-active-input")
    } else {
      shinyjs::removeCssClass(id = tid, class = "mix-active-input")
      shinyjs::removeCssClass(id = nid, class = "mix-active-input")
    }
  }

  # Live sum of the Target cover column (reads inputs directly, NOT sim_token,
  # so it updates as the user types even though the timeline stays frozen).
  mix_target_total <- reactive({
    sp <- mix_species()
    sum(vapply(sp, function(s) {
      v <- input[[paste0("target_", gsub("[^A-Za-z0-9]", "_", s))]]
      if (is.null(v) || is.na(v)) 0 else as.numeric(v)
    }, numeric(1)), na.rm = TRUE)
  })

  # Display model_result's end_cover_by_species sum (excludes CCA)
  mix_final_total <- reactive({
    if (isTRUE(model_stale())) return(0)
    mr <- model_result()
    if (!is.null(mr) && !is.null(mr$end_cover_by_species)) {
      ec <- sum(mr$end_cover_by_species) - mr$end_cover_by_species[CCA_TAXON]
      ec <- if ((is.null(ec) || is.na(ec))) 0 else ec
      return(ec)
    } else {
      bg <- baseline_growth()
      if (!is.null(bg) && is.data.frame(bg[[1]])) {
        ecb <- attr(bg[[1]], "end_cover_by_species")
        if (!is.null(ecb)) {
          sum(ecb)  - ecb[CCA_TAXON]
        } else {
          0
        }
      }
    }
  })

  output$mix_target_header <- renderUI({
    tot <- mix_target_total()
    tot <- if (tot == 0) "NA" else paste0(round(tot, 1), "%")
    HTML(paste0("Target<br/>cover (%)<br/>(total: ", tot, ")"))
  })

  output$mix_final_header <- renderUI({
    tryCatch({
      tot <- mix_final_total()
      tot <- if (tot == 0) "NA" else paste0(round(tot, 1), "%")
      HTML(paste0("Final<br/>cover (%)<br/>(total: ", tot, ")"))
    }, error = function(e) {
      HTML("Final<br/>cover (%)<br/><br/>")
    }
    )
  })

  # Live sum of the Baseline cover column (excludes UC + CCA).
  mix_baseline_total <- reactive({
    sp <- mix_species()
    sum(vapply(sp, function(s) {
      v <- input[[paste0("base_", gsub("[^A-Za-z0-9]", "_", s))]]
      if (grepl("algae", s, fixed = TRUE) || is.null(v) || is.na(v)) 0 else as.numeric(v)
    }, numeric(1)), na.rm = TRUE)
  })

  output$mix_baseline_header <- renderUI({
    tot <- mix_baseline_total()
    tot <- if (tot == 0) "NA" else paste0(round(tot, 1), "%")
    HTML(paste0("Baseline<br/>cover (%)<br/>(total: ", tot, ")"))
  })

  # Fire the "exceeded 100%" toast once per upward crossing of 100.
  mix_over_100 <- reactiveVal(FALSE)
  observe({
    tot <- mix_target_total()
    was_over <- isolate(mix_over_100())
    is_over  <- is.finite(tot) && tot > 100
    if (is_over && !was_over) {
      showNotification("Warning: Target cover has exceeded 100%", type = "warning")
    }
    if (is_over != was_over) mix_over_100(is_over)
  })

  # Auto-populate diameter/cost when a species' target exceeds its baseline and
  # the diameter/cost cell is currently blank. Never clobbers a typed value and
  # never clears on a later target reduction (blank at sim time = don't outplant).
  # Auto-fill diameter/cost and enforce Outplants-precedence.
  # Bound once per species (like remove buttons) so re-renders don't stack them.
  autofill_bound <- reactiveVal(character(0))
  observe({
    sp_all <- mix_species()
    already <- autofill_bound()
    for (s in sp_all) {
      s_ <- gsub("[^A-Za-z0-9]", "_", s)
      key_n <- paste0("count_", s_)
      key_t <- paste0("target_", s_)
      if (!(key_n %in% already)) {
        local({
          sp_ <- s_
          sp_orig <- s
          did <- paste0("diam_",   sp_)
          cid <- paste0("cost_",   sp_)
          tid <- paste0("target_", sp_)
          nid <- paste0("count_",  sp_)
          # Outplant count edited by the user -> becomes the active input:
          # clear the target cell, mark count active, paint border, fill geom.
          # A clear-to-NA drops active status (target already NA, so nothing
          # is passed for this species).
          observeEvent(input[[nid]], {
            if (isolate(mix_suppress())) return()
            cnt <- input[[nid]]
            if (!is.null(cnt) && !is.na(cnt) && cnt > 0) {
              if (!is.null(isolate(input[[tid]])) && !is.na(isolate(input[[tid]]))) {
                updateNumericInput(session, tid, value = NA)
              }
              mix_active_mode[[sp_]] <- "count"
              if (is.null(input[[did]]) || is.na(input[[did]])) {
                updateNumericInput(session, did, value = OUTPLANT_DIAM_DEFAULT)
              }
              if (is.null(input[[cid]]) || is.na(input[[cid]])) {
                updateNumericInput(session, cid, value = OUTPLANT_COST_DEFAULT)
              }
              # If additional outplanting years are set, repaint now so this
              # species gains its multi-year dropdown without waiting.
              ey <- isolate(additional_outplant_years())
              if (length(ey) > 0) {
                mix_repaint(isolate(mix_repaint()) + 1)
                # Backfill any blank per-year cells from the Y0 values so extra
                # efforts always start populated.
                d0 <- isolate(input[[did]]); c0 <- isolate(input[[cid]])
                n0 <- isolate(input[[nid]]); o0 <- isolate(input[[paste0("opc_", sp_)]])
                later::later(function() {
                  shiny::withReactiveDomain(session, {
                    for (yr in ey) {
                      fill_blank <- function(suf, val) {
                        if (is.null(val) || is.na(val)) return()
                        id_y <- paste0(suf, "_", sp_, "_y", yr)
                        cur  <- isolate(input[[id_y]])
                        if (is.null(cur) || is.na(cur)) {
                          updateNumericInput(session, id_y, value = val)
                        }
                      }
                      fill_blank("diam",  d0)
                      fill_blank("cost",  c0)
                      fill_blank("count", n0)
                      fill_blank("opc",   o0)
                    }
                  })
                }, delay = 0.3)
              }
            } else {
              # Count cleared/zeroed. If it was the active mode, drop it.
              if (identical(mix_active_mode[[sp_]], "count")) {
                mix_active_mode[[sp_]] <- NA_character_
                if (length(isolate(additional_outplant_years())) > 0) {
                  mix_repaint(isolate(mix_repaint()) + 1)
                }
              }
            }
            paint_mix_border(sp_orig, isolate(mix_active_mode[[sp_]]))
          }, ignoreInit = TRUE)
          # Target edited by the user -> becomes the active input: clear the
          # Outplants cell, mark target active, paint border, fill geom.
          observeEvent(input[[tid]], {
            if (isolate(mix_suppress())) return()
            tgt  <- input[[tid]]
            base <- .safe_num(input[[paste0("base_", sp_)]])
            if (!is.null(tgt) && !is.na(tgt)) {
              cnt <- isolate(input[[nid]])
              if (!is.null(cnt) && !is.na(cnt)) {
                updateNumericInput(session, nid, value = NA)
              }
              mix_active_mode[[sp_]] <- "target"
              if (tgt > base) {
                if (is.null(input[[did]]) || is.na(input[[did]])) {
                  updateNumericInput(session, did, value = OUTPLANT_DIAM_DEFAULT)
                }
                if (is.null(input[[cid]]) || is.na(input[[cid]])) {
                  updateNumericInput(session, cid, value = OUTPLANT_COST_DEFAULT)
                }
              }
            } else {
              # Target cleared. If it was the active mode, drop it.
              if (identical(mix_active_mode[[sp_]], "target")) {
                mix_active_mode[[sp_]] <- NA_character_
              }
            }
            paint_mix_border(sp_orig, isolate(mix_active_mode[[sp_]]))
          }, ignoreInit = TRUE)
        })
        already <- c(already, key_n)
      }
    }
    autofill_bound(already)
  })

  # Re-seed per-year extra-effort cells whenever the "Additional outplanting
  # years" box changes. For every count-mode species, any blank per-year
  # diam/cost/count/opc cell is backfilled from that species' Y0 value. Runs
  # after a short defer so the grid has rendered the new per-year inputs.
  observeEvent(additional_outplant_years(), {
    ey <- additional_outplant_years()
    sp <- mix_species()
    if (length(sp) == 0) return()

    # Force the grid to rebuild so count-mode species gain/lose per-year rows.
    mix_repaint(isolate(mix_repaint()) + 1)

    # Snapshot Y0 values + active modes now (reactive context); the deferred
    # callback is contextless.
    snap <- lapply(sp, function(s) {
      s_ <- gsub("[^A-Za-z0-9]", "_", s)
      list(
        s_    = s_,
        mode  = isolate(mix_active_mode[[s_]]),
        diam  = isolate(input[[paste0("diam_",  s_)]]),
        cost  = isolate(input[[paste0("cost_",  s_)]]),
        count = isolate(input[[paste0("count_", s_)]]),
        opc   = isolate(input[[paste0("opc_",   s_)]])
      )
    })

    later::later(function() {
      shiny::withReactiveDomain(session, {
        for (sn in snap) {
          # Only count-mode species carry per-year rows.
          if (!identical(sn$mode, "count")) next
          for (yr in ey) {
            fill_blank <- function(suf, val) {
              if (is.null(val) || is.na(val)) return()
              id_y <- paste0(suf, "_", sn$s_, "_y", yr)
              cur  <- isolate(input[[id_y]])
              if (is.null(cur) || is.na(cur)) {
                updateNumericInput(session, id_y, value = val)
              }
            }
            fill_blank("diam",  sn$diam)
            fill_blank("cost",  sn$cost)
            fill_blank("count", sn$count)
            fill_blank("opc",   sn$opc)
          }
        }
      })
    }, delay = 0.4)
  }, ignoreInit = TRUE)

  # Bind a Final-cover readout output per species (end-of-duration cover from
  # the last model run). Bound once per species; re-renders pull from
  # model_result()'s end_cover_by_species. Blank until a run produces a value.
  final_cover_bound <- reactiveVal(character(0))
  observe({
    sp_all <- mix_species()
    already <- final_cover_bound()
    for (s in sp_all) {
      s_ <- gsub("[^A-Za-z0-9]", "_", s)
      oid <- paste0("final_cover_", s_)
      if (!(oid %in% already)) {
        local({
          sp_orig <- s
          out_id  <- oid
          output[[out_id]] <- renderText({
            # A site switch marks the prior run stale; blank the column until
            # the user re-runs the simulation.
            if (isTRUE(model_stale())) return("\u2013")
            mr <- model_result()
            # Restoration run: use the model's per-species end cover.
            if (!is.null(mr) && !is.null(mr$end_cover_by_species)) {
              ec <- mr$end_cover_by_species
              if (sp_orig %in% names(ec)) return(paste0(round(ec[[sp_orig]], 1), " %"))
            }
            # Baseline-only run: use per-species end cover from baseline_growth.
            bg <- baseline_growth()
            if (!is.null(bg) && is.data.frame(bg[[1]])) {
              ecb <- attr(bg[[1]], "end_cover_by_species")
              if (!is.null(ecb) && sp_orig %in% names(ecb)) {
                return(paste0(round(ecb[[sp_orig]], 1), " %"))
              }
            }
            "\u2013"
          })
        })
        already <- c(already, oid)
      }
    }
    final_cover_bound(already)
  })

  # Fixed restoration species target-cover inputs (Restoration Mix box) ----
  restoration_species <- c(
    "Acropora cervicornis",  "Acropora palmata",
    "Colpophyllia natans",   "Diploria labyrinthiformis",
    "Montastraea cavernosa", "Orbicella faveolata",
    "Porites astreoides",    "Porites porites",
    "Pseudodiploria spp.",   "Siderastrea siderea",
    "Solenastrea bournoni",  "Stephanocoenia intersepta"
  )

  # Helper: build a column of species target-cover inputs for one morphology sub-box.
  # STATIC: inputs must not depend on model output, or the UI would rebuild
  # (resetting every input to 0) whenever the model reruns. Per-species
  # outplant counts render in separate, independent outputs below.
  # step = 0.5 (accept half-percent), ticks hidden; two-line italic labels.
  make_mix_inputs <- function(species_vec) {
    lapply(species_vec, function(s) {
      id  <- paste0("rest_target_", gsub("[^A-Za-z0-9]", "_", s))
      nid <- paste0("outplants_", gsub("[^A-Za-z0-9]", "_", s))
      tagList(
        tags$div(
          class = "mix-species-stacked",
          tags$div(class = "baseline-species-name mix-species-label",
                   title = s, HTML(make_species_label(s, split_line = FALSE))),
          fluidRow(
            column(6,
              # Blank by default (value = NA renders empty). A blank target is
              # treated as "no target" in the model, not a forced-zero target.
              numericInput(id, label = NULL, value = NA,
                          min = 0, max = 100, step = 0.1)
            ),
            column(6,
              tags$div(class = "rest-outplant-note", textOutput(nid, inline = TRUE))
            )
          )
        )
      )
    })
  }

  # Branching sub-box: two columns
  output$mix_branching <- renderUI({
    sl <- make_mix_inputs(branching_species)
    half <- ceiling(length(sl) / 2)
    fluidRow(
      column(6, tagList(sl[1:half])),
      column(6, tagList(sl[(half + 1):length(sl)]))
    )
  })

  # Weedy / Other sub-box: two columns
  output$mix_weedy <- renderUI({
    sl <- make_mix_inputs(weedy_species)
    half <- ceiling(length(sl) / 2)
    fluidRow(
      column(6, tagList(sl[1:half])),
      column(6, tagList(sl[(half + 1):length(sl)]))
    )
  })

  # Massive sub-box: four columns
  output$mix_massive <- renderUI({
    sl <- make_mix_inputs(mix_massive_species)
    n <- length(sl)
    per <- ceiling(n / 4)
    col_idx <- function(k) {
      lo <- (k - 1) * per + 1
      hi <- min(k * per, n)
      if (lo <= hi) lo:hi else integer(0)
    }
    fluidRow(
      column(3, tagList(sl[col_idx(1)])),
      column(3, tagList(sl[col_idx(2)])),
      column(3, tagList(sl[col_idx(3)])),
      column(3, tagList(sl[col_idx(4)]))
    )
  })

  # Reset every Species-Mix target input back to blank (NA).
  observeEvent(input$reset_mix, {
    for (s in mix_species()) {
      s_ <- gsub("[^A-Za-z0-9]", "_", s)
      updateNumericInput(session, paste0("target_", s_), value = NA)
      updateNumericInput(session, paste0("diam_",   s_), value = NA)
      updateNumericInput(session, paste0("cost_",   s_), value = NA)
      updateNumericInput(session, paste0("count_",  s_), value = NA)
    }
    showNotification("Cleared restoration targets.", type = "message")
  })

  ## ---------------------------------------------------------------------------
  ## Restoration Planning ----
  ## baseline / restored metrics + plotly timeline
  ## ---------------------------------------------------------------------------

  # Holds the active Shiny Progress object during a simulation run, so both the
  # baseline_growth() reactive and model_result() can advance the same bar.
  sim_progress <- reactiveVal(NULL)

  # Convenience: push a message to the active progress bar if one exists.
  progress_say <- function(msg) {
    pr <- sim_progress()
    if (!is.null(pr)) {
      tryCatch(pr$inc(amount = 0.05, message = "Simulating", detail = msg),
               error = function(e) NULL)
    }
  }

  # Baseline cover & carbonate budget from the entered/uploaded data.
  # Budget uses the area-occupied method: per species, area (m2) * rate,
  # queried from calc_rates by Taxon, normalized to per-m2, minus bioerosion.
  baseline_metrics <- reactive({
    sim_token()            # gate: recompute only when the token advances
    isolate({
    # If any of these values are empty, baseline metric calculations will not fire.
    req(outplanting_ready())
    req(nzchar(input$habitat_choice),
        nzchar(input$subregion_choice),
        nzchar(input$site_area_m2)
        )
    sim_duration <- .safe_num(input$sim_duration)
    unconsolidated_pct_cvr <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)
    sp <- setdiff(baseline_species_list(), c(UC_TAXON, OLB_TAXON))
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    # Exclude CCA from cover summation
    sum_cover_ids <- NULL
    for (id in ids) {
      if (id != "base_Crustose_coralline_algae") {
        sum_cover_ids <- c(sum_cover_ids, id)
      }
    }
    sum_covers <- sapply(sum_cover_ids, function(id) .safe_num(input[[id]]))
    total_coral_pct_cvr <- sum(sum_covers, na.rm = TRUE)

    # Restoration covers
    covers <- sapply(ids, function(id) .safe_num(input[[id]]))

    # Per-taxon cover df for porosity + budget
    cover <- uploaded_monitoring_cover()
    cover_df <- data.frame(
      taxon = as.character(cover$Taxon),
      cvr   = as.numeric(cover$Percent_Cover),
      stringsAsFactors = FALSE
    )
    cover_df$cvr[is.na(cover_df$cvr)] <- 0

    por <- assemblage_porosity(cover_df, "cvr")

    area_val_num <- .safe_num(input$site_area_m2)
    if (area_val_num <= 0) area_val_num <- 100

    # Area-occupied budget: sum over species of area_m2 * rate (by Taxon)
    patch_budget <- 0
    for (k in seq_along(sp)) {
      s <- sp[k]
      sp_area_m2 <- area_val_num * (covers[k] / 100)
      rate <- calc_rates$rate[calc_rates$Taxon == s]
      if (length(rate) == 0 || is.na(rate[1])) next
      patch_budget <- patch_budget + sp_area_m2 * rate[1]
    }

    consolidated_cover <- area_val_num -
                          (area_val_num * unconsolidated_pct_cvr / 100) -
                          (area_val_num * total_coral_pct_cvr / 100)

    # micro scaled by the CONSOLIDATED FRACTION (consol/area)
    microbioerosion <- if (consolidated_cover > 0) {
      (consolidated_cover / area_val_num) * 0.24
    } else {
      0
    }
    macrobioerosion <- resolve_regional_bioerosion(input$subregion_choice, input$habitat_choice)

    budget <- (patch_budget / area_val_num) - macrobioerosion - microbioerosion

    # Baseline-assemblage porosity from the entered baseline covers
    base_cover_df <- data.frame(taxon = sp, cvr = covers, stringsAsFactors = FALSE)
    base_cover_df$cvr[is.na(base_cover_df$cvr)] <- 0
    bp <- assemblage_porosity(base_cover_df, "cvr")

    rap_baseline <- budget / 2.9 / (1 - bp)

    list(cover = total_coral_pct_cvr, budget = budget, rap = rap_baseline, sim_duration = sim_duration)
    })
  })

  # Baseline growth series for the timeline (originals only). Available as soon
  # as species + covers + subregion/habitat are set, independent of any target.
  baseline_growth <- reactive({
    sim_token()
    isolate({ with_logged_conditions({
    req(outplanting_ready())
    habitat       <- input$habitat_choice
    subregion <- input$subregion_choice
    site_area     <- .safe_num(input$site_area_m2)
    sim_duration  <- .safe_num(input$sim_duration)
    unconsolidated_pct_cvr <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)
    if (site_area <= 0) site_area <- 100
    req(nzchar(habitat), nzchar(subregion))

    sp <- setdiff(baseline_species_list(), c(UC_TAXON, OLB_TAXON))
    if (length(sp) == 0) return(NULL)

    bdf <- data.frame(taxon = character(), current_cvr_pct = numeric(), stringsAsFactors = FALSE)
    for (s in sp) {
      base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      bdf[nrow(bdf) + 1, ] <- list(s, .safe_num(input[[base_id]]))
    }
    if (all(bdf$current_cvr_pct <= 0)) return(NULL)

    por <- assemblage_porosity(bdf, "current_cvr_pct")

    #tryCatch(
      list(
        run_baseline_growth(
          subregion = subregion,
          site_area = site_area, uc_pct = unconsolidated_pct_cvr,
          sim_duration = sim_duration,
          bleaching_severity  = .safe_num(input$dhw),
          bleaching_frequency = .safe_num(input$bleach_events),
          baseline_cover_df = bdf,
          progress_cb = progress_say
        ),
        porosity = por
      ) # ,
     # error = function(e) NULL
    #)
    }) })
  })

  # Restored (target) cover & carbonate budget (simple linear estimate; the
  # graph + saved values use the full model at the horizon instead).
  restored_metrics <- reactive({
    sim_token()
    isolate({
    b <- baseline_metrics()
    sp <- mix_species()
    tgt_vals  <- vapply(sp, function(s) .safe_num(input[[paste0("target_", gsub("[^A-Za-z0-9]", "_", s))]]), numeric(1))
    base_vals <- vapply(sp, function(s) .safe_num(input[[paste0("base_",   gsub("[^A-Za-z0-9]", "_", s))]]), numeric(1))
    # Restored cover per species = max(target, baseline); species with no target
    # keep their baseline cover.
    restored_cvr_vec <- pmax(tgt_vals, base_vals, na.rm = TRUE)
    rest_rates <- as.numeric(calc_rates$rate[match(sp, calc_rates$Taxon)])
    budget <- b$budget + sum((restored_cvr_vec - base_vals) * rest_rates / 100, na.rm = TRUE)

    total_coral_cover <- sum(restored_cvr_vec, na.rm = TRUE)

    rest_cover_df <- data.frame(taxon = sp, cvr = restored_cvr_vec, stringsAsFactors = FALSE)
    rp <- assemblage_porosity(rest_cover_df, "cvr")

    rap_restored <- budget / 2.9 / (1 - rp)

    list(cover = total_coral_cover, budget = budget, rap = rap_restored)
    })
  })

  # ---- Reactive restoration model ----
  # Derives all parameters from Shiny inputs, builds target_cover_df from the
  # per-species baseline (current) + restoration-mix (target) values.
  model_result <- reactive({
    sim_token()
    if (isTRUE(model_stale())) return(NULL)
    isolate({ with_logged_conditions({
    req(outplanting_ready())

    # Progress bar for the whole simulation. Created here, advanced by
    # progress_say() (baseline + restoration species), closed on exit.
    pr <- shiny::Progress$new(session, min = 0, max = 1)
    pr$set(message = "Simulating", detail = "Starting...", value = 0.02)
    sim_progress(pr)
    on.exit({
      tryCatch(pr$set(message = "Simulation complete", detail = "", value = 1),
               error = function(e) NULL)
      # Brief pause so "complete" is visible, then close.
      later::later(function() tryCatch(pr$close(), error = function(e) NULL), 0.6)
      sim_progress(NULL)
    }, add = TRUE)

    # Force the baseline-growth reactive to evaluate (and print its sanity
    # checks) BEFORE this model's outplanting sanity prints run below.
    invisible(baseline_growth())

    # Derived parameters
    habitat        <- input$habitat_choice
    subregion      <- input$subregion_choice
    site_area      <- .safe_num(input$site_area_m2)
    unconsolidated_pct_cvr <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)

    sim_duration  <- .safe_num(input$sim_duration)
    rest_horizon  <- .safe_num(input$rest_horizon)

    # Other living benthos: required numeric >= 0 (reserved, non-growing space).
    olb_raw <- input$base_REQUIRED_Other_living_benthos
    olb_pct <- if (is.null(olb_raw) || is.na(olb_raw)) NA_real_ else as.numeric(olb_raw)
    if (!is.finite(olb_pct) || olb_pct < 0) {
      log_msg("---- Error: 'REQUIRED Other living benthos' (>= 0) must be provided. ----")
      showNotification("Error: Enter 'REQUIRED Other living benthos' cover (>= 0) before simulating.",
                       type = "error")
      return(NULL)
    }

    # Bleaching parameters
    bleaching_severity  <- .safe_num(input$dhw)           # degree-heating weeks
    bleaching_frequency <- .safe_num(input$bleach_events) # events in a 5-year period

    req(nzchar(habitat), nzchar(subregion), site_area > 0)

    # Build target_cover_df from the FULL baseline species set (so unrestored
    # species still grow), unioned with the restoration-mix species. current =
    # baseline % cover input; target = restoration-mix slider (0 if none).
    all_sp <- mix_species()
    extra_years <- additional_outplant_years()

    target_cover_df <- data.frame(
      taxon = character(),
      current_cvr_pct = numeric(),
      target_cvr_pct = numeric(),
      outplant_diam_cm = numeric(),
      outplant_cost = numeric(),
      outplant_count = numeric(),
      opc = numeric(),
      stringsAsFactors = FALSE
    )
    extra_years_df <- data.frame(
      taxon = character(), plant_year = numeric(),
      outplant_diam_cm = numeric(), outplant_cost = numeric(),
      outplant_count = numeric(), opc = numeric(),
      stringsAsFactors = FALSE
    )
    for (s in all_sp) {
      # CCA is grown in Phase 0; OLB is reserved space. Neither is grown as a
      # coral here.
      if (str_detect(s, "algae") || is_reserved_taxon(s)) next
      s_    <- gsub("[^A-Za-z0-9]", "_", s)
      cur   <- .safe_num(input[[paste0("base_",   s_)]])
      dia_v <- input[[paste0("diam_",  s_)]]
      cst_v <- input[[paste0("cost_",  s_)]]
      opc_v <- input[[paste0("opc_",   s_)]]
      dia   <- if (is.null(dia_v) || is.na(dia_v)) NA_real_ else as.numeric(dia_v)
      cst   <- if (is.null(cst_v) || is.na(cst_v)) NA_real_ else as.numeric(cst_v)
      opc   <- if (is.null(opc_v) || is.na(opc_v)) NA_real_ else as.numeric(opc_v)

      mode <- mix_active_mode[[s_]]
      raw_t <- input[[paste0("target_", s_)]]
      raw_n <- input[[paste0("count_",  s_)]]
      tgt <- 0
      cnt <- NA_real_
      if (identical(mode, "target")) {
        tgt <- if (is.null(raw_t) || is.na(raw_t)) 0 else as.numeric(raw_t)
      } else if (identical(mode, "count")) {
        cnt <- if (is.null(raw_n) || is.na(raw_n)) NA_real_ else as.numeric(raw_n)
      }
      target_cover_df[nrow(target_cover_df) + 1, ] <- list(s, cur, tgt, dia, cst, cnt, opc)

      # Gather additional-year efforts for restored species.
      restoring <- (tgt > cur) || (!is.na(cnt) && cnt > 0)
      if (length(extra_years) > 0 && restoring) {
        for (yr in extra_years) {
          dv <- input[[paste0("diam_",  s_, "_y", yr)]]
          cv <- input[[paste0("cost_",  s_, "_y", yr)]]
          nv <- input[[paste0("count_", s_, "_y", yr)]]
          ov <- input[[paste0("opc_",   s_, "_y", yr)]]
          nvn <- if (is.null(nv) || is.na(nv)) NA_real_ else as.numeric(nv)
          if (is.na(nvn) || nvn <= 0) next
          extra_years_df[nrow(extra_years_df) + 1, ] <- list(
            s, yr,
            if (is.null(dv) || is.na(dv)) NA_real_ else as.numeric(dv),
            if (is.null(cv) || is.na(cv)) NA_real_ else as.numeric(cv),
            nvn,
            if (is.null(ov) || is.na(ov)) NA_real_ else as.numeric(ov)
          )
        }
      }
    }
    # Re-attach CCA rows to target_cover_df (current cover only) so the fixed
    # CCA layer + porosity see them, without an outplant target.
    for (s in all_sp) {
      if (!str_detect(s, "algae")) next
      s_  <- gsub("[^A-Za-z0-9]", "_", s)
      cur <- .safe_num(input[[paste0("base_", s_)]])
      target_cover_df[nrow(target_cover_df) + 1, ] <-
        list(s, cur, cur, NA_real_, NA_real_, NA_real_, NA_real_)
    }

    # Any species with a positive target uplift OR a positive outplant count
    # constitutes work to simulate. Only bail when neither exists anywhere.
    has_target_uplift <- any((target_cover_df$target_cvr_pct - target_cover_df$current_cvr_pct) > 0, na.rm = TRUE)
    has_count         <- any(target_cover_df$outplant_count > 0, na.rm = TRUE)
    if (!has_target_uplift && !has_count) {
      log_msg("No target increases or outplant counts submitted; nothing to simulate.", "\n")
      return(NULL)
    }

    res <- run_restoration_model(
      habitat = habitat, subregion = subregion,
      site_area = site_area, uc_pct = unconsolidated_pct_cvr,
      sim_duration = sim_duration, rest_horizon = rest_horizon,
      bleaching_severity = bleaching_severity,
      bleaching_frequency = bleaching_frequency,
      target_cover_df = target_cover_df,
      extra_years_df = extra_years_df,
      olb_pct = olb_pct,
      progress_cb = progress_say
    )
    res
    }) })
  })

  # ---- Baseline / restored values at the end of the simulation ----
  # Single source of truth for the saved scenario + percentile surrounds, taken
  # from the same model output the graph uses (so saved == graphed). Falls back
  # to the linear metrics estimate when there is no model output.
  final_vals <- reactive({
    b  <- baseline_metrics()
    r  <- restored_metrics()
    mr <- model_result()

    duration <- .safe_num(input$sim_duration)
    if (!is.null(mr) && nrow(mr$budget_df) > 0) {
      dr <- min(duration + 1, nrow(mr$budget_df)) # Duration row
      bd <- mr$budget_df
      list(
        b_cover  = bd$pct_cvr_orig[dr],   r_cover  = bd$pct_cvr_total[dr],
        b_budget = bd$carb_budg_orig[dr], r_budget = bd$carb_budg_total[dr],
        b_rap    = bd$RAP_orig[dr],       r_rap    = bd$RAP_total[dr]
      )
    } else { # No model result present: baseline only
      list(
        b_cover = b$cover,   r_cover = NULL, # r$cover,
        b_budget = b$budget, r_budget = NULL, # r$budget,
        b_rap = b$rap,       r_rap = NULL # r$rap
      )
    }
  })

  # After a run, populate each species' Target and Outplants cells with the
  # resultant values. Target-driven species get their solved count; count-driven
  # species get their achieved cover. The suppression flag prevents the cross-
  # clearing observers from wiping these programmatic writes.
  observeEvent(model_result(), {
    mr <- model_result()
    if (is.null(mr)) return()
    op  <- mr$outplants_by_species

    mix_suppress(TRUE)
    later::later(function() mix_suppress(FALSE), delay = 3.0)

    for (s in mix_species()) {
      s_  <- gsub("[^A-Za-z0-9]", "_", s)
      tid <- paste0("target_", s_)
      nid <- paste0("count_",  s_)

      mode <- mix_active_mode[[s_]]

      # Target-driven species: echo the solved Y0 outplant count into the
      # (inactive) Outplants cell. Final cover is shown in its own column.
      if (identical(mode, "target")) {
        if (!is.null(op) && s %in% names(op) && op[[s]] > 0) {
          updateNumericInput(session, nid, value = as.integer(op[[s]]))
        }
      }
    }
  }, ignoreInit = TRUE)

  # Hovered-pip Restored readout. Persists until the next pip hover.
  rt_restored_hover <- reactiveVal(NULL)

  observeEvent(plotly::event_data("plotly_hover", source = "rest_tl"), {
    ev <- plotly::event_data("plotly_hover", source = "rest_tl")
    cd <- ev$customdata
    # Only pip points carry customdata; line/band vertices return NULL here.
    if (is.null(cd) || length(cd) == 0 || is.na(cd[1])) return()
    parts <- suppressWarnings(as.numeric(strsplit(as.character(cd[1]), "\\|")[[1]]))
    if (length(parts) != 4 || any(is.na(parts))) return()
    rt_restored_hover(list(cover = parts[1], budget = parts[2],
                           rap = parts[3], year = parts[4]))
  }, ignoreInit = TRUE)

  # Set the Restored readout to the new result when the model changes (e.g. new target/site),
  # so a stale hover value doesn't linger against a different scenario.
  observeEvent(final_vals(), {
    fv <- final_vals()
    rt_restored_hover(list(cover = fv$r_cover, budget = fv$r_budget,
                           rap = fv$r_rap, year = .safe_num(input$sim_duration)))
  }, ignoreInit = TRUE)

  # ---- Reactive graph surrounds ----
  # Total project cost from the model
  output$model_final_cost <- renderText({
    mr <- model_result()
    if (is.null(mr)) return("Estimated cost: \u2013")
    paste0("Estimated cost: $", format(round(mr$cost), big.mark = ","))
  })

  # Warn (red) when total target cover exceeds 100% (model refuses to run)
  output$target_cover_warning <- renderUI({
    slider_ids <- paste0("rest_target_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    tot <- sum(vapply(slider_ids, function(id) .safe_num(input[[id]]), numeric(1)), na.rm = TRUE)
    if (tot > 100) {
      tags$span(class = "sim-warning",
        HTML(paste0("<span style='color:red;'>Total target cover (", round(tot, 1),
               "%) exceeds 100%. Reduce the mix to run the model.</span>"))
      )
    }
  })

  # Baseline RAP percentile (gray) + restored RAP percentile at simulation end (colored)
  output$rap_pctile_baseline <- renderUI({
    pct <- ingested_baseline_pctile()
    if (is.null(pct) || is.na(pct)) {
      return(NULL)
    }
    log_msg(paste0(" ", input$baseline_site, " Baseline RAP percentile: ", round(pct), "%"))
    tags$span(style = "color:#777777;",
      paste0("Baseline RAP percentile: ", round(pct), "%"))
  })
  output$rap_pctile_restored <- renderUI({
    mr <- model_result()
    if (is.null(mr) || nrow(mr$budget_df) == 0) {
      return(NULL)
      }
    duration <- .safe_num(input$sim_duration)
    dr <- min(duration + 1, nrow(mr$budget_df))
    pct <- rap_percentile(mr$budget_df$RAP_total[dr])
    if (is.na(pct)) {
      return(NULL)
      }
    log_msg(paste0(" ", input$baseline_site, "RAP percentile at simulation end: ", round(pct), "%", "\n\n"))
    tags$span(style = paste0("color:", percentile_color(pct), ";"),
      paste0("RAP percentile at simulation end: ", round(pct), "%")
    )
  })

  # ---- Baseline / Restored value boxes beside the timeline ----
  #  Restored: reacts to the last pip hover;
  # before any hover, default to the duration-year values.
  rt_restored_current <- reactive({
    h <- rt_restored_hover()
    if (!is.null(h)) {
      return(h)
    }
    fv <- final_vals()
    list(cover = fv$r_cover, budget = fv$r_budget, rap = fv$r_rap,
         year = .safe_num(input$sim_duration))
  })

  # Baseline: static, from baseline_metrics (Year-0).
  output$rt_baseline_cover <- renderValueBox({
    valueBox(paste0(round(baseline_metrics()$cover, 1), " %"),
      div(class = "bordered-text", "Coral cover"),
      icon = icon("percent"), color = "green")
  })
  output$rt_baseline_budget <- renderValueBox({
    valueBox(paste0(round(baseline_metrics()$budget, 2), " kg/m²/yr"),
      div(class = "bordered-text", "Carbonate budget"),
      icon = icon("balance-scale"), color = "blue")
  })
  output$rt_baseline_rap <- renderValueBox({
    valueBox(paste0(round(baseline_metrics()$rap, 2), " mm/yr"),
      div(class = "bordered-text",
        HTML(paste0("Reef accretion potential<br/>",
          "(this reef is ",
          "<span style='color:", # color determined by conditional below
          if (baseline_metrics()$rap >= 0.5) "forestgreen" else if (baseline_metrics()$rap <= -0.5) "lightcoral" else "orange",
          ";'>",
          if (baseline_metrics()$rap >= 0.5) "growing" else if (baseline_metrics()$rap <= -0.5) "eroding" else "in stasis",
          "</span>)"
          )
        )
      ),
      icon = icon("chart-line"), color = "aqua")
  })
  output$rt_baseline_title <- renderText({
    cvr <- baseline_metrics()$cover
    if (!is.null(cvr)) "Baseline" else ""
  })

  output$rt_restored_cover <- renderValueBox({
    cvr <- rt_restored_current()$cover
    valueBox(if (is.null(cvr)) "NA" else paste0(round(cvr, 1), " %"),
      div(class = "bordered-text", "Coral cover"),
      icon = icon("plus-circle"), color = "olive")
  })
  output$rt_restored_budget <- renderValueBox({
    budg <- rt_restored_current()$budget
    valueBox(if (is.null(budg)) "NA" else paste0(round(budg, 2), " kg/m²/yr"),
      div(class = "bordered-text", "Carbonate budget"),
      icon = icon("balance-scale"), color = "blue")
  })
  output$rt_restored_rap <- renderValueBox({
    rap <- rt_restored_current()$rap
    valueBox(if (is.null(rap)) "NA" else paste0(round(rap, 2), " mm/yr"),
        div(class = "bordered-text",
          HTML(if (is.null(rap)) {
            "Reef accretion potential<br/><br/>"
          } else {
              paste0("Reef accretion potential<br/>",
              "(this reef is ",
              "<span style='color:", # color determined by conditional below
              if (rt_restored_current()$rap >= 0.5) "forestgreen" else if (rt_restored_current()$rap <= -0.5) "lightcoral" else "orange",
              ";'>",
              if (rt_restored_current()$rap >= 0.5) "growing" else if (rt_restored_current()$rap <= -0.5) "eroding" else "in stasis",
              "</span>",
              ")"
              )
            }
          )
        ),
      icon = icon("chart-line"), color = "teal")
  })
  output$rt_restored_title <- renderText({
    cur <- rt_restored_current()
    if (is.null(cur$cover)) return("Restored")
    yr <- if (is.null(cur$year) || is.na(cur$year)) NA else round(cur$year)
    if (is.na(yr)) "Restored" else paste0("Restored: Year ", yr)
  })

  output$restoration_timeline <- plotly::renderPlotly({
    sim_token()                      # freeze: only a simulation run redraws
    b  <- baseline_metrics()
    bg <- baseline_growth()
    mr <- model_result()

    # Guard: on cached auto-load the observers can fire before baseline_growth()
    # has a valid frame. Treat a NULL/empty result as "no baseline yet" so we
    # never run min()/max()/ggplot on a length-0 or NULL bg_df (which produced
    # the Inf/-Inf warnings and the fortify error flashing on the plot).
    bg_ok <- !is.null(bg) &&
             is.data.frame(bg[[1]]) && nrow(bg[[1]]) > 0
    bg_df <- if (bg_ok) bg[[1]] else NULL
    bp    <- if (bg_ok) bg[[2]] else 0.6265  # baseline porosity fallback

    site_area <- isolate(.safe_num(input$site_area_m2))
    uc_pct    <- isolate(.safe_num(input$base_REQUIRED_Unconsolidated_substrate))
    macrobioerosion <- isolate(resolve_regional_bioerosion(input$subregion_choice, input$habitat_choice))

    # Apply bioerosion to baseline growth only when it is a real frame.
    if (!is.null(bg_df)) {
      be_sd_bg <- isolate(bioerosion_stdev(input$subregion_choice, input$habitat_choice))
      bg_df <- baseline_bioerosion_RAP(bg_df, site_area, uc_pct, be_micro_rate,
                                       macrobioerosion, bp,
                                       be_sd_lo = be_sd_bg[1], be_sd_hi = be_sd_bg[2])
    }

    tryCatch({
        write.csv(mr$budget_df, here("cache", "model_results.csv"))
        write.csv(bg_df, here("cache", "baseline_results.csv"))
        write.csv(b, here("cache", "baseline_metrics.csv"))
    }, error = function(e) {
      warning("Error writing budget results: ", e$message)
    })

    dur <- b$sim_duration
    horizon <- isolate(.safe_num(input$rest_horizon))
    dark <- isTRUE(input$dark_mode)
    # Dark-mode plot palette
    paper_bg <- if (dark) "#232a33" else "white"
    plot_bg  <- if (dark) "#232a33" else "white"
    font_col <- if (dark) "#e6e6e6" else "#333333"
    orig_col <- if (dark) "#cfcfcf" else "gray30"  # lighten Baseline/Original line
    # Axis breaks lighter gray in dark mode
    grid_col <- if (dark) "#5a6472" else "#d9d9d9"
    # Dark-mode-aware Year-0 annotation background/border/text
    ann_bg     <- if (dark) "#2c353f" else "white"
    ann_border <- if (dark) "#8fb8d8" else "steelblue"
    ann_font   <- if (dark) "#e6e6e6" else "#333333"
    # SLR overlay: simulation assumed to begin next year
    start_year <- as.integer(format(Sys.Date(), "%Y")) + 1
    slr_tl <- build_slr_timeline(start_year, n_years = b$sim_duration)

    # When SLR is hidden, drop it from the y-limit and cap the top at 4 mm/yr
    # (or the max RAP, whichever is higher).
    show_slr <- isTRUE(input$show_slr)
    slr_ymax <- if (show_slr && !is.null(slr_tl) && nrow(slr_tl) > 0) {
      max(slr_tl$SLR, na.rm = TRUE)
    } else {
      -Inf   # excluded from any max(); floor handled per-branch below
    }

    # Line-weight emphasis: Int heaviest, then IntLow & IntHigh, rest light
    slr_weight <- c(
      Low = 0.2, IntLow = 0.4, Int = 0.75,
      IntHigh = 0.4, High = 0.2
    )
    # Int solid; all others dashed
    slr_dash <- c(
      Low = "dash", IntLow = "dash", Int = "solid",
      IntHigh = "dash", High = "dash"
    )

    # Determine x-axis breaks
    if (dur <= 20) {
      x_breaks <- 0:dur
    } else if (dur <= 50) {
      x_breaks <- seq(0, dur, by = 5)
    } else {
      x_breaks <- seq(0, dur, by = 10)
    }

    if (is.null(mr) || nrow(mr$budget_df) == 0) {
      # No restoration target yet: show baseline growth (gray) if available.
      if (is.null(bg_df)) {
        y_lo <- rap_axis_min(-1)
        y_hi <- if (show_slr) 8 else 4
        bands <- status_bands_df(0, dur, y_lo)
        d0 <- data.frame(Year = 0:dur, RAP = NA_real_)
        p <- ggplot(d0, aes(Year, RAP)) +
          geom_rect(data = bands, inherit.aes = FALSE,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                        fill = fill, text = label), alpha = 0.30) +
          scale_fill_identity() +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(limits = c(y_lo, y_hi),
                             breaks = rap_axis_breaks(y_lo, y_hi)) +
          labs(x = "Years post-restoration", y = "RAP (mm/yr)") +
          theme_minimal(base_size = 14)
        y0_rap <- b$rap
      } else {
        y_lo <- rap_axis_min(min(bg_df$RAP_orig, na.rm = TRUE))
        rap_top <- max(c(bg_df$RAP_orig,
                         if (!is.null(bg_df$RAP_orig_max)) bg_df$RAP_orig_max else NA_real_),
                       na.rm = TRUE)
        y_hi <- if (show_slr) max(slr_ymax, rap_top) else max(4, rap_top)
        bands <- status_bands_df(0, dur, y_lo)
        p <- ggplot(bg_df, aes(x = Year)) +
          geom_rect(data = bands, inherit.aes = FALSE,
                    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                        fill = fill, text = label), alpha = 0.30) +
          scale_fill_identity() +
          # Baseline (original) growth line; hover carries cover + budget.
          # `text` is passed via aes() (not as a bare arg) to avoid the
          # "Ignoring unknown aesthetics: text" console warnings under ggplotly.
          geom_line(aes(y = RAP_orig, group = 1,
                        text = paste0("Baseline growth",
                                      "<br>Year ", Year,
                                      "<br>Coral cover:  ", round(pct_cvr_orig, 1), " %",
                                      "<br>RAP:    ", round(RAP_orig, 2), " mm/yr",
                                      "<br>Budget: ", round(carb_budg_orig, 2), " kg/m²/yr")),
                    linetype = "longdash",
                    color = orig_col, linewidth = 0.7) +
          {
            if (calc_uncert_available &&
                all(c("RAP_orig_min", "RAP_orig_max") %in% names(bg_df))) {
              list(
               geom_ribbon(aes(ymin = RAP_orig_min, ymax = RAP_orig_max),
                    fill = orig_col, alpha = 0.20
                ),
                geom_line(aes(y = RAP_orig_min, group = 11,
                              text = paste0("Baseline lower bound",
                                            "<br>Coral cover:  ", round(pct_cvr_orig_min, 1), " %",
                                            "<br>Year ", Year,
                                            "<br>RAP: ", round(RAP_orig_min, 2), " mm/yr")),
                          linetype = "longdash", color = orig_col,
                          alpha = 0.6, linewidth = 0.35),
                geom_line(aes(y = RAP_orig_max, group = 12,
                              text = paste0("Baseline upper bound",
                                            "<br>Coral cover:  ", round(pct_cvr_orig_max, 1), " %",
                                            "<br>Year ", Year,
                                            "<br>RAP: ", round(RAP_orig_max, 2), " mm/yr")),
                          linetype = "longdash", color = orig_col,
                          alpha = 0.6, linewidth = 0.35)
              )
            }
          } +

          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(limits = c(y_lo, y_hi),
                             breaks = rap_axis_breaks(y_lo, y_hi)) +
          labs(x = "Years post-restoration", y = "RAP (mm/yr)") +
          theme_minimal(base_size = 14)
        y0_rap <- bg_df$RAP_orig[1]
      }
    } else if (is.null(bg_df)) {
      # Model output exists but baseline growth isn't ready yet (cached-load
      # race). Fall back to an empty placeholder rather than indexing a NULL.
      y_lo <- rap_axis_min(-1)
      y_hi <- if (show_slr) 8 else 4
      bands <- status_bands_df(0, dur, y_lo)
      d0 <- data.frame(Year = 0:dur, RAP = NA_real_)
      p <- ggplot(d0, aes(Year, RAP)) +
        geom_rect(data = bands, inherit.aes = FALSE,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = fill, text = label), alpha = 0.30) +
        scale_fill_identity() +
        scale_x_continuous(breaks = x_breaks) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = rap_axis_breaks(y_lo, y_hi)) +
        labs(x = "Years post-restoration", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)
      y0_rap <- b$rap
    } else {
      # budget_df rows 1..(dur+1) map to Years 0..dur
      bd <- mr$budget_df

      d <- data.frame(
        Year      = 0:dur,
        RAP_orig  = bd$RAP_orig,
        RAP_total = bd$RAP_total,
        pct_cvr_orig  = bd$pct_cvr_orig,
        pct_cvr_total = bd$pct_cvr_total,
        pct_cvr_total_min = bd$pct_cvr_total_min,
        pct_cvr_total_max = bd$pct_cvr_total_max,
        carb_budg_orig  = bd$carb_budg_orig,
        carb_budg_total = bd$carb_budg_total,
        RAP_total_min = if (!is.null(bd$RAP_total_min)) bd$RAP_total_min else NA_real_,
        RAP_total_max = if (!is.null(bd$RAP_total_max)) bd$RAP_total_max else NA_real_
      )

      # Cap percent cover reports at 100
      # d$pct_cvr_min <- sapply(d$pct_cvr_min, function(p) min(100, p))
      # d$pct_cvr_max <- sapply(d$pct_cvr_max, function(p) min(100, p))

      write.csv(d, here("cache", "combined_data.csv"))
      pips <- d[d$Year %in% c(0, 1, 5, 10, 20, 50, 100, dur), ]

      y_lo <- rap_axis_min(min(d$RAP_orig, na.rm = TRUE))
      rap_top <- max(c(d$RAP_total, d$RAP_total_max), na.rm = TRUE)
      y_hi <- if (show_slr) max(slr_ymax, rap_top) else max(4, rap_top)
      bands <- status_bands_df(0, dur, y_lo)

      p <- ggplot(d, aes(x = Year)) +
        geom_rect(data = bands, inherit.aes = FALSE,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = fill, text = label), alpha = 0.30) +
        scale_fill_identity() +
        geom_ribbon(aes(ymin = 0.5, ymax = pmax(0.5, RAP_total)), # only draw the ribbon where RAP_total > 0.5
                    fill = "#1f6fd6", alpha = 0.20) +
        # Original RAP contribution (all baseline species' originals)
        geom_line(aes(y = RAP_orig, group = 1,
                      text = paste0("<br>Year ", Year,
                                    "<br>Coral cover:  ", round(pct_cvr_orig, 1), " %",
                                    "<br>RAP:    ", round(RAP_orig, 2), " mm/yr",
                                    "<br>Budget: ", round(carb_budg_orig, 2), " kg/m²/yr")),
                   linetype = "longdash",
                   color = orig_col, linewidth = 0.7) +
          {
          if (calc_uncert_available &&
              all(c("RAP_orig_min", "RAP_orig_max") %in% names(bg_df))) {
            list(
              geom_ribbon(aes(ymin = bg_df$RAP_orig_min, ymax = bg_df$RAP_orig_max),
                  fill = orig_col, alpha = 0.20
              ),
              geom_line(aes(y = bg_df$RAP_orig_min, group = 11,
                            text = paste0("Baseline lower bound",
                                          "<br>Coral cover:  ", round(bg_df$pct_cvr_orig_min, 1), " %",
                                          "<br>Year ", Year,
                                          "<br>RAP: ", round(bg_df$RAP_orig_min, 2), " mm/yr")),
                        linetype = "longdash", color = orig_col,
                        alpha = 0.6, linewidth = 0.35),
              geom_line(aes(y = bg_df$RAP_orig_max, group = 12,
                            text = paste0("Baseline upper bound",
                                          "<br>Coral cover:  ", round(bg_df$pct_cvr_orig_max, 1), " %",
                                          "<br>Year ", Year,
                                          "<br>RAP: ", round(bg_df$RAP_orig_max, 2), " mm/yr")),
                        linetype = "longdash", color = orig_col,
                        alpha = 0.6, linewidth = 0.35)
            )
          }
        } +
        # Total RAP: purple line
        geom_line(aes(y = RAP_total, group = 2,
                      text = paste0("<br>Year ", Year,
                                    "<br>Coral cover:  ", round(pct_cvr_total, 1), " %",
                                    "<br>RAP:    ", round(RAP_total, 2), " mm/yr",
                                  "<br>Budget: ", round(carb_budg_total, 2), " kg/m²/yr")),
                  color = "#7b3fbf", linewidth = 1.1) +
        {
          if (calc_uncert_available &&
              all(c("RAP_total_min", "RAP_total_max") %in% names(d))) {
            list(
              geom_ribbon(aes(ymin = RAP_total_min, ymax = RAP_total_max),
                  fill = "#7b3fbf", alpha = 0.20
              ),
              geom_line(aes(y = RAP_total_min, group = 21,
                            text = paste0("Lower bound",
                                          "<br>Year ", Year,
                                          "<br>Coral cover: ", round(pct_cvr_total_min, 1), " %",
                                          "<br>RAP: ", round(RAP_total_min, 2), " mm/yr")),
                        color = "#7b3fbf", alpha = 0.6, linewidth = 0.55),
              geom_line(aes(y = RAP_total_max, group = 22,
                            text = paste0("Upper bound",
                                          "<br>Year   ", Year,
                                          "<br>Coral cover: ", round(pct_cvr_total_max, 1), " %",
                                          "<br>RAP:   ", round(RAP_total_max, 2), " mm/yr")),
                        color = "#7b3fbf", alpha = 0.6, linewidth = 0.55)
            )
          }
        } +
        geom_point(
          data = pips,
          aes(y = RAP_total, text = paste0(
            "Year ", Year,
            "<br>Projected cover:  ", round(pct_cvr_total, 1), "%",
            "<br>Projected RAP:    ", round(RAP_total, 2), " mm/yr",
            "<br>Projected budget: ", round(carb_budg_total, 2), " kg CaCO₃/m²/yr"
          ),
          customdata = paste(round(pct_cvr_total, 4),
                             round(carb_budg_total, 4),
                             round(RAP_total, 4),
                             Year, sep = " |")),
          size = 4, color = "#7b3fbf"
        ) +
        scale_x_continuous(breaks = x_breaks) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = rap_axis_breaks(y_lo, y_hi)) +
        labs(x = "Year", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)
      y0_rap <- d$RAP_orig[1]
    }

    # Restoration-horizon marker: gray dashed vertical line + hover
    if (dur > horizon) {
      p <- p + geom_vline(
        aes(xintercept = horizon, text = "Restoration horizon"),
        linetype = "dashed", color = "gray50"
      )
    }

    # Bleaching-event markers: thin red vertical lines at the years bleaching
    # occurs, matching simulate_growth's frequency rule (loop i -> Year i-1).
    bfreq <- isolate(.safe_num(input$bleach_events))
    bleach_years <- integer(0)
    if (bfreq > 0) {
      for (i in 1:(dur + 1)) {
        if ((bfreq == 1 && i %% 4 == 0) ||
            (bfreq == 2 && i %% 2 == 0) ||
            (bfreq == 5)) {
          bleach_years <- c(bleach_years, i - 1.5)  # Year = loop index - 1.5 (display "mid-year")
        }
      }
    }
    if (length(bleach_years)) {
      p <- p + geom_vline(
        data = data.frame(bx = bleach_years),
        aes(xintercept = bx, text = "Bleaching event"),
        inherit.aes = FALSE,
        linetype = "solid", color = "red", linewidth = 0.7, alpha = 0.6
      )
    }

    # Add one blue SLR line per scenario, ordered so heavy lines draw on top.
    if (show_slr && !is.null(slr_tl) && nrow(slr_tl) > 0) {
      scn_order <- c("Low", "High", "IntHigh", "IntLow", "Int")
      scn_order <- scn_order[scn_order %in% unique(slr_tl$Scenario)]
      for (scn in scn_order) {
        sd <- slr_tl[slr_tl$Scenario == scn, ]
        p <- p + geom_line(
          data = sd,
          aes(x = Year, y = SLR, group = Scenario,
              text = paste0(Scenario,
                            "<br>Year ", Year,
                            "<br>SLR: ", round(SLR, 2), " mm/yr")),
          color = "#1f6fd6",
          linewidth = unname(slr_weight[scn]),
          linetype = unname(slr_dash[scn]),
          inherit.aes = FALSE
        )
      }
    }

    gp <- plotly::ggplotly(p, tooltip = "text", source = "rest_tl")
    gp <- plotly::event_register(gp, "plotly_hover")

    # Year-0 baseline annotation ----
    # shows CURRENT RAP + cover + budget (always on).
    # Background/border/text respond to Dark Mode.
    cur_budget <- ingested_current_budget()

    # Build plotly layout ----
    gp <- gp |>
      plotly::layout(
        paper_bgcolor = paper_bg,
        plot_bgcolor  = plot_bg,
        font = list(color = font_col),
        xaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        yaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col)
      )


    # Geologic accretion baseline: draw as a data-space trace (renders reliably
    # under ggplotly, unlike a layout shape) at y = 3.1.
    geo_x <- c(0, dur)
    gp <- gp |>
      plotly::add_trace(
        x = geo_x, y = c(3.1, 3.1),
        type = "scatter", mode = "lines",
        line = list(color = "gold", width = 2, dash = "dash"),
        showlegend = FALSE,
        inherit = FALSE
      )

    # Manual legend: each entry is a real two-point line placed far outside the
    # visible x-range (so it never renders in-panel) but still registers a
    # legend swatch. NA-only traces get culled by plotly, so use real coords.
    off_x <- c(-1e6, -1e6 + 1)
    legend_entry <- function(g, name, color, dash = "solid") {
      w <- 2
      plotly::add_trace(
        g, x = off_x, y = c(0, 0), type = "scatter", mode = "lines",
        line = list(color = color, dash = dash, width = w),
        name = name, showlegend = TRUE, inherit = FALSE,
        hoverinfo = "skip"
      )
    }
    # Vertical-tick swatch (for entries that are drawn as vertical lines in the
    # panel: restoration horizon, bleaching events). plotly has no vertical
    # legend line; the "line-ns-open" marker is a short vertical bar.
    legend_entry_vline <- function(g, name, color) {
      plotly::add_trace(
        g, x = off_x, y = c(0, 0), type = "scatter", mode = "markers",
        marker = list(color = color, symbol = "line-ns-open", size = 12,
                      line = list(color = color, width = 2)),
        name = name, showlegend = TRUE, inherit = FALSE,
        hoverinfo = "skip"
      )
    }
    gp <- gp |>
      legend_entry("Baseline RAP  ", "gray", dash = "dash") |>
      legend_entry("Restored RAP  ", "#7b3fbf") |>
      legend_entry("Geologic baseline RAP  ", "gold", dash = "dash") |>
      legend_entry_vline("Restoration horizon  ", "gray")
      if (show_slr) gp <- legend_entry(gp, "Sea-level rise  ", "#1f6fd6", dash = "dash")
      if (length(bleach_years)) gp <- legend_entry_vline(gp, "Bleaching event  ", "red")

    # ggplotly defaults showlegend to FALSE at the layout level; force it on and
    # fix the x-range so the off-canvas legend traces don't expand the axis.
    gp <- gp |> plotly::layout(
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.05, y = 1.08,
                    font = list(color = font_col),
                    bgcolor = "rgba(0,0,0,0)"),
      xaxis = list(range = c(0, dur))
    )

    gp
  })

  ## ---------------------------------------------------------------------------
  ## Auto-populated scenario naming ----
  ## Project name mirrors the selected baseline site. Scenario name follows:
  ##   <Gspe(highest-cover restoration species)>_<freq>B_<dhw>DHW_<horizon>_<duration>
  ## Both refresh as their driving inputs change.
  ## ---------------------------------------------------------------------------
  observeEvent(input$baseline_site, {
    if (nzchar(.safe_num_chr(input$baseline_site))) {
      updateTextInput(session, "scenario_project", value = input$baseline_site)
    }
  }, ignoreInit = TRUE)

  observe({
    # Highest-target-cover species in the restoration mix (Gspe code)
    sp <- mix_species()
    targets <- vapply(sp, function(s) {
      .safe_num(input[[paste0("target_", gsub("[^A-Za-z0-9]", "_", s))]])
    }, numeric(1))
    plants <- vapply(sp, function(s) {
      .safe_num(input[[paste0("count_", gsub("[^A-Za-z0-9]", "_", s))]])
    }, numeric(1))
    top_sp <- if (length(sp) && any(targets > 0)) sp[which.max(targets)] else NA_character_
    # Judget top_sp by outplants if no target present.
    if (is.na(top_sp)) {
      top_sp <- if (length(sp) && any(plants > 0)) sp[which.max(plants)] else NA_character_
    }
    sp_code <- if (!is.na(top_sp)) abbrev_species_code(top_sp) else "NA"

    freq <- .safe_num(input$bleach_events)
    dhw  <- .safe_num(input$dhw)
    hz   <- .safe_num(input$rest_horizon)
    dur  <- .safe_num(input$sim_duration)

    suggested <- paste0(sp_code, "_", freq, "B_", dhw, "DHW_", hz, "_", dur)
    updateTextInput(session, "scenario_name", value = suggested)
  })

  ## ---------------------------------------------------------------------------
  ## Save scenario (Restoration Planning) ----
  ## ---------------------------------------------------------------------------
  observeEvent(input$save_scenario, {
    # Evaluate name inputs independently:
    if (input$scenario_project == "") {
      showNotification("Enter a project name.", type = "error")
      return(NULL)
    }
    if (input$scenario_name == "") {
      showNotification("Enter a scenario name.", type = "error")
      return(NULL)
    }

    # Wire the "Save scenario" button press to also run the simulation,
    # so the appropriate simulated values for the current slider values are saved.
    sim_token(sim_token() + 1)
    write_cached_baseline()
    base_mets <- baseline_metrics()
    mr <- model_result()
    fv <- final_vals()   # baseline/restored evaluated at the restoration horizon

    restored_rap <- fv$r_rap
    baseline_rap <- fv$b_rap
    restored_cvr <- fv$r_cover
    baseline_cvr <- fv$b_cover

    # Compare restored cover and baseline cover at the end of the simulation
    added_cover <- restored_cvr - baseline_cvr
    # Prefer model-derived cost when available; else illustrative fallback
    cost <- if (!is.null(mr)) mr$cost else added_cover * OUTPLANT_COST_DEFAULT * 100
    outplants <- if (!is.null(mr) && length(mr$outplants)) mr$outplants else NA
    elev_gain_10yr <- restored_rap * 10 # mm over 10 years

    # Legacy ROI field retained for backward compatibility; the Scenario
    # Comparison plot now recomputes ROI from net kg CaCO3 / cost at render.
    # roi <- if (cost > 0) (elev_gain_10yr / cost) * 1000 else 0

    # Build the scenario, forcing every field to a length-1 scalar
    scalar1 <- function(x) if (is.null(x) || length(x) == 0) NA else x[[1]]
    scenario <- list(
      project = scalar1(input$scenario_project),
      scenario = scalar1(input$scenario_name),
      site = scalar1(input$baseline_site),
      subregion = scalar1(input$subregion_choice),
      habitat = scalar1(input$habitat_choice),
      site_area_m2 = .safe_num(input$site_area_m2),
      baseline_cover = scalar1(baseline_cvr),
      restored_cover = scalar1(restored_cvr),
      baseline_budget = scalar1(fv$b_budget),
      restored_budget = scalar1(fv$r_budget),
      baseline_rap = scalar1(baseline_rap),
      restored_rap = scalar1(restored_rap),
      outplants = scalar1(outplants),
      dhw = .safe_num(input$dhw),
      bleach_events = .safe_num(input$bleach_events),
      rest_horizon = .safe_num(input$rest_horizon),
      sim_duration = .safe_num(input$sim_duration),
      cost = scalar1(cost),
      # roi = scalar1(roi),
      elev_gain_10yr = scalar1(elev_gain_10yr),
      saved = as.character(Sys.time()),
      # Nested per-species mix (variable-length), keyed by full species name.
      additional_outplant_years = additional_outplant_years(),
      mix = {
        sp <- mix_species()
        ey <- additional_outplant_years()
        setNames(lapply(sp, function(s) {
          s_ <- gsub("[^A-Za-z0-9]", "_", s)
          gv <- function(p, suf = "") { v <- input[[paste0(p, "_", s_, suf)]]; if (is.null(v) || is.na(v)) NA else as.numeric(v) }
          base_list <- list(
            baseline_cover   = .safe_num(input[[paste0("base_", s_)]]),
            target_cover     = gv("target"),
            outplant_diam_cm = gv("diam"),
            outplant_cost    = gv("cost"),
            outplant_count   = gv("count"),
            opc              = gv("opc"),
            morphology       = morph_class(s)
          )
          if (length(ey) > 0) {
            base_list$extra_years <- setNames(lapply(ey, function(yr) {
              list(
                outplant_diam_cm = gv("diam",  paste0("_y", yr)),
                outplant_cost    = gv("cost",  paste0("_y", yr)),
                outplant_count   = gv("count", paste0("_y", yr)),
                opc              = gv("opc",   paste0("_y", yr))
              )
            }), as.character(ey))
          }
          base_list
        }), sp)
      }
    )

    fname <- file.path(
      scenario_dir,
      paste0(
        gsub("[^A-Za-z0-9]", "_", input$scenario_project), "__",
        gsub("[^A-Za-z0-9]", "_", input$scenario_name), ".json"
      )
    )
    write_json(scenario, fname, auto_unbox = TRUE, pretty = TRUE)

    showNotification(
      paste0("Saved scenario '", input$scenario_name, "' under project '", input$scenario_project, "'."),
      type = "message"
    )
  })

  ## ---------------------------------------------------------------------------
  ## Restoration Monitoring tab ----
  ## ---------------------------------------------------------------------------

  # Shared reader: dispatch on extension (.xlsx vs .csv).
  read_monitoring_file <- function(path, name) {
    ext <- tolower(tools::file_ext(name))
    tryCatch(
      if (ext == "xlsx") {
        read_excel_quiet(path, sheet = "Coral Cover input")
      } else {
        read.csv(path, stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(paste("Could not read file:", e$message), type = "error")
        NULL
      }
    )
  }

  # Cache buster: bumped when the user clears the cache so the reactives below
  # re-evaluate and drop any auto-loaded cached data.
  monitoring_cache_token <- reactiveVal(0)

  # ---- Uploaded coral-cover .xlsx (Monitoring) ----
  # Keep the raw datapath to read multiple sheets. Coral cover comes
  # from the default/first sheet; the pipeline reads Years_Post_Restoration +
  # per-Taxon Percent_Cover + Site_Area_m2. Falls back to the cached file.
  monitoring_cover_path <- reactive({
    monitoring_cache_token()
    f <- input$upload_cover
    if (!is.null(f)) {
      ext <- tolower(tools::file_ext(f$name))
      # Cache a copy (preserve extension) for auto-reload next launch
      tryCatch(
        file.copy(f$datapath, paste0(cached_cover_stub, ".", ext), overwrite = TRUE),
        error = function(e) NULL
      )
      return(list(path = f$datapath, ext = ext))
    }
    cp <- find_cached(cached_cover_stub)
    if (!is.null(cp)) return(list(path = cp, ext = tolower(tools::file_ext(cp))))
    NULL
  })

  # Parsed coral-cover data (first sheet). Only .xlsx drives the monitoring
  # pipeline; a .csv is still read for the site dropdown / legacy display.
  uploaded_monitoring_cover <- reactive({
    info <- monitoring_cover_path()
    if (is.null(info)) return(NULL)
    if (info$ext == "xlsx") {
      tryCatch(read_excel_quiet(info$path, sheet = "Coral Cover input"), error = function(e) {
        showNotification(paste("Could not read cover file:", e$message), type = "error")
        NULL
      })
    } else {
      tryCatch(read.csv(info$path, stringsAsFactors = FALSE), error = function(e) {
        showNotification(paste("Could not read cover file:", e$message), type = "error")
        NULL
      })
    }
  })

  # Wire the monitoring "Select site" dropdown.
  #  - Cover file loaded -> populated EXCLUSIVELY from the file's Unique_Site_ID
  #    (first value auto-selected so the pipeline fires immediately).
  #  - No file -> NCRMP df$site_id list (marker clicks can drive selection).
  observe({
    up <- uploaded_monitoring_cover()
    if (!is.null(up) && "Unique_Site_ID" %in% names(up)) {
      site_ids <- unique(as.character(up$Unique_Site_ID[!is.na(up$Unique_Site_ID)]))
      current  <- isolate(input$monitoring_selected_site)
      sel <- if (!is.null(current) && current %in% site_ids) current
             else if (length(site_ids)) site_ids[1] else ""
      updateSelectizeInput(session, "monitoring_selected_site",
        choices = site_ids, selected = sel, server = TRUE)
    } else {
      updateSelectizeInput(session, "monitoring_selected_site",
        choices = unique(df$site_id), server = TRUE)
    }
  })

  # ---- Uploaded observed-bioerosion .xlsx (Monitoring) ----
  # Three sheets: Parrotfish / Urchins / Sponges. Keep the path so all three
  # can be read. Falls back to the cached file.
  monitoring_bioerosion_path <- reactive({
    monitoring_cache_token()
    f <- input$upload_bioerosion
    if (!is.null(f)) {
      ext <- tolower(tools::file_ext(f$name))
      tryCatch(
        file.copy(f$datapath, paste0(cached_bioerosion_stub, ".", ext), overwrite = TRUE),
        error = function(e) NULL
      )
      return(list(path = f$datapath, ext = ext))
    }
    cp <- find_cached(cached_bioerosion_stub)
    if (!is.null(cp)) return(list(path = cp, ext = tolower(tools::file_ext(cp))))
    NULL
  })

  # Parsed observed-bioerosion sheets (list of three data frames or NULLs).
  uploaded_monitoring_bioerosion <- reactive({
    info <- monitoring_bioerosion_path()
    if (is.null(info) || info$ext != "xlsx") return(NULL)
    list(
      Parrotfish = read_sheet_safe(info$path, "Parrotfish"),
      Urchins    = read_sheet_safe(info$path, "Urchins"),
      Sponges    = read_sheet_safe(info$path, "Sponges")
    )
  })

  # Clear cache: delete cached cover + bioerosion files and re-evaluate.
  observeEvent(input$monitoring_clear_cache, {
    removed <- 0
    for (stub in c(cached_cover_stub, cached_bioerosion_stub)) {
      cp <- find_cached(stub)
      if (!is.null(cp) && file.exists(cp)) {
        if (isTRUE(file.remove(cp))) removed <- removed + 1
      }
    }
    monitoring_cache_token(monitoring_cache_token() + 1)
    showNotification(
      paste0("Cleared ", removed, " cached monitoring file(s)."),
      type = "message"
    )
  })

  # Monitoring template downloads (served from \www)
  output$monitoring_cover_template_dl <- downloadHandler(
    filename = function() "Restoration_Monitoring_Cover_TEMPLATE.xlsx",
    content = function(file) {
      file.copy(here("www", "Restoration_Monitoring_Cover_TEMPLATE.xlsx"), file, overwrite = TRUE)
    }
  )
  output$monitoring_bioerosion_template_dl <- downloadHandler(
    filename = function() "Bioerosion_Data_TEMPLATE.xlsx",
    content = function(file) {
      file.copy(here("www", "Bioerosion_Data_TEMPLATE.xlsx"), file, overwrite = TRUE)
    }
  )

  # ---- Observed bioerosion per Years_Post_Restoration ----
  # Returns list(by_year = named numeric [year -> kg/m2/yr], unobserved = chr).
  # Per-taxon-type fallback to regional rates when a sheet is empty. NULL when
  # no observed-bioerosion file is present (caller then uses regional rates).
  monitoring_bioerosion_by_year <- reactive({
    sheets <- uploaded_monitoring_bioerosion()

    if (is.null(sheets)) return(NULL)

    cvr <- uploaded_monitoring_cover()

    # Regional split for per-taxon-type fallback
    site <- input$monitoring_selected_site
    req(!is.null(site), nzchar(site))

    cvr_recs <- cvr[cvr$Unique_Site_ID == site, ]
    hab <- cvr_recs[1, "Habitat"]
    subregion <- cvr_recs[1, "Subregion"]
    reg <- resolve_species_bioerosion(subregion, hab)

    subset_site <- function(x) {
      if (sheet_is_empty(x)) return(x)
      if ("Unique_Site_ID" %in% names(x))
        x[as.character(x$Unique_Site_ID) == site, , drop = FALSE] else x
    }
    pf <- subset_site(sheets$Parrotfish)
    ur <- subset_site(sheets$Urchins)
    sp <- subset_site(sheets$Sponges)

    # Union of all Years_Post_Restoration values across non-empty sheets
    yr_of <- function(x) if (sheet_is_empty(x) || !("Years_Post_Restoration" %in% names(x))) numeric(0) else unique(x$Years_Post_Restoration)
    all_years <- sort(unique(c(yr_of(pf), yr_of(ur), yr_of(sp), yr_of(cvr_recs))))
    if (length(all_years) == 0) return(NULL)

    unobserved_all <- character(0)
    by_year <- setNames(numeric(length(all_years)), as.character(all_years))

    for (yr in all_years) {
      # Parrotfish
      if (!sheet_is_empty(pf)) {
        rows <- pf[pf$Years_Post_Restoration == yr & pf$Life_phase != "JU", , drop = FALSE]
        # If no data for this year, use the closest year:
        # Repeat strategy for urchins & sponges
        if (all(is.na(rows))) {
          i <- nearest_value(pf$Years_Post_Restoration, yr)
          rows <- pf[pf$Years_Post_Restoration == i & pf$Life_phase != "JU", , drop = FALSE]
        }
        pfr <- compute_parrotfish_erosion(rows, sp_erosion_parrotfish)
        pf_val <- pfr$total

        unobserved_all <- c(unobserved_all, pfr$unobserved)
      } else {
        pf_val <- reg$parrotfish
      }
      # Urchins
      ur_val <- if (!sheet_is_empty(ur)) {
        rows <- ur[ur$Years_Post_Restoration == yr, , drop = FALSE]
        if (all(is.na(rows))) {
          i <- nearest_value(ur$Years_Post_Restoration, yr)
          rows <- ur[ur$Years_Post_Restoration == i, , drop = FALSE]
        }
        compute_urchin_erosion(rows, sp_erosion_urchins)
      } else {
        reg$urchin
      }
      # Sponges
      sp_val <- if (!sheet_is_empty(sp)) {
        rows <- sp[sp$Years_Post_Restoration == yr, , drop = FALSE]
        if (all(is.na(rows))) {
          i <- nearest_value(sp$Years_Post_Restoration, yr)
          rows <- sp[sp$Years_Post_Restoration == i, , drop = FALSE]
        }
        compute_sponge_erosion(rows, sp_erosion_sponges)
      } else {
        reg$sponge
      }

      by_year[as.character(yr)] <- .safe0(pf_val) + .safe0(ur_val) + .safe0(sp_val)
    }
    list(by_year = by_year, unobserved = unique(unobserved_all))
  })

  # Red error when an uploaded cover file lacks a nonzero UC substrate value.
    output$monitoring_cover_uc_warning <- renderUI({
      if (isFALSE(monitoring_uc_ok())) {
        showNotification(
          "Error: Uploaded coral cover data does not include unconsolidated substrate.",
          type = "error"
        )
      }
    })

  # Disable the Monitoring download button in the bad-UC state.
  observe({
    if (isFALSE(monitoring_uc_ok())) {
      shinyjs::disable("monitoring_download_report")
    } else {
      shinyjs::enable("monitoring_download_report")
    }
  })

  # Red warning for any unobserved parrotfish size classes
  output$bioerosion_parrotfish_warning <- renderUI({
    bio <- monitoring_bioerosion_by_year()
    if (is.null(bio) || length(bio$unobserved) == 0) return(NULL)
    msgs <- vapply(bio$unobserved, function(u) {
      paste0("An unobserved parrotfish size class has been entered: ", u)
    }, character(1))
    for (m in msgs) {
      showNotification(m, type = "error")
    }
  })

  # Nearest-year bioerosion substitution: for a requested year, return the
  # observed total from the closest Years_Post_Restoration (ties -> past).
  nearest_bioerosion <- function(bio, target_year) {
    if (is.null(bio) || length(bio$by_year) == 0) return(NA_real_)
    ys <- as.numeric(names(bio$by_year))
    if (target_year %in% ys) return(unname(bio$by_year[as.character(target_year)]))
    d <- abs(ys - target_year)
    # Prefer past (smaller year) on ties: order by distance, then by year asc
    idx <- order(d, ys)[1]
    unname(bio$by_year[idx])
  }

  # ---- Monitoring per-year RAP series (file-driven) ----
  # Fires only when a coral-cover .xlsx is present. Iterates each
  # Years_Post_Restoration, computes the carbonate budget from that year's
  # per-Taxon Percent_Cover (area-occupied method), subtracts generalized
  # microbioerosion + bioerosion (observed if present, else regional), and
  # derives RAP with assemblage-based porosity. -1 == Baseline.

  # Does the uploaded cover file carry an unconsolidated-substrate record
  # for the selected site? Monitoring calculations require it.
  monitoring_uc_ok <- reactive({
    info <- monitoring_cover_path()
    if (is.null(info) || info$ext != "xlsx") return(NA)  # not applicable yet
    cover <- uploaded_monitoring_cover()
    if (is.null(cover) || !all(c("Taxon", "Percent_Cover") %in% names(cover))) return(NA)
    site <- input$monitoring_selected_site
    if (is.null(site) || !nzchar(site)) return(NA)
    if ("Unique_Site_ID" %in% names(cover)) {
      cover <- cover[as.character(cover$Unique_Site_ID) == site, , drop = FALSE]
    }
    if (nrow(cover) == 0) return(NA)
    uc_row <- suppressWarnings(as.numeric(
      cover$Percent_Cover[cover$Taxon == "REQUIRED Unconsolidated substrate"]
    ))
    uc_row <- uc_row[!is.na(uc_row)]
    if (length(uc_row) == 0) {
      msg <- paste0("Restoration monitoring site ",
              input$monitoring_selected_site,
              " does not have an 'Unconsolidated sediment' entry."
            )
      log_msg(msg)
      showNotification(msg, type = "error")
      return(FALSE)
    }
    TRUE
  })

  monitoring_series <- reactive({
    with_logged_conditions ({
    info <- monitoring_cover_path()
    if (is.null(info) || info$ext != "xlsx") return(NULL)

    cover <- uploaded_monitoring_cover()
    if (is.null(cover)) return(NULL)

    log_msg(" ============================ ")
    log_msg(
      paste0("Plotting Restoration Monitoring series: ",
        input$monitoring_selected_site,
        "... "
        )
      )
    log_msg(" ============================ \n")

    needed <- c("Years_Post_Restoration", "Taxon", "Percent_Cover", "Unique_Site_ID")
    if (!all(needed %in% names(cover))) {
      log_msg("Error: Restoration Monitoring Coral cover .xlsx is missing a required field.")
      showNotification("Error: Restoration Monitoring Coral cover .xlsx is missing a required field.",
      type = "error")
      return(NULL)
    }

    # Require a valid unconsolidated-substrate entry; otherwise don't fire.
    if (isFALSE(monitoring_uc_ok())) return(NULL)

    monitoring_uc <- cover
    site <- input$monitoring_selected_site
    req(!is.null(site), nzchar(site))
    cover <- cover[as.character(cover$Unique_Site_ID) == site, , drop = FALSE]
    if (nrow(cover) == 0) return(NULL)

    # Site area from the cover .xlsx (Site_Area_m2); default 100
    site_area <- if ("Site_Area_m2" %in% names(cover) && any(!is.na(cover$Site_Area_m2))) {
      .safe0(cover$Site_Area_m2[!is.na(cover$Site_Area_m2)][1])
    } else {
      100
    }
    if (site_area <= 0) site_area <- 100

    bio <- monitoring_bioerosion_by_year()  # NULL -> regional fallback below

    # Regional (per-m2) bioerosion for the no-observed-file case
    reg_total <- resolve_regional_bioerosion(input$subregion_choice, input$habitat_choice)

    years <- sort(unique(cover$Years_Post_Restoration))
    out <- data.frame()

    for (yr in years) {
      rows <- cover[cover$Years_Post_Restoration == yr, , drop = FALSE]

      # Per-taxon cover df for porosity + budget
      cover_df <- data.frame(
        taxon = as.character(rows$Taxon),
        cvr   = as.numeric(rows$Percent_Cover),
        stringsAsFactors = FALSE
      )
      cover_df$cvr[is.na(cover_df$cvr)] <- 0

      por <- assemblage_porosity(cover_df, "cvr")

      # Area-occupied patch calcification flux (kg CaCO3/yr), then per-m2
      patch_budget <- 0
      for (i in seq_len(nrow(cover_df))) {
        s   <- cover_df$taxon[i]
        cvr <- cover_df$cvr[i]
        rate <- calc_rates$rate[calc_rates$Taxon == s]
        if (length(rate) == 0 || is.na(rate[1])) next
        patch_budget <- patch_budget + site_area * (cvr / 100) * rate[1]
      }
      gross_budget <- patch_budget / site_area

      # Bounded gross budgets (calcification bounds only; no bioerosion band
      # on the Monitoring tab). Fall back to the mean when bounds are absent.
      patch_budget_min <- 0
      patch_budget_max <- 0
      for (i in seq_len(nrow(cover_df))) {
        s   <- cover_df$taxon[i]
        cvr <- cover_df$cvr[i]
        rate <- calc_rates$rate[calc_rates$Taxon == s]
        if (length(rate) == 0 || is.na(rate[1])) next
        b <- calc_rate_bounds(s)
        cr_lo <- if (is.finite(b[1])) b[1] else rate[1]
        cr_hi <- if (is.finite(b[2])) b[2] else rate[1]
        patch_budget_min <- patch_budget_min + site_area * (cvr / 100) * cr_lo
        patch_budget_max <- patch_budget_max + site_area * (cvr / 100) * cr_hi
      }
      gross_budget_min <- patch_budget_min / site_area
      gross_budget_max <- patch_budget_max / site_area

      # Bioerosion for this year: observed (nearest-year substitution) or regional
      bio_year <- if (!is.null(bio)) nearest_bioerosion(bio, yr) else reg_total
      if (is.na(bio_year)) bio_year <- reg_total

      # Net budget = gross - generalized micro - (observed/regional) macro
      be_micro_effect <- cover_df$cvr[cover_df$taxon == "REQUIRED Unconsolidated substrate"] * be_micro_rate
      net_budget <- gross_budget - be_micro_effect - .safe0(bio_year)
      rap <- net_budget / 2.9 / (1 - por)
      # Bounded RAP uses the SAME average bioerosion (calc uncertainty only)
      net_budget_min <- gross_budget_min - be_micro_effect - .safe0(bio_year)
      net_budget_max <- gross_budget_max - be_micro_effect - .safe0(bio_year)
      rap_min <- net_budget_min / 2.9 / (1 - por)
      rap_max <- net_budget_max / 2.9 / (1 - por)
      total_coral_pct_cvr <- sum(cover_df$cvr, na.rm = TRUE)

      out <- rbind(out, data.frame(
        Year = yr, RAP = rap, budget = net_budget, cover = total_coral_pct_cvr,
        RAP_min = rap_min, RAP_max = rap_max,
        stringsAsFactors = FALSE
      ))
    }
    out <- out[order(out$Year), ]
    # print(out)
    # out
  }) })

  # Helper: baseline metrics for the selected NCRMP site (upload overrides df).
  # When the monitoring pipeline is active, "Baseline" is the -1 row.
  monitoring_baseline_vals <- reactive({
    # Uploaded cover file present but missing a valid UC value: go blank.
    if (isFALSE(monitoring_uc_ok())) return(NULL)
    ms <- monitoring_series()
    if (!is.null(ms)) {
      # Use the minimum present year as the "Baseline" year
      r <- ms[which.min(ms$Year), ] # [1, ]
      return(list(cover = r$cover, budget = r$budget, rap = r$RAP))
    }

    req(input$monitoring_selected_site)
    dat <- df |> filter(site_id == input$monitoring_selected_site) |> slice(1)
    rap <- if (!is.null(dat$rap) && !is.na(dat$rap)) dat$rap else dat$net_G / 2.9 / (1 - 0.6265)
    list(
      cover  = dat$hardCoral_PrctCvr,
      budget = dat$net_G,
      rap    = rap
    )
  })

  # Helper: restored metrics.
  #  - Monitoring pipeline active -> the LAST (max year) row is "Restored".
  #  - No cover upload (map-driven) -> project the SELECTED SITE's own baseline
  #    with the Home-tab linear model + the map's target-cover inputs.
  monitoring_restored_vals <- reactive({
    if (isFALSE(monitoring_uc_ok())) return(NULL)
    ms <- monitoring_series()
    if (!is.null(ms) && nrow(ms) > 0) {
      r <- ms[which.max(ms$Year), ]
      return(list(cover = r$cover, budget = r$budget, rap = r$RAP))
    }

    base <- monitoring_baseline_vals()
    inc <- if (is.null(input$target_cover_increase)) 0 else input$target_cover_increase
    restored_rap    <- base$rap + cover_rap_slope * inc
    restored_cover  <- base$cover + inc
    restored_budget <- restored_rap * 2.9 * (1 - 0.6265)
    list(cover = restored_cover, budget = restored_budget, rap = restored_rap)
  })

  output$monitoring_baseline_cover <- renderValueBox({
    req(monitoring_baseline_vals())
    valueBox(paste0(round(monitoring_baseline_vals()$cover, 1), " %"),
      div(class = "bordered-text", "Coral cover"),
      icon = icon("percent"), color = "green"
    )
  })
  output$monitoring_baseline_budget <- renderValueBox({
    req(monitoring_baseline_vals())
    valueBox(paste0(round(monitoring_baseline_vals()$budget, 2), " kg/m²/yr"),
      div(class = "bordered-text", "Carbonate budget"),
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$monitoring_baseline_rap <- renderValueBox({
    req(monitoring_baseline_vals())
    valueBox(paste0(round(monitoring_baseline_vals()$rap, 2), " mm/yr"),
      div(class = "bordered-text",
        HTML(paste0("Reef accretion potential<br/>",
          "(this reef is ",
          "<span style='color:", # color determined by conditional below
          if (monitoring_baseline_vals()$rap >= 0.5) "forestgreen" else if (monitoring_baseline_vals()$rap <= -0.5) "lightcoral" else "orange",
          ";'>",
          if (monitoring_baseline_vals()$rap >= 0.5) "growing" else if (monitoring_baseline_vals()$rap <= -0.5) "eroding" else "in stasis",
          "</span>",
          ")"
          )
        )
      ),
      icon = icon("chart-line"), color = "aqua"
    )
  })

  # Hovered-pip Restored readout for the Monitoring timeline. Persists until the
  # next pip hover; cleared when the selected site changes so a stale hover
  # doesn't linger against a different series.
  monitor_restored_hover <- reactiveVal(NULL)

  observeEvent(plotly::event_data("plotly_hover", source = "monitor_tl"), {
    ev <- plotly::event_data("plotly_hover", source = "monitor_tl")
    cd <- ev$customdata
    if (is.null(cd) || length(cd) == 0 || is.na(cd[1])) return()
    parts <- strsplit(as.character(cd[1]), "\\|")[[1]]
    parts <- trimws(parts)
    num <- suppressWarnings(as.numeric(parts))
    if (length(num) != 4) return()
    monitor_restored_hover(list(
      cover  = num[1],   # may be NA (map-driven)
      budget = num[2],   # may be NA (map-driven)
      rap    = num[3],
      year   = num[4]
    ))
  }, ignoreInit = TRUE)

  # Clear hover when the site changes.
  observeEvent(input$monitoring_selected_site, {
    monitor_restored_hover(NULL)
  }, ignoreInit = TRUE)

  # Current Restored readout: prefer the last pip hover, else the max-year
  # restored values. NA fields in a hover fall back to the restored defaults
  # (covers the map-driven case where cover/budget aren't per-year).
  monitoring_restored_current <- reactive({
    base_r <- monitoring_restored_vals()
    h <- monitor_restored_hover()
    if (is.null(h)) {
      if (is.null(base_r)) return(NULL)
      return(list(cover = base_r$cover, budget = base_r$budget,
                  rap = base_r$rap, year = NA))
    }
    list(
      cover  = if (is.na(h$cover))  (if (!is.null(base_r)) base_r$cover  else NA) else h$cover,
      budget = if (is.na(h$budget)) (if (!is.null(base_r)) base_r$budget else NA) else h$budget,
      rap    = h$rap,
      year   = h$year
    )
  })

  output$monitoring_restored_cover <- renderValueBox({
    cur <- monitoring_restored_current()
    cvr <- if (is.null(cur)) NULL else cur$cover
    valueBox(if (is.null(cvr) || is.na(cvr)) "NA" else paste0(round(cvr, 1), " %"),
      div(class = "bordered-text", "Coral cover"),
      icon = icon("plus-circle"), color = "olive"
    )
  })
  output$monitoring_restored_budget <- renderValueBox({
    cur <- monitoring_restored_current()
    budg <- if (is.null(cur)) NULL else cur$budget
    valueBox(if (is.null(budg) || is.na(budg)) "NA" else paste0(round(budg, 2), " kg/m²/yr"),
      div(class = "bordered-text", "Carbonate budget"),
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$monitoring_restored_rap <- renderValueBox({
    cur <- monitoring_restored_current()
    rap <- if (is.null(cur)) NULL else cur$rap
    valueBox(if (is.null(rap) || is.na(rap)) "NA" else paste0(round(rap, 2), " mm/yr"),
      div(class = "bordered-text",
        HTML(if (is.null(rap) || is.na(rap)) {
          "Reef accretion potential<br/><br/>"
        } else {
          paste0("Reef accretion potential<br/>",
            "(this reef is ",
            "<span style='color:",
            if (rap >= 0.5) "forestgreen" else if (rap <= -0.5) "lightcoral" else "orange",
            ";'>",
            if (rap >= 0.5) "growing" else if (rap <= -0.5) "eroding" else "in stasis",
            "</span>)"
          )
        })
      ),
      icon = icon("chart-line"), color = "teal"
    )
  })

  # Restored title mirrors the Outplanting tab: "Restored: Year X" when a
  # file-driven monitoring series exists (X = latest observed year); otherwise
  # the map-driven 10-year projection -> "Restored: Year 10". Falls back to
  # "Restored" when nothing is available.
  output$monitoring_restored_title <- renderText({
    if (isFALSE(monitoring_uc_ok())) return("Restored")
    cur <- monitoring_restored_current()
    if (is.null(cur)) return("Restored")
    yr <- cur$year
    if (!is.null(yr) && !is.na(yr)) {
      lbl <- if (round(yr) == -1) "Baseline" else paste0("Year ", round(yr))
      return(paste0("Restored: ", lbl))
    }
    # No hover yet: label by the series' terminal year.
    ms <- monitoring_series()
    if (!is.null(ms) && nrow(ms) > 0) {
      ty <- round(max(ms$Year, na.rm = TRUE))
      if (is.finite(ty)) return(paste0("Restored: Year ", ty))
    }
    if (!is.null(monitoring_restored_vals())) return("Restored: Year 10")
    "Restored"
  })

  # Impact summary text box (reuses shared builder)
  output$monitoring_impact_summary <- renderUI({
    req(monitoring_baseline_vals(), monitoring_restored_vals())
    b <- monitoring_baseline_vals()
    r <- monitoring_restored_vals()
    build_impact_summary(
      label = paste0("Site: ", input$monitoring_selected_site),
      b_cover = b$cover, r_cover = r$cover,
      b_budget = b$budget, r_budget = r$budget,
      b_rap = b$rap, r_rap = r$rap
    )
  })

  # Timeline: RAP over the simulation duration with SLR reference lines (plotly).
  #  - Cover .xlsx uploaded  -> real per-year RAP series (Baseline at x=-1).
  #  - No upload (map-driven) -> smooth interpolation Baseline (Y0) -> Restored (Y10).
  # Reference lines (geologic + Int rates) + y-axis floor + dark-mode shared.
  output$monitoring_timeline <- plotly::renderPlotly({
    req(!isFALSE(monitoring_uc_ok()))
    ms <- monitoring_series()

    dark <- isTRUE(input$dark_mode)
    paper_bg <- if (dark) "#232a33" else "white"
    plot_bg  <- if (dark) "#232a33" else "white"
    font_col <- if (dark) "#e6e6e6" else "#333333"
    grid_col <- if (dark) "#5a6472" else "#d9d9d9"

    # Int reference rates (mm/yr) at 2030 / 2050 / 2070
    slr_refs <- c(
      "Int @2030" = Int_rate_at(2030),
      "Int @2050" = Int_rate_at(2050),
      "Int @2070" = Int_rate_at(2070)
    )
    geo_baseline <- 3.1

    if (!is.null(ms) && nrow(ms) > 0) {
      # ---- File-driven: real per-year RAP series (Baseline at x = -1) ----
      x_min <- min(ms$Year)
      x_max <- max(ms$Year)
      # x breaks: label -1 as "Baseline", then integer years
      x_vals   <- sort(unique(ms$Year))
      x_labels <- ifelse(x_vals == -1, "Baseline", as.character(x_vals))

      slr_for_axis <- if (isTRUE(input$monitoring_show_slr)) slr_refs else numeric(0)
      data_min <- min(c(ms$RAP, ms$RAP_min, geo_baseline, slr_for_axis, -0.5), na.rm = TRUE)
      y_lo <- rap_axis_min(data_min)
      y_hi <- max(c(ms$RAP, ms$RAP_max, geo_baseline, slr_for_axis), na.rm = TRUE) + 1
      bands <- status_bands_df(x_min, x_max, y_lo)

      ribbon_df <- insert_threshold_crossings(ms, xcol = "Year", threshold = 0.5)

      p <- ggplot(ms, aes(x = Year)) +
        geom_rect(data = bands, inherit.aes = FALSE,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = fill, text = label), alpha = 0.30) +
        scale_fill_identity() +
        geom_ribbon(data = ribbon_df, inherit.aes = FALSE,
                    aes(x = Year, ymin = 0.5, ymax = ribbon_max),
                    fill = "#1f6fd6", alpha = 0.20) +
        geom_line(aes(y = RAP, group = 1,
                      text = paste0(ifelse(Year == -1, "Baseline", paste0("Year ", Year)),
                                    "<br>RAP: ", round(RAP, 2), " mm/yr",
                                    "<br>Coral cover: ", round(cover, 1), " %",
                                    "<br>Budget: ", round(budget, 2), " kg/m²/yr")),
                  color = "#7b3fbf", linewidth = 1.4) +
        {
          if (calc_uncert_available &&
              all(c("RAP_min", "RAP_max") %in% names(ms))) {
            list(
              geom_ribbon(aes(ymin = RAP_min, ymax = RAP_max),
                  fill = "#7b3fbf", alpha = 0.20
              ),
              geom_line(aes(y = RAP_min, group = 91,
                            text = paste0("Lower bound",
                                          "<br>RAP: ", round(RAP_min, 2), " mm/yr")),
                        color = "#7b3fbf", alpha = 0.6, linewidth = 0.7),
              geom_line(aes(y = RAP_max, group = 92,
                            text = paste0("Upper bound",
                                          "<br>RAP: ", round(RAP_max, 2), " mm/yr")),
                        color = "#7b3fbf", alpha = 0.6, linewidth = 0.7)
            )
          }
        } +
        geom_point(aes(y = RAP,
                       text = paste0(
                         ifelse(Year == -1, "Baseline", paste0("Year ", Year)),
                         "<br>RAP: ", round(RAP, 2), " mm/yr"),
                       customdata = paste(round(cover, 4),
                                          round(budget, 4),
                                          round(RAP, 4),
                                          Year, sep = " |")),
                   color = "#7b3fbf", size = 3) +
        scale_x_continuous(breaks = x_vals, labels = x_labels) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = rap_axis_breaks(y_lo, y_hi)) +
        labs(x = "Years post-restoration", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)

      band_x <- c(x_min, x_max)
    } else {
      # ---- Map-driven: smooth interpolation Baseline (Y0) -> Restored (Y10) ----
      b <- monitoring_baseline_vals()
      r <- monitoring_restored_vals()
      dur <- 10
      years <- 0:dur
      rap_series <- b$rap + (r$rap - b$rap) * (years / dur)
      tl <- data.frame(Year = years, RAP = rap_series, stringsAsFactors = FALSE)

      slr_for_axis <- if (isTRUE(input$monitoring_show_slr)) slr_refs else numeric(0)
      data_min <- min(c(tl$RAP, geo_baseline, slr_for_axis, -0.5), na.rm = TRUE)
      y_lo <- rap_axis_min(data_min)
      y_hi <- max(c(tl$RAP, geo_baseline, slr_for_axis), na.rm = TRUE) + 1
      bands <- status_bands_df(0, dur, y_lo)

      ribbon_df <- insert_threshold_crossings(tl, xcol = "Year", threshold = 0.5)

      p <- ggplot(tl, aes(x = Year)) +
        geom_rect(data = bands, inherit.aes = FALSE,
                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = fill, text = label), alpha = 0.30) +
        scale_fill_identity() +
        geom_ribbon(data = ribbon_df, inherit.aes = FALSE,
                    aes(x = Year, ymin = 0.5, ymax = ribbon_max),
                    fill = "#1f6fd6", alpha = 0.20) +
        geom_line(aes(y = RAP, group = 1,
                      text = paste0("Year ", Year,
                                    "<br>RAP: ", round(RAP, 2), " mm/yr")),
                  color = "#7b3fbf", linewidth = 1.4) +
        geom_point(aes(y = RAP,
                       text = paste0("Year ", Year,
                                     "<br>RAP: ", round(RAP, 2), " mm/yr"),
                       customdata = paste(NA, NA, round(RAP, 4), Year, sep = " |")),
                   color = "#7b3fbf", size = 3) +
        scale_x_continuous(breaks = years) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = rap_axis_breaks(y_lo, y_hi)) +
        labs(x = "Years post-restoration", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)

      band_x <- c(0, dur)
    }

    gp <- plotly::ggplotly(p, tooltip = "text", source = "monitor_tl") |>
      plotly::layout(
        paper_bgcolor = paper_bg,
        plot_bgcolor  = plot_bg,
        font = list(color = font_col),
        xaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        yaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        legend = list(orientation = "h", x = 0, y = 1.1)
      )
    gp <- plotly::event_register(gp, "plotly_hover")

    # Geologic baseline (gold dashed)
    gp <- gp |>
      plotly::add_trace(
        x = band_x, y = c(geo_baseline, geo_baseline),
        type = "scatter", mode = "lines",
        line = list(color = "gold", width = 2, dash = "dash"),
        showlegend = FALSE, inherit = FALSE,
        hoverinfo = "text",
        text = paste0("Geologic baseline RAP: ", geo_baseline, " mm/yr")
      )

    # Int reference rates (blue dashed) at 2030 / 2050 / 2070 — only when toggled.
    slr_ann_col <- "#1f6fd6"
    if (isTRUE(input$monitoring_show_slr)) {
      for (nm in names(slr_refs)) {
        yv <- slr_refs[[nm]]
        if (is.na(yv)) next
        gp <- gp |>
          plotly::add_trace(
            x = band_x, y = c(yv, yv),
            type = "scatter", mode = "lines",
            line = list(color = slr_ann_col, width = 1.5, dash = "dash"),
            showlegend = FALSE, inherit = FALSE,
            hoverinfo = "text",
            text = paste0(nm, ": ", round(yv, 2), " mm/yr")
          ) |>
          plotly::add_annotations(
            x = 0.02, y = yv, xref = "paper", yref = "y",
            text = paste0(nm, ": ", round(yv, 2), " mm/yr"),
            showarrow = FALSE, yshift = 9, xanchor = "left",
            font = list(color = slr_ann_col, size = 10),
            bgcolor = paper_bg, opacity = 0.85
          )
      }
    }

    gp
  })

  # Download report for the Restoration Monitoring tab
  output$monitoring_download_report <- downloadHandler(
    filename = function() {
      paste0("carbonate_report_", input$monitoring_selected_site, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      ms <- monitoring_series()
      if (!is.null(ms) && nrow(ms) > 0) {
        out <- data.frame(
          Years_Post_Restoration = ms$Year,
          total_coral_cover_pct = ms$cover,
          Net_Budget_kg_m2_yr = ms$budget,
          RAP_mm_yr = ms$RAP
        )
        write.csv(out, file, row.names = FALSE)
      } else {
        b <- monitoring_baseline_vals()
        r <- monitoring_restored_vals()
        out <- data.frame(
          Site = input$monitoring_selected_site,
          Metric = c("Coral cover (%)", "Carbonate budget (kg/m²/yr)", "Reef accretion potential (mm/yr)"),
          Baseline = c(b$cover, b$budget, b$rap),
          Restored = c(r$cover, r$budget, r$rap)
        )
        out$Change <- out$Restored - out$Baseline
        write.csv(out, file, row.names = FALSE)
      }
    }
  )

  ## ---------------------------------------------------------------------------
  ## Scenario Comparison tab ----
  ## ---------------------------------------------------------------------------

  # Read all saved scenario .json files (sanitized to one clean row each)
  all_scenarios <- reactive({
    input$sc_refresh
    input$save_scenario # refresh after a save
    files <- list.files(scenario_dir, pattern = "\\.json$", full.names = TRUE)
    if (length(files) == 0) {
      return(data.frame())
    }
    rows <- lapply(files, function(f) {
      s <- tryCatch(fromJSON(f), error = function(e) NULL)
      scenario_to_row(s)
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0) return(data.frame())
    do.call(rbind, rows)
  })

  # Populate the project selector
  observe({
    sc <- all_scenarios()
    projects <- if (nrow(sc)) sort(unique(sc$project)) else character(0)
    updateSelectInput(session, "sc_project", choices = projects)
  })

  # Populate the scenario multi-select based on chosen project.
  # All scenarios within the selected project are ENABLED (selected) by default.
  observe({
    sc <- all_scenarios()
    req(input$sc_project)
    scen <- if (nrow(sc)) sort(unique(sc$scenario[sc$project == input$sc_project])) else character(0)
    cmap <- sc_color_map()

    if (length(scen) == 0) {
      updateCheckboxGroupInput(session, "sc_scenarios",
                               choiceNames = list(), choiceValues = list(),
                               selected = character(0))
      return()
    }

    # choiceNames accepts HTML/tag objects and renders them as markup;
    # choiceValues carries the plain scenario name used as the input value.
    choice_names <- lapply(scen, function(s) {
      col <- if (s %in% names(cmap)) cmap[[s]] else "#cccccc"
      tags$span(
        tags$span(style = paste0(
          "display:inline-block; width:12px; height:12px; border:1px solid #888; ",
          "border-radius:2px; margin-right:6px; vertical-align:middle; background:", col, ";"
        )),
        tags$span(style = "vertical-align:middle;", s)
      )
    })

    updateCheckboxGroupInput(session, "sc_scenarios",
                             choiceNames = choice_names,
                             choiceValues = as.list(scen),
                             selected = scen)
  })

  # ---- Session-persistent scenario color map ----
  # Colors are assigned by scenario name once and retained for the session, so
  # toggling a scenario on/off never reshuffles the palette. New names get the
  # next unused color from the (red/green-excluded) pastel pool.
  sc_color_map <- reactiveVal(setNames(character(0), character(0)))

  observe({
    sc <- all_scenarios()
    if (!nrow(sc)) return()
    all_names <- sort(unique(sc$scenario))
    cmap <- sc_color_map()
    missing <- setdiff(all_names, names(cmap))
    if (length(missing) == 0) return()

    used  <- unname(cmap)
    avail <- setdiff(scenario_pastel_pool, used)
    if (length(avail) < length(missing)) {
      # Extend by interpolation if we run out of distinct swatches
      avail <- setdiff(colorRampPalette(scenario_pastel_pool)(length(cmap) + length(missing)), used)
    }
    new_cols <- setNames(avail[seq_along(missing)], missing)
    sc_color_map(c(cmap, new_cols))
  })

  # Filtered scenarios for plotting (coerce numeric cols used by the plots)
  sc_selected <- reactive({
    sc <- all_scenarios()
    req(nrow(sc) > 0, input$sc_project, input$sc_scenarios)
    d <- sc[sc$project == input$sc_project & sc$scenario %in% input$sc_scenarios, ]
    num_cols <- c("cost", "roi", "restored_rap", "elev_gain_10yr",
                  "baseline_cover", "restored_cover", "baseline_budget",
                  "restored_budget", "baseline_rap", "site_area_m2")
    for (col in num_cols) {
      if (col %in% names(d)) d[[col]] <- as.numeric(d[[col]])
    }
    # Net kg CaCO3 (added product of restoration at the end of the simulation):
    #   (restored_budget - baseline_budget) * site_area_m2  [no sim_duration]
    d$net_kg <- (d$restored_budget - d$baseline_budget) * d$site_area_m2
    # ROI redefined as net kg CaCO3 per dollar
    d$roi_kg_per_dollar <- ifelse(d$cost > 0, d$net_kg / d$cost, NA_real_)
    d
  })

  # Session-persistent pastel color mapping for the selected scenarios.
  # Reads from sc_color_map so colors never shift when a scenario is toggled.
  sc_colors <- reactive({
    d <- sc_selected()
    if (nrow(d) == 0) return(character(0))
    cmap <- sc_color_map()
    nm   <- sort(unique(d$scenario))
    have <- nm[nm %in% names(cmap)]
    miss <- setdiff(nm, names(cmap))
    out  <- cmap[have]
    if (length(miss)) out <- c(out, setNames(scenario_palette(miss), miss))
    out
  })

  # Shared ggplot dark-mode theme add-on for the Scenario Comparison plots.
  sc_theme <- reactive({
    dark <- isTRUE(input$dark_mode)
    font_col <- if (dark) "#e6e6e6" else "#333333"
    grid_col <- if (dark) "#5a6472" else "#d9d9d9"
    plot_bg  <- if (dark) "#232a33" else "white"
    list(
      theme_minimal(base_size = 14) +
        theme(
          plot.background   = element_rect(fill = plot_bg, color = NA),
          panel.background  = element_rect(fill = plot_bg, color = NA),
          panel.grid.major  = element_line(color = grid_col),
          panel.grid.minor  = element_line(color = grid_col),
          text        = element_text(color = font_col),
          axis.text   = element_text(color = font_col),
          axis.title  = element_text(color = font_col),
          legend.text = element_text(color = font_col),
          legend.title = element_text(color = font_col)
        )
    )
  })

  # Scenario comparison table (replaces the collapsible impact-summary cards).
  # One row per selected scenario; columns are the key comparison metrics with
  # baseline -> restored deltas computed inline.
  output$sc_compare_dt <- DT::renderDT({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))

    b_pct <- vapply(d$baseline_rap, rap_percentile, numeric(1))
    r_pct <- vapply(d$restored_rap, rap_percentile, numeric(1))

    tbl <- data.frame(
      Scenario            = d$scenario,
      Site                = d$site,
      Habitat             = d$habitat,
      `Baseline cover (%)`   = round(d$baseline_cover, 1),
      `Restored cover (%)`   = round(d$restored_cover, 1),
      `Δ cover (%)`          = round(d$restored_cover - d$baseline_cover, 1),
      `Baseline budget`      = round(d$baseline_budget, 2),
      `Restored budget`      = round(d$restored_budget, 2),
      `Δ budget`             = round(d$restored_budget - d$baseline_budget, 2),
      `Baseline RAP (mm/yr)` = round(d$baseline_rap, 2),
      `Restored RAP (mm/yr)` = round(d$restored_rap, 2),
      `Δ RAP (mm/yr)`        = round(d$restored_rap - d$baseline_rap, 2),
      `Baseline pctile`      = round(b_pct),
      `Restored pctile`      = round(r_pct),
      `Outplants`            = suppressWarnings(as.integer(d$outplants)),
      `Cost ($)`             = round(as.numeric(d$cost)),
      `Net kg CaCO₃`         = round(d$net_kg),
      `ROI (kg/$)`           = round(d$roi_kg_per_dollar, 3),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      tbl,
      rownames = FALSE,
      extensions = c("FixedColumns"),
      options = list(
        scrollX = TRUE,
        scrollY = "45vh",
        fixedColumns = list(leftColumns = 1),
        scrollCollapse = TRUE,
        paging = FALSE,
        dom = "t",
        order = list(list(0, "asc"))
      )
    )
  })

  # Project cost bar (pastel per scenario, dark-mode aware)
  output$sc_cost_bar <- renderPlot({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    cols <- sc_colors()
    ggplot(d, aes(x = scenario, y = cost, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "Project cost ($)") +
      scale_fill_manual(values = cols) +
      sc_theme()[[1]] +
      theme(legend.position = "none", axis.text.x = element_blank())
      # To use angled scenario names as x-axis labels instead: element_text(angle = 30, hjust = 1))
  }, bg = "transparent")

  # ROI bar: net kg CaCO3 per dollar (pastel per scenario, dark-mode aware)
  output$sc_roi_bar <- renderPlot({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    cols <- sc_colors()
    ggplot(d, aes(x = scenario, y = roi_kg_per_dollar, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "ROI (net kg CaCO\u2083/$)") +
      scale_fill_manual(values = cols) +
      sc_theme()[[1]] +
      theme(legend.position = "none", axis.text.x = element_blank())
  }, bg = "transparent")

  # Per-scenario RAP bar with reference lines + status bands.
  # Bands ride through tooltip="text" as "erosion"/"stasis".
  output$sc_rap_bar <- plotly::renderPlotly({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    cols <- sc_colors()

    dark <- isTRUE(input$dark_mode)
    paper_bg <- if (dark) "#232a33" else "white"
    font_col <- if (dark) "#e6e6e6" else "#333333"
    grid_col <- if (dark) "#5a6472" else "#d9d9d9"

    geo_baseline <- 3.1
    slr_refs <- c(
      "Int @2030" = Int_rate_at(2030),
      "Int @2050" = Int_rate_at(2050),
      "Int @2070" = Int_rate_at(2070)
    )

    slr_for_axis <- if (isTRUE(input$sc_show_slr)) slr_refs else numeric(0)
    data_min <- min(c(d$restored_rap, geo_baseline, slr_for_axis, -0.5), na.rm = TRUE)
    y_lo <- rap_axis_min(data_min)
    y_hi <- max(c(d$restored_rap, geo_baseline, slr_for_axis), na.rm = TRUE) + 1

    d$scenario <- factor(d$scenario, levels = d$scenario)
    n_sc <- nrow(d)
    # geom_rect status bands spanning the full categorical width
    bands <- status_bands_df(0.4, n_sc + 0.6, y_lo)

    p <- ggplot(d, aes(x = scenario, y = restored_rap, fill = scenario)) +
      geom_col(aes(text = paste0(scenario,
                                 "<br>Restored RAP: ", round(restored_rap, 2), " mm/yr")),
               alpha = 1, width = 0.7) +
      # Removed bands; they draw in front of the scenario bars. Changed to lines.
      # geom_rect(data = bands, inherit.aes = FALSE,
      #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
      #               fill = fill, text = label), alpha = 0.30) +
      # scale_fill_manual(values = c(cols, setNames(c("red", "yellow"), c("red", "yellow")))) +
      scale_fill_manual(values = cols) +
      scale_y_continuous(limits = c(y_lo, y_hi), breaks = rap_axis_breaks(y_lo, y_hi)) +
      labs(x = NULL, y = "Restored RAP (mm/yr)") +
      sc_theme()[[1]] +
      theme(legend.position = "none", axis.text.x = element_blank())

    gp <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = paper_bg, plot_bgcolor = paper_bg,
        font = list(color = font_col),
        xaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        yaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col)
      )

    # Geologic baseline always drawn; SLR Int rates only when toggled on.
    # ref_df expanded to include stasis and erosion thresholds:
    ref_df <- data.frame(
      label = c("Geologic baseline", "Stasis", "Erosion"),
      yval  = c(geo_baseline, 0.5, -0.5),
      col   = c("gold", "orange", "red"),
      stringsAsFactors = FALSE
    )
    if (isTRUE(input$sc_show_slr)) {
      ref_df <- rbind(ref_df, data.frame(
        label = names(slr_refs),
        yval  = unname(slr_refs),
        col   = rep("#1f6fd6", length(slr_refs)),
        stringsAsFactors = FALSE
      ))
    }
    ref_df <- ref_df[is.finite(ref_df$yval), ]
    for (k in seq_len(nrow(ref_df))) {
      gp <- gp |>
        plotly::add_trace(
          x = c(0.4, n_sc + 0.6), y = c(ref_df$yval[k], ref_df$yval[k]),
          type = "scatter", mode = "lines",
          line = list(color = ref_df$col[k], width = 1.4, dash = "dash"),
          showlegend = FALSE, inherit = FALSE, hoverinfo = "text",
          text = paste0(ref_df$label[k], ": ", round(ref_df$yval[k], 2), " mm/yr")
        )
    }
    gp
  })
  # Download the selected scenarios as a .csv report
  output$sc_download_csv <- downloadHandler(
    filename = function() {
      paste0("scenario_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- sc_selected()
      write.csv(d, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)