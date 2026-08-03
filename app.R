# Carbonate Budget Restoration Tool ----

# Adapted by Connor M. Jenkins at the U.S. Geological Survey St. Petersburg Coastal and Marine Science Center
# from Alice Webb's Reef Persistence Tool. Adaptation conceptualized and guided by Lauren T. Toth (USGS) and John T. Morris (NOAA).

# Call Packages ----
library(sf)
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
library(dashboardthemes) # (optional to use dark theme)

# Enable automatic reloading of the app when code changes are detected
options(shiny.autoreload = TRUE)

# Quiet Excel reader ----
# readxl auto-repairs blank/duplicate headers and prints "New names: `` -> `...2`"
# to the console. .name_repair = "unique_quiet" suppresses that for every read.
read_excel_quiet <- function(path, ...) {
  readxl::read_excel(path, .name_repair = "unique_quiet", ...)
}

# Ingest data ----

# call data for world map
world_data   <- ggplot2::map_data("world")
worldcountry <- fortify(world_data)

# Model observational data:
# Assemblage-specific porosity
porosity     <- read.csv(here("data", "Porosity.csv"))
# Species-specific growth rates
growth_rates <- read.csv(here("data", "growth_rates_ReefBudget_NCRMP.csv"))
# Species-specific calcification rates (same file as calc_rates, kept explicit)
calc_rates   <- read.csv(here("data", "Travis_Calcification_Rates.csv"))
# Species-specific average colony diameters
diams        <- read.csv(here("data", "NCRMP_Colony_Diam_Florida.csv"))
# Region- and habitat-specific bioerosion rates
bioerosion   <- read.csv(here("data", "Bioerosion_Rates_Regional.csv"))

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

species_bioerosion_path <- here("data", "Bioerosion_Rates_Species.xlsx")
sp_erosion_parrotfish <- read_sheet_safe(species_bioerosion_path, "Parrotfish")
sp_erosion_urchins    <- read_sheet_safe(species_bioerosion_path, "Urchins")
sp_erosion_sponges    <- read_sheet_safe(species_bioerosion_path, "Sponges")

# Ingest NCRMP carbonate budget data
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

# Pastel palette keyed to the Region field
region_levels <- sort(unique(regions_sf$Region))
pastel_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))(length(region_levels))
region_pal <- colorFactor(pastel_colors, domain = region_levels)


# Ingest baseline cover data template to retrieve list of taxa
taxa <- read_excel_quiet(here("www", "Baseline_Cover_TEMPLATE.xlsx"), sheet = "Taxa")
taxa <- taxa$Taxon

# Ingest IPCC AR6 sea-level projections (PSMSL id 363, "Total" sheet)
slr_raw <- read_excel_quiet(
  here("data", "ipcc_ar6_sea_level_projection_psmsl_id_363.xlsx"),
  sheet = "Total"
)

# Keep only the median (quantile 50), medium-confidence rows
slr_med <- slr_raw[slr_raw$quantile == 50 & slr_raw$confidence == "medium", , drop = FALSE]

# Identify year columns (headers that are purely 4-digit numeric)
slr_year_cols <- names(slr_med)[grepl("^[0-9]{4}$", names(slr_med))]
slr_years_all <- as.integer(slr_year_cols)

# Build a per-scenario lookup: scenario -> named numeric vector (year -> m)
slr_scenarios <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")

slr_by_scenario <- lapply(slr_scenarios, function(scn) {
  row <- slr_med[slr_med$scenario == scn, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  vals_cm <- as.numeric(unlist(row[1, slr_year_cols]))
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
    fit  <- approxfun(ax, ay_m, rule = 2) # linear SLR (m) vs calendar year

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

# SSP245 annual SLR RATE (mm/yr) at a specific calendar year. Computed as the
# year-over-year difference of the interpolated cumulative SLR, matching the
# rate convention used by build_slr_timeline. Used for the Scenario Comparison
# RAP barplot reference lines (2030 / 2050 / 2100).
ssp245_rate_at <- function(cal_year) {
  vec <- slr_by_scenario[["ssp245"]]
  if (is.null(vec)) return(NA_real_)
  ax <- as.integer(names(vec))
  if (length(ax) < 2) return(NA_real_)
  fit <- approxfun(ax, as.numeric(vec), rule = 2)   # cumulative SLR (m) vs year
  (fit(cal_year) - fit(cal_year - 1)) * 1000        # m/yr -> mm/yr
}

# Calculate reef accretion potential
df$rap <- df$net_G / 2.9 / (1 - 0.6265)
df$current_state <- ifelse(df$rap > 0.5, "Growth", ifelse(df$rap < -0.5, "Erosion", "Stasis"))

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
cover_rap_lm <- lm(rap ~ hardCoral_PrctCvr, data = df)
cover_rap_slope <- unname(coef(cover_rap_lm)["hardCoral_PrctCvr"])

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
  "outplant_size", "outplant_cost", "dhw", "bleach_events",
  "rest_horizon", "sim_duration", "cost", "roi", "elev_gain_10yr", "saved"
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

# Shared restoration species list ----
# (used across multiple tabs)
restoration_species_global <- c(
  "Acropora palmata", "Acropora cervicornis",
  "Montastraea cavernosa", "Orbicella faveolata",
  "Colpophyllia natans", "Porites astreoides",
  "Siderastrea siderea", "Stephanocoenia intersepta",
  "Diploria labyrinthiformis", "Solenastrea bournoni"
)

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
  if (length(parts) < 2) return(s)            # nothing to abbreviate
  paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
}

# Abbreviate a species name to "Gspe" (1 genus letter + 3 species letters).
# Leaves "Genus spp." intact.
abbrev_species_code <- function(s) {
  if (grepl("spp\\.$", s)) return(s)
  parts <- stringr::str_split(s, " ")[[1]]
  if (length(parts) < 2) return(s)
  paste0(substr(parts[1], 1, 1), substr(parts[2], 1, 3))
}

# Two-line species label for sliders: genus on line 1, remainder on line 2.
species_label_2line <- function(s) {
  parts <- stringr::str_split(s, " ")[[1]]
  if (length(parts) < 2) return(HTML(s))
  HTML(paste0(parts[1], "<br>", paste(parts[-1], collapse = " ")))
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
  if (y_lo <= -2) {
    lo_even <- floor(y_lo / 2) * 2
    seq(lo_even, hi_even, by = 2)
  } else {
    c(-1, seq(0, hi_even, by = 2))
  }
}

# Status-band data frame builder ----
# Two-row df (Erosion + Stasis) carrying a `label` column that rides through
# ggplotly's tooltip="text" channel, so bands hover as "Erosion"/"Stasis"
# instead of "trace 0"/"trace 1". Drawn with geom_rect (annotate() can't carry
# a tooltip aesthetic).
status_bands_df <- function(xmin, xmax, y_lo) {
  data.frame(
    xmin = c(xmin, xmin), xmax = c(xmax, xmax),
    ymin = c(y_lo, -0.5),  ymax = c(-0.5, 0.5),
    fill = c("red", "yellow"), label = c("Erosion", "Stasis"),
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
    y1 <- df$RAP[i]; y2 <- df$RAP[i + 1]
    # Straddles the threshold (strictly on opposite sides)?
    if (is.finite(y1) && is.finite(y2) &&
        ((y1 < threshold & y2 > threshold) | (y1 > threshold & y2 < threshold))) {
      x1 <- df[[xcol]][i]; x2 <- df[[xcol]][i + 1]
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
# (from acer_model_mockup.R)
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

# Post-bleaching production losses:
# Reductions applied the 1st .. 4th years after bleaching occurs
pbr <- c(0.60, 0.35, 0.15, 0.05)

# Mortality by colony size:
mortality_by_size <- function(size) {
  # Placeholder function - replace with actual mortality rates by size
  if (size < 5) {
    0.35
  } else if (size >= 5 && size < 10) {
    0.30
  } else if (size >= 10 && size < 15) {
    0.25
  } else {
  0.20  # Default: 20% mortality for larger colonies
  }
}

# Identify massive corals
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
  "Solenastrea hyades"
)

# Assemblage-porosity selector ----
# Chooses Acropora / Massive / Mixed porosity (as a proportion) from a
# cover_df with columns `taxon` and a numeric cover column named by cover_col.
assemblage_porosity <- function(cover_df, cover_col) {
  # Empty input -> Mixed-assemblage porosity (proportion), no classification
  if (is.null(cover_df) || nrow(cover_df) == 0) {
    return(porosity$Porosity[porosity$Assemblage == "Mixed"] / 100)
  }
  # Collapse any duplicate taxa (repeated xlsx rows, auto + manual adds) so each
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
  # Only branching corals in the Keys are A. cervicornis and A. palmata
  acer_pct <- if ("Acropora cervicornis" %in% cover_df$taxon) cover_df[cover_df$taxon == "Acropora cervicornis", cover_col] else 0
  apal_pct <- if ("Acropora palmata"     %in% cover_df$taxon) cover_df[cover_df$taxon == "Acropora palmata", cover_col] else 0

  if (total_pct > 0 && (acer_pct + apal_pct) > total_pct * 0.75) {
    por <- porosity$Porosity[porosity$Assemblage == "Acropora"]
  } else if (total_pct > 0 && massive_pct > total_pct * 0.75) {
    por <- porosity$Porosity[porosity$Assemblage == "Massive"]
  } else {
    por <- porosity$Porosity[porosity$Assemblage == "Mixed"]
  }
  por / 100 # proportion
}

baseline_bioerosion_RAP <- function(bg_df, site_area, uc_pct, be_micro_rate, macrobioerosion, bp) {
    # Apply bioerosion to baseline growth df:
    bg_df$consol_area_orig <- site_area - (site_area * uc_pct / 100) - (site_area * bg_df$pct_cvr_orig / 100)
    bg_df$microbioerosion  <- (bg_df$consol_area_orig / site_area) * be_micro_rate # Multiply the general microbioerosion rate by the proportion of available consolidated sediment
    bg_df$calc_budg_orig   <- bg_df$calc_budg_orig - bg_df$microbioerosion - macrobioerosion
    bg_df$RAP_orig         <- bg_df$calc_budg_orig / 2.9 / (1 - bp)

    bg_df
}

# ----------------------------------------------------------------------------
# Growth simulation ----
# Applicable to original colonies and new outplants, and reused by both the
# restoration model and the baseline-growth reactive. Performs its own per-
# species lookups (growth rate, DHW slope) from `species` + `bleaching_severity`.
# Applies Year-1 outplant mortality (group == "outplant" only), per-event
# bleaching dieoff, post-bleaching growth reduction, and bioerosion.
# Returns a per-year data.frame (area, calc_accr, calc_budg, RAP, pct_cvr)
# plus the final colony count and final area.
#
# UNITS NOTE: contrib is a whole-patch flux (kg CaCO3/yr). RAP normalizes it to
# a per-m2 basis by dividing by site_area before the /2.9/(1-por) conversion.
# ----------------------------------------------------------------------------
simulate_growth <- function(group, species, colony_count, colony_diam, duration,
                            site_area, uc_pct, macrobioerosion,
                            bleaching_severity, bleaching_frequency
                            ) {

  # Per-species lookups
  genus        <- stringr::str_split(species, " ")[[1]][1]
  sp_dhw_slope <- dhw_slope_lookup_fk$slope_pct_per_dhw[dhw_slope_lookup_fk$taxon == genus]
  if (length(sp_dhw_slope) == 0) sp_dhw_slope <- 0.80 # generic fallback
  sp_dhw_loss      <- sp_dhw_slope * bleaching_severity / 100 # % -> proportion
  sp_dhw_mortality <- 0.25 # 25% of the cover loss applied as whole-colony mortality
                           # (generalized default, see about species-specific values later)
  sp_growth_rate   <- subset(growth_rates, growth_rates["name"] == species)["planar_mean"][, 1] / 1000

  out_df <- data.frame()
  new_size          <- colony_diam
  colony_count_thisrun  <- colony_count # working colony count for this run
  last_bleach_year  <- 1            # placeholder

  for (i in 1:duration) { # R starts counting at 1 so "Year 0" = Year 1; "Year 10" = Year 11

    # Incorporate Mote outplant mortality observations during Year 0 = "Year 1"
    # Assume 30% outplant die-off. Apply before growth calculation.
    # Should colony numbers be rounded at every step or only at the end?
    if (group == "outplant" && i == 1) {
      colony_count_thisrun <- round(colony_count_thisrun * (1 - mortality_by_size(colony_diam)))
    }

    # Calculate post-bleaching growth reduction from last year's bleaching
    years_since_last_bleach <- i - last_bleach_year
    if (years_since_last_bleach <= 4 && years_since_last_bleach > 0) {
        # Apply post-bleaching production losses
        reduction <- pbr[years_since_last_bleach]
    } else {
        reduction <- 0
    }

    # Will bleaching occur this year?:
    bleaching <- FALSE
    if ((bleaching_frequency == 1 && i %% 4 == 0)    # Every 4th year
     || (bleaching_frequency == 2 && i %% 2 == 0)    # Even years
     || (bleaching_frequency == 5 && i %% 1 == 0)) { # Every year
        # If so, kill colonies before growth if bleaching occurs
        # Apply species-specific dieoff proportion to the colony count:
        bleaching <- TRUE
        last_bleach_year <- i
        colony_count_thisrun <- round(colony_count_thisrun * (1 - sp_dhw_loss * sp_dhw_mortality))
    }

    # Grow the surviving colonies
    # Apply post-bleaching growth reduction to planar growth rate
    new_size <- new_size + sp_growth_rate * (1 - reduction)
    new_area <- (new_size / 2) ^ 2 * pi * colony_count_thisrun

    # If bleaching, apply the remaining bleaching stress that did not cause mortality
    # as a reduction to the new_area created this year:
    if (bleaching) {
      new_area <- new_area * (1 - sp_dhw_loss * (1 - sp_dhw_mortality))
    }

    # Calculate the carbonate budget contribution from this species for this year
    contrib <- calc_rates$rate[calc_rates$Taxon == species] * new_area
    budget  <- contrib / site_area

    # Populate the output dataframe with total values as of this year:
    out_df[i, "area"]       <- new_area # Calcifier area
    out_df[i, "calc_accr"]  <- contrib # Site-wide carbonate accretion contribution (kg CaCO3 / yr)
    out_df[i, "calc_budg"]  <- budget # Calcifier carbonate budget (kg CaCO3 / m2 / yr)
    out_df[i, "pct_cvr"]    <- new_area / site_area * 100 # Calcifier percent cover
  }

  list(df = out_df, final_count = colony_count_thisrun, final_area = new_area)
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

# Baseline-only original growth ----
# Thin wrapper over simulate_growth: loops the selected species (originals
# only) and sums the per-year RAP + % cover + budget. Porosity from the
# baseline assemblage.
run_baseline_growth <- function(site_area, uc_pct, sim_duration,
                                bleaching_severity, bleaching_frequency,
                                baseline_cover_df) {

  n <- sim_duration + 1
  total_rap  <- rep(0, n)
  total_cvr  <- rep(0, n)
  total_budg <- rep(0, n)

  for (row_i in seq_len(nrow(baseline_cover_df))) {
    species        <- baseline_cover_df$taxon[row_i]
    current_sp_pct <- baseline_cover_df$current_cvr_pct[row_i]
    if (is.na(current_sp_pct) || current_sp_pct <= 0) next

    current_sp_m <- site_area * (current_sp_pct / 100)
    sp_diam <- subset(diams, diams["name"] == species)["length_mean"][, 1] / 100
    if (length(sp_diam) == 0 || is.na(sp_diam)) next
    orig_colonies <- round(current_sp_m / ((sp_diam / 2) ^ 2 * pi))

    sim <- simulate_growth(group = "original", species = species,
                           colony_count = orig_colonies,
                           colony_diam = sp_diam, duration = n,
                           site_area = site_area, uc_pct = uc_pct,
                           bleaching_severity = bleaching_severity,
                           bleaching_frequency = bleaching_frequency)

    #total_rap  <- total_rap  + sim$df$RAP
    total_cvr  <- total_cvr  + sim$df$pct_cvr
    total_budg <- total_budg + sim$df$calc_budg
  }
  df <- data.frame(Year = 0:sim_duration, #RAP_orig = total_rap,
             pct_cvr_orig = total_cvr, calc_budg_orig = total_budg)
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
                                  outplant_diam, outplant_cost,
                                  bleaching_severity, bleaching_frequency,
                                  target_cover_df) {

  # Guard: refuse if total target cover exceeds 100% of the site.
  total_target_pct <- sum(target_cover_df$target_cvr_pct, na.rm = TRUE)
  if (is.finite(total_target_pct) && total_target_pct > 100) {
    return(list(budget_df = data.frame(),
                outplants_by_species = c(),
                outplants = 0, cost = 0,
                error = paste0("Total target cover (", round(total_target_pct, 1),
                               "%) exceeds 100%.")))
  }

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
  calc_budg_orig  <- rep(0, n)
  # RAP_orig        <- rep(0, n)
  pct_cvr_orig    <- rep(0, n)
  area_new        <- rep(0, n)
  calc_accr_new   <- rep(0, n)
  calc_budg_new   <- rep(0, n)
  # RAP_new         <- rep(0, n)
  pct_cvr_new     <- rep(0, n)

  outplants_by_species <- c()   # named: species -> outplant count
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
    sp_growth_rate <- subset(growth_rates, growth_rates["name"] == species)["planar_mean"][, 1] / 1000
    if (length(sp_growth_rate) == 0 || is.na(sp_growth_rate)) next

    current_sp_m  <- site_area * (current_sp_pct / 100)
    orig_colonies <- round(current_sp_m / ((sp_diam / 2) ^ 2 * pi))

    orig_list <- simulate_growth(group = "original", species = species,
                                 colony_count = orig_colonies, colony_diam = sp_diam,
                                 duration = n,
                                 site_area = site_area, uc_pct = uc_pct,
                                 bleaching_severity = bleaching_severity,
                                 bleaching_frequency = bleaching_frequency)
    od <- orig_list[[1]] # original df
    area_orig       <- area_orig       + od$area
    calc_accr_orig  <- calc_accr_orig  + od$calc_accr
    calc_budg_orig  <- calc_budg_orig  + od$calc_budg
    # RAP_orig        <- RAP_orig        + od$RAP
    pct_cvr_orig    <- pct_cvr_orig    + od$pct_cvr
    any_growth <- TRUE
  }

  # ---- Phase 1 + 2: outplant solve + full-duration growth (restored only) ----
  for (row_i in seq_len(nrow(target_cover_df))) {
    species        <- target_cover_df$taxon[row_i]
    target_sp_pct  <- target_cover_df$target_cvr_pct[row_i]
    current_sp_pct <- target_cover_df$current_cvr_pct[row_i]

    # Only species with a positive amount to grow get outplants
    sp_to_grow_pct <- target_sp_pct - current_sp_pct
    if (is.na(sp_to_grow_pct) || sp_to_grow_pct <= 0) next

    # Colony-count seeding needs the species diameter (also used by the sim)
    sp_diam <- subset(diams, diams["name"] == species)["length_mean"][, 1] / 100
    # ^ 0.2 m
    if (length(sp_diam) == 0 || is.na(sp_diam)) next
    # Skip species with no growth-rate record (sim would produce NA)
    sp_growth_rate <- subset(growth_rates, growth_rates["name"] == species)["planar_mean"][, 1] / 1000
    if (length(sp_growth_rate) == 0 || is.na(sp_growth_rate)) next

    current_sp_m <- site_area * (current_sp_pct / 100)
    target_sp_m  <- site_area * (target_sp_pct / 100)

    # Remaining target size to grow, after original growth to the HORIZON.
    # Grow this species' originals once to read its area at the horizon year.
    orig_colonies <- round(current_sp_m / ((sp_diam / 2) ^ 2 * pi))
    orig_only <- simulate_growth(group = "original", species = species,
                                 colony_count = orig_colonies, colony_diam = sp_diam,
                                 duration = n,
                                 site_area = site_area, uc_pct = uc_pct,
                                 bleaching_severity = bleaching_severity,
                                 bleaching_frequency = bleaching_frequency)
    orig_area_at_horizon <- orig_only[[1]][["area"]][min(rest_horizon + 1, n)]
    sp_to_grow_m <- target_sp_m - orig_area_at_horizon

    # ---- PHASE 1: solve outplant count to hit target ----
    if (rest_horizon == 0) {
      # Immediate coverage: enough outplants of the given size to hit the target
      # % cover at Year 0. No growth search needed -- divide the remaining area
      # to fill by a single outplant's footprint and round up.
      outplant_area <- (outplant_diam / 2) ^ 2 * pi
      outplant_guess <- if (outplant_area > 0) {
        max(0, ceiling(sp_to_grow_m / outplant_area))
      } else {
        0
      }
    } else {
      # Iterative solve: find the count that reaches target by the HORIZON year.
      # Have to start with a guess for the number of required outplants
      outplant_guess <- 70

      reiterate <- TRUE
      guard <- 0 # safety cap to prevent runaway loops
      while (reiterate) {
        guard <- guard + 1
        if (guard > 5000) break

        starting_outplant_area <- outplant_guess * (outplant_diam / 2) ^ 2 * pi
        needed_outplant_growth <- sp_to_grow_m - starting_outplant_area

        # Search runs ONLY to the restoration horizon
        search_list <- simulate_growth(group = "outplant", species = species,
                                       colony_count = outplant_guess, colony_diam = outplant_diam,
                                       duration = rest_horizon + 1,
                                       site_area = site_area, uc_pct = uc_pct,
                                       bleaching_severity = bleaching_severity,
                                       bleaching_frequency = bleaching_frequency)

        new_area <- search_list[[3]] # area at the horizon year

        # Get within the nearest 0.1 m^2
        if (needed_outplant_growth - new_area < -0.1) { #Too much growth by the horizon:
          # Use fewer outplants
          outplant_guess <- outplant_guess - 1
          if (outplant_guess < 0) { outplant_guess <- 0; reiterate <- FALSE }
        } else if (needed_outplant_growth - new_area > 0.1) { # Not enough growth:
          # Use more outplants
          outplant_guess <- outplant_guess + 1
        } else {
          reiterate <- FALSE
        }
      }
    }

    # ---- PHASE 2: run the solved outplant count for the FULL simulation duration ----
    new_list <- simulate_growth(group = "outplant", species = species,
                                colony_count = outplant_guess, colony_diam = outplant_diam,
                                duration = n,
                                site_area = site_area, uc_pct = uc_pct,
                                bleaching_severity = bleaching_severity,
                                bleaching_frequency = bleaching_frequency)
    nd <- new_list[[1]]
    area_new       <- area_new       + nd$area
    calc_accr_new  <- calc_accr_new  + nd$calc_accr
    calc_budg_new  <- calc_budg_new  + nd$calc_budg
    # RAP_new        <- RAP_new        + nd$RAP
    pct_cvr_new    <- pct_cvr_new    + nd$pct_cvr

    outplants_by_species[species] <- outplant_guess
    total_cost <- total_cost + outplant_guess * outplant_cost
    any_growth <- TRUE
  }

  if (!any_growth) {
    return(list(budget_df = data.frame(),
                outplants_by_species = outplants_by_species,
                outplants = 0, cost = 0))
  }

  budget_df <- data.frame(
    area_orig = area_orig, calc_accr_orig = calc_accr_orig,
    calc_budg_orig = calc_budg_orig, # RAP_orig = RAP_orig,
    pct_cvr_orig = pct_cvr_orig,
    area_new = area_new, calc_accr_new = calc_accr_new,
    calc_budg_new = calc_budg_new, # RAP_new = RAP_new,
    pct_cvr_new = pct_cvr_new
  )

  budget_df$area_total       <- budget_df$area_orig       + budget_df$area_new
  budget_df$calc_accr_total  <- budget_df$calc_accr_orig  + budget_df$calc_accr_new
  budget_df$calc_budg_total  <- budget_df$calc_budg_orig  + budget_df$calc_budg_new
  budget_df$pct_cvr_total    <- budget_df$pct_cvr_orig    + budget_df$pct_cvr_new
  # budget_df$RAP_total        <- budget_df$RAP_orig        + budget_df$RAP_new

  # Apply bioerosion to final carbonate budget:
  # Determine area of consolidated substrate
  budget_df$consol_area_total <- site_area - (site_area * uc_pct / 100) - (site_area * budget_df$pct_cvr_total / 100)
  # Multiply the general microbioerosion rate by the proportion of available consolidated substrate
  budget_df$microbioerosion   <- (budget_df$consol_area_total / site_area) * be_micro_rate
  # Recalculate carbonate budget, accounting for bioerosion
  budget_df$calc_budg_total     <- budget_df$calc_budg_total - budget_df$microbioerosion - macrobioerosion
  # Recalculate RAP from adjusted carbonate budget
  budget_df$RAP_total         <- budget_df$calc_budg_total / 2.9 / (1 - por)

  # Apply the SAME bioerosion to the ORIGINALS-only budget so RAP_orig is net
  # (matches baseline_bioerosion_RAP). Without this the dashed "Original" line
  # is gross while the standalone Baseline line is net, making Baseline look
  # dramatically lower at zero target.
  budget_df$consol_area_orig <- site_area - (site_area * uc_pct / 100) - (site_area * budget_df$pct_cvr_orig / 100)
  budget_df$be_micro_orig    <- (budget_df$consol_area_orig / site_area) * be_micro_rate
  budget_df$calc_budg_orig   <- budget_df$calc_budg_orig - budget_df$be_micro_orig - macrobioerosion
  budget_df$RAP_orig         <- budget_df$calc_budg_orig / 2.9 / (1 - por)

  list(
    budget_df = budget_df,
    outplants_by_species = outplants_by_species,
    outplants = sum(outplants_by_species),
    cost      = total_cost
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
state_colors <- c("Growth" = "#0099FF", "Stasis" = "#FFFF99", "Erosion" = "#FF6600")
num_pal_state <- colorFactor(
  palette = unname(state_colors),
  levels  = names(state_colors)
)

# Restoration Mix groupings ----
# Branching (tan) | Weedy/Other (lime) across the top; Massive (gray) below.
branching_species <- c("Acropora cervicornis", "Acropora palmata")
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

weedy_species <- c("Porites astreoides", "Porites porites")

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
    sprintf("%+.2f", d_budget), " kg/m\u00b2/yr</b></p>",
    "<p>", arrow(d_rap), " Reef accretion change: <b>",
    sprintf("%+.2f", d_rap), " mm/yr</b></p>",
    "<p>", arrow(d_pct), " RAP percentile change: <b>",
    if (is.na(d_pct)) "\u2013" else sprintf("%+.0f", d_pct), " points</b>",
    if (!is.na(b_pct) && !is.na(r_pct))
      paste0(" <span style='color:#777;'>(", round(b_pct), "% \u2192 ", round(r_pct), "%)</span>")
    else "",
    "</p>",
    "<hr>",
    "<p>", 
    if (!is.na(r_rap) && r_rap >= 3.1 && r_rap < 6.3) {
      "<span style='color:orangered;'>Restored accretion exceeds the geologic baseline, but sea-level rise will exceed restored accretion by 2030.</span>"
    } else if (!is.na(r_rap) && r_rap >= 6.3 && r_rap < 8) {
      "<span style='color:orange;'>Restored accretion exceeds the geologic baseline, but sea-level rise will exceed restored accretion by 2050.</span>"
    } else if (!is.na(r_rap) && r_rap >= 8 && r_rap < 9.2) {
      "<span style='color:yellow;'>Restored accretion exceeds the geologic baseline, but sea-level rise will exceed restored accretion by 2100.</span>"
    } else if (!is.na(r_rap) && r_rap >= 9.2) {
      "<span style='color:green;'>Restored accretion exceeds the geologic baseline and will exceed sea-level rise at least until 2100.</span>"
    } else {
      "<span style='color:red;'>Restored accretion is still exceeded by the geological baseline and sea-level rise.</span>"
    }, "</p>"
  ))
}

# Shiny User Interface ----
# Converted from bootstrapPage/navbarPage to shinydashboard::dashboardPage

## Header ----
# Dark Mode toggle lives in the top ribbon (as a right-aligned dropdown item)
header <- dashboardHeader(
  title = "Carbonate Budget Restoration Tool",
  titleWidth = 380,
  tags$li(
    class = "dropdown",
    tags$div(
      class = "dark-mode-switch",
      style = "padding: 12px 15px 0 0;",
      materialSwitch(
        inputId = "dark_mode",
        label = "Dark Mode",
        status = "primary",
        right = TRUE,
        inline = TRUE
      )
    )
  )
)

## Sidebar ----
sidebar <- dashboardSidebar(
  width = 230,
  sidebarMenu(
    id = "nav",
    menuItem("Reef Site Map", tabName = "home", icon = icon("map")),
    menuItem("Restoration Planning", tabName = "restoration", icon = icon("seedling")),
    menuItem("Scenario Comparison", tabName = "scenarios", icon = icon("scale-balanced")),
    menuItem("Restoration Monitoring", tabName = "monitoring", icon = icon("chart-column")),
    menuItem("About this App", tabName = "about", icon = icon("circle-info"))
  )
)

## Body ----
body <- dashboardBody(
  useShinyjs(),

  # Tag Setup ----
  tags$head(
    includeHTML(here("gtag.html")),
    includeCSS(here("styles.css")),
    # Preserve custom background color (optional)
    tags$style(HTML("
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
        position: absolute; top: 175px; right: 10px; z-index: 1000;
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
        white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
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
        margin: -6px 0 6px 2px;
      }
      /* Restoration mix: morphology sub-boxes as compact fieldsets (matches
         the Bleaching Scenario legend style) */
      .mix-fieldset {
        border-radius: 6px; padding: 8px 10px; margin-bottom: 8px;
      }
      .mix-fieldset > legend {
        width: auto; font-size: 13px; font-weight: bold;
        margin-bottom: 4px; padding: 0 6px; border: none;
      }
      .mix-branching { border: 2px solid #c8a165; }   /* tan */
      .mix-branching > legend { color: #a97d3e; }
      .mix-massive   { border: 2px solid #9e9e9e; }   /* gray */
      .mix-massive   > legend { color: #6f6f6f; }
      .mix-weedy     { border: 2px solid #7bc043; }   /* lime */
      .mix-weedy     > legend { color: #5a9130; }
      
      /* Two-line, italic slider labels in the mix sub-boxes */
      .mix-fieldset .control-label { font-style: italic; line-height: 1.2; }
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
      .restoration-toprow > [class*='col-'] { padding-left: 5px; padding-right: 5px; }

      /* Outplant parameter inputs: Inline label + narrow box*/
      .param-inline-row {
        display: flex; align-items: center; justify-content: space-between;
        gap: 6px; margin-bottom: 8px;
      }
      .param-inline-row .param-label {
        flex: 1 1 auto; font-weight: normal; font-size: 14px;
      }
      .param-inline-row .shiny-input-container { width: auto; margin-bottom: 0; }
      .param-inline-row input {
        width: 9ch; min-width: 9ch; padding: 2px 4px; text-align: right;
      }
      .param-inline-row .param-unit {
        flex: 0 0 auto; font-weight: normal; font-size: 14px; width: 4ch;
      }

      /* Red warning under the bioerosion upload widget */
      .bioerosion-warning { color: #d9534f; font-size: 12px; font-weight: bold; margin-top: 4px; }

      /* Inline upload label + Download template button */
      .upload-label-row {
        display: flex; align-items: center; justify-content: space-between;
        gap: 8px; margin-bottom: 4px;
      }
      .upload-label-row .control-label { margin: 0; font-weight: bold; }

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
      body.dark-mode .map-controls-panel strong { color: #e6e6e6 !important; }
    ")),
    # Toggle the body dark-mode class from the switch
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_dark', function(on) {
        document.body.classList.toggle('dark-mode', on);
      });
    "))
  ),

  tabItems(
    # Home Tab ----
    tabItem(
      tabName = "home",
      div(
        class = "home-map-outer",
        leafletOutput("mymap", width = "100%", height = "100%"),
        # Right-aligned data caption overlaid on the top-right corner of the map
        tags$div(
          style = "position: absolute; top: 45px; right: 10px;
          z-index: 1000; display: flex; gap: 8px;",
          tags$li(
            class = "dropdown",
            tags$span(
              style = "color: white; line-height: 50px; margin-right: 15px; font-size: 18px;
                       text-shadow: -1px -1px 0 black, 1px -1px 0 black,
                                    -1px 1px 0 black, 1px 1px 0 black;
                                    ",
              "Displaying 2014-2024 NCRMP data"
            )
          )
        ),
        # USGS & NOAA logos
        tags$div(
          style = "position: absolute; top: 90px; right: 10px;
                   z-index: 1000; display: flex; gap: 10px;",

          tags$img(src = "usgsLogo.png", style = "height: 75px;"),
          tags$img(src = "noaaLogo.png", style = "height: 75px;")
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
            sliderInput("target_cover_increase", "Target Percent-Cover Increase",
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
            radioButtons("symbolize_by", "Symbolize by:",
              choices = c(
                "Reef Accretion Potential (RAP)" = "rap",
                "Reef State"                     = "current_state",
                "Gross Bioerosion"               = "grossE_G"
              ),
              selected = "rap"
            ),

            tags$hr(),

            # Named-reef labels toggle
            checkboxInput("show_named_reefs", "Show Named Reefs", value = FALSE),

            tags$hr(),

            # Point-size stepper (moved into Map Controls)
            tags$div(
              style = "font-size: 13px; margin-bottom: 4px; color: #333;",
              "Point size"
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

    # Restoration Planning Tab ----
    tabItem(
      tabName = "restoration",
      # Vertical layout: horizontal input row on top, timeline on the bottom
      # `restoration-toprow` class tightens the inter-box gutters.
      fluidRow(
        class = "restoration-toprow",
        # ---- Input element 1: Baseline cover (subsumed from Baseline Input) ----
        column(
          width = 4,
          shinydashboard::box(
            title = "Baseline Cover",
            width = 12, status = "primary", solidHeader = TRUE,
            column(width = 12,
              fluidRow(
                # Left column: controls
                column(
                  width = 6,
                  tags$div(
                    class = "upload-label-row",
                    tags$span(class = "control-label", tags$strong("Load from file (.xlsx)")),
                    downloadButton("baseline_template_dl", "Download template", class = "btn-sm")
                  ),
                  fileInput("baseline_upload", NULL, accept = c(".xlsx")
                  ),
                  # Typed input allowed (create = TRUE) for scratch-built scenarios
                  selectizeInput(
                    "baseline_site",
                    label = tags$strong("Site"),
                    choices = c("\u2013 Select site \u2013" = ""),
                    selected = "",
                    options = list(create = TRUE, placeholder = "Select or type a site...")
                  ),
                  tags$div(
                    class = "param-inline-row",
                    tags$span(class = "param-label", "Latitude:"),
                    numericInput("site_latitude", label = NULL,
                      value = NA, min = -90, max = 90, step = 0.00001
                    ),
                    tags$span(class = "param-unit", "\u00b0")
                  ),
                  tags$div(
                    class = "param-inline-row",
                    tags$span(class = "param-label", "Longitude:"),
                    numericInput("site_longitude", label = NULL,
                      value = NA, min = -180, max = 180, step = 0.00001
                    ),
                    tags$span(class = "param-unit", "\u00b0")
                  ),
                  tags$div(
                    class = "param-inline-row",
                    tags$span(class = "param-label", tags$strong("Site area:")),
                    numericInput("site_area_m2", label = NULL,
                    value = 100, min = 1, max = 10000, step = 1
                    ),
                    tags$span(class = "param-unit", "m\u00b2")
                  ),
                  selectInput(
                    "subregion_choice",
                    label = tags$strong("Subregion"),
                    choices = c("\u2013 Select subregion \u2013" = "",
                                unname(subregion_labels)),
                    selected = ""
                  ),
                  selectInput(
                    "habitat_choice",
                    label = tags$strong("Habitat"),
                    choices = c("\u2013 Select habitat \u2013" = ""),
                    selected = ""
                  ),
                  actionButton("baseline_delete_cache", "Clear cache",
                                 icon = icon("trash"), class = "btn-sm")
                ),

                # Right column: baseline species cover list + add-species dropdown
                column(
                  width = 6,
                  tags$strong("Baseline species cover (%)"),
                  # Auto-populated rows (from .xlsx) + manually added rows, with
                  # the species-picker dropdown rendered below the list.
                  uiOutput("baseline_cover_inputs"),
                  tags$div(
                    style = "margin-top:8px;",
                    downloadButton("baseline_save_dl", "Save baseline",
                                   icon = icon("floppy-disk"), class = "btn-sm")
                  )
                )
              )
            )
          )
        ),

        # ---- Input element 2: Restoration mix (subsumed from Baseline Input) ----
        column(
          width = 4,
          shinydashboard::box(
            title = "Restoration Mix",
            width = 12, status = "success", solidHeader = TRUE,
            div(tags$strong("Target cover (%) post-restoration:")),
            # Top row: Branching (2 cols) + Weedy/Other (2 cols), split evenly
            fluidRow(
              column(6,
                tags$fieldset(
                  class = "mix-fieldset mix-branching",
                  tags$legend("Branching"),
                  uiOutput("mix_branching")
                )
              ),
              column(6,
                tags$fieldset(
                  class = "mix-fieldset mix-weedy",
                  tags$legend("Weedy / Other"),
                  uiOutput("mix_weedy")
                )
              )
            ),
            # Bottom row: Massive across all 4 columns
            fluidRow(
              column(12,
                tags$fieldset(
                  class = "mix-fieldset mix-massive",
                  tags$legend("Massive"),
                  uiOutput("mix_massive")
                )
              )
            )
          )
        ),

        # ---- Input element 3: outplant + bleaching parameters ----
        column(
          width = 4,
          shinydashboard::box(
            title = "Restoration Parameters",
            width = 12, status = "warning", solidHeader = TRUE,

            # Outplant parameters
            column(width = 6,
              tags$div(
                class = "param-inline-row",
                tags$span(class = "param-label", "Avg. outplant diameter:"),
                numericInput("outplant_size", label = NULL,
                  value = 5, min = 1, max = 100, step = 0.1
                ),
                tags$span(class = "param-unit", "cm"),
              ),
              tags$div(
                class = "param-inline-row",
                tags$span(class = "param-label", "Avg. outplant cost: "),
                numericInput("outplant_cost", label = NULL,
                  value = 10, min = 1, max = 100, step = 0.01
                ),
                tags$span(class = "param-unit", "$")
              ),

              # Timeline parameters
              sliderInput("rest_horizon", "Restoration horizon (years)",
                value = 10, min = 0, max = 30, step = 5
                ),
              # Sim duration shares the horizon's 0-30 domain so tick geometry
              # lines up natively; a snap-to-10 floor keeps it >= 10 years.
              sliderInput("sim_duration", "Simulation duration (years)",
                value = 10, min = 0, max = 30, step = 5
                ),
              # Red warning when horizon exceeds sim duration
              uiOutput("sim_duration_warning")
            ),

            # Bleaching scenario (vertical, red outline)
            column(width = 6,
              tags$fieldset(
                style = "border: 2px solid #d9534f; border-radius: 6px;
                        padding: 10px; margin-top: 12px;",
                tags$legend(
                  style = "width: auto; font-size: 15px; font-weight: bold;
                          color: #d9534f; padding: 0 6px;",
                  "Bleaching Scenario"
                ),
                sliderInput("dhw", "Degree-Heating Weeks",
                  min = 8, max = 24, value = 8, step = 1
                ),
                sliderTextInput(
                  inputId = "bleach_events",
                  label = "Events / 5 years",
                  choices = c(0, 1, 2, 5),
                  selected = 0,
                  grid = TRUE
                )
              )
            )
          )
        )
      ),

      # --- Timeline (bottom) ---
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "Projected Reef Accretion Potential (RAP)",
            width = 12, status = "info", solidHeader = TRUE,
            # Reactive surrounds (inline): baseline pctile | restored pctile | cost
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center;
                       gap:12px; padding:2px 6px; font-size:14px; font-weight:bold;",
              tags$div(style = "display:flex; gap:16px; align-items:center;",
                htmlOutput("rap_pctile_baseline", inline = TRUE),
                htmlOutput("rap_pctile_restored", inline = TRUE)
              ),
              tags$div(style = "color:#2f4f2f; text-align:right;",
                textOutput("model_final_cost", inline = TRUE)
              )
            ),
            uiOutput("target_cover_warning"),
            plotly::plotlyOutput("restoration_timeline", height = "320px")
          )
        )
      ),

      fluidRow(
        column(width = 8),
        column(width = 4,
          # "Save Scenario" box
          shinydashboard::box(
            title = "Save Scenario", width = 12, solidHeader = TRUE, status = "primary",
            column(width = 5,
                textInput("scenario_project", "Project name", value = "")
            ),
            column(width = 5,
                textInput("scenario_name", "Scenario name", value = "")
            ),
            column(width = 2,
              fluidRow(
                tags$h1(), # Used to keep the button aligned
                actionButton("save_scenario", "Save", icon = icon("floppy-disk"))
              )
            )
          )
        )
      )
    ),

    # Scenario Comparison Tab ----
    # Sidebar: project (single select), scenario (multi select from saved .json),
    #          download report (.csv), + collapsible per-scenario Impact Summary
    # Main:    cost bar, ROI bar, per-scenario RAP bar
    tabItem(
      tabName = "scenarios",
      fluidRow(
        # Sidebar (left)
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
              downloadButton("sc_download_csv", "Download report (.csv)")
            )
          ),

          # Collapsible per-scenario Impact Summary (below Scenario Selection).
          # Each scenario is a nested collapsible panel within this box.
          shinydashboard::box(
            title = HTML("Impact Summary at<br/>Restoration Horizon"), width = 12,
            status = "info", solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("sc_impact_summaries")
          )
        ),
        # Main content (right)
        column(
          width = 9,
          fluidRow(
            column(
              width = 6,
              shinydashboard::box(
                title = "Project Cost", width = 12,
                status = "info", solidHeader = TRUE,
                plotOutput("sc_cost_bar", height = "300px")
              )
            ),
            column(
              width = 6,
              shinydashboard::box(
                title = "Return on Investment", width = 12,
                status = "info", solidHeader = TRUE,
                plotOutput("sc_roi_bar", height = "300px")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              shinydashboard::box(
                title = "Reef Accretion Potential (RAP) by Scenario", width = 12,
                status = "success", solidHeader = TRUE,
                plotly::plotlyOutput("sc_rap_bar", height = "350px")
              )
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
        # Sidebar (left)
        column(
          width = 3,
          shinydashboard::box(
            title = "Inputs", width = 12, status = "primary", solidHeader = TRUE,
            tags$div(
              class = "upload-label-row",
              tags$span(class = "control-label", "Upload coral cover data"),
              downloadButton("monitoring_cover_template_dl", "Download template",
                             class = "btn-sm")
            ),
            fileInput("upload_cover", NULL,
              accept = c(".csv", ".xlsx")
            ),
            tags$div(
              class = "upload-label-row",
              tags$span(class = "control-label", "Upload bioerosion data"),
              downloadButton("monitoring_bioerosion_template_dl", "Download template",
                             class = "btn-sm")
            ),
            fileInput("upload_bioerosion", NULL,
              accept = c(".csv", ".xlsx")
            ),
            
            # Red warning for unobserved parrotfish size classes
            uiOutput("bioerosion_parrotfish_warning"),
            selectizeInput("monitoring_selected_site", tags$strong("Select site"),
              choices = NULL,
              options = list(placeholder = "Select a site...")
            ),
            br(),
            # Download + Clear cache side-by-side
            tags$div(
              style = "display:flex; gap:8px; align-items:center;",
              downloadButton("cc_download_report", "Download report"),
              actionButton("cc_clear_cache", "Clear cache", icon = icon("trash"))
            )
          )
        ),
        # Main content (right)
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
                valueBoxOutput("cc_baseline_cover", width = NULL),
                valueBoxOutput("cc_baseline_budget", width = NULL),
                valueBoxOutput("cc_baseline_rap", width = NULL)
              ),
              column(
                width = 4,
                tags$h4("Restored", style = "text-align:center; font-weight:bold;"),
                valueBoxOutput("cc_restored_cover", width = NULL),
                valueBoxOutput("cc_restored_budget", width = NULL),
                valueBoxOutput("cc_restored_rap", width = NULL)
              ),
              column(
                width = 4,
                tags$h4("Impact summary", style = "text-align:center; font-weight:bold;"),
                div(
                  class = "impact-summary-box",
                  style = "background:#f7f7f7; border:1px solid #ddd;
                           border-radius:6px; padding:12px; min-height:180px;",
                  htmlOutput("cc_impact_summary")
                )
              )
            )
          ),
          # Timeline
          shinydashboard::box(
            title = "Reef Accretion Potential over 10 Years", width = 12,
            status = "success", solidHeader = TRUE,
            plotly::plotlyOutput("cc_timeline", height = "350px")
          )
        )
      )
    ),

    # "About this Site" Tab ----
    tabItem(
      tabName = "about",
      shinydashboard::box(
        width = 12, status = "primary", solidHeader = FALSE,
        tags$div(
          tags$h4("Aim"),
          HTML("The aim of this application is to provide a predictive tool for decision makers to assess reef restoration efforts under future climate change 
            and bleaching scenarios. The modelling approach that is used to build projections in this interactive tool is described in a forthcoming journal publication."), tags$br(),
          tags$br(),
          tags$h4("Background"),
          HTML("For reef framework to persist, constructional processes by corals and other calcifers need 
           to outpace loss due to physical, chemical, and biological erosion. This balance is both delicate and 
           dynamic and is currently threatened by the effects of sea-level rise, ocean warming, and ocean acidifcation.
           
           Although the protection and recovery of ecosystem functions are at the center of most restoration 
           and conservation programs, decision makers are limited by the lack of predictive tools to forecast 
           reef accretion under different emission and bleaching scenarios."),
          tags$br(),
          tags$br(),
          HTML("The Carbonate Budget Restoration Tool will enable decision makers to evaluate the impact of reef restoration decisions 
                in the context of climate change and a variety of bleaching scenarios."), tags$br(),
          tags$br(),
          tags$h4("Code"),
          "Code and input data used to generate this Shiny app are available on ", tags$a(href = "https://github.com/laurenttoth/CarbonateBudgetRestorationTool", "Github."), tags$br(),
          tags$br(),
          tags$h4("Sources"),
          "NASA sea-level rise data: ", tags$br(),
          "Coral morphologies: ", tags$br(),
          tags$br(),
          tags$h4("Authors"),
          "Connor M. Jenkins, St. Petersburg Coastal and Marine Science Center, USGS, St. Petersburg, Florida, USA;", tags$br(),
          "Dr. Lauren T. Toth, St. Petersburg Coastal and Marine Science Center, USGS, St. Petersburg, Florida, USA;", tags$br(),
          "Dr. John Morris, Atlantic Oceanographic and Meteorological Laboratory, NOAA, Miami, Florida, USA", tags$br(),
          tags$br(),
          tags$h4("Contact"),
          "Lauren Toth: ", tags$a(href = "mailto:ltoth@usgs.gov", "ltoth@usgs.gov"), tags$br(),
          tags$br(),
          tags$br(),
          tags$h4("Acknowledgments"),
          "A special thanks to Dr. Alice Webb and her team, who originally developed the ", tags$a(href = "https://github.com/alice35/ReefPersistence_app", "Reef Persistence Tool"),
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
          "The paper describing the original Reef Persistence Tool is published in ", tags$a(href="https://www.nature.com/articles/s41598-022-26930-4", "Scientific Reports"), ".", tags$br(),

          # Add logo panel
          absolutePanel(
            id = "absPanel",
            top = "62%",
            left = "72.5%",
            width = "30%",
            fixed = TRUE,
            fluidRow(
              column(width = 5),
              column(width = 4,
                tags$img(src = "noaaLogo.png", width = "200px", height = "200px")
              ),
              column(width = 3),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$img(src = "usgsLogo.png", width = "450px", height = "150px")
            )
          )
        )
      )
    )
  )
)

ui <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)

# Shiny Server ----
server <- function(input, output, session) {
  # define reactVal to store coordinates
  reef_name       <- reactiveVal()
  reef_year       <- reactiveVal(2019)
  initial_budget  <- reactiveVal(NULL)

  .safe_num <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) 0 else as.numeric(x)
  }
  .safe_num_chr <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
  }

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

  # Warn (red text) when the restoration horizon exceeds the simulation
  # duration; the model refuses to run in that case (see model_result()).
  output$sim_duration_warning <- renderUI({
    h <- .safe_num(input$rest_horizon)
    dur <- .safe_num(input$sim_duration)
    if (h > dur) {
      tags$div(class = "sim-warning",
        HTML("<span style='color: red;'>The simulation duration must meet or exceed the restoration horizon.</span>"))
    }
  })

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
    d$restored_state <- ifelse(d$restored_rap > 0.5, "Growth", ifelse(d$restored_rap < -0.5, "Erosion", "Stasis"))

    # Halo / fill classification (only meaningful when inc > 0)
    classify <- function(base, restored) {
      if (base > 0.5) {
        "skyblue"  # Was already growing
      } else if (base < -0.5 && restored >= 0.5) {
        "darkgreen" # Erosion to growth
      } else if (base >= -0.5 && base < 0.5 && restored >= 0.5) {
        "limegreen" # Stasis to growth
      } else if (base <= -0.5 && restored > -0.5 && restored < 0.5) {
        "palegreen" # Erosion to stasis
      } else if (restored > -0.5 && restored < 0.5) {
        "ivory" # Stasis to stasis
      } else if (restored <= -0.5) {
        "red" # Still eroding
      } else {
        NA_character_
      }
    }
    d$halo <- mapply(classify, d$rap, d$restored_rap)
    d
  })

  # Initialize leaflet map ----
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles(providers$Esri.WorldImagery,
        options = providerTileOptions(attribution = 'Map data &copy; <a href="https://www.esri.com/">Esri</a>')
      ) |>
      setView(lng = -81, lat = 25.5, zoom = 8) |>

      # Dedicated low pane for the named-reef circles so they sit above the
      # basemap/regions but BELOW the clickable site markers (default ~600).
      addMapPane("named_reefs_pane", zIndex = 410) |>

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

      # Static control: White text instruction
      addControl(
        html = "<div
                  style='
                    font-size: 22px;
                    font-weight: bold;
                    color: white;
                    text-shadow:
                      -1px -1px 0 black,
                      1px -1px 0 black,
                      -1px 1px 0 black,
                      1px 1px 0 black;'>
                  Click on a site to<br> find out more</div>",
        position = "bottomright",
        className = "map-title"
      )
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
    fill_opacity <- rep(0.85, nrow(d))
    if (field == "rap" && inc > 0) {
      red_idx <- which(d$halo == "red")
      fill_cols[red_idx] <- "red"
      fill_opacity[red_idx] <- 0.25
    }

    # Draw halos first (underneath) when slider is active
    if (inc > 0) {
      halo_cols <- c(skyblue = "skyblue", darkgreen = "darkgreen", limegreen = "limegreen", palegreen = "palegreen", ivory = "ivory", red = "red")
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
        fillOpacity = fill_opacity,
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
          "<td style='padding: 2px 0;'>", round(parrotfish_G, 2), "kg CaCO\u00b3/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Gross bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(grossE_G, 2), "kg CaCO\u00b3/yr</td></tr>",
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
    rest_colors = c("Growth  → Growth"  = "skyblue",
                    "Erosion → Growth"  = "darkgreen",
                    "Stasis  → Growth"  = "limegreen",
                    "Erosion → Stasis"  = "palegreen",
                    "Stasis  → Stasis"  = "ivory",
                    "Erosion → Erosion" = "red")

    proxy <- proxy |>
      addLegend("bottomleft",
        colors = unname(rest_colors),
        labels = names(rest_colors),
        title = "Restoration Potential",
        opacity = 1
      )

    # Legend for the selected symbolize-by field
    if (field == "current_state") {
      proxy <- proxy |>
        addLegend("bottomleft",
          colors = unname(state_colors), labels = names(state_colors),
          title = "Reef Status", opacity = 1
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
        "grossE_G" = "Gross<br/>bioerosion<br/>(kg CaCO\u00b3/yr)"
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

    # Restored RAP: active site uses the model's horizon value; others default
    # to their baseline RAP (set in baseline_map_sites).
    sel_row <- which(bs$Unique_Site_ID == sel_sid)
    if (length(sel_row) == 1) {
      mr <- model_result()
      if (!is.null(mr) && nrow(mr$budget_df) > 0) {
        horizon <- .safe_num(input$rest_horizon)
        hr <- min(horizon + 1, nrow(mr$budget_df))
        bs$restored_rap[sel_row] <- mr$budget_df$RAP_total[hr]
      }
    }
    bs$restored_state <- ifelse(bs$restored_rap > 0.5, "Growth",
                         ifelse(bs$restored_rap < -0.5, "Erosion", "Stasis"))

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
        halo_col <- if (base_rap > 0.5) {
          "skyblue"
        } else if (base_rap < -0.5 && restored_rap >= 0.5) {
          "darkgreen"
        } else if (base_rap >= -0.5 && base_rap < 0.5 && restored_rap >= 0.5) {
          "limegreen"
        } else if (base_rap <= -0.5 && restored_rap > -0.5 && restored_rap < 0.5) {
          "palegreen"
        } else if (restored_rap > -0.5 && restored_rap < 0.5) {
          "ivory"
        } else {
          "red"
        }
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
          "<td style='padding: 2px 0;'>", round(pfish_kg, 2), " kg CaCO\u00b3/yr</td></tr>",
          "<tr><td style='padding: 2px 8px 2px 0; font-weight: bold;'>Gross bioerosion:</td>",
          "<td style='padding: 2px 0;'>", round(gross_be_kg, 2), " kg CaCO\u00b3/yr</td></tr>",
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

  # ---- Named-reef labeled points ----
  # 250 m real-world circles (addCircles uses meters), light blue at 0.3 alpha,
  # each permanently labeled by its "Location" attribute (white text, black
  # outline). Toggled by the "Show Named Reefs" checkbox; own group so it never
  # churns with the NCRMP / baseline redraws.
  observe({
    proxy <- leafletProxy("mymap") |>
      clearGroup("named_reefs")

    if (!isTRUE(input$show_named_reefs) || is.null(named_reefs_sf) ||
        nrow(named_reefs_sf) == 0) {
      return(proxy)
    }

    labs <- as.character(named_reefs_sf$Location)

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
            "font-size" = "13px",
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
      if (is.na(lat) || is.na(lon)) return(NULL)

      area_val <- if ("Site_Area_m2" %in% names(sr) && any(!is.na(sr$Site_Area_m2))) {
        .safe_num(sr$Site_Area_m2[!is.na(sr$Site_Area_m2)][1])
      } else 100
      if (area_val <= 0) area_val <- 100

      # Per-site subregion / habitat for bioerosion resolution. Subregion may be
      # a full label or a code in the file; map to the code the data files use.
      raw_sub <- if ("Subregion" %in% names(sr) && any(!is.na(sr$Subregion))) {
        as.character(sr$Subregion[!is.na(sr$Subregion)][1])
      } else NA_character_
      # Data files use full names; expand a code to its label if one slips in.
      sub_full <- if (!is.na(raw_sub) && raw_sub %in% names(subregion_labels)) {
        unname(subregion_labels[raw_sub])
      } else raw_sub
      habitat <- if ("Habitat" %in% names(sr) && any(!is.na(sr$Habitat))) {
        as.character(sr$Habitat[!is.na(sr$Habitat)][1])
      } else NA_character_

      # Bioerosion terms (regional). parrotfish split out for the popup.
      be_split <- if (!is.na(sub_full) && !is.na(habitat)) {
        resolve_species_bioerosion(sub_full, habitat)
      } else list(parrotfish = 0, urchin = 0, sponge = 0)
      be_macro <- .safe0(be_split$parrotfish) + .safe0(be_split$urchin) + .safe0(be_split$sponge)

      # Unconsolidated substrate % from the special Taxon row (excluded from cover)
      uc_pct <- 0
      if (all(c("Taxon", "Percent_Cover") %in% names(sr))) {
        uc_row <- sr$Percent_Cover[sr$Taxon == "REQUIRED_Unconsolidated_substrate"]
        if (length(uc_row) && !is.na(uc_row[1])) uc_pct <- suppressWarnings(as.numeric(uc_row[1]))
      }

      # Area-occupied gross budget, total cover (skip the UC pseudo-taxon)
      patch <- 0; total_cover <- 0
      if (all(c("Taxon", "Percent_Cover") %in% names(sr))) {
        for (i in seq_len(nrow(sr))) {
          s <- sr$Taxon[i]
          if (identical(s, "REQUIRED_Unconsolidated_substrate")) next
          cvr <- suppressWarnings(as.numeric(sr$Percent_Cover[i]))
          if (is.na(cvr)) next
          total_cover <- total_cover + cvr
          rate <- calc_rates$rate[calc_rates$Taxon == s]
          if (length(rate) == 0 || is.na(rate[1])) next
          patch <- patch + area_val * (cvr / 100) * rate[1]
        }
      }
      gross_budget <- patch / area_val

      # Microbioerosion on consolidated substrate (UC now read from the file)
      consol_area <- area_val - (area_val * uc_pct / 100) - (area_val * total_cover / 100)
      be_micro <- if (consol_area > 0) (consol_area / area_val) * be_micro_rate else 0

      net_budget <- gross_budget - be_micro - be_macro
      rap <- net_budget / 2.9 / (1 - 0.6265)
      state <- if (rap > 0.5) "Growth" else if (rap < -0.5) "Erosion" else "Stasis"

      # Gross bioerosion figure for popup (whole-patch kg/yr, to echo NCRMP)
      gross_be_kg <- (be_micro + be_macro) * area_val
      pfish_kg    <- .safe0(be_split$parrotfish) * area_val

      data.frame(
        Unique_Site_ID = sid, lat = lat, lon = lon,
        rap = rap, state = state, restored_rap = rap,  # default: baseline
        cover = total_cover, pfish_kg = pfish_kg, gross_be_kg = gross_be_kg,
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
      selected = if (length(hab) == 1) hab else ""
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
      updateSelectizeInput(session, "baseline_site",
        choices = c("\u2013 Select site \u2013" = "", site_ids),
        selected = if (length(site_ids)) site_ids[1] else ""
      )
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
    # Cache a copy for auto-reload on next launch
    tryCatch(
      file.copy(input$baseline_upload$datapath, cached_baseline_path, overwrite = TRUE),
      error = function(e) NULL
    )
  })

# On launch: if a cached baseline exists, auto-load it via the same path.
  if (file.exists(cached_baseline_path)) {
    ingest_baseline_file(cached_baseline_path)
  }

  # Download the blank baseline-cover template (.xlsx) from GitHub (raw URL).
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
      sp <- setdiff(baseline_species_list(), "REQUIRED_Unconsolidated_substrate")
      covers <- vapply(sp, function(s) {
        .safe_num(input[[paste0("base_", gsub("[^A-Za-z0-9]", "_", s))]])
      }, numeric(1))

      # Subregion written as the FULL name (not the abbreviated code). The
      # in-app value is already the full label; keep it as-is, mapping a code
      # back to its label if somehow a code is present.
      sub_lbl <- input$subregion_choice
      sub_full <- if (sub_lbl %in% names(subregion_labels)) {
        unname(subregion_labels[sub_lbl])   # code -> full label
      } else {
        sub_lbl                              # already a full label
      }

      out <- data.frame(
        Unique_Site_ID = rep(if (nzchar(input$baseline_site)) input$baseline_site else "SITE_1",
                             length(sp)),
        Subregion      = rep(sub_full, length(sp)),
        Habitat        = rep(input$habitat_choice, length(sp)),
        Site_Area_m2   = rep(.safe_num(input$site_area_m2), length(sp)),
        Latitude       = rep(.safe_num(input$site_latitude), length(sp)),
        Longitude      = rep(.safe_num(input$site_longitude), length(sp)),
        Taxon          = sp,
        Percent_Cover  = covers,
        stringsAsFactors = FALSE
      )

      # Write the "Coral Cover input" sheet the ingestion path reads.
      writexl::write_xlsx(list("Coral Cover input" = out), path = file)
    }
  )

  # When the selected site changes, filter the upload to that site and
  # push subregion / habitat / area / species / covers into the inputs.
  observeEvent(input$baseline_site, {
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
    } else NA
    lon_val <- if ("Longitude" %in% names(site_rows) && any(!is.na(site_rows$Longitude))) {
      as.numeric(site_rows$Longitude[!is.na(site_rows$Longitude)][1])
    } else NA
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
    # habitat choices are in place before we set the selection)
    if ("Habitat" %in% names(site_rows) && any(!is.na(site_rows$Habitat))) {
      hab_val <- as.character(site_rows$Habitat[!is.na(site_rows$Habitat)][1])
      later::later(function() {
        updateSelectInput(session, "habitat_choice", selected = hab_val)
      }, delay = 0.2)
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

      # When baseline cover data is ingested, calculate the current carbonate
      # budget by area occupied per species. For each species, convert its
      # percent cover to occupied area (m^2), then multiply that area by the
      # species' calc_rates['rate'] (queried by Taxon). Subtract habitat
      # bioerosion so the Year-0 pip/popup shows a true net budget.
      area_val_num <- .safe_num(area_val)
      sp_budget <- 0
      for (s in sp) {
        cvr <- covers_vec[[s]]
        if (is.na(cvr)) next
        sp_area_m2 <- area_val_num * (cvr / 100)               # occupied area (m^2)
        rate <- calc_rates$rate[calc_rates$Taxon == s]     # query by Taxon
        if (length(rate) == 0 || is.na(rate[1])) next
        sp_budget <- sp_budget + sp_area_m2 * rate[1]          # kg CaCO3/yr (patch)
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
  }, ignoreInit = TRUE)

  # Add-species dropdown: append the chosen species to the baseline list
  # (dropdown itself is rendered below the list in baseline_cover_inputs).
  observeEvent(input$add_baseline_species, {
    s <- input$add_baseline_species
    req(nzchar(s))
    cur <- baseline_species_list()
    if (!(s %in% cur)) baseline_species_list(c(cur, s))
    # Reset the picker so the same species can't stack + the placeholder returns
    updateSelectizeInput(session, "add_baseline_species", selected = "")
  }, ignoreInit = TRUE)

  # Dynamic per-species numericInputs: species:%cover, with the add-species
  # dropdown rendered BELOW the list (or as the sole component when empty).
  output$baseline_cover_inputs <- renderUI({
    sp <- baseline_species_list()
    covers <- uploaded_covers()

    # Build one compact row (name + squished input) per species
    rows <- lapply(sp, function(s) {
      id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      val <- if (!is.null(covers) && s %in% names(covers) && !is.na(covers[[s]])) {
        covers[[s]]
      } else {
        0
      }
      tags$div(
        class = "baseline-species-row",
        tags$span(class = "baseline-species-name", title = s, s),
        tags$div(
          class = "baseline-species-input",
          numericInput(id, label = NULL, value = val, min = 0, max = 50, step = 0.1)
        )
      )
    })

    # Add-species picker, always rendered below the (possibly empty) list.
    # Not-yet-listed taxa only, so it can't add a duplicate.
    remaining <- setdiff(sort(unique(taxa)), sp)
    picker <- selectizeInput(
      "add_baseline_species", label = NULL,
      choices = c("+ Add species..." = "", remaining),
      selected = "",
      options = list(placeholder = "+ Add species...")
    )

    tagList(rows, tags$div(style = "margin-top: 6px;", picker))
  })

  # Fixed restoration species sliders (Restoration Mix box) ----
  restoration_species <- c(
    "Acropora cervicornis",  "Acropora palmata",
    "Colpophyllia natans",   "Diploria labyrinthiformis",
    "Montastraea cavernosa", "Orbicella faveolata",
    "Porites astreoides",    "Porites porites",
    "Pseudodiploria spp.",   "Siderastrea siderea",
    "Solenastrea bournoni",  "Stephanocoenia intersepta"
  )

  # Helper: build a column of species sliders for one morphology sub-box.
  # STATIC: sliders must not depend on model output, or the UI would rebuild
  # (resetting every slider to 0) whenever the model reruns. Per-species
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
                   title = s, HTML(species_label_2line(s))),
          numericInput(id, label = NULL, value = 0,
                       min = 0, max = 100, step = 0.1)
        ),
        tags$div(class = "rest-outplant-note", textOutput(nid, inline = TRUE))
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

  # Populate each per-species outplant caption independently of the sliders,
  # so updating counts never rebuilds (and thus never resets) the sliders.
  # Uses the "Gspe" code (1 genus letter + 3 species letters).
  observe({
    op <- model_outplants()   # named vector: species -> outplant count
    for (s in restoration_species) {
      local({
        sp  <- s
        nid <- paste0("outplants_", gsub("[^A-Za-z0-9]", "_", sp))
        output[[nid]] <- renderText({
          n_out <- if (!is.null(op) && sp %in% names(op)) op[[sp]] else NA
          if (!is.na(n_out) && n_out > 0) paste0(abbrev_species_code(sp), ": ", n_out, " outplants") else ""
        })
      })
    }
  })

  # Seed restoration-mix sliders from matching baseline values.
  # Re-fires whenever the baseline species set OR any per-species baseline
  # numericInput changes, so the mix keeps honoring the baseline cover input.
  observeEvent(
    {
      # Depend on the species set and on every dynamic base_ input value
      sel <- baseline_species_list()
      vals <- lapply(restoration_species, function(s) {
        input[[paste0("base_", gsub("[^A-Za-z0-9]", "_", s))]]
      })
      list(sel, vals)
    },
    {
      sel <- baseline_species_list()
      # brief defer so the dynamic base_ inputs exist before they are read
      later::later(function() {
        for (s in restoration_species) {
          rest_id <- paste0("rest_target_", gsub("[^A-Za-z0-9]", "_", s))
          if (s %in% sel) {
            base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
            isolate({
              updateNumericInput(session, rest_id, value = .safe_num(input[[base_id]]))
            })
          } else {
            # Species not in the active site: clear any leftover target value
            isolate({
              updateNumericInput(session, rest_id, value = 0)
            })
          }
        }
      }, delay = 0.2)
    },
    ignoreNULL = FALSE
  )

  ## ---------------------------------------------------------------------------
  ## Restoration Planning ----
  ## baseline / restored metrics + plotly timeline
  ## ---------------------------------------------------------------------------

  # Reactive store of RAP values (shared with Monitoring tab)
  rap_values <- reactiveValues(
    baseline = NULL,
    restored = NULL
  )

  # Baseline cover & carbonate budget from the entered/uploaded data.
  # Budget uses the area-occupied method: per species, area (m^2) * rate,
  # queried from calc_rates by Taxon, normalized to per-m2, minus bioerosion.
  baseline_metrics <- reactive({
    sim_duration <- .safe_num(input$sim_duration)
    unconsolidated_pct_cvr <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)
    sp <- setdiff(baseline_species_list(), "REQUIRED_Unconsolidated_substrate")
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    covers <- sapply(ids, function(id) .safe_num(input[[id]]))
    total_coral_pct_cvr <- sum(covers, na.rm = TRUE)

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
    } else 0
    macrobioerosion <- resolve_regional_bioerosion(input$subregion_choice, input$habitat_choice)

    budget <- (patch_budget / area_val_num) - macrobioerosion - microbioerosion

    # Baseline-assemblage porosity from the entered baseline covers
    base_cover_df <- data.frame(taxon = sp, cvr = covers, stringsAsFactors = FALSE)
    base_cover_df$cvr[is.na(base_cover_df$cvr)] <- 0
    bp <- assemblage_porosity(base_cover_df, "cvr")

    rap_values$baseline <- budget / 2.9 / (1 - bp)

    list(cover = total_coral_pct_cvr, budget = budget, rap = rap_values$baseline, sim_duration = sim_duration)
  })

  # Baseline growth series for the timeline (originals only). Available as soon
  # as species + covers + subregion/habitat are set, independent of any target.
  baseline_growth <- reactive({
    habitat       <- input$habitat_choice
    subregion <- input$subregion_choice
    site_area     <- .safe_num(input$site_area_m2)
    sim_duration  <- .safe_num(input$sim_duration)
    unconsolidated_pct_cvr <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)
    if (site_area <= 0) site_area <- 100
    req(nzchar(habitat), nzchar(subregion))

    sp <- setdiff(baseline_species_list(), "REQUIRED_Unconsolidated_substrate")
    if (length(sp) == 0) return(NULL)

    bdf <- data.frame(taxon = character(), current_cvr_pct = numeric(), stringsAsFactors = FALSE)
    for (s in sp) {
      base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      bdf[nrow(bdf) + 1, ] <- list(s, .safe_num(input[[base_id]]))
    }
    if (all(bdf$current_cvr_pct <= 0)) return(NULL)

    por <- assemblage_porosity(bdf, "current_cvr_pct")

    tryCatch(list(
        run_baseline_growth(
          site_area = site_area, uc_pct = unconsolidated_pct_cvr,
          sim_duration = sim_duration,
          bleaching_severity  = .safe_num(input$dhw),
          bleaching_frequency = .safe_num(input$bleach_events),
          baseline_cover_df = bdf
        ),
        porosity = por
      ),
      error = function(e) NULL
    )
  })

  # Restored (target) cover & carbonate budget (simple linear estimate; the
  # graph + saved values use the full model at the horizon instead).
  restored_metrics <- reactive({
    b <- baseline_metrics()
    slider_ids <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    rest_vals <- sapply(slider_ids, function(id) .safe_num(input[[id]]))
    rest_rates <- as.numeric(calc_rates$rate[match(restoration_species, calc_rates$Species)])
    net_rest <- sum(rest_vals * rest_rates / 100, na.rm = TRUE)

    total_coral_cover <- sum(rest_vals, na.rm = TRUE)
    budget <- b$budget + net_rest

    # Restored-assemblage porosity: baseline covers unioned with the mix
    # additions, summed per taxon.
    base_sp <- setdiff(baseline_species_list(), "REQUIRED_Unconsolidated_substrate")
    all_sp  <- union(base_sp, restoration_species)
    combined_cvr <- vapply(all_sp, function(s) {
      base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      rest_id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
      .safe_num(input[[base_id]]) + .safe_num(input[[rest_id]])
    }, numeric(1))
    rest_cover_df <- data.frame(taxon = all_sp, cvr = combined_cvr, stringsAsFactors = FALSE)
    rp <- assemblage_porosity(rest_cover_df, "cvr")

    rap_values$restored <- budget / 2.9 / (1 - rp)

    list(cover = total_coral_cover, budget = budget, rap = rap_values$restored)
  })

  # ---- Reactive restoration model ----
  # Derives all parameters from Shiny inputs, builds target_cover_df from the
  # per-species baseline (current) + restoration-mix (target) values.
  model_result <- reactive({
    # Derived parameters
    habitat        <- input$habitat_choice
    subregion <- input$subregion_choice
    site_area      <- .safe_num(input$site_area_m2)
    unconsolidated_pct_cvr <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)

    # User-input outplant parameters
    outplant_diam <- .safe_num(input$outplant_size) / 100 # cm -> m
    outplant_cost <- .safe_num(input$outplant_cost)
    sim_duration  <- .safe_num(input$sim_duration)
    rest_horizon  <- .safe_num(input$rest_horizon)

    # Refuse to run when the horizon exceeds the simulation duration (a red
    # warning shows below the sim-duration slider).
    if (rest_horizon > sim_duration) return(NULL)

    # Bleaching parameters
    bleaching_severity  <- .safe_num(input$dhw)           # degree-heating weeks
    bleaching_frequency <- .safe_num(input$bleach_events) # events in a 5-year period

    req(nzchar(habitat), nzchar(subregion), site_area > 0)

    # Build target_cover_df from the FULL baseline species set (so unrestored
    # species still grow), unioned with the restoration-mix species. current =
    # baseline % cover input; target = restoration-mix slider (0 if none).
    base_sp <- setdiff(baseline_species_list(), "REQUIRED_Unconsolidated_substrate")
    all_sp <- union(base_sp, restoration_species)

    target_cover_df <- data.frame(
      taxon = character(),
      current_cvr_pct = numeric(),
      target_cvr_pct = numeric(),
      stringsAsFactors = FALSE
    )
    for (s in all_sp) {
      base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      rest_id <- paste0("rest_target_", gsub("[^A-Za-z0-9]", "_", s))
      current_sp_pct <- .safe_num(input[[base_id]])
      target_sp_pct  <- .safe_num(input[[rest_id]])
      target_cover_df[nrow(target_cover_df) + 1, ] <- list(s, current_sp_pct, target_sp_pct)
    }

    # Nothing present to grow at all -> no model output
    if (all(target_cover_df$current_cvr_pct <= 0 & target_cover_df$target_cvr_pct <= 0)) {
      return(NULL)
    }

    #tryCatch(
      run_restoration_model(
        habitat = habitat, subregion = subregion,
        site_area = site_area, uc_pct = unconsolidated_pct_cvr,
        sim_duration = sim_duration, rest_horizon = rest_horizon,
        outplant_diam = outplant_diam, outplant_cost = outplant_cost,
        bleaching_severity = bleaching_severity,
        bleaching_frequency = bleaching_frequency,
        target_cover_df = target_cover_df
      ) #,
    #   error = function(e) {
    #     showNotification(paste("Model error:", e$message), type = "error")
    #     NULL
    #   }
    # )
  })

  # ---- Baseline / restored values at the restoration horizon ----
  # Single source of truth for the saved scenario + percentile surrounds, taken
  # from the same model output the graph uses (so saved == graphed). Falls back
  # to the linear metrics estimate when there is no model output.
  horizon_vals <- reactive({
    mr <- model_result()
    b  <- baseline_metrics()
    r  <- restored_metrics()
    horizon <- .safe_num(input$rest_horizon)
    if (!is.null(mr) && nrow(mr$budget_df) > 0) {
      hr <- min(horizon + 1, nrow(mr$budget_df))
      bd <- mr$budget_df
      list(
        b_cover  = bd$pct_cvr_orig[hr],   r_cover  = bd$pct_cvr_total[hr],
        b_budget = bd$calc_budg_orig[hr], r_budget = bd$calc_budg_total[hr],
        b_rap    = bd$RAP_orig[hr],       r_rap    = bd$RAP_total[hr]
      )
    } else {
      list(
        b_cover = b$cover, r_cover = r$cover,
        b_budget = b$budget, r_budget = r$budget,
        b_rap = b$rap, r_rap = r$rap
      )
    }
  })

  # ---- Per-species outplant counts ----
  # (drives the Restoration Mix captions)
  model_outplants <- reactive({
    mr <- model_result()
    if (is.null(mr)) return(NULL)
    mr$outplants_by_species
  })

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
      tags$div(class = "sim-warning",
        paste0("Total target cover (", round(tot, 1),
               "%) exceeds 100%. Reduce the mix to run the model."))
    }
  })

  # Baseline RAP percentile (gray) + restored RAP percentile at horizon (colored)
  output$rap_pctile_baseline <- renderUI({
    pct <- ingested_baseline_pctile()
    if (is.null(pct) || is.na(pct)) return(NULL)
    tags$span(style = "color:#777777;",
      paste0("Baseline RAP percentile: ", round(pct), "%"))
  })
  output$rap_pctile_restored <- renderUI({
    mr <- model_result()
    if (is.null(mr) || nrow(mr$budget_df) == 0) return(NULL)
    horizon <- .safe_num(input$rest_horizon)
    hr <- min(horizon + 1, nrow(mr$budget_df))
    pct <- rap_percentile(mr$budget_df$RAP_total[hr])
    if (is.na(pct)) return(NULL)
    tags$span(style = paste0("color:", percentile_color(pct), ";"),
      paste0("RAP percentile at restoration horizon: ", round(pct), "%"))
  })

  output$restoration_timeline <- plotly::renderPlotly({
    b <- baseline_metrics()
    mr <- model_result()
    bg <- baseline_growth()

    # Guard: on cached auto-load the observers can fire before baseline_growth()
    # has a valid frame. Treat a NULL/empty result as "no baseline yet" so we
    # never run min()/max()/ggplot on a length-0 or NULL bg_df (which produced
    # the Inf/-Inf warnings and the fortify error flashing on the plot).
    bg_ok <- !is.null(bg) &&
             is.data.frame(bg[[1]]) && nrow(bg[[1]]) > 0
    bg_df <- if (bg_ok) bg[[1]] else NULL
    bp    <- if (bg_ok) bg[[2]] else 0.6265  # baseline porosity fallback

    site_area <- .safe_num(input$site_area_m2)
    uc_pct    <- .safe_num(input$base_REQUIRED_Unconsolidated_substrate)
    macrobioerosion <- resolve_regional_bioerosion(input$subregion_choice, input$habitat_choice)

    # Apply bioerosion to baseline growth only when it is a real frame.
    if (!is.null(bg_df)) {
      bg_df <- baseline_bioerosion_RAP(bg_df, site_area, uc_pct, be_micro_rate, macrobioerosion, bp)
    }

    tryCatch({
        write.csv(mr$budget_df, here("cache", "model_results.csv"))
        write.csv(bg_df, here("cache", "baseline_results.csv"))
        write.csv(b, here("cache", "baseline_metrics.csv"))
    }, error = function(e) {
      warning("Error writing budget results: ", e$message)
    })

    dur <- b$sim_duration
    horizon <- .safe_num(input$rest_horizon)
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

    # Line-weight emphasis: ssp245 heaviest, then ssp126 & ssp370, rest light
    slr_weight <- c(
      ssp119 = 0.2, ssp126 = 0.4, ssp245 = 0.75,
      ssp370 = 0.4, ssp585 = 0.2
    )
    # ssp245 solid; all others dashed
    slr_dash <- c(
      ssp119 = "dash", ssp126 = "dash", ssp245 = "solid",
      ssp370 = "dash", ssp585 = "dash"
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
        y_hi <- 8
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
          labs(x = "Year", y = "RAP (mm/yr)") +
          theme_minimal(base_size = 14)
        y0_rap <- b$rap
      } else {
        y_lo <- rap_axis_min(min(bg_df$RAP_orig, na.rm = TRUE))
        y_hi <- max(max(slr_tl$SLR), max(bg_df$RAP_orig, na.rm = TRUE))
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
                                      "<br>Cover: ", round(pct_cvr_orig, 1), " %",
                                      "<br>RAP: ", round(RAP_orig, 2), " mm/yr",
                                      "<br>Budget: ", round(calc_budg_orig, 2), " kg/m\u00b2/yr")),
                    linetype = "longdash",
                    color = orig_col, linewidth = 0.7) +

          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(limits = c(y_lo, y_hi),
                             breaks = rap_axis_breaks(y_lo, y_hi)) +
          labs(x = "Year", y = "RAP (mm/yr)") +
          theme_minimal(base_size = 14)
        y0_rap <- bg_df$RAP_orig[1]
      }
    } else if (is.null(bg_df)) {
      # Model output exists but baseline growth isn't ready yet (cached-load
      # race). Fall back to an empty placeholder rather than indexing a NULL.
      y_lo <- rap_axis_min(-1)
      y_hi <- 8
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
        labs(x = "Year", y = "RAP (mm/yr)") +
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
        calc_budg_orig  = bd$calc_budg_orig,
        calc_budg_total = bd$calc_budg_total
      )

      write.csv(d, here("cache", "combined_data.csv"))

      pips <- d[d$Year %in% c(1, 5, 10, 20, 50, 100, dur), ]

      y_lo <- rap_axis_min(min(d$RAP_orig, na.rm = TRUE))
      y_hi <- max(max(slr_tl$SLR), max(d$RAP_total, na.rm = TRUE))
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
                                    "<br>Cover: ", round(pct_cvr_orig, 1), " %",
                                    "<br>RAP: ", round(RAP_orig, 2), " mm/yr",
                                    "<br>Budget: ", round(calc_budg_orig, 2), " kg/m\u00b2/yr")),
                   linetype = "longdash",
                   color = orig_col, linewidth = 0.7) +
        # Total RAP: dark green
        geom_line(aes(y = RAP_total, group = 2,
                      text = paste0("<br>Year ", Year,
                                    "<br>Cover: ", round(pct_cvr_total, 1), " %",
                                    "<br>RAP: ", round(RAP_total, 2), " mm/yr",
                                    "<br>Budget: ", round(calc_budg_total, 2), " kg/m\u00b2/yr")),
                  color = "darkgreen", linewidth = 1.1) +
        geom_point(
          data = pips,
          aes(y = RAP_total, text = paste0(
            "Year ", Year,
            "<br>Cover: ", round(pct_cvr_total, 1), "%",
            "<br>RAP: ", round(RAP_total, 2), " mm/yr",
            "<br>Budget: ", round(calc_budg_total, 2), " kg/m\u00b2/yr"
          )),
          size = 4, color = "darkgreen"
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

    # Add one blue SLR line per scenario, ordered so heavy lines draw on top.
    if (!is.null(slr_tl) && nrow(slr_tl) > 0) {
      scn_order <- c("ssp119", "ssp585", "ssp370", "ssp126", "ssp245")
      scn_order <- scn_order[scn_order %in% unique(slr_tl$Scenario)]
      for (scn in scn_order) {
        sd <- slr_tl[slr_tl$Scenario == scn, ]
        p <- p + geom_line(
          data = sd,
          aes(x = Year, y = SLR, group = Scenario,
              text = paste0(toupper(Scenario),
                            "<br>Year ", Year,
                            "<br>SLR: ", round(SLR, 2), " mm/yr")),
          color = "#1f6fd6",
          linewidth = unname(slr_weight[scn]),
          linetype = unname(slr_dash[scn]),
          inherit.aes = FALSE
        )
      }
    }

    gp <- plotly::ggplotly(p, tooltip = "text")

    # Year-0 baseline annotation ----
    # shows CURRENT RAP + cover + budget (always on).
    # Background/border/text respond to Dark Mode.
    cur_budget <- ingested_current_budget()
    # show_budget <- if (!is.null(cur_budget)) cur_budget else b$budget
    y0_label <- paste0(
      "Baseline<br>Cover: ", round(b$cover, 1), "%",
      "<br>RAP: ", round(b$rap, 2), " mm/yr",
      "<br>Budget: ", round(b$budget, 2), " kg/m\u00b2/yr"
    )

    # Build plotly layout ----
    gp <- gp |>
      plotly::layout(
        annotations = list(
          list(
            x = 0, y = y0_rap, text = y0_label,
            showarrow = TRUE, arrowhead = 0, ax = 40, ay = -60,
            align = "left", bgcolor = ann_bg, bordercolor = ann_border,
            borderwidth = 2, font = list(size = 11, color = ann_font)
          )
        ),
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
      ) |>
      plotly::add_annotations(
        x = 0.92, y = 3.1, xref = "paper", yref = "y",
        text = "Geologic baseline RAP: 3.1 mm/yr",
        showarrow = FALSE, yshift = 9,
        font = list(color = "#b8860b", size = 11),
        bgcolor = paper_bg, opacity = 0.9
      )

    gp
  })

  ## ---------------------------------------------------------------------------
  ## Save scenario (Restoration Planning) ----
  ## ---------------------------------------------------------------------------
  observeEvent(input$save_scenario, {
    shiny::validate(shiny::need(nzchar(input$scenario_project), "Enter a project name."))
    shiny::validate(shiny::need(nzchar(input$scenario_name), "Enter a scenario name."))

    mr <- model_result()
    hv <- horizon_vals()   # baseline/restored evaluated at the restoration horizon

    total_coral_pct_cvr  <- hv$r_cover
    restored_rap <- hv$r_rap
    baseline_rap <- hv$b_rap

    # Prefer model-derived cost when available; else illustrative fallback
    added_cover <- hv$r_cover - hv$b_cover
    cost <- if (!is.null(mr)) mr$cost else added_cover * .safe_num(input$outplant_cost) * 100
    outplants <- if (!is.null(mr) && length(mr$outplants)) mr$outplants else NA
    elev_gain_10yr <- restored_rap * 10 # mm over 10 years
    # Legacy ROI field retained for backward compatibility; the Scenario
    # Comparison plot now recomputes ROI from net kg CaCO3 / cost at render.
    roi <- if (cost > 0) (elev_gain_10yr / cost) * 1000 else 0

    # Build the scenario, forcing every field to a length-1 scalar
    scalar1 <- function(x) if (is.null(x) || length(x) == 0) NA else x[[1]]
    scenario <- list(
      project = scalar1(input$scenario_project),
      scenario = scalar1(input$scenario_name),
      site = scalar1(input$baseline_site),
      subregion = scalar1(input$subregion_choice),
      habitat = scalar1(input$habitat_choice),
      site_area_m2 = .safe_num(input$site_area_m2),
      total_coral_pct_cvr = scalar1(total_coral_pct_cvr),
      baseline_cover = scalar1(hv$b_cover),
      restored_cover = scalar1(hv$r_cover),
      baseline_budget = scalar1(hv$b_budget),
      restored_budget = scalar1(hv$r_budget),
      baseline_rap = scalar1(baseline_rap),
      restored_rap = scalar1(restored_rap),
      outplants = scalar1(outplants),
      outplant_size = .safe_num(input$outplant_size),
      outplant_cost = .safe_num(input$outplant_cost),
      dhw = .safe_num(input$dhw),
      bleach_events = .safe_num(input$bleach_events),
      rest_horizon = .safe_num(input$rest_horizon),
      sim_duration = .safe_num(input$sim_duration),
      cost = scalar1(cost),
      roi = scalar1(roi),
      elev_gain_10yr = scalar1(elev_gain_10yr),
      saved = as.character(Sys.time())
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
  observeEvent(input$cc_clear_cache, {
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

    # Regional split for per-taxon-type fallback
    reg <- resolve_species_bioerosion(input$subregion_choice, input$habitat_choice)

    site <- input$monitoring_selected_site
    req(!is.null(site), nzchar(site))
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
    all_years <- sort(unique(c(yr_of(pf), yr_of(ur), yr_of(sp))))
    if (length(all_years) == 0) return(NULL)

    unobserved_all <- character(0)
    by_year <- setNames(numeric(length(all_years)), as.character(all_years))

    for (yr in all_years) {
      # Parrotfish
      if (!sheet_is_empty(pf)) {
        rows <- pf[pf$Years_Post_Restoration == yr & pf$Life_phase != "JU", , drop = FALSE]
        pfr <- compute_parrotfish_erosion(rows, sp_erosion_parrotfish)
        pfr <- compute_parrotfish_erosion(rows, sp_erosion_parrotfish)
        pf_val <- pfr$total
        unobserved_all <- c(unobserved_all, pfr$unobserved)
      } else {
        pf_val <- reg$parrotfish
      }
      # Urchins
      ur_val <- if (!sheet_is_empty(ur)) {
        compute_urchin_erosion(ur[ur$Years_Post_Restoration == yr, , drop = FALSE], sp_erosion_urchins)
      } else reg$urchin
      # Sponges
      sp_val <- if (!sheet_is_empty(sp)) {
        compute_sponge_erosion(sp[sp$Years_Post_Restoration == yr, , drop = FALSE], sp_erosion_sponges)
      } else reg$sponge

      by_year[as.character(yr)] <- .safe0(pf_val) + .safe0(ur_val) + .safe0(sp_val)
    }

    list(by_year = by_year, unobserved = unique(unobserved_all))
  })

  # Red warning for any unobserved parrotfish size classes
  output$bioerosion_parrotfish_warning <- renderUI({
    bio <- monitoring_bioerosion_by_year()
    if (is.null(bio) || length(bio$unobserved) == 0) return(NULL)
    msgs <- vapply(bio$unobserved, function(u) {
      paste0("An unobserved parrotfish size class has been entered: ", u,
             ". Ensure parrotfish bioerosion data has been entered accurately.")
    }, character(1))
    tags$div(class = "bioerosion-warning",
      lapply(msgs, function(m) tags$div(m))
    )
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
  monitoring_series <- reactive({
    info <- monitoring_cover_path()
    if (is.null(info) || info$ext != "xlsx") return(NULL)

    cover <- uploaded_monitoring_cover()
    if (is.null(cover)) return(NULL)

    needed <- c("Years_Post_Restoration", "Taxon", "Percent_Cover", "Unique_Site_ID")
    if (!all(needed %in% names(cover))) return(NULL)

    site <- input$monitoring_selected_site
    req(!is.null(site), nzchar(site))
    cover <- cover[as.character(cover$Unique_Site_ID) == site, , drop = FALSE]
    if (nrow(cover) == 0) return(NULL)

    # Site area from the cover .xlsx (Site_Area_m2); default 100
    site_area <- if ("Site_Area_m2" %in% names(cover) && any(!is.na(cover$Site_Area_m2))) {
      .safe0(cover$Site_Area_m2[!is.na(cover$Site_Area_m2)][1])
    } else 100
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

      # Bioerosion for this year: observed (nearest-year substitution) or regional
      bio_year <- if (!is.null(bio)) nearest_bioerosion(bio, yr) else reg_total
      if (is.na(bio_year)) bio_year <- reg_total

      # Net budget = gross - generalized micro - (observed/regional) macro
      # NOTE: COME BACK AND FIX THIS SO MICROBIOEROSION IS APPLIED TO CONSOLIDATED SUBSTRATE
      net_budget <- gross_budget - be_micro_rate - .safe0(bio_year)
      rap <- net_budget / 2.9 / (1 - por)
      total_coral_pct_cvr <- sum(cover_df$cvr, na.rm = TRUE)

      out <- rbind(out, data.frame(
        Year = yr, RAP = rap, budget = net_budget, cover = total_coral_pct_cvr,
        stringsAsFactors = FALSE
      ))
    }
    out[order(out$Year), ]
  })

  # Helper: baseline metrics for the selected NCRMP site (upload overrides df).
  # When the monitoring pipeline is active, "Baseline" is the -1 row.
  cc_baseline_vals <- reactive({
    ms <- monitoring_series()
    if (!is.null(ms) && any(ms$Year == -1)) {
      r <- ms[ms$Year == -1, ][1, ]
      return(list(cover = r$cover, budget = r$budget, rap = r$RAP))
    }

    req(input$monitoring_selected_site)
    dat <- df |> filter(site_id == input$monitoring_selected_site) |> slice(1)
    list(
      cover  = dat$hardCoral_PrctCvr,
      budget = dat$net_G,
      rap    = if (!is.null(dat$rap) && !is.na(dat$rap)) dat$rap else dat$net_G / 2.9 / (1 - 0.6265)
    )
  })

  # Helper: restored metrics.
  #  - Monitoring pipeline active -> the LAST (max year) row is "Restored".
  #  - No cover upload (map-driven) -> project the SELECTED SITE's own baseline
  #    with the Home-tab linear model + the map's target-cover inputs.
  cc_restored_vals <- reactive({
    ms <- monitoring_series()
    if (!is.null(ms) && nrow(ms) > 0) {
      r <- ms[which.max(ms$Year), ]
      return(list(cover = r$cover, budget = r$budget, rap = r$RAP))
    }

    base <- cc_baseline_vals()
    inc <- if (is.null(input$target_cover_increase)) 0 else input$target_cover_increase
    restored_rap    <- base$rap + cover_rap_slope * inc
    restored_cover  <- base$cover + inc
    restored_budget <- restored_rap * 2.9 * (1 - 0.6265)
    list(cover = restored_cover, budget = restored_budget, rap = restored_rap)
  })

  output$cc_baseline_cover <- renderValueBox({
    valueBox(paste0(round(cc_baseline_vals()$cover, 1), " %"),
      "Baseline coral cover",
      icon = icon("percent"), color = "green"
    )
  })
  output$cc_baseline_budget <- renderValueBox({
    valueBox(paste0(round(cc_baseline_vals()$budget, 2), " kg/m\u00b2/yr"),
      "Baseline carbonate budget",
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$cc_baseline_rap <- renderValueBox({
    valueBox(paste0(round(cc_baseline_vals()$rap, 2), " mm/yr"),
      "Baseline reef accretion",
      icon = icon("chart-line"), color = "aqua"
    )
  })

  output$cc_restored_cover <- renderValueBox({
    valueBox(paste0(round(cc_restored_vals()$cover, 1), " %"),
      "Restored coral cover",
      icon = icon("plus-circle"), color = "olive"
    )
  })
  output$cc_restored_budget <- renderValueBox({
    valueBox(paste0(round(cc_restored_vals()$budget, 2), " kg/m\u00b2/yr"),
      "Restored carbonate budget",
      icon = icon("balance-scale"), color = "blue"
    )
  })
  output$cc_restored_rap <- renderValueBox({
    valueBox(paste0(round(cc_restored_vals()$rap, 2), " mm/yr"),
      "Restored reef accretion",
      icon = icon("chart-line"), color = "teal"
    )
  })

  # Impact summary text box (reuses shared builder)
  output$cc_impact_summary <- renderUI({
    b <- cc_baseline_vals()
    r <- cc_restored_vals()
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
  # Reference lines (geologic + SSP245 rates) + y-axis floor + dark-mode shared.
  output$cc_timeline <- plotly::renderPlotly({
    ms <- monitoring_series()

    dark <- isTRUE(input$dark_mode)
    paper_bg <- if (dark) "#232a33" else "white"
    plot_bg  <- if (dark) "#232a33" else "white"
    font_col <- if (dark) "#e6e6e6" else "#333333"
    grid_col <- if (dark) "#5a6472" else "#d9d9d9"

    # SSP245 reference rates (mm/yr) at 2030 / 2050 / 2100
    slr_refs <- c(
      "SSP245 @2030" = ssp245_rate_at(2030),
      "SSP245 @2050" = ssp245_rate_at(2050),
      "SSP245 @2100" = ssp245_rate_at(2100)
    )
    geo_baseline <- 3.1

    if (!is.null(ms) && nrow(ms) > 0) {
      # ---- File-driven: real per-year RAP series (Baseline at x = -1) ----
      x_min <- min(ms$Year); x_max <- max(ms$Year)
      # x breaks: label -1 as "Baseline", then integer years
      x_vals   <- sort(unique(ms$Year))
      x_labels <- ifelse(x_vals == -1, "Baseline", as.character(x_vals))

      data_min <- min(c(ms$RAP, geo_baseline, slr_refs, -0.5), na.rm = TRUE)
      y_lo <- rap_axis_min(data_min)
      y_hi <- max(c(ms$RAP, geo_baseline, slr_refs), na.rm = TRUE) + 1
      bands <- status_bands_df(x_min, x_max, y_lo)

      ribbon_df <- insert_threshold_crossings(ms, xcol = "Year", threshold = 0.5)
      print(ribbon_df)

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
                                    "<br>Cover: ", round(cover, 1), " %",
                                    "<br>Budget: ", round(budget, 2), " kg/m\u00b2/yr")),
                  color = "forestgreen", linewidth = 1.4) +
        geom_point(aes(y = RAP, text = paste0(
                        ifelse(Year == -1, "Baseline", paste0("Year ", Year)),
                        "<br>RAP: ", round(RAP, 2), " mm/yr")),
                   color = "forestgreen", size = 3) +
        scale_x_continuous(breaks = x_vals, labels = x_labels) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = rap_axis_breaks(y_lo, y_hi)) +
        labs(x = "Years post-restoration", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)

      band_x <- c(x_min, x_max)
    } else {
      # ---- Map-driven: smooth interpolation Baseline (Y0) -> Restored (Y10) ----
      b <- cc_baseline_vals(); r <- cc_restored_vals()
      dur <- 10; years <- 0:dur
      rap_series <- b$rap + (r$rap - b$rap) * (years / dur)
      tl <- data.frame(Year = years, RAP = rap_series, stringsAsFactors = FALSE)

      data_min <- min(c(tl$RAP, geo_baseline, slr_refs, -0.5), na.rm = TRUE)
      y_lo <- rap_axis_min(data_min)
      y_hi <- max(c(tl$RAP, geo_baseline, slr_refs), na.rm = TRUE) + 1
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
                  color = "forestgreen", linewidth = 1.4) +
        scale_x_continuous(breaks = years) +
        scale_y_continuous(limits = c(y_lo, y_hi),
                           breaks = rap_axis_breaks(y_lo, y_hi)) +
        labs(x = "Year", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)

      band_x <- c(0, dur)
    }

    gp <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = paper_bg,
        plot_bgcolor  = plot_bg,
        font = list(color = font_col),
        xaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        yaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        legend = list(orientation = "h", x = 0, y = 1.1)
      )

    # Geologic baseline (gold dashed)
    gp <- gp |>
      plotly::add_trace(
        x = band_x, y = c(geo_baseline, geo_baseline),
        type = "scatter", mode = "lines",
        line = list(color = "gold", width = 2, dash = "dash"),
        showlegend = FALSE, inherit = FALSE,
        hoverinfo = "text",
        text = paste0("Geologic baseline RAP: ", geo_baseline, " mm/yr")
      ) |>
      plotly::add_annotations(
        x = 0.98, y = geo_baseline, xref = "paper", yref = "y",
        text = paste0("Geologic baseline RAP: ", geo_baseline, " mm/yr"),
        showarrow = FALSE, yshift = 9, xanchor = "right",
        font = list(color = "#b8860b", size = 11),
        bgcolor = paper_bg, opacity = 0.9
      )

    # SSP245 reference rates (blue dashed) at 2030 / 2050 / 2100
    slr_ann_col <- "#1f6fd6"
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

    gp
  })

  # Download report for the Restoration Monitoring tab
  output$cc_download_report <- downloadHandler(
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
        b <- cc_baseline_vals()
        r <- cc_restored_vals()
        out <- data.frame(
          Site = input$monitoring_selected_site,
          Metric = c("Coral cover (%)", "Carbonate budget (kg/m2/yr)", "Reef accretion (mm/yr)"),
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
  # Populate the scenario multi-select based on chosen project.
  # All scenarios within the selected project are ENABLED (selected) by default.
  observe({
    sc <- all_scenarios()
    req(input$sc_project)
    scen <- if (nrow(sc)) sort(unique(sc$scenario[sc$project == input$sc_project])) else character(0)
    updateCheckboxGroupInput(session, "sc_scenarios",
                             choices = scen, selected = scen)
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
    # Net kg CaCO3 (added product of restoration at the horizon):
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

  # Collapsible per-scenario Impact Summary (reuses the shared builder).
  # Each scenario is a nested collapsible panel; its border uses the scenario's
  # assigned pastel color.
  output$sc_impact_summaries <- renderUI({
    d <- sc_selected()
    if (nrow(d) == 0) {
      return(tags$em("Select one or more scenarios."))
    }
    cols <- sc_colors()
    panels <- lapply(seq_len(nrow(d)), function(i) {
      row <- d[i, ]
      col <- if (row$scenario %in% names(cols)) cols[[row$scenario]] else "#ddd"
      summary_html <- build_impact_summary(
        label = paste0("Scenario: ", row$scenario),
        b_cover = row$baseline_cover, r_cover = row$restored_cover,
        b_budget = row$baseline_budget, r_budget = row$restored_budget,
        b_rap = row$baseline_rap, r_rap = row$restored_rap
      )
      panel_id <- paste0("sc_panel_", gsub("[^A-Za-z0-9]", "_", row$scenario))
      tags$div(
        style = paste0("border:2px solid ", col, "; border-radius:6px; margin-bottom:8px;"),
        tags$div(
          style = paste0("cursor:pointer; padding:6px 10px; font-weight:bold; ",
                         "background:", col, "33; border-radius:4px 4px 0 0;"),
          onclick = paste0("var b=document.getElementById('", panel_id,
                           "'); b.style.display=(b.style.display==='none')?'block':'none';"),
          tags$span(row$scenario),
          tags$span(icon("chevron-down"), style = "float:right;")
        ),
        tags$div(
          id = panel_id,
          class = "impact-summary-box",
          style = "display:block; background:#f7f7f7; padding:10px;",
          summary_html
        )
      )
    })
    tagList(panels)
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
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  }, bg = "transparent")

  # ROI bar: net kg CaCO3 per dollar (pastel per scenario, dark-mode aware)
  output$sc_roi_bar <- renderPlot({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    cols <- sc_colors()
    ggplot(d, aes(x = scenario, y = roi_kg_per_dollar, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "ROI (net kg CaCO\u2083 per $)") +
      scale_fill_manual(values = cols) +
      sc_theme()[[1]] +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  }, bg = "transparent")

  # Per-scenario RAP bar with reference lines + status bands.
  # Bands ride through tooltip="text" as "Erosion"/"Stasis".
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
      "SSP245 @2030" = ssp245_rate_at(2030),
      "SSP245 @2050" = ssp245_rate_at(2050),
      "SSP245 @2100" = ssp245_rate_at(2100)
    )

    data_min <- min(c(d$restored_rap, geo_baseline, slr_refs, -0.5), na.rm = TRUE)
    y_lo <- rap_axis_min(data_min)
    y_hi <- max(c(d$restored_rap, geo_baseline, slr_refs), na.rm = TRUE) + 1

    d$scenario <- factor(d$scenario, levels = d$scenario)
    n_sc <- nrow(d)
    # geom_rect status bands spanning the full categorical width
    bands <- status_bands_df(0.4, n_sc + 0.6, y_lo)

    p <- ggplot(d, aes(x = scenario, y = restored_rap, fill = scenario)) +
      geom_rect(data = bands, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    fill = fill, text = label), alpha = 0.30) +
      geom_col(aes(text = paste0(scenario,
                                 "<br>Restored RAP: ", round(restored_rap, 2), " mm/yr")),
               alpha = 1, width = 0.7) +
      scale_fill_manual(values = c(cols, setNames(c("red","yellow"), c("red","yellow")))) +
      scale_y_continuous(limits = c(y_lo, y_hi), breaks = rap_axis_breaks(y_lo, y_hi)) +
      labs(x = NULL, y = "Restored RAP (mm/yr)") +
      sc_theme()[[1]] +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

    gp <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        paper_bgcolor = paper_bg, plot_bgcolor = paper_bg,
        font = list(color = font_col),
        xaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col),
        yaxis = list(color = font_col, gridcolor = grid_col, tickcolor = grid_col)
      )

    ref_df <- data.frame(
      label = c("Geologic baseline", names(slr_refs)),
      yval  = c(geo_baseline, unname(slr_refs)),
      col   = c("#b8860b", rep("#1f6fd6", length(slr_refs))),
      stringsAsFactors = FALSE
    )
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