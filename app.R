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
library(readxl)
library(plotly)
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
calc_rates   <- read.csv(here("data", "Travis_rates.csv"))
# Species-specific average colony diameters
diams        <- read.csv(here("data", "NCRMP_colony_dia_Florida.csv"))
# Region- and habitat-specific bioerosion rates
bioerosion   <- read.csv(here("data", "Bioerosion.csv"))

# Ingest NCRMP carbonate budget data
df <- read.csv(here("data", "NCRMP_CarbonateBudgets_2014_to_2024.csv"))

# Create unique site IDs in case PRIMARY_SAMPLE_UNIT is reused/not unique
df$site_id <- paste(df$YEAR, df$SUB_REGION, df$PRIMARY_SAMPLE_UNIT, sep = "_")

sites <- sort(df$site_id)

# Ingest regions polygon shapefile
regions_sf <- sf::st_read(here("data", "regions", "regions.shp"), quiet = TRUE)

# Ensure geographic CRS (WGS84) so it aligns with the leaflet basemap
regions_sf <- sf::st_transform(regions_sf, 4326)

# Pastel palette keyed to the Region field
region_levels <- sort(unique(regions_sf$Region))
pastel_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))(length(region_levels))
region_pal <- colorFactor(pastel_colors, domain = region_levels)


# Ingest baseline cover data template to retrieve list of taxa
base_cover_df <- read_excel(here("data", "Baseline_cover_TEMPLATE.xlsx"), sheet = "Coral Cover input")
taxa <- read_excel(here("data", "Baseline_cover_TEMPLATE.xlsx"), sheet = "Taxa")
taxa <- taxa$Taxon

# Ingest IPCC AR6 sea-level projections (PSMSL id 363, "Total" sheet)
slr_raw <- readxl::read_excel(
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

# Calculate reef accretion potential
df$rap <- df$net_G / 2.9 / (1 - 0.6265)
df$current_state <- ifelse(df$rap > 0.5, "Growth", ifelse(df$rap < -0.5, "Erosion", "Stasis"))

# RAP percentile helper ----
# Rank a RAP value against the NCRMP baseline distribution (df$rap).
rap_percentile <- function(rap_value) {
  vals <- df$rap[is.finite(df$rap)]
  if (length(vals) == 0 || is.na(rap_value)) return(NA_real_)
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

# Canonical field order for saved scenarios (used to sanitize read + write) ----
scenario_fields <- c(
  "project", "scenario", "site", "subregion", "habitat", "site_area_m2",
  "total_cover", "baseline_cover", "restored_cover",
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
# Reverse map (label -> code) for looking up habitat sets / model data
subregion_codes <- setNames(names(subregion_labels), unname(subregion_labels))

# Abbreviate a species name to "G. species" unless it ends with "spp."
abbrev_species <- function(s) {
  if (grepl("spp\\.$", s)) return(s)          # leave "Genus spp." intact
  parts <- stringr::str_split(s, " ")[[1]]
  if (length(parts) < 2) return(s)            # nothing to abbreviate
  paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
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
  0.20  # Default: 0% mortality for larger colonies
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
# (shared by the model and baseline grower) 
# Chooses Acropora / Massive / Mixed porosity (as a proportion) from a
# cover_df with columns `taxon` and a numeric cover column named by cover_col.
assemblage_porosity <- function(cover_df, cover_col) {
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

# ----------------------------------------------------------------------------
# Growth simulation ----
# Applicable to original colonies and new outplants, and reused by both the
# restoration model and the baseline-growth reactive. Performs its own per-
# species lookups (growth rate, DHW slope) from `species` + `bleaching_severity`.
# Applies Year-1 outplant mortality (group == "outplant" only), per-event
# bleaching dieoff, post-bleaching growth reduction, and bioerosion.
# Returns a per-year data.frame (area, calc_total, calc_budg, RAP, pct_cvr)
# plus the final colony count and final area.
#
# UNITS NOTE: contrib is a whole-patch flux (kg CaCO3/yr). RAP normalizes it to
# a per-m2 basis by dividing by site_area before the /2.9/(1-por) conversion.
# ----------------------------------------------------------------------------
simulate_growth <- function(group, species, colony_count, colony_diam, duration,
                            site_area, por, be_micro, be_nonmicro,
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
  run_colony_count  <- colony_count # working colony count for this run
  last_bleach_year  <- 1            # placeholder

  for (i in 1:duration) { # R starts counting at 1 so "Year 0" = Year 1; "Year 10" = Year 11

    # Incorporate Mote outplant mortality observations during Year 0 = "Year 1"
    # Assume 30% outplant die-off. Apply before growth calculation.
    # Should colony numbers be rounded at every step or only at the end?
    if (group == "outplant" && i == 1) {
      run_colony_count <- round(run_colony_count * (1 - mortality_by_size(colony_diam)))
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
        run_colony_count <- round(run_colony_count * (1 - sp_dhw_loss * sp_dhw_mortality))
    }

    # Grow the surviving colonies
    # Apply post-bleaching growth reduction to planar growth rate
    new_size <- new_size + sp_growth_rate * (1 - reduction)
    new_area <- (new_size / 2) ^ 2 * pi * run_colony_count

    # If bleaching, apply the remaining bleaching stress that did not cause mortality
    # as a reduction to the new_area created this year:
    if (bleaching) {
      new_area <- new_area * (1 - sp_dhw_loss * (1 - sp_dhw_mortality))
    }

    # Incorporate generalized microbioerosion:
    be_micro_effect    <- new_area * be_micro
    # Region/habitat-specific (non-micro)bioerosion rate:
    be_nonmicro_effect <- new_area * be_nonmicro

    # Calculate the carbonate budget contribution for this year
    contrib <- calc_rates$rate[calc_rates$Taxon == species] * new_area
    contrib <- contrib - be_micro_effect - be_nonmicro_effect
    budget  <- contrib / site_area
    # Populate the output dataframe with total values as of this year:
    out_df[i, "area"]       <- new_area # Calcifier area
    out_df[i, "calc_total"] <- contrib # Site-wide calcite contribution (kg CaCO3 / yr)
    out_df[i, "calc_budg"]  <- budget # Calcifier carbonate budget (kg CaCO3 / m2 / yr)
    out_df[i, "RAP"]        <- budget / 2.9 / (1 - por) # Reef accretion potential
    out_df[i, "pct_cvr"]    <- new_area / site_area * 100 # Calcifier percent cover
  }

  list(df = out_df, final_count = run_colony_count, final_area = new_area)
}

# Resolve habitat-specific bioerosion ----
# (non-microbioerosion; kg CaCO3/m2/yr)
resolve_bioerosion <- function(subregion, habitat) {
  be_sub <- bioerosion[bioerosion$SUB_REGION == subregion, ]
  be_hab <- be_sub[be_sub$HABITAT_TYPE == habitat, ]
  be_pfish  <- if (nrow(be_hab)) be_hab$AVG_PARROTFISH[1] else 0
  be_urchin <- if (nrow(be_hab)) be_hab$AVG_URCHIN[1] else 0
  be_macro  <- if (nrow(be_hab)) be_hab$AVG_MACROBIOEROSION[1] else 0
  sum(be_pfish, be_urchin, be_macro, na.rm = TRUE)
}

# Generalized Caribbean microbioerosion rate: 0.24 kg CaCO3/m2/yr
be_micro_rate <- 0.24

# Baseline-only original growth ----
# Thin wrapper over simulate_growth: loops the selected species (originals
# only) and sums the per-year RAP + % cover + budget. Porosity from the
# baseline assemblage.
run_baseline_growth <- function(habitat, subregion, site_area, sim_duration,
                                bleaching_severity, bleaching_frequency,
                                baseline_cover_df) {
  be_nonmicro <- resolve_bioerosion(subregion, habitat)
  por <- assemblage_porosity(baseline_cover_df, "current_cvr_pct")

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
                           colony_count = orig_colonies, colony_diam = sp_diam,
                           duration = n,
                           site_area = site_area, por = por,
                           be_micro = be_micro_rate, be_nonmicro = be_nonmicro,
                           bleaching_severity = bleaching_severity,
                           bleaching_frequency = bleaching_frequency)
    total_rap  <- total_rap  + sim$df$RAP
    total_cvr  <- total_cvr  + sim$df$pct_cvr
    total_budg <- total_budg + sim$df$calc_budg
  }
  data.frame(Year = 0:sim_duration, RAP_orig = total_rap,
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
run_restoration_model <- function(habitat, subregion, site_area,
                                  sim_duration, rest_horizon,
                                  outplant_diam, outplant_cost,
                                  bleaching_severity, bleaching_frequency,
                                  target_cover_df) {

  # Subregion/habitat-specific non-microbioerosion + generalized microerosion
  be_nonmicro <- resolve_bioerosion(subregion, habitat)
  be_micro    <- be_micro_rate

  # Assign porosity by target assemblage
  por <- assemblage_porosity(target_cover_df, "target_cvr_pct")

  n <- sim_duration + 1

  # ---- Accumulators ----
  # Originals (all baseline species) and new outplant growth (restored only).
  area_orig <- rep(0, n); calc_total_orig <- rep(0, n); calc_budg_orig <- rep(0, n)
  RAP_orig  <- rep(0, n); pct_cvr_orig    <- rep(0, n)
  area_new  <- rep(0, n); calc_total_new  <- rep(0, n); calc_budg_new  <- rep(0, n)
  RAP_new   <- rep(0, n); pct_cvr_new     <- rep(0, n)

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
                                 site_area = site_area, por = por,
                                 be_micro = be_micro, be_nonmicro = be_nonmicro,
                                 bleaching_severity = bleaching_severity,
                                 bleaching_frequency = bleaching_frequency)
    od <- orig_list[[1]]
    area_orig       <- area_orig       + od$area
    calc_total_orig <- calc_total_orig + od$calc_total
    calc_budg_orig  <- calc_budg_orig  + od$calc_budg
    RAP_orig        <- RAP_orig        + od$RAP
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
                                 site_area = site_area, por = por,
                                 be_micro = be_micro, be_nonmicro = be_nonmicro,
                                 bleaching_severity = bleaching_severity,
                                 bleaching_frequency = bleaching_frequency)
    orig_area_at_horizon <- orig_only[[1]][["area"]][min(rest_horizon + 1, n)]
    sp_to_grow_m <- target_sp_m - orig_area_at_horizon

    # ---- PHASE 1: solve outplant count to hit target by the RESTORATION HORIZON ----
    # Have to start with a guess for the number of required outplants
    outplant_guess <- 70

    reiterate <- TRUE
    guard <- 0 # safety cap to prevent runaway loops
    while (reiterate) {
      guard <- guard + 1
      if (guard > 5000) break

      starting_outplant_area <- outplant_guess * (outplant_diam / 2) ^ 2 * pi
      needed_outplant_growth <- sp_to_grow_m - starting_outplant_area

      # Search runs ONLY to the restoration horizon (horizon 0 => year-1 area,
      # i.e. the immediate post-outplant footprint)
      search_list <- simulate_growth(group = "outplant", species = species,
                                     colony_count = outplant_guess, colony_diam = outplant_diam,
                                     duration = rest_horizon + 1,
                                     site_area = site_area, por = por,
                                     be_micro = be_micro, be_nonmicro = be_nonmicro,
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

    # ---- PHASE 2: run the solved outplant count for the FULL simulation duration ----
    new_list <- simulate_growth(group = "outplant", species = species,
                                colony_count = outplant_guess, colony_diam = outplant_diam,
                                duration = n,
                                site_area = site_area, por = por,
                                be_micro = be_micro, be_nonmicro = be_nonmicro,
                                bleaching_severity = bleaching_severity,
                                bleaching_frequency = bleaching_frequency)
    nd <- new_list[[1]]
    area_new       <- area_new       + nd$area
    calc_total_new <- calc_total_new + nd$calc_total
    calc_budg_new  <- calc_budg_new  + nd$calc_budg
    RAP_new        <- RAP_new        + nd$RAP
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
    area_orig = area_orig, calc_total_orig = calc_total_orig,
    calc_budg_orig = calc_budg_orig, RAP_orig = RAP_orig, pct_cvr_orig = pct_cvr_orig,
    area_new = area_new, calc_total_new = calc_total_new,
    calc_budg_new = calc_budg_new, RAP_new = RAP_new, pct_cvr_new = pct_cvr_new
  )
  budget_df$area_total      <- budget_df$area_orig       + budget_df$area_new
  budget_df$calc_total_all  <- budget_df$calc_total_orig + budget_df$calc_total_new
  budget_df$calc_budg_total <- budget_df$calc_budg_orig  + budget_df$calc_budg_new
  budget_df$RAP_total       <- budget_df$RAP_orig        + budget_df$RAP_new
  budget_df$pct_cvr_total   <- budget_df$pct_cvr_orig    + budget_df$pct_cvr_new

  list(
    budget_df = budget_df,
    outplants_by_species = outplants_by_species,
    outplants = sum(outplants_by_species),
    cost      = total_cost
  )
}


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
pal_parrotfish <- make_wr_pal("parrotfish_G")
pal_gross      <- make_wr_pal("grossE_G")

# Reversed palettes for legend displays
num_pal_rev        <- colorNumeric(colors, domain = at, reverse = TRUE)
pal_parrotfish_rev <- make_wr_pal("parrotfish_G", rev = TRUE)
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

# Shared impact-summary HTML builder ----
# Reused by the Monitoring tab and the Scenario Comparison tab. Takes baseline
# and restored scalars and renders the same three-delta summary.
build_impact_summary <- function(label, b_cover, r_cover, b_budget, r_budget,
                                 b_rap, r_rap) {
  d_cover  <- r_cover - b_cover
  d_budget <- r_budget - b_budget
  d_rap    <- r_rap - b_rap
  arrow <- function(x) if (is.na(x)) "\u2013" else if (x > 0) "\u25B2" else if (x < 0) "\u25BC" else "\u2013"
  HTML(paste0(
    "<p><b>", label, "</b></p>",
    "<p>", arrow(d_cover), " Coral cover change: <b>",
    sprintf("%+.1f", d_cover), " %</b></p>",
    "<p>", arrow(d_budget), " Carbonate budget change: <b>",
    sprintf("%+.2f", d_budget), " kg/m\u00b2/yr</b></p>",
    "<p>", arrow(d_rap), " Reef accretion change: <b>",
    sprintf("%+.2f", d_rap), " mm/yr</b></p>",
    "<hr>",
    "<p>", if (!is.na(r_rap) && r_rap >= 4) {
      "Restored accretion keeps pace with current sea-level rise."
    } else {
      "Restored accretion still falls short of current sea-level rise."
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
    menuItem("About this Site", tabName = "about", icon = icon("circle-info"))
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
        position: absolute; top: 160px; right: 10px; z-index: 1000;
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
                   z-index: 1000; display: flex; gap: 8px;",

          tags$img(src = "usgsLogo.png", style = "height: 60px;"),
          tags$img(src = "noaaLogo.png", style = "height: 60px;")
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
            tags$strong("Filter"),
            shinyWidgets::dropdownButton(
              inputId = "filter_year_dd",
              label = "Year",
              circle = FALSE, width = "100%", status = "default",
              checkboxGroupInput("filter_year", NULL,
                choices = year_choices, selected = year_choices
              )
            ),
            tags$div(style = "height:6px;"),
            shinyWidgets::dropdownButton(
              inputId = "filter_habitat_dd",
              label = "Habitat",
              circle = FALSE, width = "100%", status = "default",
              checkboxGroupInput("filter_habitat", NULL,
                choices = habitat_choices, selected = habitat_choices
              )
            ),

            tags$hr(),

            # Symbolize by: exclusive radio buttons
            radioButtons("symbolize_by", "Symbolize by:",
              choices = c(
                "Reef Accretion Potential (RAP)" = "rap",
                "Reef State"                     = "current_state",
                "Parrotfish Bioerosion"          = "parrotfish_G",
                "Gross Bioerosion"               = "grossE_G"
              ),
              selected = "rap"
            ),

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

            fluidRow(
              # Left column: controls
              column(
                width = 6,
                fileInput("baseline_upload", "Load from file (.xlsx)",
                  accept = c(".xlsx")
                ),
                # Typed input allowed (create = TRUE) for scratch-built scenarios
                selectizeInput(
                  "baseline_site",
                  label = "Site",
                  choices = c("\u2014 Select site \u2014" = ""),
                  selected = "",
                  options = list(create = TRUE, placeholder = "Select or type a site...")
                ),
                tags$div(
                  class = "param-inline-row",
                  tags$span(class = "param-label", "Site area:"),
                  numericInput("site_area_m2", label = NULL,
                  value = 100, min = 1, max = 10000, step = 1
                  ),
                  tags$span(class = "param-unit", "m\u00b2")
                ),
                selectInput(
                  "subregion_choice",
                  label = "Subregion",
                  choices = c("\u2014 Select subregion \u2014" = "",
                              unname(subregion_labels)),
                  selected = ""
                ),
                selectInput(
                  "habitat_choice",
                  label = "Habitat",
                  choices = c("\u2014 Select habitat \u2014" = ""),
                  selected = ""
                ),
                selectizeInput(
                  "baseline_species",
                  "Select your species:",
                  choices = sort(unique(taxa)),
                  multiple = TRUE,
                  options = list(maxItems = 20, placeholder = "Select species...")
                ),

              ),

              # Right column: vertical species:value list
              column(
                width = 6,
                tags$strong("Baseline species cover (%)"),
                uiOutput("baseline_cover_inputs"),

                # Scenario save controls
                tags$hr(),
                textInput("scenario_project", "Project name", value = ""),
                textInput("scenario_name", "Scenario name", value = ""),
                actionButton("save_scenario", "Save scenario", icon = icon("floppy-disk"))
              )
            )
          )
        ),

        # ---- Input element 2: Restoration mix (subsumed from Baseline Input) ----
        column(
          width = 5,
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
          width = 3,
          shinydashboard::box(
            title = "Restoration Parameters",
            width = 12, status = "warning", solidHeader = TRUE,

            # Outplant parameters
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
            sliderInput("sim_duration", "Simulation duration (years)",
              value = 10, min = 10, max = 30, step = 5
              ),

            # Bleaching scenario (vertical, red outline)
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
      ),

      # --- Timeline (bottom) ---
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "Projected Reef Accretion Potential (RAP)",
            width = 12, status = "info", solidHeader = TRUE,
            # Reactive surround: total project cost from the model
            div(
              style = "font-size:14px; font-weight:bold; color:#2f4f2f; padding:2px 6px; text-align:right;",
              textOutput("model_final_cost")
            ),
            plotly::plotlyOutput("restoration_timeline", height = "280px")
          )
        )
      )
    ),

    # Scenario Comparison Tab ----
    # Sidebar: project (single select), scenario (multi select from saved .json),
    #          download report (.csv), + collapsible per-scenario Impact Summary
    # Main:    "Year 10 Outcome Summary" -> cost bar, ROI bar, RAP/Elev scatter
    tabItem(
      tabName = "scenarios",
      fluidRow(
        # Sidebar (left)
        column(
          width = 3,
          shinydashboard::box(
            title = "Scenario Selection", width = 12,
            status = "primary", solidHeader = TRUE,
            selectInput("sc_project", "Project name", choices = NULL),
            checkboxGroupInput("sc_scenarios", "Scenarios", choices = NULL),
            actionButton("sc_refresh", "Refresh list", icon = icon("rotate")),
            br(), br(),
            downloadButton("sc_download_csv", "Download report (.csv)")
          ),
          # Collapsible per-scenario Impact Summary (below Scenario Selection)
          shinydashboard::box(
            title = "Impact Summary", width = 12,
            status = "info", solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("sc_impact_summaries")
          )
        ),
        # Main content (right)
        column(
          width = 9,
          tags$h2("Outcome Summary: Year 10",
            style = "text-align:center; color:black; font-weight:bold;"
          ),
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
                title = "Carbonate Budget: RAP & Elevation Gain", width = 12,
                status = "success", solidHeader = TRUE,
                plotOutput("sc_scatter", height = "350px")
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
            fileInput("upload_cover", "Upload coral cover data",
              accept = c(".csv", ".xlsx")
            ),
            fileInput("upload_bioerosion", "Upload bioerosion data",
              accept = c(".csv", ".xlsx")
            ),
            selectizeInput("monitoring_selected_site", "Select site",
              choices = NULL,
              options = list(placeholder = "Select a site...")
            ),
            br(),
            downloadButton("cc_download_report", "Download report")
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
          "The aim of this site is to provide a predictive tool for decision makers to assess regional responses under future climate change
            and to evaluate the potential impact of local initiatives to mitigate effects of ocean acidification and warming.The modelling
            approach that is used to built projections in this interactive tool is described in ",
          tags$a(href = "https://www.nature.com/articles/s41598-022-26930-4", "an article,"), "published in Scientific reports.",
          tags$br(), tags$br(), tags$h4("Background"),
          "For reef framework to persist, constructional processes by corals and other calcifers need
           to outpace loss due to physical, chemical, and biological erosion. This balance is both delicate and
           dynamic and is currently threatened by the effects of ocean warming and acidifcation.

           Although the protection and recovery of ecosystem functions are at the center of most restoration
           and conservation programs, decision makers are limited by the lack of predictive tools to forecast
           habitat persistence under diferent emission scenarios.",
          tags$br(), tags$br(),
          "The Carbonate Budget Restoration Tool will enable decision makers to evaluate impact of local restoraton initiatives on reef habitat persistence in the context of climate change.",
          tags$br(), tags$br(), tags$h4("Code"),
          "Code and input data used to generate this Shiny mapping tool are available on Github.",
          tags$br(), tags$br(), tags$h4("Sources"),
          tags$br(), tags$br(), tags$h4("Authors"),
          "Dr Alice Webb,Atlantic Oceanographic and Meteorological Laboratory, Ocean Chemistry and Ecosystem Division, NOAA, USA;", tags$br(),
          "Geography, College of Life and Environmental Sciences, University of Exeter, UK", tags$br(),
          "Patrick Kiel, Atlantic Oceanographic andMeteorological Laboratory, Ocean Chemistry and Ecosystem Division,NOAA, Miami, Florida, USA;", tags$br(),
          "Cooperative Institute for Marine and Atmospheric Studies, University of Miami, USA", tags$br(),
          "Mike Jankulak, Atlantic Oceanographic andMeteorological Laboratory, Ocean Chemistry and Ecosystem Division,NOAA, Miami, Florida, USA;", tags$br(),
          "Cooperative Institute for Marine and Atmospheric Studies, University of Miami, USA", tags$br(),
          "Dr Ian Enochs, Atlantic Oceanographic andMeteorological Laboratory, Ocean Chemistry and Ecosystem Division,NOAA, USA", tags$br(),
          tags$br(), tags$br(), tags$h4("Contact"),
          "alice.webb@noaa.gov", tags$br(), tags$br(),
          tags$img(src = "noaaLogo.png", width = "150px", height = "150px")
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

  # sim_duration cannot go below the current restoration horizon. When the
  # horizon rises above the current sim_duration, push sim_duration up and lift
  # its lower bound to match the horizon (but never below its own floor of 10).
  observeEvent(input$rest_horizon, {
    h <- .safe_num(input$rest_horizon)
    cur <- .safe_num(input$sim_duration)
    new_min <- max(h, 10)
    updateSliderInput(session, "sim_duration",
      min = new_min,
      value = max(cur, new_min)
    )
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
      setView(lng = -80.6097, lat = 25, zoom = 8) |>

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
        "parrotfish_G" = pal_parrotfish,
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
        popup = ~ paste0(
          "<span style='font-size: 20px; color: black;'>NCREMP Site: ", site_id, "</span>",
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
        "parrotfish_G" = pal_parrotfish_rev,
        "grossE_G" = pal_gross_rev
      )
      ttl <- switch(field,
        "parrotfish_G" = "Parrotfish<br/>bioerosion<br/>(kg CaCO\u00b3/yr)",
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

  # capture the selected reef name (map click no longer wires other tabs)
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click

    reef_name(df |>
                filter(LAT_DEGREES == click$lat & LON_DEGREES == click$lng) |>
                pull(site_id) |>
                unique())

    print(paste("Selected reef:", reef_name()))
  })

  ## ---------------------------------------------------------------------------
  ## Baseline cover ----
  ## dynamic per-species inputs + upload auto-populate
  ## ---------------------------------------------------------------------------

  # Full uploaded "Coral Cover input" sheet (all sites)
  baseline_upload_data <- reactiveVal(NULL)
  # Holds Taxon -> Percent_Cover for the CURRENTLY SELECTED site
  uploaded_covers <- reactiveVal(NULL)
  # Current carbonate budget computed at ingest time (for Year-0 pip / popup)
  ingested_current_budget <- reactiveVal(NULL)
  # Baseline RAP percentile computed at ingest (gray surround on the graph)
  ingested_baseline_pctile <- reactiveVal(NULL)

  # Habitat choices ----
  # respond to changes in the selected subregion
  observeEvent(input$subregion_choice, {
    # Map the full label back to a code for the habitat-set switch
    code <- if (input$subregion_choice %in% names(subregion_codes)) {
      subregion_codes[[input$subregion_choice]]
    } else {
      input$subregion_choice
    }
    hab <- switch(code,
      "UK"     = c("Inshore", "Offshore", "MidChannel"),
      "MK"     = c("Inshore", "Offshore", "MidChannel"),
      "LK"     = c("Inshore", "Offshore", "MidChannel"),
      "DRTO"   = c("Bank", "Forereef", "Lagoon"),
      "BISC"   = c("Inshore", "Offshore", "MidChannel"),
      "SEFCRI" = c("SEFCRI"),
      character(0)
    )
    updateSelectInput(session, "habitat_choice",
      choices = c("\u2014 Select habitat \u2014" = "", hab),
      selected = if (length(hab) == 1) hab else ""
    )
  }, ignoreInit = TRUE)

  # Shared baseline-ingest routine ----
  # Reused by both the fileInput upload and the cached-file auto-load so the
  # two paths behave identically. `path` points to an .xlsx on disk.
  ingest_baseline_file <- function(path) {
    up <- tryCatch(
      readxl::read_excel(path, sheet = "Coral Cover input"),
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
        choices = c("\u2014 Select site \u2014" = "", site_ids),
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
      updateSelectizeInput(session, "baseline_species", selected = sp)

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
      erow <- bioerosion[bioerosion$Location == hab_now,
        c("AVG_PARROTFISH", "AVG_URCHIN", "AVG_MICROBIOEROSION"), drop = FALSE]
      erosion <- if (nrow(erow)) sum(as.numeric(erow[1, ]), na.rm = TRUE) else 0
      # Normalize to per-m2 to keep budget in kg/m2/yr like the site formula
      cur_budget <- (sp_budget / area_val_num) - erosion
      ingested_current_budget(cur_budget)
      # Baseline RAP percentile vs. the NCRMP distribution (gray surround)
      ingested_baseline_pctile(rap_percentile(cur_budget / 2.9 / (1 - 0.6265)))
    }
  }, ignoreInit = TRUE)

  # Dynamic per-species numericInputs: species:%cover
  output$baseline_cover_inputs <- renderUI({
    req(input$baseline_species)
    sp <- input$baseline_species
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
          numericInput(id, label = NULL, value = val, min = 0, max = 20, step = 0.1)
        )
      )
    })
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
  # step = 0.5 (accept half-percent), ticks hidden (breaks at 5% not shown).
  make_mix_sliders <- function(species_vec) {
    lapply(species_vec, function(s) {
      id  <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
      nid <- paste0("outplants_", gsub("[^A-Za-z0-9]", "_", s))
      tagList(
        sliderInput(id, label = s, min = 0, max = 20, value = 0, step = 0.5,
                    post = "%", ticks = FALSE),
        tags$div(class = "rest-outplant-note", textOutput(nid, inline = TRUE))
      )
    })
  }

  # Branching sub-box: two columns
  output$mix_branching <- renderUI({
    sl <- make_mix_sliders(branching_species)
    half <- ceiling(length(sl) / 2)
    fluidRow(
      column(6, tagList(sl[1:half])),
      column(6, tagList(sl[(half + 1):length(sl)]))
    )
  })

  # Weedy / Other sub-box: two columns
  output$mix_weedy <- renderUI({
    sl <- make_mix_sliders(weedy_species)
    half <- ceiling(length(sl) / 2)
    fluidRow(
      column(6, tagList(sl[1:half])),
      column(6, tagList(sl[(half + 1):length(sl)]))
    )
  })

  # Massive sub-box: four columns
  output$mix_massive <- renderUI({
    sl <- make_mix_sliders(mix_massive_species)
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
  # Species name is abbreviated to "G. species" unless it ends with "spp.".
  observe({
    op <- model_outplants()   # named vector: species -> outplant count
    for (s in restoration_species) {
      local({
        sp  <- s
        nid <- paste0("outplants_", gsub("[^A-Za-z0-9]", "_", sp))
        output[[nid]] <- renderText({
          n_out <- if (!is.null(op) && sp %in% names(op)) op[[sp]] else NA
          if (!is.na(n_out) && n_out > 0) paste0(abbrev_species(sp), ": ", n_out, " outplants") else ""
        })
      })
    }
  })

  # Seed restoration-mix sliders from matching baseline_species values.
  # Re-fires whenever the baseline species set OR any per-species baseline
  # numericInput changes, so the mix keeps honoring the baseline cover input.
  observeEvent(
    {
      # Depend on the species set and on every dynamic base_ input value
      sel <- input$baseline_species
      vals <- lapply(restoration_species, function(s) {
        input[[paste0("base_", gsub("[^A-Za-z0-9]", "_", s))]]
      })
      list(sel, vals)
    },
    {
      sel <- input$baseline_species
      # brief defer so the dynamic base_ inputs exist before we read them
      later::later(function() {
        for (s in restoration_species) {
          if (s %in% sel) {
            base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
            rest_id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
            isolate({
              updateSliderInput(session, rest_id, value = .safe_num(input[[base_id]]))
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
    unconsolidated_cover <- .safe_num(input$unconsolidated_substrate)
    sp <- if (is.null(input$baseline_species)) character(0) else input$baseline_species
    sp <- setdiff(sp, "REQUIRED_Unconsolidated_substrate")
    ids <- paste0("base_", gsub("[^A-Za-z0-9]", "_", sp))
    covers <- sapply(ids, function(id) .safe_num(input[[id]]))
    total_cover <- sum(covers, na.rm = TRUE) - unconsolidated_cover

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

    row <- bioerosion[bioerosion$Location == input$habitat_choice,
      c("AVG_PARROTFISH", "AVG_URCHIN", "AVG_MICROBIOEROSION"), drop = FALSE]
    erosion <- if (nrow(row)) sum(as.numeric(row[1, ]), na.rm = TRUE) else 0
    budget <- (patch_budget / area_val_num) - erosion

    rap_values$baseline <- budget / 2.9 / (1 - 0.6265)

    list(cover = total_cover, budget = budget, rap = rap_values$baseline, sim_duration = sim_duration)
  })

  # Baseline growth series for the timeline (originals only). Available as soon
  # as species + covers + subregion/habitat are set, independent of any target.
  baseline_growth <- reactive({
    habitat       <- input$habitat_choice
    subregion_lbl <- input$subregion_choice
    site_area     <- .safe_num(input$site_area_m2)
    sim_duration  <- .safe_num(input$sim_duration)
    if (site_area <= 0) site_area <- 100
    subregion <- if (subregion_lbl %in% names(subregion_codes)) subregion_codes[[subregion_lbl]] else subregion_lbl
    req(nzchar(habitat), nzchar(subregion_lbl))

    sp <- setdiff(if (is.null(input$baseline_species)) character(0) else input$baseline_species,
                  "REQUIRED_Unconsolidated_substrate")
    if (length(sp) == 0) return(NULL)

    bdf <- data.frame(taxon = character(), current_cvr_pct = numeric(), stringsAsFactors = FALSE)
    for (s in sp) {
      base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      bdf[nrow(bdf) + 1, ] <- list(s, .safe_num(input[[base_id]]))
    }
    if (all(bdf$current_cvr_pct <= 0)) return(NULL)

    tryCatch(
      run_baseline_growth(
        habitat = habitat, subregion = subregion, site_area = site_area,
        sim_duration = sim_duration,
        bleaching_severity  = .safe_num(input$dhw),
        bleaching_frequency = .safe_num(input$bleach_events),
        baseline_cover_df = bdf
      ),
      error = function(e) NULL
    )
  })

  # Restored (target) cover & carbonate budget
  restored_metrics <- reactive({
    b <- baseline_metrics()
    slider_ids <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", restoration_species))
    rest_vals <- sapply(slider_ids, function(id) .safe_num(input[[id]]))
    rest_rates <- as.numeric(calc_rates$rate[match(restoration_species, calc_rates$Species)])
    net_rest <- sum(rest_vals * rest_rates / 100, na.rm = TRUE)

    total_cover <- b$cover + sum(rest_vals, na.rm = TRUE)
    budget <- b$budget + net_rest

    rap_values$restored <- budget / 2.9 / (1 - 0.6265)

    list(cover = total_cover, budget = budget, rap = rap_values$restored)
  })

  # ---- Reactive restoration model ----
  # Derives all parameters from Shiny inputs, builds target_cover_df from the
  # per-species baseline (current) + restoration-mix (target) values.
  model_result <- reactive({
    # Derived parameters
    habitat        <- input$habitat_choice
    subregion_lbl  <- input$subregion_choice
    site_area      <- .safe_num(input$site_area_m2)

    # Map the full subregion label back to the code used in the data files
    subregion <- if (subregion_lbl %in% names(subregion_codes)) {
      subregion_codes[[subregion_lbl]]
    } else {
      subregion_lbl
    }

    # User-input outplant parameters
    outplant_diam <- .safe_num(input$outplant_size) / 100 # cm -> m
    outplant_cost <- .safe_num(input$outplant_cost)
    sim_duration  <- .safe_num(input$sim_duration)
    rest_horizon  <- .safe_num(input$rest_horizon)

    # Bleaching parameters
    bleaching_severity  <- .safe_num(input$dhw)           # degree-heating weeks
    bleaching_frequency <- .safe_num(input$bleach_events) # events in a 5-year period

    req(nzchar(habitat), nzchar(subregion_lbl), site_area > 0)

    # Build target_cover_df from the FULL baseline species set (so unrestored
    # species still grow), unioned with the restoration-mix species. current =
    # baseline % cover input; target = restoration-mix slider (0 if none).
    base_sp <- setdiff(if (is.null(input$baseline_species)) character(0) else input$baseline_species,
                       "REQUIRED_Unconsolidated_substrate")
    all_sp <- union(base_sp, restoration_species)

    target_cover_df <- data.frame(
      taxon = character(),
      current_cvr_pct = numeric(),
      target_cvr_pct = numeric(),
      stringsAsFactors = FALSE
    )
    for (s in all_sp) {
      base_id <- paste0("base_", gsub("[^A-Za-z0-9]", "_", s))
      rest_id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
      current_sp_pct <- .safe_num(input[[base_id]])
      target_sp_pct  <- .safe_num(input[[rest_id]])
      target_cover_df[nrow(target_cover_df) + 1, ] <- list(s, current_sp_pct, target_sp_pct)
    }

    # Nothing present to grow at all -> no model output
    if (all(target_cover_df$current_cvr_pct <= 0 & target_cover_df$target_cvr_pct <= 0)) {
      return(NULL)
    }

    tryCatch(
      run_restoration_model(
        habitat = habitat, subregion = subregion, site_area = site_area,
        sim_duration = sim_duration, rest_horizon = rest_horizon,
        outplant_diam = outplant_diam, outplant_cost = outplant_cost,
        bleaching_severity = bleaching_severity,
        bleaching_frequency = bleaching_frequency,
        target_cover_df = target_cover_df
      ),
      error = function(e) {
        showNotification(paste("Model error:", e$message), type = "error")
        NULL
      }
    )
  })

  # ---- Per-species outplant counts ----
  # (drives the Restoration Mix captions)
  model_outplants <- reactive({
    mr <- model_result()
    if (is.null(mr)) return(NULL)
    mr$outplants_by_species
  })

  # ---- Reactive graph surround ----
  # total project cost from the model
  output$model_final_cost <- renderText({
    mr <- model_result()
    if (is.null(mr)) return("Final cost: \u2014")
    paste0("Final cost: $", format(round(mr$cost), big.mark = ","))
  })

  output$restoration_timeline <- plotly::renderPlotly({
    b <- baseline_metrics()
    mr <- model_result()
    dur <- b$sim_duration
    horizon <- .safe_num(input$rest_horizon)
    dark <- isTRUE(input$dark_mode)
    # Dark-mode plot palette
    paper_bg <- if (dark) "#232a33" else "white"
    plot_bg  <- if (dark) "#232a33" else "white"
    font_col <- if (dark) "#e6e6e6" else "#333333"
    orig_col <- if (dark) "#cfcfcf" else "gray30"  # lighten Baseline/Original line
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

    # Restored/baseline RAP percentile surrounds (upper-left)
    horizon_row <- min(horizon + 1, dur + 1)
    restored_pct <- if (!is.null(mr) && nrow(mr$budget_df) > 0) {
      rap_percentile(mr$budget_df$RAP_total[horizon_row])
    } else NA_real_
    baseline_pct <- ingested_baseline_pctile()

    if (is.null(mr) || nrow(mr$budget_df) == 0) {
      # No restoration target yet: show baseline growth (gray) if available.
      bg <- baseline_growth()
      if (is.null(bg)) {
        d0 <- data.frame(Year = 0:dur, RAP = NA_real_)
        p <- ggplot(d0, aes(Year, RAP)) +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(limits = c(-1, 8)) +
          labs(x = "Year", y = "RAP (mm/yr)") +
          theme_minimal(base_size = 14)
        y0_rap <- b$rap
      } else {
        p <- ggplot(bg, aes(x = Year)) +
          annotate("rect", xmin = 0, xmax = dur, ymin = -Inf, ymax = -0.5,
                   fill = "red", alpha = 0.10) +
          annotate("rect", xmin = 0, xmax = dur, ymin = -0.5, ymax = 0.5,
                   fill = "yellow", alpha = 0.10) +
          # Baseline (original) growth line; hover carries cover + budget
          geom_line(aes(y = RAP_orig, group = 1,
                        text = paste0("Baseline growth",
                                      "<br>Year ", Year,
                                      "<br>RAP: ", round(RAP_orig, 2), " mm/yr",
                                      "<br>Cover: ", round(pct_cvr_orig, 1), " %",
                                      "<br>Budget: ", round(calc_budg_orig, 2), " kg/m\u00b2/yr")),
                    color = orig_col, linewidth = 1.1) +
          scale_x_continuous(breaks = x_breaks) +
          scale_y_continuous(limits = c(min(-1, min(bg$RAP_orig, na.rm = TRUE)), max(max(slr_tl$SLR), max(bg$RAP_orig, na.rm = TRUE)))) +
          labs(x = "Year", y = "RAP (mm/yr)") +
          theme_minimal(base_size = 14)
        y0_rap <- bg$RAP_orig[1]
      }
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

      pips <- d[d$Year %in% c(1, 5, 10, 20, 50, 100, dur), ]

      p <- ggplot(d, aes(x = Year)) +
        annotate("rect", xmin = 0, xmax = dur, ymin = -Inf, ymax = -0.5,
                 fill = "red", alpha = 0.10) +
        annotate("rect", xmin = 0, xmax = dur, ymin = -0.5, ymax = 0.5,
                 fill = "yellow", alpha = 0.10) +
        geom_ribbon(aes(ymin = RAP_orig, ymax = RAP_total),
                    fill = "green", alpha = 0.20) +
        # Original RAP contribution (all baseline species' originals)
        geom_line(aes(y = RAP_orig, group = 1,
                      text = paste0("Original RAP",
                                    "<br>Year ", Year,
                                    "<br>RAP: ", round(RAP_orig, 2), " mm/yr",
                                    "<br>Cover: ", round(pct_cvr_orig, 1), " %",
                                    "<br>Budget: ", round(calc_budg_orig, 2), " kg/m\u00b2/yr")),
                  color = orig_col, linewidth = 1.1) +
        # Total RAP: dark green
        geom_line(aes(y = RAP_total, group = 2,
                      text = paste0("Total RAP",
                                    "<br>Year ", Year,
                                    "<br>RAP: ", round(RAP_total, 2), " mm/yr",
                                    "<br>Cover: ", round(pct_cvr_total, 1), " %",
                                    "<br>Budget: ", round(calc_budg_total, 2), " kg/m\u00b2/yr")),
                  color = "darkgreen", linewidth = 1.2) +
        geom_point(
          data = pips,
          aes(y = RAP_total, text = paste0(
            "Year ", Year,
            "<br>Projected cover: ", round(pct_cvr_total, 1), "%",
            "<br>Projected RAP: ", round(RAP_total, 2), " mm/yr",
            "<br>Projected budget: ", round(calc_budg_total, 2), " kg CaCO3/m\u00b2/yr"
          )),
          size = 4, color = "darkgreen"
        ) +
        scale_x_continuous(breaks = x_breaks) +
        scale_y_continuous(limits = c(min(-1, min(d$RAP_orig, na.rm = TRUE)), max(max(slr_tl$SLR), max(d$RAP_total, na.rm = TRUE)))) +
        labs(x = "Year", y = "RAP (mm/yr)") +
        theme_minimal(base_size = 14)
      y0_rap <- d$RAP_total[1]
    }

    # Gold geological-accretion baseline at 3.1 mm/yr (hover annotation)
    p <- p + geom_hline(
      aes(yintercept = 3.1,
          text = "Geological accretion baseline"),
      color = "gold", linetype = "dashed", linewidth = 0.8
    )

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

    # Year-0 baseline annotation with CURRENT RAP + cover + budget (always on)
    cur_budget <- ingested_current_budget()
    show_budget <- if (!is.null(cur_budget)) cur_budget else b$budget
    y0_label <- paste0(
      "Baseline<br>Cover: ", round(b$cover, 1), "%",
      "<br>RAP: ", round(b$rap, 2), " mm/yr",
      "<br>Budget: ", round(show_budget, 2), " kg/m\u00b2/yr"
    )

    # Percentile surrounds (upper-left): baseline (gray) above restored (colored)
    ann_list <- list(
      list(
        x = 0, y = y0_rap, text = y0_label,
        showarrow = TRUE, arrowhead = 2, ax = 40, ay = -40,
        align = "left", bgcolor = "white", bordercolor = "steelblue",
        borderwidth = 2, font = list(size = 11)
      )
    )
    if (!is.na(baseline_pct)) {
      ann_list[[length(ann_list) + 1]] <- list(
        xref = "paper", yref = "paper", x = 0.01, y = 0.99,
        xanchor = "left", yanchor = "top", showarrow = FALSE,
        text = paste0("Baseline RAP percentile: ", round(baseline_pct), "%"),
        font = list(size = 12, color = "#777777")
      )
    }
    if (!is.na(restored_pct)) {
      ann_list[[length(ann_list) + 1]] <- list(
        xref = "paper", yref = "paper", x = 0.01, y = 0.90,
        xanchor = "left", yanchor = "top", showarrow = FALSE,
        text = paste0("RAP percentile at restoration horizon: ", round(restored_pct), "%"),
        font = list(size = 12, color = percentile_color(restored_pct))
      )
    }

    gp <- gp |>
      plotly::layout(
        annotations = ann_list,
        paper_bgcolor = paper_bg,
        plot_bgcolor  = plot_bg,
        font = list(color = font_col),
        xaxis = list(color = font_col),
        yaxis = list(color = font_col)
      )

    gp
  })

  ## ---------------------------------------------------------------------------
  ## Save scenario (Restoration Planning) ----
  ## ---------------------------------------------------------------------------
  observeEvent(input$save_scenario, {
    shiny::validate(shiny::need(nzchar(input$scenario_project), "Enter a project name."))
    shiny::validate(shiny::need(nzchar(input$scenario_name), "Enter a scenario name."))

    r <- restored_metrics()
    b <- baseline_metrics()
    mr <- model_result()

    total_cover  <- r$cover
    restored_rap <- r$rap
    baseline_rap <- b$rap

    # Prefer model-derived cost when available; else illustrative fallback
    added_cover <- r$cover - b$cover
    cost <- if (!is.null(mr)) mr$cost else added_cover * .safe_num(input$outplant_cost) * 100
    outplants <- if (!is.null(mr) && length(mr$outplants)) mr$outplants else NA
    elev_gain_10yr <- restored_rap * 10 # mm over 10 years
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
      total_cover = scalar1(total_cover),
      baseline_cover = scalar1(b$cover),
      restored_cover = scalar1(r$cover),
      baseline_budget = scalar1(b$budget),
      restored_budget = scalar1(r$budget),
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

  # Uploaded coral-cover data (overrides df when present). Accepts .csv or .xlsx.
  uploaded_monitoring_cover <- reactive({
    f <- input$upload_cover
    if (is.null(f)) {
      return(NULL)
    }
    ext <- tolower(tools::file_ext(f$name))
    tryCatch(
      if (ext == "xlsx") {
        readxl::read_excel(f$datapath)
      } else {
        read.csv(f$datapath, stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(paste("Could not read cover file:", e$message), type = "error")
        NULL
      }
    )
  })

  # Wire the monitoring "Select site" dropdown to the uploaded cover file
  # (mirrors the planning-tab baseline behavior). Falls back to df$site_id.
  observe({
    up <- uploaded_monitoring_cover()
    choices <- if (!is.null(up) && "site_id" %in% names(up)) {
      unique(as.character(up$site_id[!is.na(up$site_id)]))
    } else {
      unique(df$site_id)
    }
    updateSelectizeInput(session, "monitoring_selected_site",
      choices = choices, server = TRUE
    )
  })

  # Uploaded bioerosion data (overrides bioerosion when present)
  uploaded_monitoring_bioerosion <- reactive({
    f <- input$upload_bioerosion
    if (is.null(f)) {
      return(NULL)
    }
    ext <- tolower(tools::file_ext(f$name))
    tryCatch(
      if (ext == "xlsx") {
        readxl::read_excel(f$datapath)
      } else {
        read.csv(f$datapath, stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(paste("Could not read bioerosion file:", e$message), type = "error")
        NULL
      }
    )
  })

  # Helper: baseline metrics for the selected NCRMP site (upload overrides df)
  cc_baseline_vals <- reactive({
    req(input$monitoring_selected_site)

    up <- uploaded_monitoring_cover()
    if (!is.null(up) && "site_id" %in% names(up) &&
        input$monitoring_selected_site %in% up$site_id) {
      dat <- up[up$site_id == input$monitoring_selected_site, , drop = FALSE][1, ]
    } else {
      dat <- df |> filter(site_id == input$monitoring_selected_site) |> slice(1)
    }

    list(
      cover  = dat$hardCoral_PrctCvr,
      budget = dat$net_G,
      rap    = if (!is.null(dat$rap) && !is.na(dat$rap)) dat$rap else dat$net_G / 2.9 / (1 - 0.6265)
    )
  })

  # Helper: restored metrics. Uses the interactive restored RAP from the
  # Restoration Planning tab if available, else falls back to baseline.
  cc_restored_vals <- reactive({
    base <- cc_baseline_vals()
    restored_rap <- if (!is.null(rap_values$restored)) rap_values$restored else base$rap
    restored_budget <- if (!is.null(rap_values$restored)) {
      restored_rap * 2.9 * (1 - 0.6265)
    } else {
      base$budget
    }
    # Restored cover from the planning-tab restoration mix, else baseline
    r <- restored_metrics()
    restored_cover <- if (!is.null(r$cover) && r$cover > 0) r$cover else base$cover
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

  # Timeline: RAP over the simulation duration with SLR reference lines (plotly)
  output$cc_timeline <- plotly::renderPlotly({
    b <- cc_baseline_vals()
    r <- cc_restored_vals()
    b <- baseline_metrics()
    dur <- b$sim_duration
    years <- 0:dur

    tl <- data.frame(
      Year = rep(years, 2),
      RAP  = c(rep(b$rap, length(years)), rep(r$rap, length(years))),
      Scenario = rep(c("Baseline", "Restored"), each = length(years))
    )

    p <- ggplot(tl, aes(Year, RAP, color = Scenario,
                        group = Scenario,
                        text = paste0(
                          Scenario, "<br>Year: ", Year,
                          "<br>RAP: ", round(RAP, 2), " mm/yr"
                        ))) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = 4, linetype = "dashed", color = "deepskyblue3") +
      geom_hline(yintercept = 40, linetype = "dashed", color = "#0b3d91") +
      scale_color_manual(values = c("Baseline" = "lightgreen", "Restored" = "forestgreen")) +
      labs(x = "Year", y = "RAP (mm/yr)", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        legend = list(orientation = "h", x = 0, y = 1.1),
        annotations = list(
          list(x = 0.5, y = 4, text = "Current SLR (4 mm/yr)",
               showarrow = FALSE, xanchor = "left", yshift = 10,
               font = list(color = "deepskyblue3")),
          list(x = 0.5, y = 40, text = "Future SLR (40 mm/yr)",
               showarrow = FALSE, xanchor = "left", yshift = 10,
               font = list(color = "#0b3d91"))
        )
      )
  })

  # Download report for the Restoration Monitoring tab
  output$cc_download_report <- downloadHandler(
    filename = function() {
      paste0("carbonate_report_", input$monitoring_selected_site, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
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

  # Populate the scenario multi-select based on chosen project
  observe({
    sc <- all_scenarios()
    req(input$sc_project)
    scen <- if (nrow(sc)) sort(unique(sc$scenario[sc$project == input$sc_project])) else character(0)
    updateCheckboxGroupInput(session, "sc_scenarios", choices = scen)
  })

  # Filtered scenarios for plotting (coerce numeric cols used by the plots)
  sc_selected <- reactive({
    sc <- all_scenarios()
    req(nrow(sc) > 0, input$sc_project, input$sc_scenarios)
    d <- sc[sc$project == input$sc_project & sc$scenario %in% input$sc_scenarios, ]
    num_cols <- c("cost", "roi", "restored_rap", "elev_gain_10yr",
                  "baseline_cover", "restored_cover", "baseline_budget",
                  "restored_budget", "baseline_rap")
    for (col in num_cols) {
      if (col %in% names(d)) d[[col]] <- as.numeric(d[[col]])
    }
    d
  })

  # Collapsible per-scenario Impact Summary (reuses the shared builder)
  output$sc_impact_summaries <- renderUI({
    d <- sc_selected()
    if (nrow(d) == 0) {
      return(tags$em("Select one or more scenarios."))
    }
    panels <- lapply(seq_len(nrow(d)), function(i) {
      row <- d[i, ]
      summary_html <- build_impact_summary(
        label = paste0("Scenario: ", row$scenario),
        b_cover = row$baseline_cover, r_cover = row$restored_cover,
        b_budget = row$baseline_budget, r_budget = row$restored_budget,
        b_rap = row$baseline_rap, r_rap = row$restored_rap
      )
      tags$div(
        class = "impact-summary-box",
        style = "background:#f7f7f7; border:1px solid #ddd; border-radius:6px;
                 padding:10px; margin-bottom:8px;",
        summary_html
      )
    })
    tagList(panels)
  })

  # Project cost bar
  output$sc_cost_bar <- renderPlot({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    ggplot(d, aes(x = scenario, y = cost, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "Project cost ($)") +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # ROI bar
  output$sc_roi_bar <- renderPlot({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    ggplot(d, aes(x = scenario, y = roi, fill = scenario)) +
      geom_col() +
      labs(x = NULL, y = "ROI (mm elevation per $1k)") +
      scale_fill_brewer(palette = "Greens") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })

  # RAP & elevation-gain scatter
  output$sc_scatter <- renderPlot({
    d <- sc_selected()
    shiny::validate(shiny::need(nrow(d) > 0, "Select one or more scenarios."))
    ggplot(d, aes(x = restored_rap, y = elev_gain_10yr, color = scenario)) +
      geom_point(size = 4) +
      geom_text(aes(label = scenario), show.legend = FALSE, vjust = -1) +
      geom_vline(xintercept = 4, linetype = "dashed", color = "deepskyblue3") +
      labs(
        x = "Restored reef accretion (mm/yr)",
        y = "Elevation gain over 10 yr (mm)",
        color = NULL
      ) +
      theme_minimal(base_size = 14)
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