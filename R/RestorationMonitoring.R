########################
# Restoration Monitoring
########################
library(tidyr)
library(dplyr)

monitoring_data <- load.csv("Restoration_Monitoring_Cover_TEMPLATE.csv", header = TRUE)  # invalid: load.csv is not a function

# Calculate taxon-level carbonate budgets
monitoring_data$gp <- (year0_taxa$percent_cover / 100) * travis_rates

# for each unique value in Years_Post_Restoration (including fractional years) and each unique site if there are multiple
monitoring_data_site <- monitoring_data %>%
  group_by(unique_site_id, years_post_restoration) %>%
  # Unconsolidated substrate needs to be removed for coral cover calculation
  # Should CCA also be removed for this???
  filter(percent_cover != "REQUIRED_Unconsolidated_substrate") %>%
  summarize(gp_site = sum(gp), coral_cover = sum(percent_cover))

# Calculate substrate for microbioerosion
substrate <- monitoring_data %>%
  group_by(unique_site_id, years_post_restoration) %>%
  summarize(substrate = 100 - sum(percent_cover))

# Calculate microbioerosion
substrate$microbioerosion <- (substrate$substrate / 100) * 0.24

# If no data provided or if incomplete data provided (e.g. parrotfish but no sponges)
# Add bioerosion for each site
monitoring_data_site <- monitoring_data_site %>%
  left_join(bioerosion, by = c("Habitat", "Subegion")) %>%  # invalid: "Subegion" misspelled
  left_join(substrate, by = c("Habitat", "Subegion"))  # invalid: "Subegion" misspelled

# If bioerosion input data provided apply the following calculations, as relevant
# If data available for every timepoint in the Monitoring_data apply timepoint specific rates
# Else apply single value or average value from whatever is provided

# Parrotfish Bioerosion
# ParrotfishBioerosionRates.csv has species, size class, and life-phase-specific bioerosion rates
# Parrotfish from user input
# remove juveniles, which are assumed to contribute minimally to bioerosion
parrotfish <- parrotfish[parrotfish$size != "0-9", ]
# Create a variable that merges text for life phase and size to align with rates table
parrotfish$phase_size <- paste(parrotfish$life_phase, parrotfish$fork_length_cm, sep = "")

parrotfish_rates <- read.csv("ParrotfishBioerosionRates.csv", header = TRUE)
parrotfish_rates <- na.omit(parrotfish_rates)

# merge rates and parrtofish data
parrotfish_rates <- merge(parrotfish, parrotfish_rates, by = c("Taxon", "PhaseSize"), all.x = TRUE)

# Calculate bioerosion from each individual parrotfish observed
parrotfish_rates$bioerosion <- (parrotfish_rates$count * parrotfish_rates$rate) /
  parrotfish_rates$survey_area_m2

# Sum for site and/or timepoint
parrotfish_rates_site <- parrotfish_rates %>%
  group_by(unique_site_id, years_post_restoration) %>%
  summarize(sum_bioerosion = sum(bioerosion, na.rm = TRUE), sum_counts = sum(count))

# Urchin Bioerosion
# UrchinBioerosionRates.csv has species an test-size specific bioerosion rates
# test sizes are median of range in input
# If test size not incuded use 30 cm as most urching on FCR are relatively small
urchin_rates <- read.csv("UrchinBioerosionRates.csv", header = TRUE)
# Urchins from user input
urchin_bioerosion <- merge(urchins, urchin_rates, by = c("Taxon", "Test.size"), all.x = TRUE)

# Calcualte individual bioerosion of all urchins
# Survey_Area_m2 from input data
# urchin test sizes are in mm
urchin_bioerosion$bioerosion <- ((urchins$count / urchins$survey_area_m2) *
                                   urchin_bioerosion$bioerosion.rate * 365) / 1000

# Sum for site and/or timepoint
urchin_rates_site <- urchin_bioerosion %>%
  group_by(unique_site_id, years_post_restoration) %>%
  summarize(sum_bioerosion = sum(bioerosion, na.rm = TRUE))

# Sponge Bioerosion
# Sponge from user input
# SpongeBioerosionRates.csv has species-specific sponge bioerosion rates
sponge_rates <- read.csv("SpongeBioerosionRates.csv", header = TRUE)
sponge_rates <- merge(sponges, sponge_rates, by = "Taxon", all.x = TRUE)

# convert sponge areas in cm2 to m2
sponge_rates$area <- sponge_rates$area * 0.0001

# Calcuate bioerosion by each observed sponge
# Survey_Area_m2 from input data
sponge_rates$bioerosion <- (sponge_rates$area * sponge_rates$bioerosion.rate) /
  sponge_rates$survey_area_m2

# Sum for site and/or timepoint
sponge_rates_site <- sponge_rates %>%
  group_by(unique_site_id, years_post_restoration) %>%
  summarize(sum_bioerosion = sum(bioerosion, na.rm = TRUE))

# Calculate net budget
monitoring_data_site$np <- monitoring_data_site$gp - sum(
  monitoring_data_site$microbioerosion,
  monitoring_data_site$parrotfish,
  monitoring_data_site$urchins,
  monitoring_data_site$bio_sponges
)

# Calculate reef-accretion potential
# use porosity dataset lookup:
# quantify mix on min(Years_Post_Restoration)
# if restoration mix >75% Acropora spp., use Acropora porosity; if restoration mix >75% massives/other use Massive porosity
# else use mixed
# 2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018
monitoring_data_site$rap <- monitoring_data_site$np / 2.9 / (1 - porosity)

# Plot for each Years_Post_Restoration time point for the user-selected site (based on dropdown)
# hline for "Geological baseline" = 3 mm/yr #https://doi.org/10.1111/gcb.14389
# hline for "Present-day sea-level rise" = 2.64 mm/yr #https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8724580
# future sea-level rise displayed based on user selections of 2050 vs 2010 and scenario
# use this look up: AR6_SLR_KW.csv SLR_Rate
# text display for scenario = paste(Year, Scenario)
# data from: https://sealevel.nasa.gov/ipcc-ar6-sea-level-projection-tool

### Ranking of Reef-Accretion Potential ###
# Needs to be reported in the context of what this actually means for whether the reef is growing and how fast
percentile <- length(baseline_budgets$rap[baseline_budgets$rap < monitoring_data_site$rap]) /
  length(baseline_budgets$rap) * 100
