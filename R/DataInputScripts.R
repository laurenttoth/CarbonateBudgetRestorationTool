#### Data input calculations ####

# Data input template
# Baseline_cover_TEMPLATE.xlxs

library(readxl)

baseline <- read_excel("Baseline_cover_TEMPLATE.xlxs", sheet = "Coral cover input")

# Need to run data checks:
# 1. All required fields included? print "Required data missing: VARIABLE NAME"
# 2. If sum Percent_Cover for a site <1% print "Total % Cover <1%: Ensure input data are percentage not proportional cover"
# 3. Correct subregion habitat combinations
# 4. Check Lat/Long within Florida bounding box
# Other?

# Carbonate budget calculation modified from Alice's code
# Math for this tab starts on L29

baseline <- coral_data %>%  # baseline cover input from dropdown or data uploaded using template
  left_join(travis_rates, by = "Taxon") %>%  # add species-specific calcification rates
  left_join(bioerosion, by = c("Habitat", "Subregion")  # invalid: missing closing paren
  ) %>%  # in Bioerosion.csv #add bioerosion; not actually needed until next step
  # Also need to add mid-shore and DRTO habitats to Bioerosion spreadsheet
  mutate(contribution = percent_cover * rate / 100)  # result gives gross carbonate production by taxon

gross_budget <- sum(baseline$contribution, na.rm = TRUE)  # this is actually gross budget
# This calculation has been modified see RestorationScenarioModeling.R
erosion_total <- baseline %>%
  # microbioerosion removed from Alice's script because it should be calculated separately
  dplyr::distinct(c(habitat, subregion), ave_parrotfish + ave_urchin + ave_macrobioerosion) %>%
  dplyr::summarise(total = ave_parrotfish + ave_urchin + ave_macrobioerosion) %>%
  dplyr::pull(total)

# Calculate microbioerosion based on available substrate
# Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
# both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
baseline_substrate <- 100 - sum(baseline$percent_cover)

# Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1
# (Perry and Lange, 2019)
# substrate divided by 100 to convert % cover to proportional cover
micro_baseline <- (baseline_substrate / 100) * 0.24

# Add microbioerosion to total bioerosion
# Total=AVE_PARROTFISH + AVE_URCHIN + AVE_MACROBIOEROSION for the same subregion and habitat as the input data
erosion_total <- erosion_total + micro_baseline

net_budget <- gross_budget - erosion_total

# 2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018
baseline_rap <- net_budget / 2.9 / (1 - 0.6265)

# see PPT mock-up for result display

# if baseline_rap < -0.5 reef status text = "Your reef is ERODING"
# if baseline_rap > 0.5 reef status text = "Your Reef is GROWING"
# if baseline_rap between -0.5 and 0.5 reef status text = "Your reef is in STASIS"
