# Restoration Scenario Modeling ----

## Notes About Modifications to Alice's Version of the App ----
# Data input moves to new page

# add two additional restoration species Pseudodiploria strigosa and Pseudodiploria clivosa
restoration_species <- c(
  "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
  "Orbicella faveolata", "Colpophyllia natans", "Pseudodiploria strigosa",
  "Pseudodiploria clivosa", "Porites astreoides", "Siderastrea siderea",
  "Stephanocoenia intersepta", "Diploria labyrinthiformis", "Solenastrea bournoni"
)

# cap slider at 50% (could be lower)
output$restoration_sliders <- renderUI({
  # build your list of sliderInput()s
  sliders <- lapply(restoration_species, function(s) {
    id <- paste0("rest_slider_", gsub("[^A-Za-z0-9]", "_", s))
    sliderInput(id, label = s, min = 0, max = 50, value = 0, step = 0.5, post = "%")
  })

  half <- ceiling(length(sliders) / 2)

  fluidRow(
    column(6, tagList(sliders[1:half])),
    column(6, tagList(sliders[(half + 1):length(sliders)]))
  )
})

# Math Not Scripts ----

## Year 0 ----
# Baseline from DataInputScripts.R
# Restored_Cover from slider inputs

# Potential way to code for math below:
year0_taxa <-
  merge(baseline, input_cover, travis_rates) |>
  arrange(taxon) |>
  mutate(total_cover = percent_cover + restored_cover) |>  # unconsolidated substrate included here
  filter(taxon != required_unconsolidated_substrate) |>  # removed for below
  mutate(coral_cover = percent_cover + restored_cover)
mutate(gp = (coral_cover / 100) * rate))  # invalid: stray closing paren, missing pipe

# Calcuate total calcifier cover after restoration
# For all calcifier cover based calculations need to remove unconsolidated substrate see Coral_Cover above
year0_taxa$total_cover <- baseline$percent_cover + year0_taxa$restored_cover

# Calcuate gross production by taxon in kg/m2/y
# Cover always needs to be proportional for GP calculations
year0_taxa$gp <- (year0_taxa$total_cover / 100) * travis_rates  # taxon-specific calcification rates

# sum calcification rates of all calcifying taxa to get site-level gross production
year0_site$gp <- sum(year0_taxa$gp)

# Calculate microbioerosion based on available substrate in kg/m2/y
# Note that Total_Cover here should include total calcifier cover + unconsolidated substrate
# both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
year0_substrate <- 100 - sum(year0_taxa$total_cover)

# Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1
# (Perry and Lange, 2019)
micro_year0 <- (year0_substrate / 100) * 0.24

# Other Bioerosion data from Bioerosion.csv dataset
# Bioerosion is specific to Habitat and Subregion (which are included in Baseline_cover_TEMPLATE.csv)
# NOTE that these values will not change from year to year in the model but Microbioerosion will be
# recalculated at each time step
# This variabile should actually be calculated in the DataInput module
site_bioerosion <- sum(bioerosion$ave_parrotfish, bioerosion$ave_urchins, bioerosion$ave_macrobioerosion)

# Calculate site-level net production in kg/m2/y
year0_site$np <- year0_site$gp - (site_bioerosion + micro_year0)

# Calculate site-level reef-accretion potential in mm/y
# use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity;
# if restoration mix >75% massives/other use Massive porosity
# or could weight based on relative contribution
# else use mixed
# 2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018
year0_site$rap <- year0_site$np / 2.9 / (1 - porosity)

## Year 1 ----
# Calculate change in percent cover after one year based on species-specific growth and mortality rates

# Partial mortality rates for branching and "other" corals from Browne et al. 2026 applied to Baseline_Cover
# https://doi.org/10.1016/j.ecolmodel.2026.111672
# Assuming that whole colony mortailty rates for established colonies in the absence of bleaching is negligible
# Browne et al. estimates whole-colony mortality at 0.002-0.00076 for branching and 0.002-0.0002 for
# foliose and other morphologies
# foliose same as other in Browne et al. so lumped
# these are rates for the 50 cm (middle) size class
# these are percentage of surface area (4.14 and 2.96%) should be mathemetically equivalent to cover
# LT created Morphology_for_mortality_LOOKUP (currently .xlsx): note that foliose retained as category
# but foliose and other should be treated as other
branching_mortality <- 0.0414
other_mortality <- 0.0296

# Calculate mortality of baseline colonies by taxa
year1_taxa$baseline_cover_start <- baseline$percent_cover * (1 - x_mortality)

# Calculate growth of colonies
# Site_Area from user input
# calculate total area occupied by each coral taxa within the plot in m2
year1_taxa$baseline_coral_area <- site_area * (baseline$percent_cover / 100)

# Join NCRMP_colony_dia_Florida.csv
# these colony diameters are in cm
florida_colony_dia <- read.csv("NCRMP_colony_dia_Florida.csv", header = TRUE)
# Use regional median taxon specific colony diameters (length_mean) to estimate number of colonies
# for each species
# round to a whole number so individuals can be modeled
# diameters are in cm so need to be divided by 10000
# use equation for area of a circle to convert diameters in m2 to area
year1_taxa$baseline_ncolonies <- round(
  year1_taxa$baseline_coral_area / ((((florida_colony_dia / 10000) / 2)^2) * pi)
)

# Calculate individual colony diameters for each taxon based on the rounded estimate of colony number
# this step is necessary because the outcome of the previous calculation is rounded to a whole number
# Convert area to diameter in m
year1_taxa$baseline_coral_dia <- sqrt(
  (year1_taxa$baseline_coral_area / year1_taxa$baseline_ncolonies) / pi
) * 2

# growth_rates_ReefBudget_NCRMP.csv has species-specific coral growth rates (vertical extension and
# planar) in cm
# apply planar growth to each coral
# I calculated the ratio of coral diameters to heights across all the Atlantic NCRMP Demographic data
# I then multiplied that ratio by the ReefBudget mean_ext rate to estimate colony diameter (planar)
# growth rate in cm
coral_growth <- read.csv("growth_rates_ReefBudget_NCRMP.csv", header = TRUE)
# dividing by 100 converts cm rates to m which is units for coral area
year1_taxa$year1_coral_dia <- baseline_coral_dia + (coral_growth$planar_mean / 100)  # invalid: baseline_coral_dia undefined (missing year1_taxa$)

# calculate new species-specific area assuming each colony is a circle
# area in m2
year1_taxa$year1_coral_area <- (((year1_coral_dia / 2)^2) * pi) * year1_taxa$baseline_ncolonies  # invalid: year1_coral_dia undefined (missing year1_taxa$)

# calculate new species-specific coral cover
year1_taxa$baseline_cover_end <- (year1_coral_area / site_area) * 100  # invalid: year1_coral_area undefined (missing year1_taxa$)

# New outplants have high mortality rates so this extra step is applied for the first year post outplanting
# mortality will also be scaled by outplant_size input, but haven't totally figured out how yet
# likely will have Mote_mortality (taxon-specific mortality rates) scaled by user input of outplant size
# so match Mote_mortality based on taxon and initial outplant size for calculation

# mortality of restored colonies
# will be look-up with species-specific rates
# use Mote_mortality=0.3 for now
year1_taxa$restored_cover_start <- year0_taxa$restored_cover * (1 - mote_mortality)

# Calculate growth of colonies
# Site_Area from user input
# calculate total area occupied by each individual coral within the plot
year1_taxa$restored_area_start <- site_area * (year1_taxa$restored_cover / 100)

# use user-input outplant size (in cm2) to calculate the estimated number of outplants
### Is this the best input unit ###
# round to a whole number so individuals can be modeled
# this step and the next may not be necessary because we have input size and could get dia from that
# but should test
year1_taxa$restored_ncolonies <- round(year1_taxa$restored_area_start / (outplant_size / 10000))

# Calculate individual colony diameters in m for each taxon based on the rounded estimate of colony number
year1_taxa$restored_coral_dia_start <- sqrt(
  (year1_taxa$restored_area_start / year1_taxa$restored_ncolonies) / pi
) * 2

# Grow the diameter of the colonies
# coral_growth is taxon specific
year1_taxa$year1_coral_dia_end <- year1_taxa$restored_coral_dia_start +
  (coral_growth$planar_mean / 100)

# calculate new species-specific area
year1_taxa$restored_area_end <- (((year1_taxa$year1_coral_dia_end / 2)^2) * pi) *
  year1_taxa$restored_ncolonies

# calculate new species-specific coral cover
year1_taxa$restored_cover_end <- (year1_taxa$restored_area_end / site_area) * 100

# Total Year 1 coral cover
year1_taxa$total_cover <- year1_taxa$baseline_cover + year1_taxa$restored_cover

# Calcuate gross production by taxon
year1_taxa$gp <- (year1_taxa$total_cover / 100) * travis_rates  # taxon-specific calcification rates

# sum to get site-level gross production
year1_site$gp <- sum(year1_taxa$gp)

# Calculate microbioerosion based on available substrate
# Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
# both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
year1_substrate <- 100 - sum(year1_taxa$total_cover)

# Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1
# (Perry and Lange, 2019)
micro_year1 <- (year1_substrate / 100) * 0.24

# Other Bioerosion data from Bioerosion.csv dataset
# Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
# NOTE that these values will not change from year to year in the model but Micro will
# This variabile should actually be calculated in the DataInput module
site_bioerosion <- sum(bioerosion$ave_parrotfish, bioerosion$ave_urchins, bioerosion$ave_macrobioerosion)

# Calculate site-level net production
year1_site$np <- year1_site$gp - (site_bioerosion + micro_year1)

# Calculate site-level reef-accretion potential
# use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity;
# if restoration mix >75% massives/other use Massive porosity
# else use mixed
# 2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018
year1_site$rap <- year1_site$np / 2.9 / (1 - porosity)

## Year 5 ----
# note that as currently conceptualized this will only model 1-4 years not the full 5 year period
# for the year 10 output

bleaching_severity <- 0  # can equal 8,12,16,20,24 based on user input
# do we need a 4 DHW scenario
bleaching_frequency <- 0  # can equal 1, 2, or annual (=4 or 5)

### Loop this for Year 1-2, 2-3, 3-4, and 4-5 ###
# Calculate decrease in coral cover with natural and bleaching-related mortality
# DHW_mortailty_rates from Webb et al. 2025
# https://doi.org/10.1038/s41598-025-28828-3
# these values are DHW and taxon specific
# see source(DHW.R)
# bleaching_frequency determines the number of years DHW_mortality_rates are applied
# middle year for frequency=1, every other year for frequency=2, and every year for annual

# For now I'm assuming 25% of colonies die outright, but we should revisit this
# Remaining 75% decline is partial mortality that reduces effective colony mortality
# maybe create lookup table for DHW based proportions of total colony versus partial mortality
year5_taxa$post_bleaching_restored_cover_1 <- year1_taxa$restored_cover_end + (dhw_mortality * 0.25)
year5_taxa$post_bleaching_restored_area_1 <- site_area * (year5_taxa$post_bleaching_restored_cover_1 / 100)
year5_taxa$post_bleaching_restored_ncolonies <- year1_taxa$restored_ncolonies -
  year5_taxa$post_bleaching_restored_area_1 / (((year1$restored_coral_dia_end / 2)^2) * pi)

# NColonies needs to be an integer for calculations below
year5_taxa$post_bleaching_restored_ncolonies_integer <- as.integer(
  year5_taxa$post_bleaching_restored_ncolonies
)

# Calculate new cover based on colonies that were lost
year5_taxa$coral_area_post_colony_mortality <- (year5_taxa$post_bleaching_restored_ncolonies_integer) *
  (((year1_taxa$restored_coral_dia_end / 2)^2) * pi)
year5_taxa$coral_cover_post_colony_mortality <- site_area *
  (year5_taxa$coral_area_post_colony_mortality / 100)

# Residual (decimal) from above, back into partial mortality calculation
year5_taxa$total_dhw_residual <- year5_taxa$post_bleaching_restored_ncolonies -
  year5_taxa$post_bleaching_restored_ncolonies_integer
year5_taxa$total_dhw_residual_area <- year5_taxa$total_dhw_residual *
  (((year1$restored_coral_dia_end / 2)^2) * pi)
year5_taxa$total_dhw_residual_cover_decline <- year5_taxa$total_dhw_residual_area / site_area

# Add residual cover decline to 75% of total scenario mortality
# baseline partial mortality also gets added in here
year5_taxa$restored_cover_post_mortality <- year5_taxa$coral_cover_post_colony_mortality +
  ((dhw_mortality * 0.75 - year5_taxa$total_dhw_residual_cover_decline) * (1 - x_mortality))

# calculate growth of corals that didn't die
# Site_Area from user input
# calculate total area occupied by each individual coral within the plot
year5_taxa$restored_area_start <- site_area * (year5_taxa$restored_cover_post_mortality / 100)

year5_taxa$restored_coral_dia_start <- sqrt(
  (year5_taxa$restored_area_start / year5_taxa$post_bleaching_restored_ncolonies_integer) / pi
) * 2

# Grow the diameter of the colonies
# coral_growth is taxon specific
# see bottom of DHW.R there are penalties for coral growth after bleaching
year5_taxa$year5_coral_dia_end <- restored_coral_dia_start +  # invalid: restored_coral_dia_start undefined (missing year5_taxa$)
  (coral_growth$planar_mean / 100 * bleach_severe_reduction_yearx)

# calculate new species-specific area
year5_taxa$restored_area_end <- (((restored_coral_dia_end / 2)^2) * pi) * post_bleaching_restored_ncolonies_integer  # invalid: undefined vars (missing year5_taxa$ prefixes)

# calculate new species-specific coral cover
year5_taxa$restored_cover_end <- (restored_area_end / site_area) * 100  # invalid: restored_area_end undefined (missing year5_taxa$)

# Repeat L196-232 for Baseline coral assemblage
# because dia of colonies is different so they need to be kept separate throughout

# For now I'm assuming 25% of colonies die outright
# Remaining 75% decline is partial mortality that reduces effective colony mortality
year5_taxa$post_bleaching_baseline_cover_1 <- year1_taxa$baseline_cover_end + (dhw_mortality * 0.25)
year5_taxa$post_bleaching_baseline_area_1 <- site_area * (year5_taxa$post_bleaching_baseline_cover_1 / 100)
year5_taxa$post_bleaching_baseline_ncolonies <- ear1_taxa$baseline_ncolonies -  # invalid: "ear1_taxa" typo for "year1_taxa"
  year5_taxa$post_bleaching_baseline_area_1 / (((year1$baseline_coral_dia_end / 2)^2) * pi)

# NColonies needs to be an integer for calculations below
year5_taxa$post_bleaching_baseline_ncolonies_integer <- as.integer(
  year5_taxa$post_bleaching_baseline_ncolonies
)

# Calculate new cover based on colonies that were lost
year5_taxa$coral_area_post_colony_mortality <- (year5_taxa$post_bleaching_baseline_ncolonies_integer) *
  (((year1_taxa$baseline_coral_dia_end / 2)^2) * pi)
year5_taxa$coral_cover_post_colony_mortality <- site_area *
  (year5_taxa$coral_area_post_colony_mortality / 100)

# Residual (decimal) from above, back into partial mortality calculation
year5_taxa$total_dhw_residual <- year5_taxa$post_bleaching_baseline_ncolonies -
  year5_taxa$post_bleaching_baseline_ncolonies_integer
year5_taxa$total_dhw_residual_area <- year5_taxa$total_dhw_residual *
  (((year1$baseline_coral_dia_end / 2)^2) * pi)
year5_taxa$total_dhw_residual_cover_decline <- (year5_taxa$total_dhw_residual_area / site_area) * 100

# Add residual cover decline to 75% of total scenario mortality
# baseline partial mortality also gets included in here
# DHW mortality and the residual coral cover decline are both in % cover
# residual decline subtracted because it is positive but DHW is negative
year5_taxa$baseline_cover_post_mortality <- (
  year5_taxa$coral_cover_post_colony_mortality +
    ((dhw_mortality * 0.57) - year5_taxa$total_dhw_residual_cover_decline)
) * (1 - x_mortality)

# calculate growth of corals that didn't die
# Site_Area in m2 from user input
# calculate total area occupied by each individual coral within the plot
year5_taxa$baseline_area_start <- site_area * (year5_taxa$baseline_cover_post_mortality / 100)

year5_taxa$baseline_coral_dia_start <- sqrt(
  (year5_taxa$baseline_area_start / year5_taxa$post_bleaching_baseline_ncolonies_integer) / pi
) * 2

# Grow the diameter of the colonies
# coral_growth is taxon specific
# see bottom of DHW.R there are penalties for coral growth after bleaching
year5_taxa$year5_coral_dia_end <- baseline_coral_dia_start +  # invalid: baseline_coral_dia_start undefined (missing year5_taxa$)
  (coral_growth$planar_mean / 100 * bleach_severe_reduction_yearx)

# calculate new species-specific area
year5_taxa$baseline_area_end <- (((baseline_coral_dia_end / 2)^2) * pi) * post_bleaching_baseline_ncolonies_integer  # invalid: undefined vars (missing year5_taxa$ prefixes)

# calculate new species-specific coral cover
year5_taxa$baseline_cover_end <- (baseline_area_end / site_area) * 100  # invalid: baseline_area_end undefined (missing year5_taxa$)

# Total Year_X coral cover
year5_taxa$total_cover <- year5_taxa$baseline_cover_end + year5_taxa$restored_cover_end

# Calculate gross production
year5_taxa$gp <- (year5_taxa$coral_cover / 100) * travis_rates  # taxon-specific calcification rates

# sum to get site-level gross production
year5_site$gp <- sum(year5_taxa$gp)

# Calculate microbioerosion based on available substrate
# Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
# both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
year5_substrate <- 100 - sum(year5_taxa$total_cover)

# Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1
# (Perry and Lange, 2019)
micro_year5 <- (year5_substrate / 100) * 0.24

# Other Bioerosion data from Bioerosion.csv dataset
# Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
# NOTE that these values will not change from year to year in the model but Micro will
# This variabile should actually be calculated in the DataInput module
site_bioerosion <- sum(bioerosion$ave_parrotfish, bioerosion$ave_urchins, bioerosion$ave_macrobioerosion)

# Calculate site-level net production
year5_site$np <- year5_site$gp - (site_bioerosion + micro_year5)

# Calculate site-level reef-accretion potential
# use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity;
# if restoration mix >75% massives/other use Massive porosity
# else use mixed
# 2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018
year5_site$rap <- year5_site$np / 2.9 / (1 - porosity)

## Year 10 ----

### Loop this for Year 5-6, 6-7, 7-8, 8-9, and 9-10 ###
# Calculate decrease in coral cover with natural and bleaching-related mortality
# DHW_mortailty_rates from Webb et al. 2025
# these values are DHW and taxon specific
# see source(DHW.R)
# bleaching_frequency determines the number of years DHW_mortality_rates are applied
# could be random or for simplified version (L126) either middle year for 1, every other year for 2,
# and every year for annual
# For now I'm assuming 25% of colonies die outright, but we should revisit this
# Remaining 75% decline is partial mortality that reduces effective colony mortality
year10_taxa$post_bleaching_restored_cover_1 <- year5_taxa$restored_cover_end + (dhw_mortality * 0.25)
year10_taxa$post_bleaching_restored_area_1 <- site_area * (year10_taxa$post_bleaching_restored_cover_1 / 100)
year10_taxa$post_bleaching_restored_ncolonies <- year5_taxa$restored_ncolonies -
  year10_taxa$post_bleaching_restored_area_1 / (((year1$restored_coral_dia_end / 2)^2) * pi)

# NColonies needs to be an integer for calculations below
year10_taxa$post_bleaching_restored_ncolonies_integer <- as.integer(
  year10_taxa$post_bleaching_restored_ncolonies
)

# Calculate new cover based on colonies that were lost
year10_taxa$coral_area_post_colony_mortality <- (year10_taxa$post_bleaching_restored_ncolonies_integer) *
  (((year5_taxa$restored_coral_dia_end / 2)^2) * pi)
year10_taxa$coral_cover_post_colony_mortality <- site_area *
  (year10_taxa$coral_area_post_colony_mortality / 100)

# Residual (decimal) from above, back into partial mortality calculation
year10_taxa$total_dhw_residual <- year10_taxa$post_bleaching_restored_ncolonies -
  year10_taxa$post_bleaching_restored_ncolonies_integer
year10_taxa$total_dhw_residual_area <- year10_taxa$total_dhw_residual *
  (((year1$restored_coral_dia_end / 2)^2) * pi)
year10_taxa$total_dhw_residual_cover_decline <- year10_taxa$total_dhw_residual_area / site_area

# Add residual cover decline to 75% of total scenario mortality
# baseline partial mortality also gets added in here
year10_taxa$restored_cover_post_mortality <- year10_taxa$coral_cover_post_colony_mortality +
  ((dhw_mortality * 0.57 - year10_taxa$total_dhw_residual_cover_decline) * (1 - x_mortality))

# calculate growth of corals that didn't die
# Site_Area from user input
# calculate total area occupied by each individual coral within the plot
year10_taxa$restored_area_start <- site_area * (year10_taxa$restored_cover_post_mortality / 100)

year10_taxa$restored_coral_dia_start <- sqrt(
  (year10_taxa$restored_area_start / year10_taxa$post_bleaching_restored_ncolonies_integer) / pi
) * 2

# Grow the diameter of the colonies
# coral_growth is taxon specific
# see bottom of DHW.R there are penalties for coral growth after bleaching
year10_taxa$year10_coral_dia_end <- restored_coral_dia_start +  # invalid: restored_coral_dia_start undefined (missing year10_taxa$)
  (coral_growth$planar_mean / 100 * bleach_severe_reduction_yearx)

# calculate new species-specific area
year10_taxa$restored_area_end <- (((restored_coral_dia_end / 2)^2) * pi) * post_bleaching_restored_ncolonies_integer  # invalid: undefined vars (missing year10_taxa$ prefixes)

# calculate new species-specific coral cover
year10_taxa$restored_cover_end <- (restored_area_end / site_area) * 100  # invalid: restored_area_end undefined (missing year10_taxa$)

# Repeat L196-232 for Baseline coral assemblage
# because dia of colonies is different so they need to be kept separate throughout

# For now I'm assuming 25% of colonies die outright
# Remaining 75% decline is partial mortality that reduces effective colony mortality
year10_taxa$post_bleaching_baseline_cover_1 <- year5_taxa$baseline_cover_end + (dhw_mortality * 0.25)
year10_taxa$post_bleaching_baseline_area_1 <- site_area * (year10_taxa$post_bleaching_baseline_cover_1 / 100)
year10_taxa$post_bleaching_baseline_ncolonies <- year5_taxa$restored_ncolonies -
  year10_taxa$post_bleaching_baseline_area_1 / (((year1$baseline_coral_dia_end / 2)^2) * pi)

# NColonies needs to be an integer for calculations below
year10_taxa$post_bleaching_baseline_ncolonies_integer <- as.integer(
  year10_taxa$post_bleaching_baseline_ncolonies
)

# Calculate new cover based on colonies that were lost
year10_taxa$coral_area_post_colony_mortality <- (year10_taxa$post_bleaching_baseline_ncolonies_integer) *
  (((year5_taxa$baseline_coral_dia_end / 2)^2) * pi)
year10_taxa$coral_cover_post_colony_mortality <- site_area *
  (year10_taxa$coral_area_post_colony_mortality / 100)

# Residual (decimal) from above, back into partial mortality calculation
year10_taxa$total_dhw_residual <- year10_taxa$post_bleaching_baseline_ncolonies -
  year10_taxa$post_bleaching_baseline_ncolonies_integer
year10_taxa$total_dhw_residual_area <- year10_taxa$total_dhw_residual *
  (((year1$baseline_coral_dia_end / 2)^2) * pi)
year10_taxa$total_dhw_residual_cover_decline <- (year10_taxa$total_dhw_residual_area / site_area) * 100

# Add residual cover decline to 75% of total scenario mortality
# baseline partial mortality also gets included in here
# DHW mortality and the residual coral cover decline are both in % cover
# residual decline subtracted because it is positive but DHW is negative
year10_taxa$baseline_cover_post_mortality <- (
  year10_taxa$coral_cover_post_colony_mortality +
    ((dhw_mortality * 0.57) - year10_taxa$total_dhw_residual_cover_decline)
) * (1 - x_mortality)

# calculate growth of corals that didn't die
# Site_Area in m2 from user input
# calculate total area occupied by each individual coral within the plot
year10_taxa$baseline_area_start <- site_area * (year10_taxa$baseline_cover_post_mortality / 100)

year10_taxa$baseline_coral_dia_start <- sqrt(
  (year10_taxa$baseline_area_start / year10_taxa$post_bleaching_baseline_ncolonies_integer) / pi
) * 2

# Grow the diameter of the colonies
# coral_growth is taxon specific
# see bottom of DHW.R there are penalties for coral growth after bleaching
year10_taxa$year10_coral_dia_end <- baseline_coral_dia_start +  # invalid: baseline_coral_dia_start undefined (missing year10_taxa$)
  (coral_growth$planar_mean / 100 * bleach_severe_reduction_year0)

# calculate new species-specific area
year10_taxa$baseline_area_end <- (((baseline_coral_dia_end / 2)^2) * pi) * post_bleaching_baseline_ncolonies_integer  # invalid: undefined vars (missing year10_taxa$ prefixes)

# calculate new species-specific coral cover
year10_taxa$baseline_cover_end <- (baseline_area_end / site_area) * 100  # invalid: baseline_area_end undefined (missing year10_taxa$)

# Total Year_X coral cover
year10_taxa$total_cover <- year10_taxa$baseline_cover_end + year5_taxa$restored_cover_end

# Calculate gross production
year10_taxa$gp <- (year10_taxa$coral_cover / 100) * travis_rates  # taxon-specific calcification rates

# sum to get site-level gross production
year10_site$gp <- sum(year10_taxa$gp)

# Calculate microbioerosion based on available substrate
# Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
# both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
year10_substrate <- 100 - sum(year10_taxa$total_cover)

# Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1
# (Perry and Lange, 2019)
micro_year5 <- (year10_substrate / 100) * 0.24

# Other Bioerosion data from Bioerosion.csv dataset
# Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
# NOTE that these values will not change from year to year in the model but Micro will
# This variabile should actually be calculated in the DataInput module
site_bioerosion <- sum(bioerosion$ave_parrotfish, bioerosion$ave_urchins, bioerosion$ave_macrobioerosion)

# Calculate site-level net production
year10_site$np <- year10_site$gp - (site_bioerosion + micro_year5)

# Calculate site-level reef-accretion potential
# use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity;
# if restoration mix >75% massives/other use Massive porosity
# else use mixed
# 2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018
year10_site$rap <- year10_site$np / 2.9 / (1 - porosity)

## Restoration Impacts and Return-on-Investment ----

### Change in Average Reef Elevation ###

# Need matrix of estimated outplant height by species or at least morphology
# ??? Sara Williams can probably provide this ???#
initial_outplant_height <- 2  # cm

# For time 0
# species-specific cover should be multiplied by species-specific average outplant height
year0_taxa$elevation <- year0_taxa$restored_cover * initial_outplant_size  # invalid: initial_outplant_size undefined (defined above as initial_outplant_height)
year0_site$elevation <- sum(year0_taxa$elevation) * (sum(year0_taxa$restored_cover) / 100)
# I think this math works

# For each time point 1, 5, and 10 years after outplanting
year_x_taxa$elevation <- (year(x - 1)_taxa$restored_cover * coral_growth$mean_extension_rate)  # invalid: "year(x - 1)_taxa" is not valid R syntax
year_x_site$elevation <- year(x - 1)$elevation +  # invalid: "year(x - 1)" is not valid R syntax
  (sum(year_x_taxa$elevation) * (sum(year_x_taxa$restored_cover) / 100))

### Ranking of Reef-Accretion Potential ###
# Needs to be reported in the context of what this actually means for whether the reef is growing
# and how fast
baseline_percentile <- length(baseline_budgets$rap[baseline_budgets$rap < baseline_rap) /  # invalid: missing closing bracket before closing paren
  length(baseline_budgets$rap) * 100
year_x_percentile <- length(baseline_budgets$rap[baseline_budgets$rap < year_x_site$rap]) /
  length(baseline_budgets$rap) * 100

library(tidyr)
library(dplyr)
# Determine relationship between RAP increase and Ranking increase
ranking_summary <- tibble(rap = baseline_budgets$rap) |>
  crossing(threshold = seq(-1, 1, 0.05)) |>
  group_by(threshold) |>
  summarise(pct_below = mean(rap < threshold, na.rm = TRUE) * 100) |>
  as.data.frame()

## Calculate ROI ----
# ????John????
