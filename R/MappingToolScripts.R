####Mapping Tab####

#load map base layer

#load carbonate budget data
setwd("Data")
Baseline_budgets<-read.csv("NCRMP_CarbonateBudgets_2014_to_2024.csv", header=T)

#add Reef-accretion potential to spreadsheet
Baseline_budgets$RAP<-Baseline_budgets$net_G/2.9/(1-0.6265)

setwd("..")

#Points on map should be colored based on "ReefAccretionPotential" (RAP), which has units of mm y-1
#Existing color scale for carbonate production should work
#But it might good to have hard breaks between negative (< -0.5 mm y-1), neutral (0 +/- 0.5 mm -y), positive (> 0.5 mm y-1)
#Aligns with Perry et al. 2014 (https://doi.org/10.1098/rspb.2014.2018)

#Allow filter by year, subregion, habitat
#Toggle to show just coral cover, just bioerosion, net/RAP

#slider on page will allow users to adjust potential restored coral cover, let's set at 0-30% at 5% intervals for now
#if we set it to 50%, one site will exceed 100% coral cover if restored_cover is maxed out
#calling this  "restored_cover" in the eq below
#dummy to test
restored_cover=5

#ran this to get relationship between coral cover and RAP:
LM_CC_RAP<-lm(Baseline_budgets$ReefAccretionPotential~Baseline_budgets$percentCover_HardCoral)

#summary(LM_CC_RAP)
CC_RAP_slope<-LM_CC_RAP$coefficients[2]
CC_RAP_intercept<-LM_CC_RAP$coefficients[1]

Baseline_budgets$restored_RAP<-CC_RAP_slope*(Baseline_budgets$percentCover_HardCoral+restored_cover)+CC_RAP_intercept

#check to make sure we're not exceeding 100% cover
#table(Baseline_budgets$percentCover_HardCoral+restored_cover>100) #ok at 30% max

#after slider input, sites restored_RAP < -0.5 should be grayed out
#and key for these should say something like "Restoration cannot mitigate reef erosion"
#sites that now have a neutral budget should be in yellow
#with text something like "Restoration will mitigate erosion"
#sites that have a positive budget should be in blue
#if "ReefAccretionPotential" was already positive, perhaps we increase point transparency
#text "This reef was growing prior to restoration"
#if "ReefAccretionPotential" was negative or neutral and is now positive, we highlight in some way, perhaps white outline?
#text "Restoration could resume reef growth"

###CONNOR how big of a lift is this? Could wait, if needed
#Another thing we should probably include here is an option for users to click on individual points to see site metadata:
#lat,long, water depth, baseline coral cover and baseline RAP, eroding/stasis/growing with and without restoration
#perhaps metadata could include SL rise projections under 2.6, 4.5, 8.5??

#Think about evaluating balance between microbioerosion and parrotfish; high micro low parrotfish = ideal
#add additional toggles e.g. select sites with low parrotfish bioerosion

