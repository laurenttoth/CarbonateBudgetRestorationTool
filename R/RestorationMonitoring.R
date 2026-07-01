########################
#Restoration Monitoring#
########################
library(tidyr)
library(dplyr)

Monitoring_data<-load.csv("Restoration_Monitoring_Cover_TEMPLATE.csv", header=T)

#Calculate taxon-level carbonate budgets
Monitoring_data$GP<-(Year0_taxa$Percent_Cover/100)*TravisRates

#for each unique value in Years_Post_Restoration (including fractional years) and each unique site if there are multiple
Monitoring_data_site<-Monitoring_data %>%
  group_by(Unique_Site_ID, Years_Post_Restoration) %>%
  #Unconsolidated substrate needs to be removed for coral cover calculation
  #Should CCA also be removed for this???
  filter(Percent_Cover!="REQUIRED_Unconsolidated_substrate") %>%
  summarize(GP_site=sum(GP), Coral_Cover=sum(Percent_Cover))

#Calculate substrate for microbioerosion
Substrate<-Monitoring_data %>%
  group_by(Unique_Site_ID, Years_Post_Restoration) %>%
  summarize(Substrate=100-sum(Percent_Cover))

#Calculate microbioerosion
Substrate$Microbioerosion<-(Substrate$Substrate/100)*0.24

#If no data provided or if incomplete data provided (e.g. parrotfish but no sponges)
#Add bioerosion for each site
Monitoring_data_site<-Monitoring_data_site %>%
  left_join(Bioerosion, by = c("Habitat", "Subegion")) %>% #add bioerosion
  left_join(Substrate, by = c("Habitat", "Subegion"))

#If bioerosion input data provided apply the following calculations, as relevant
#If data available for every timepoint in the Monitoring_data apply timepoint specific rates
#Else apply single value or average value from whatever is provided

#Parrotfish Bioerosion
#ParrotfishBioerosionRates.csv has species, size class, and life-phase-specific bioerosion rates
#Parrotfish from user input
#remove juveniles, which are assumed to contribute minimally to bioerosion
Parrotfish<-Parrotfish[Parrotfish$Size!="0-9",]
#Create a variable that merges text for life phase and size to align with rates table
Parrotfish$PhaseSize<-paste(Parrotfish$Life_phase, Parrotfish$Fork_length_cm, sep="")

ParrotfishRates<-read.csv("ParrotfishBioerosionRates.csv", header=T)
ParrotfishRates<-na.omit(ParrotfishRates)

#merge rates and parrtofish data
Parrotfish_rates<-merge(Parrotfish, ParrotfishRates, by = c("Taxon","PhaseSize"), all.x = T)

#Calculate bioerosion from each individual parrotfish observed
Parrotfish_rates$Bioerosion<-(Parrotfish_rates$Count*Parrotfish_rates$Rate)/Parrotfish_rates$Survey_Area_m2

#Sum for site and/or timepoint
Parrotfish_rates_site<-Parrotfish_rates %>%
  group_by(Unique_Site_ID, Years_Post_Restoration) %>%
  summarize(SumBioerosion=sum(Bioerosion, na.rm=T), SumCounts=sum(Count))

#Urchin Bioerosion
#UrchinBioerosionRates.csv has species an test-size specific bioerosion rates
#test sizes are median of range in input
#If test size not incuded use 30 cm as most urching on FCR are relatively small
UrchinRates<-read.csv("UrchinBioerosionRates.csv", header=T)
#Urchins from user input
UrchinBioerosion<-merge(Urchins, UrchinRates, by = c("Taxon","Test.size"), all.x = T)

#Calcualte individual bioerosion of all urchins
#Survey_Area_m2 from input data
#urchin test sizes are in mm
UrchinBioerosion$Bioerosion<-((Urchins$Count/Urchins$Survey_Area_m2)*UrchinBioerosion$Bioerosion.rate*365)/1000

#Sum for site and/or timepoint
Urchin_rates_site<-UrchinBioerosion %>%
  group_by(Unique_Site_ID, Years_Post_Restoration) %>%
  summarize(SumBioerosion=sum(Bioerosion, na.rm=T))

#Sponge Bioerosion
#Sponge from user input
#SpongeBioerosionRates.csv has species-specific sponge bioerosion rates
SpongeRates<-read.csv("SpongeBioerosionRates.csv", header=T)
Sponge_rates <- merge(Sponges, SpongeRates, by = "Taxon", all.x = T)

#convert sponge areas in cm2 to m2
Sponge_rates$Area<-Sponge_rates$Area*0.0001

#Calcuate bioerosion by each observed sponge
#Survey_Area_m2 from input data
Sponge_rates$Bioerosion <- (Sponge_rates$Area * Sponge_rates$Bioerosion.rate)/Sponge_rates$Survey_Area_m2

#Sum for site and/or timepoint
Sponge_rates_site<-Sponge_rates %>%
  group_by(Unique_Site_ID, Years_Post_Restoration) %>%
  summarize(SumBioerosion=sum(Bioerosion, na.rm=T))

#Calculate net budget
Monitoring_data_site$NP<-Monitoring_data_site$GP-sum(Monitoring_data_site$Microbioerosion, Monitoring_data_site$Parrotfish, Monitoring_data_site$Urchins, Monitoring_data_site$BioSponges)

#Calculate reef-accretion potential
#use porosity dataset lookup:
#quantify mix on min(Years_Post_Restoration)
#if restoration mix >75% Acropora spp., use Acropora porosity; if restoration mix >75% massives/other use Massive porosity
#else use mixed
Monitoring_data_site$RAP<-Monitoring_data_site$NP/2.9/(1- porosity) #2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018

#Plot for each Years_Post_Restoration time point for the user-selected site (based on dropdown)
#hline for "Geological baseline" = 3 mm/yr #https://doi.org/10.1111/gcb.14389
#hline for "Present-day sea-level rise" = 2.64 mm/yr #https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8724580
#future sea-level rise displayed based on user selections of 2050 vs 2010 and scenario
#use this look up: AR6_SLR_KW.csv SLR_Rate
#text display for scenario = paste(Year, Scenario)
#data from: https://sealevel.nasa.gov/ipcc-ar6-sea-level-projection-tool

###Ranking of Reef-Accretion Potential###
#Needs to be reported in the context of what this actually means for whether the reef is growing and how fast
Percentile <-length(Baseline_budgets$RAP[Baseline_budgets$RAP < Monitoring_data_site$RAP]) / length(Baseline_budgets$RAP) * 100



