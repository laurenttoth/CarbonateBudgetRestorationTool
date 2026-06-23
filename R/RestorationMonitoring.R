########################
#Restoration Monitoring#
########################
library(tidyr)
library(dplyr)

Monitoring_data<-load.csv("Restoration_Monitoring_TEMPLATE.csv", header=T)

#Calculate taxon-level carbonate budgets
Monitoring_data$GP<-Year0_taxa$Percent_Cover*TravisRates

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

#Add bioerosion for each site
Monitoring_data_site<-Monitoring_data_site %>%
  left_join(Bioerosion, by = c("Habitat", "Subegion") #add bioerosion
  left_join(Substrate, by = c("Habitat", "Subegion"))

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



