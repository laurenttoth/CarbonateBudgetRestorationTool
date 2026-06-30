###############################
#Restoration scenario modeling#
###############################

############Notes about modifications to Alice's version of the app##############
#Data input moves to new page

#add two additional restoration species Pseudodiploria strigosa and Pseudodiploria clivosa
restoration_species <- c(
  "Acropora palmata", "Acropora cervicornis", "Montastraea cavernosa",
  "Orbicella faveolata", "Colpophyllia natans", "Pseudodiploria strigosa", "Pseudodiploria clivosa", "Porites astreoides",
  "Siderastrea siderea", "Stephanocoenia intersepta",'Diploria labyrinthiformis','Solenastrea bournoni'
)

#cap slider at 50% (could be lower)
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

####################
##MATH NOT SCRIPTS##
####################

##########Year 0##########
#Baseline from DataInputScripts.R
#Restored_Cover from slider inputs

#Potential way to code for math below:
Year0_taxa=
  merge(Baseline,Input_Cover, TravisRates) %>%
  arrange(Taxon) %>%
  mutate(Total_Cover=Percent_Cover+Restored_Cover) %>%
  filter(Taxon!=REQUIRED_Unconsolidated_substrate) %>%
  mutate(GP=Total_Cover*rate)

#Calcuate total calcifier cover after restoration
#For all calcifier cover based calculations need to remove unconsolidated substrate
Year0_taxa$Total_Cover<-Baseline$Percent_Cover+Year0_taxa$Restored_Cover

#Calcuate gross production by taxon
Year0_taxa$GP<-Year0_taxa$Total_Cover*TravisRates #taxon-specific calcification rates

#sum to get site-level gross production
Year0_site$GP<-sum(Year0_taxa$GP)

#Calculate microbioerosion based on available substrate
#Note that Total_Cover here should include total calcifier cover + unconsolidated substrate
#both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
Year0_substrate<-100-sum(Year0_taxa$Total_Cover)

#Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1 (Perry and Lange, 2019)
Micro_Year0<-(Year0_substrate/100)*0.24

#Other Bioerosion data from Bioerosion.csv dataset
#Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
#NOTE that these values will not change from year to year in the model but Micro will
#This variabile should actually be calculated in the DataInput module
Site_bioerosion<-sum(Bioerosion$Parrotfish, Bioerosion$Urchins, Bioerosion$BioSponges)

#Calculate site-level net production
Year0_site$NP<-Year0_site$GP-(Site_bioerosion+Micro_Year0)

#Calculate site-level reef-accretion potential
#use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity; if restoration mix >75% massives/other use Massive porosity
#could weight based on relative contribution
#else use mixed
Year0_site$RAP<-Year0_site$NP/2.9/(1- porosity) #2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018

##########Year 1##########
#Calculate change in percent cover after one year based on species-specific growth and mortality rates

#Partial mortality rates for branching and "other" corals from Browne et al. in press applied to Baseline_Cover
#Assuming that whole colony mortailty rates for established colonies in the absence of bleaching is negligible
#Browne et al. estimates whole-colony mortality at 0.002-0.00076 for branching and 0.002-0.0002 for foliose and other morphologies
#foliose same as other in Browne et al. so lumped
#these are rates for the 50 cm (middle) size class
#LT created Morphology_for_mortality_LOOKUP (currently .xlsx): note that foliose retained as category but foliose and other should be treated as other
branching_mortality<-0.0414
other_mortality<-0.0296

#Calculate mortality of baseline colonies by taxa
Year1_taxa$Baseline_Cover_Start<-Baseline$Percent_Cover*(1-x_mortality)

#Calculate growth of colonies
#Site_Area from user input
#calculate total area occupied by each coraltaxa within the plot
Year1_taxa$Baseline_Coral_Area<-Site_Area*(Baseline$Percent_Cover/100)

#Join NCRMP_colony_dia_Florida.csv
Florida_colony_dia<-read.csv("NCRMP_colony_dia_Florida.csv", header=T)
#Use regional median taxon specific colony diameters (length_mean) to estimate number of colonies for each species
#round to a whole number so individuals can be modeled
Year1_taxa$Baseline_NColonies<-round(Year1_taxa$Baseline_Coral_Area/Florida_colony_dia)

#Calculate individual colony diameters for each taxon based on the rounded estimate of colony number
Year1_taxa$Baseline_Coral_Dia<-Year1_taxa$Baseline_Coral_Area/Year1_taxa$Baseline_NColonies

#growth_rates_ReefBudget_NCRMP.csv has species-specific coral growth rates in cm
#apply planar growth to each coral
#I calculated the ratio of coral diameters to heights across all the Atlantic NCRMP Demographic data
#I then multiplied that ratio by the ReefBudget mean_ext rate to estimate colony diameter growth
coral_growth<-read.csv("growth_rates_ReefBudget_NCRMP.csv", header=T)
#dividing by 100 converts cm rates to m2 which is units for coral area
Year1_taxa$Year1_Coral_Dia<-Baseline_Coral_Dia+((coral_growth$planar_mean/100))

#calculate new species-specific area assuming each colony is a circle
Year1_taxa$Year1_Coral_Area<-(((Year1_Coral_Dia/2)^2)*pi)*NColonies

#calculate new species-specific coral cover
Year1_taxa$Baseline_Cover_End<-(Year1_Coral_Area/Site_Area)*100

#New outplants have high mortality rates so this extra step is applied for the first year post outplanting
#mortality will also be scaled by outplant_size input, but haven't totally figured out how yet
#likely will have Mote_mortality (taxon-specific mortality rates) scaled by user input of outplant size
#so match Mote_mortality based on taxon and initial outplant size for calculation

#mortality of restored colonies
#will be look-up with species-specific rates
#use Mote_mortality=0.3 for now
Year1_taxa$Restored_Cover_Start<-Year0_taxa$Restored_Cover*(1-Mote_mortality)

#Calculate growth of colonies
#Site_Area from user input
#calculate total area occupied by each individual coral within the plot
Year1_taxa$Restored_Area_Start<-Site_Area*(Year1_taxa$Restored_Cover/100)

#use user-input outplant size (in area) to calculate the estimated number of outplants
#round to a whole number so individuals can be modeled
#this step and the next may not be necessary because we have input size and could get dia from that
#but should test
Year1_taxa$Restored_NColonies<-round(Year1_taxa$Restored_Area_Start/Outplant_size)

#Calculate individual colony diameters for each taxon based on the rounded estimate of colony number
Year1_taxa$Restored_Coral_Dia_Start<-Year1_taxa$Restored_Area_Start/Year1_taxa$Restored_NColonies

#Grow the diameter of the colonies
#coral_growth is taxon specific
Year1_taxa$Year1_Coral_Dia_End<-Year1_taxa$Restored_Coral_Dia_Start+((coral_growth$planar_mean/100))

#calculate new species-specific area
Year1_taxa$Restored_Area_End<-(((Year1_taxa$Restored_Coral_Dia_End/2)^2)*pi)*Year1_taxa$Restored_NColonies

#calculate new species-specific coral cover
Year1_taxa$Restored_Cover_End<-(Year1_taxa$Restored_Area_End/Site_Area)*100

#Total Year 1 coral cover
Year1_taxa$Total_cover<-Year1_taxa$Baseline_Cover + Year1_taxa$Restored_Cover

#Calcuate gross production by taxon
Year1_taxa$GP<-Year1_taxa$Total_cover*TravisRates #taxon-specific calcification rates

#sum to get site-level gross production
Year1_site$GP<-sum(Year1_taxa$GP)

#Calculate microbioerosion based on available substrate
#Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
#both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
Year1_substrate<-100-sum(Year1_taxa$Total_Cover)

#Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1 (Perry and Lange, 2019)
Micro_Year1<-(Year1_substrate/100)*0.24

#Other Bioerosion data from Bioerosion.csv dataset
#Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
#NOTE that these values will not change from year to year in the model but Micro will
#This variabile should actually be calculated in the DataInput module
Site_bioerosion<-sum(Bioerosion$Parrotfish, Bioerosion$Urchins, Bioerosion$BioSponges)

#Calculate site-level net production
Year1_site$NP<-Year1_site$GP-(Site_bioerosion+Micro_Year1)

#Calculate site-level reef-accretion potential
#use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity; if restoration mix >75% massives/other use Massive porosity
#else use mixed
Year1_site$RAP<-Year1_site$NP/2.9/(1-porosity) #2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018

##########Year 5##########
#note that as currently conceptualized this will only model 1-4 years not the full 5 year period for the year 10 output

bleaching_severity<-0 #can equal 8,12,16,20,24 based on user input
bleaching_frequency<-0 #can equal 1, 2, or annual (=4 or 5)

###Loop this for Year 1-2, 2-3, 3-4, and 4-5###
#Calculate decrease in coral cover with natural and bleaching-related mortality
#DHW_mortailty_rates from Webb et al. 2025
#these values are DHW and taxon specific
#see source(DHW.R)
#bleaching_frequency determines the number of years DHW_mortality_rates are applied
#middle year for frequency=1, every other year for frequency=2, and every year for annual

#For now I'm assuming 25% of colonies die outright, but we should revisit this
#Remaining 75% decline is partial mortality that reduces effective colony mortality
Year5_taxa$Post_bleaching_restored_cover_1<-Year1_taxa$Restored_Cover_End+(DHW_mortality*0.25)
Year5_taxa$Post_bleaching_restored_area_1<-Site_Area*(Year5_taxa$Post_bleaching_restored_cover_1/100)
Year5_taxa$Post_bleaching_restored_NColonies<-Year5_taxa$Post_bleaching_restored_area_1/(((Year1$Restored_Coral_Dia_End/2)^2)*pi)

#NColonies needs to be an integer for calculations below
Year5_taxa$Post_bleaching_restored_NColonies_integer<-as.integer(Year5_taxa$Post_bleaching_restored_NColonies)

#Calculate new cover based on colonies that were lost
Year5_taxa$Coral_Area_post_colony_mortality<-(Year5_taxa$Post_bleaching_restored_NColonies_integer)*(((Year1_taxa$Restored_Coral_Dia_End/2)^2)*pi)
Year5_taxa$Coral_Cover_post_colony_mortality<-Site_Area*(Year5_taxa$Coral_Area_post_colony_mortality/100)

#Residual (decimal) from above, back into partial mortality calculation
Year5_taxa$Total_DHW_residual<-Year5_taxa$Post_bleaching_restored_NColonies-Year5_taxa$Post_bleaching_restored_NColonies_integer
Year5_taxa$Total_DHW_residual_area<-Year5_taxa$Total_DHW_residual*(((Year1$Restored_Coral_Dia_End/2)^2)*pi)
Year5_taxa$Total_DHW_residual_cover_decline<-Year5_taxa$Total_DHW_residual_area/Site_Area

#Add residual cover decline to 75% of total scenario mortality
#baseline partial mortality also gets added in here
Year5_taxa$Restored_cover_post_mortality<-Year5_taxa$Coral_Cover_post_colony_mortality+((DHW_mortality*0.57)-sum(Year5_taxa$Total_DHW_residual_cover_decline+x_mortality))

#calculate growth of corals that didn't die
#Site_Area from user input
#calculate total area occupied by each individual coral within the plot
Year5_taxa$Restored_Area_Start<-Site_Area*(Year5_taxa$Restored_cover_post_mortality/100)

Year5_taxa$Restored_Coral_Dia_Start<-Year5_taxa$Restored_Area_Start/Year5_taxa$Post_bleaching_restored_NColonies_integer

#Grow the diameter of the colonies
#coral_growth is taxon specific
Year5_taxa$Year5_Coral_Dia_End<-Restored_Coral_Dia_Start+((coral_growth$planar_mean/100))

#calculate new species-specific area
Year5_taxa$Restored_Area_End<-(((Restored_Coral_Dia_End/2)^2)*pi)*Post_bleaching_restored_NColonies_integer

#calculate new species-specific coral cover
Year5_taxa$Restored_Cover_End<-(Restored_Area_End/Site_Area)*100

#Repeat L196-232 for Baseline coral assemblage
#because dia of colonies is different so they need to be kept separate throughout

#For now I'm assuming 25% of colonies die outright
#Remaining 75% decline is partial mortality that reduces effective colony mortality
Year5_taxa$Post_bleaching_Baseline_cover_1<-Year1_taxa$Baseline_Cover_End+(DHW_mortality*0.25)
Year5_taxa$Post_bleaching_Baseline_area_1<-Site_Area*(Year5_taxa$Post_bleaching_Baseline_cover_1/100)
Year5_taxa$Post_bleaching_Baseline_NColonies<-Year5_taxa$Post_bleaching_Baseline_area_1/(((Year1$Baseline_Coral_Dia_End/2)^2)*pi)

#NColonies needs to be an integer for calculations below
Year5_taxa$Post_bleaching_Baseline_NColonies_integer<-as.integer(Year5_taxa$Post_bleaching_Baseline_NColonies)

#Calculate new cover based on colonies that were lost
Year5_taxa$Coral_Area_post_colony_mortality<-(Year5_taxa$Post_bleaching_Baseline_NColonies_integer)*(((Year1_taxa$Baseline_Coral_Dia_End/2)^2)*pi)
Year5_taxa$Coral_Cover_post_colony_mortality<-Site_Area*(Year5_taxa$Coral_Area_post_colony_mortality/100)

#Residual (decimal) from above, back into partial mortality calculation
Year5_taxa$Total_DHW_residual<-Year5_taxa$Post_bleaching_Baseline_NColonies-Year5_taxa$Post_bleaching_Baseline_NColonies_integer
Year5_taxa$Total_DHW_residual_area<-Year5_taxa$Total_DHW_residual*(((Year1$Baseline_Coral_Dia_End/2)^2)*pi)
Year5_taxa$Total_DHW_residual_cover_decline<-Year5_taxa$Total_DHW_residual_area/Site_Area

#Add residual cover decline to 75% of total scenario mortality
#baseline partial mortality also gets added in here
Year5_taxa$Baseline_cover_post_mortality<-Year5_taxa$Coral_Cover_post_colony_mortality+((DHW_mortality*0.57)-sum(Year5_taxa$Total_DHW_residual_cover_decline+x_mortality))

#calculate growth of corals that didn't die
#Site_Area from user input
#calculate total area occupied by each individual coral within the plot
Year5_taxa$Baseline_Area_Start<-Site_Area*(Year5_taxa$Baseline_cover_post_mortality/100)

Year5_taxa$Baseline_Coral_Dia_Start<-Year5_taxa$Baseline_Area_Start/Year5_taxa$Post_bleaching_Baseline_NColonies_integer

#Grow the diameter of the colonies
#coral_growth is taxon specific
Year5_taxa$Year5_Coral_Dia_End<-Baseline_Coral_Dia_Start+((coral_growth$planar_mean/100))

#calculate new species-specific area
Year5_taxa$Baseline_Area_End<-(((Baseline_Coral_Dia_End/2)^2)*pi)*Post_bleaching_Baseline_NColonies_integer

#calculate new species-specific coral cover
Year5_taxa$Baseline_Cover_End<-(Baseline_Area_End/Site_Area)*100

#Total Year_X coral cover
Year5_taxa$Total_cover<-Year5_taxa$Baseline_Cover_End + Year1_taxa$Restored_Cover_End

#Calculate gross production
Year5_taxa$GP<-Year5_taxa$Coral_Cover*TravisRates #taxon-specific calcification rates

#sum to get site-level gross production
Year5_site$GP<-sum(Year5_taxa$GP)

#Calculate microbioerosion based on available substrate
#Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
#both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
Year5_substrate<-100-sum(Year5_taxa$Total_Cover)

#Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1 (Perry and Lange, 2019)
Micro_Year5<-(Year5_substrate/100)*0.24

#Other Bioerosion data from Bioerosion.csv dataset
#Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
#NOTE that these values will not change from year to year in the model but Micro will
#This variabile should actually be calculated in the DataInput module
Site_bioerosion<-sum(Bioerosion$Parrotfish, Bioerosion$Urchins, Bioerosion$BioSponges)

#Calculate site-level net production
Year5_site$NP<-Year5_site$GP-(Site_bioerosion+Micro_Year5)

#Calculate site-level reef-accretion potential
#use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity; if restoration mix >75% massives/other use Massive porosity
#else use mixed
Year5_site$RAP<-Year5_site$NP/2.9/(1- porosity) #2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018

##########Year 10##########

###Loop this for Year 5-6, 6-7, 7-8, 8-9, and 9-10###
#Calculate decrease in coral cover with natural and bleaching-related mortality
#DHW_mortailty_rates from Webb et al. 2025
#these values are DHW and taxon specific
#see source(DHW.R)
#bleaching_frequency determines the number of years DHW_mortality_rates are applied
#could be random or for simplified version (L126) either middle year for 1, every other year for 2, and every year for annual
#For now I'm assuming 25% of colonies die outright
#Remaining 75% decline is partial mortality that reduces effective colony mortality
Year10_taxa$Post_bleaching_restored_cover_1<-Year1_taxa$Restored_Cover_End+(DHW_mortality*0.25)
Year10_taxa$Post_bleaching_restored_area_1<-Site_Area*(Year10_taxa$Post_bleaching_restored_cover_1/100)
Year10_taxa$Post_bleaching_restored_NColonies<-Year10_taxa$Post_bleaching_restored_area_1/(((Year1$Restored_Coral_Dia_End/2)^2)*pi)

#NColonies needs to be an integer for calculations below
Year10_taxa$Post_bleaching_restored_NColonies_integer<-as.integer(Year10_taxa$Post_bleaching_restored_NColonies)

#Calculate new cover based on colonies that were lost
Year10_taxa$Coral_Area_post_colony_mortality<-(Year10_taxa$Post_bleaching_restored_NColonies_integer)*(((Year5_taxa$Restored_Coral_Dia_End/2)^2)*pi)
Year10_taxa$Coral_Cover_post_colony_mortality<-Site_Area*(Year10_taxa$Coral_Area_post_colony_mortality/100)

#Residual (decimal) from above, back into partial mortality calculation
Year10_taxa$Total_DHW_residual<-Year10_taxa$Post_bleaching_restored_NColonies-Year10_taxa$Post_bleaching_restored_NColonies_integer
Year10_taxa$Total_DHW_residual_area<-Year10_taxa$Total_DHW_residual*(((Year1$Restored_Coral_Dia_End/2)^2)*pi)
Year10_taxa$Total_DHW_residual_cover_decline<-Year10_taxa$Total_DHW_residual_area/Site_Area

#Add residual cover decline to 75% of total scenario mortality
#baseline partial mortality also gets added in here
Year10_taxa$Restored_cover_post_mortality<-Year10_taxa$Coral_Cover_post_colony_mortality+((DHW_mortality*0.57)-sum(Year10_taxa$Total_DHW_residual_cover_decline+x_mortality))

#calculate growth of corals that didn't die
#Site_Area from user input
#calculate total area occupied by each individual coral within the plot
Year10_taxa$Restored_Area_Start<-Site_Area*(Year10_taxa$Restored_cover_post_mortality/100)

Year10_taxa$Restored_Coral_Dia_Start<-Year10_taxa$Restored_Area_Start/Year10_taxa$Post_bleaching_restored_NColonies_integer

#Grow the diameter of the colonies
#coral_growth is taxon specific
Year10_taxa$Year10_Coral_Dia_End<-Restored_Coral_Dia_Start+((coral_growth$planar_mean/100))

#calculate new species-specific area
Year10_taxa$Restored_Area_End<-(((Restored_Coral_Dia_End/2)^2)*pi)*Post_bleaching_restored_NColonies_integer

#calculate new species-specific coral cover
Year10_taxa$Restored_Cover_End<-(Restored_Area_End/Site_Area)*100

#Repeat L196-232 for Baseline coral assemblage
#because dia of colonies is different so they need to be kept separate throughout

#For now I'm assuming 25% of colonies die outright
#Remaining 75% decline is partial mortality that reduces effective colony mortality
Year10_taxa$Post_bleaching_Baseline_cover_1<-Year1$Baseline_Cover_End+(DHW_mortality*0.25)
Year10_taxa$Post_bleaching_Baseline_area_1<-Site_Area*(Year10_taxa$Post_bleaching_Baseline_cover_1/100)
Year10_taxa$Post_bleaching_Baseline_NColonies<-Year10_taxa$Post_bleaching_Baseline_area_1/(((Year1$Baseline_Coral_Dia_End/2)^2)*pi)

#NColonies needs to be an integer for calculations below
Year10_taxa$Post_bleaching_Baseline_NColonies_integer<-as.integer(Year10_taxa$Post_bleaching_Baseline_NColonies)

#Calculate new cover based on colonies that were lost
Year10_taxa$Coral_Area_post_colony_mortality<-(Year10_taxa$Post_bleaching_Baseline_NColonies_integer)*(((Year5_taxa$Baseline_Coral_Dia_End/2)^2)*pi)
Year10_taxa$Coral_Cover_post_colony_mortality<-Site_Area*(Year10_taxa$Coral_Area_post_colony_mortality/100)

#Residual (decimal) from above, back into partial mortality calculation
Year10_taxa$Total_DHW_residual<-Year10_taxa$Post_bleaching_Baseline_NColonies-Year10_taxa$Post_bleaching_Baseline_NColonies_integer
Year10_taxa$Total_DHW_residual_area<-Year10_taxa$Total_DHW_residual*(((Year1$Baseline_Coral_Dia_End/2)^2)*pi)
Year10_taxa$Total_DHW_residual_cover_decline<-Year10_taxa$Total_DHW_residual_area/Site_Area

#Add residual cover decline to 75% of total scenario mortality
#baseline partial mortality also gets added in here
Year10_taxa$Baseline_cover_post_mortality<-Year10_taxa$Coral_Cover_post_colony_mortality+((DHW_mortality*0.57)-sum(Year10_taxa$Total_DHW_residual_cover_decline+x_mortality))

#calculate growth of corals that didn't die
#Site_Area from user input
#calculate total area occupied by each individual coral within the plot
Year10_taxa$Baseline_Area_Start<-Site_Area*(Year10_taxa$Baseline_cover_post_mortality/100)

Year10_taxa$Baseline_Coral_Dia_Start<-Year10_taxa$Baseline_Area_Start/Year10_taxa$Post_bleaching_Baseline_NColonies_integer

#Grow the diameter of the colonies
#coral_growth is taxon specific
Year10_taxa$Year10_Coral_Dia_End<-Baseline_Coral_Dia_Start+((coral_growth$planar_mean/100))

#calculate new species-specific area
Year10_taxa$Baseline_Area_End<-(((Baseline_Coral_Dia_End/2)^2)*pi)*Post_bleaching_Baseline_NColonies_integer

#calculate new species-specific coral cover
Year10_taxa$Baseline_Cover_End<-(Baseline_Area_End/Site_Area)*100

#Total Year_X coral cover
Year10_taxa$Total_cover<-Year10_taxa$Baseline_Cover_End + Year5_taxa$Restored_Cover_End

#Calculate gross production
Year10_taxa$GP<-Year10_taxa$Coral_Cover*TravisRates #taxon-specific calcification rates

#sum to get site-level gross production
Year10_site$GP<-sum(Year10_taxa$GP)

#Calculate microbioerosion based on available substrate
#Note that Percent_Cover here should include total calcifier cover + unconsolidated substrate
#both of which are substracted from 100 to get available, consolidated substrate for microbioerosion
Year10_substrate<-100-sum(Year10_taxa$Total_Cover)

#Proportion of available substrate * generalized Caribbean microbioerosion rate of 0.24 kg m-2 y-1 (Perry and Lange, 2019)
Micro_Year10<-(Year10_substrate/100)*0.24

#Other Bioerosion data from Bioerosion.csv dataset
#Bioerosion is specific to Habitat and Region (which are included in Baseline_cover_TEMPLATE.csv)
#NOTE that these values will not change from year to year in the model but Micro will
#This variabile should actually be calculated in the DataInput module
Site_bioerosion<-sum(Bioerosion$Parrotfish, Bioerosion$Urchins, Bioerosion$BioSponges)

#Calculate site-level net production
Year10_site$NP<-Year10_site$GP-(Site_bioerosion+Micro_Year10)

#Calculate site-level reef-accretion potential
#use porosity dataset lookup: if restoration mix >75% Acropora spp., use Acropora porosity; if restoration mix >75% massives/other use Massive porosity
#else use mixed
Year10_site$RAP<-Year10_site$NP/2.9/(1- porosity) #2.9 = CaCO3 density from Kinsey 1985, 0.6265 = regional average framework porosity from Toth et al. 2018

##########Restoration Impacts and Return-on-Investment##########

###Change in Average Reef Elevation###

#Need matrix of estimated outplant height by species or at least morphology
#??? Sara Williams can probably provide this ???#
Initial_outplant_height<-2 #cm

#For time 0
#species-specific cover should be multiplied by species-specific average outplant height
Year0_taxa$Elevation<-Year0_taxa$Restored_Cover*Initial_outplant_size
Year0_site$Elevation<-sum(Year0_taxa$Elevation)*(sum(Year0_taxa$Restored_Cover)/100)
#I think this math works

#For each time point 1, 5, and 10 years after outplanting
YearX_taxa$Elevation<-(Year(X-1)_taxa$Restored_Cover*coral_growth$Mean_extension_rate)
YearX_site$Elevation<-Year(X-1)$Elevation+(sum(YearX_taxa$Elevation)*(sum(YearX_taxa$Restored_Cover)/100))

###Ranking of Reef-Accretion Potential###
#Needs to be reported in the context of what this actually means for whether the reef is growing and how fast
Baseline_Percentile<-length(Baseline_budgets$RAP[Baseline_budgets$RAP < Baseline_RAP) / length(Baseline_budgets$RAP) * 100
YearX_Percentile<-length(Baseline_budgets$RAP[Baseline_budgets$RAP < YearX_site$RAP]) / length(Baseline_budgets$RAP) * 100

library(tidyr)
library(dplyr)
#Determine relationship between RAP increase and Ranking increase
Ranking_summary <- tibble(RAP = Baseline_budgets$RAP) %>%
  crossing(threshold = seq(-1, 1, 0.05)) %>%
  group_by(threshold) %>%
  summarise(pct_below = mean(RAP < threshold, na.rm = TRUE) * 100) %>%
  as.data.frame()

####Calculate ROI####
#????John????
