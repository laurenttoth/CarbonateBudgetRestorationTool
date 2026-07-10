#Carbonate Budget Uncertainties#

#Here's a list of carbonate budget uncertainties I think we should consider propagating

###1.###
#Species-specific calcification rates in TravisRates.csv as 50%
#with Upper bound = 75 percentile and Lower bound 25 percentile of MonteCarlo simulation
#these uncertainties are slightly asymmetrical, not sure how we want to handle that
#these taxon-level uncertainties need to be combined to get uncertainty on site-level gross production

###2.###
#In Bioerosion.csv, each of the Habitat and Subregion-specific AVE rates have an associated STDEV
#These uncertainties will be applied at the site level

#ReefBudget doesn't report an uncertainty for Microbioerosion

###3.###
#Mortality uncertainties
#Since we're using partial mortality rate based on mean colony size from Browne et al.
#we could conservatively apply difference between that rate and the highest rate as uncertainty
#branching=0.0418	other=0.0299

#SD of mortality rates from Mote for Year 1 Restored community

#DHW-related declines:
#can likely get taxon-level uncertainties from Alice
#for simplified estimate can use reported 95% CIs from Dry Tortugas and Florida Keys
# 95% CI: ~ +/-0.74 (this is from Dry Tortugas where CI is broader)
#note these uncertainties are not actually symmetrical

###4.###
#Growth rates:

#Uncertainty in colony diameters used to estimate growth NCRMP_colony_dia_Florida
#has lower (25%) and upper (75%) quantiles

#growth_rates_ReefBudget_NCRMP.csv has lower (25%) and upper (75%) quantiles for planar growth
#that dataset also has SD for upward growth rates


