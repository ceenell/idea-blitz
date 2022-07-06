## Exploration for a cluster bubble chart data visualization


# prelims -----------------------------------------------------------------

library(tidyverse)
library(readxl)


## Objectives:
### Understand community data
### look at differences in magnitude and composition through time

## Inspiration
### fact sheet - https://pubs.usgs.gov/fs/2022/3011/fs20223011.pdf

# data --------------------------------------------------------------------

# data provided by IRB team
list.files('in/NGWOS Ecology Data KSummers')

# toxin data
toxins <- read_excel('in/NGWOS Ecology Data KSummers/CyanoToxinResults.2021.xlsx')

# species data
list.files('in/NGWOS Ecology Data KSummers/Species Identification and Enumeration/')

# includes richness, shannon, simpson, and evenness across algae concentration, total area, volume, biovolume, biomass concentration 
# first sheet seems most valuable but also data for diatoms in 2
sp_indices <- read_excel('in/NGWOS Ecology Data KSummers/Species Identification and Enumeration/Indices_Complete_Set_392_01202022.xls')

# sheet 1 = total concentration calculations, some sampling metadata
# sheet 2 = concetrations by divisions in each sample
# sheet 4 = class
# subsequent sheets break down by different taxonomic levels all the way to species
# 'environmental_tolerance' sheet shows all taxonomic groups with coloniality traits, structure, ph spectrum, nutrient spectrum and other good stuff
sp_sums <- read_excel('in/NGWOS Ecology Data KSummers/Species Identification and Enumeration/Pre_Sums_392_1_Algae_01202022.xls')

## same data but in a single sheet with ids to various levels of taxonomic resolution
sp_raw <- read_excel('in/NGWOS Ecology Data KSummers/Species Identification and Enumeration/rawdata_392_1_algae_01202022.xls')


# taste and odor - pdf for each timestep
# need to scrape
list.files('in/NGWOS Ecology Data KSummers/Taste and Odor Compounds')
# contains avg concentration of sample in table 1, different mumber of rows per file but same cols
# MIB and Geosmin stand out 



# notes -------------------------------------------------------------------

# questions
## what do the sample ID's mean in the TO data?

# look deeper
## what are MIB and Geosmin?