################################################################################
# Task: Combine all 32 wetArea (and hydrograph and annualHydroperiod - just 
# change the folder name) .csv's for each ibis site (change folder name)
# For the sites using zones (malheur and saltLake)
# Nov 2020
################################################################################

# Load Packages -----

  library('plyr')
  library("tidyverse")
  library('readr')
  options(scipen=999)

# Set Working directory -----

  setwd("/Users/sc148852/Box/R/ibisSites/saltLakeBlob") # <-- change site folder here

# Define folder -----

  # folder = 'wetArea'
  # folder = 'wetAreaExtMar'
  # folder = 'wetAreaExtApr'
  # folder = 'hydroGra'
  folder = 'hydroGraExtMar'
  # folder = 'hydroGraExtApr' # <-- change .csv folder here (ie wetArea, hydroPer, etc)
  
# Import all csv files in 'folder' and bind rows using dplyr and sapply

  files <- list.files(path = folder, pattern = "*.csv", full.names = T)
  wetCompile <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

# export newly merged csv file

  wetCompile <- tbl_df(wetCompile)
  write.csv(wetCompile, file = paste(folder,'.csv', sep =""))

# or formatt as needed using dplyr then export...
# wetCompile <- as_tibble(wetCompile) %>%
#   select('02FEB':'wetId') %>% 
#   mutate(across(c('02FEB':'11NOV'))*2.47105) # convert all month columns to acres from hect
#  
# write.csv(wetCompile, file = paste(folder,'.csv', sep =""))