################################################################################
# Task: clean and format climate and irrigation data for randomForest
# 
# Dec 31 2020
#
# Update 1/13/2021: I switched from the annual data to the seasonal data and 
# the code below reflects these changes.
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('gridExtra')

# Data -----

  clim <- fread('01_data/10_climate_vars_raw/climateVars.csv')
  tmin <- fread('01_data/10_climate_vars_raw/tmin.csv')
  irr <-  fread('01_data/12_irrMap_raw/irrMap.csv')
  wet <- fread('01_data/11_RF_vars_clean/ibisSeas02.csv')
  #wet <- fread('01_data/16_wetAnnual_clean/wetAnnual.csv')
  
# Clean and format data -----
  
  irrHa <- irr %>% 
    gather(year, irr_ha, '1986':'2019') %>% 
    dplyr::select(HUC4, irr_ha, year) %>% 
    mutate(year = as.numeric(year))
  
  tminOnly <- tmin %>% 
    filter(year > '1979') %>% 
    filter(year < '2019') %>% 
    rename(tmin = mean) %>% 
    dplyr::select(HUC4, tmin, year) 
      
  climVars <- clim %>%
    filter(year > '1979') %>%                                                   # remove data for 1979 - I don't need data for this year 
    filter(year < '2019') %>% 
    dplyr::select('HUC4', 'NAME', 'aet', 'pr', 'ro', 'swe', 'year') %>% 
    full_join(tminOnly, by = c('HUC4', 'year')) %>%
    full_join(irrHa, by = c('HUC4', 'year'))
  
# Join climate vars to wetland variables
  
  RFvars <- wet %>% 
    full_join(climVars, by = c('year', 'HUC4')) %>% 
    dplyr::select('siteName','ecoHydro', 'HUC4', 'year', 'wetSum',               #'Public', 'Private', 
           'aet', 'pr', 'ro', 'swe', 'tmin', 'irr_ha')
  
# Write data -----
  
  fwrite(RFvars, '01_data/11_RF_vars_clean/RFvars04.csv')
  
#===============================================================================  
# Load in hydroperiod data + extract hydroperiods + clean and format for RF
  
# Load data -----
  
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv') 
  RFvars <- fread('01_data/11_RF_vars_clean/RFvars04.csv')
  
# Extract hydroperiods and reshape data -----
  
  hydroperiods <- hydroGra %>% 
    dplyr::select(siteName, year, month, period, HUC4, wetHa) %>% 
    group_by(HUC4, month, year, period) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(HUC4, year, period) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    spread(period, wetMean)
  
# Join hydroperiod data to other variables -----
  
  RFvarsPeriod <- RFvars %>% 
    full_join(hydroperiods, by = c('HUC4', 'year'))
  
# Write data -----
  
  fwrite(RFvarsPeriod, '01_data/11_RF_vars_clean/RFvars06.csv')
 
#===============================================================================  
#===============================================================================   
#=============================================================================== 
  
# I'm trying to understand what changes I made to the dataset are causing the
# changes in the RF analysis. The below code is for me to try to get back to the
# original data that I used for the first round of anaylsis (before I deleted all
# the duplicates) to see where the differences are. 
  
      # Data -----

  clim <- fread('01_data/10_climate_vars_raw/climateVars.csv')
  tmin <- fread('01_data/10_climate_vars_raw/tmin.csv')
  irr <-  fread('01_data/12_irrMap_raw/irrMap.csv')
  wet <- fread('01_data/11_RF_vars_clean/ibisSeas02.csv')
  #wet <- fread('01_data/16_wetAnnual_clean/wetAnnual.csv')
  
# Clean and format data -----
  
  irrHa <- irr %>% 
    gather(year, irr_ha, '1986':'2019') %>% 
    dplyr::select(HUC4, irr_ha, year) %>% 
    mutate(year = as.numeric(year))
  
  tminOnly <- tmin %>% 
    filter(year > '1979') %>% 
    # filter(year < '2019') %>% 
    rename(tmin = mean) %>% 
    dplyr::select(HUC4, tmin, year) 
      
  climVars <- clim %>%
    filter(year > '1979') %>%                                                   # remove data for 1979 - I don't need data for this year 
    # filter(year < '2019') %>% 
    dplyr::select('HUC4', 'NAME', 'aet', 'pr', 'ro', 'swe', 'year') %>% 
    full_join(tminOnly, by = c('HUC4', 'year')) %>%
    full_join(irrHa, by = c('HUC4', 'year'))
  
# Join climate vars to wetland variables
  
  RFvars <- wet %>% 
    full_join(climVars, by = c('year', 'HUC4')) %>% 
    dplyr::select('siteName','ecoHydro', 'HUC4', 'year', 'wetSum',               #'Public', 'Private', 
           'aet', 'pr', 'ro', 'swe', 'tmin', 'irr_ha')
  
# Write data -----
  
  fwrite(RFvars, '01_data/11_RF_vars_clean/RFvarstest01.csv')
  

#===============================================================================
# Load in hydroperiod data + extract hydroperiods + clean and format for RF
  
# Load data -----
  
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin01.csv') 
  RFvars <- fread('01_data/11_RF_vars_clean/RFvars04.csv')
  
# Extract hydroperiods and reshape data -----
  
  hydroperiods <- hydroGra %>% 
    dplyr::select(siteName, year, month, period, HUC4, wetHa) %>% 
    group_by(HUC4, year, period) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    spread(period, wetSum)
  
# Join hydroperiod data to other variables -----
  
  RFvarsPeriod <- RFvars %>% 
    full_join(hydroperiods, by = c('HUC4', 'year'))
  
# Write data -----
  
  fwrite(RFvarsPeriod, '01_data/11_RF_vars_clean/RFvarstest02.csv')
  
#===============================================================================  
#===============================================================================  


  hydroTest <- fread('01_data/08_final_hydro_data/hydroGraFin07.csv')

  wetTest <- hydroTest %>% 
    group_by(siteName, ecoHydro, HUC4, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup()
  
  
  # Clean and format data -----
  
  irrHa <- irr %>% 
    gather(year, irr_ha, '1986':'2019') %>% 
    dplyr::select(HUC4, irr_ha, year) %>% 
    mutate(year = as.numeric(year))
  
  tminOnly <- tmin %>% 
    filter(year > '1979') %>% 
    filter(year < '2019') %>%
    rename(tmin = mean) %>% 
    dplyr::select(HUC4, tmin, year) 
      
  climVars <- clim %>%
    filter(year > '1979') %>%                                                   # remove data for 1979 - I don't need data for this year 
    filter(year < '2019') %>%
    dplyr::select('HUC4', 'NAME', 'aet', 'pr', 'ro', 'swe', 'year') %>% 
    full_join(tminOnly, by = c('HUC4', 'year')) %>%
    full_join(irrHa, by = c('HUC4', 'year'))
  
# Join climate vars to wetland variables
  
  RFvars <- wetTest %>% 
    full_join(climVars, by = c('year', 'HUC4')) %>% 
    dplyr::select('siteName','ecoHydro', 'HUC4', 'year', 'wetSum',               #'Public', 'Private', 
           'aet', 'pr', 'ro', 'swe', 'tmin', 'irr_ha') 

# hydroperiods -----  
 hydroperiods <- hydroTest %>% 
    dplyr::select(siteName, year, month, period, HUC4, wetHa) %>% 
    group_by(HUC4, year, period) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    spread(period, wetSum)
  
# Join hydroperiod data to other variables -----
  
  RFvarsPeriod <- RFvars %>% 
    full_join(hydroperiods, by = c('HUC4', 'year'))
  
# Write data -----
  
  fwrite(RFvarsPeriod, '01_data/11_RF_vars_clean/RFvarstest03.csv') 
  
  
  
  
  
  
  
  
  
  
  
  
  