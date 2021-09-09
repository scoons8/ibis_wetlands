################################################################################
# Task: 
#
# Plot ibis numbers against wet_ha. 
#
# Notes:
# Use site-level data, not blob-level data.
#
# Jan 28 2021
################################################################################
 
# Load packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')

# Read data -----
 
  ibis <- fread('01_data/21_ibisCounts/IbisData_May11.csv')
  water <- fread('01_data/19_hydroCleaned/hydroGraSiteFin02.csv') 
  sites <- fread("01_data/09_site_names/siteNamesID.csv")
  
#===============================================================================  
  # Group and summarize hydrograph data -----
  
    hydroTerm <- water %>% 
        filter(month > 3, month < 9) %>% 
        mutate(year = as.numeric(year),
                term = 't1',                                
                term = replace(term, year >2003, 't2')) %>%
        na.omit()
      
  # Join site coordinates to water data -----
    
    # summarize data before join -----
      
      siteHydro <- hydroTerm %>% 
          group_by(idPoly, siteName, year, term, ecoHydro) %>%           
          summarise(wetHa = sum(wetHa)) %>%     
          ungroup() %>% 
          left_join(sites, by = c('idPoly', 'siteName'))
  
  # Summarize by latitude -----
  
    siteWater <- siteHydro %>% 
      group_by(siteName, Latitude, Longitude, year, ecoHydro) %>% 
      summarize(wetSum = sum(wetHa)) %>% 
      ungroup()

#=============================================================================== 
  # Clean ibis data and join -----
  
    ibis01 <- ibis %>%
      rename('siteName' = 'Site Category',
             'birdNum' = 'Num Breeding Birds',
             'year' = 'Year') %>% 
      select('siteName', 'Latitude', 'Longitude', 'birdNum', 'year' ) %>% 
      right_join(siteWater, by = c('Latitude', 'Longitude', 'year', 'siteName')) %>% 
      na.omit()
  
#===============================================================================  
  # Hydroperiod filtering
  
   siteHydro02 <- hydroTerm %>% 
          group_by(idPoly, siteName, year, term, ecoHydro, period) %>%           
          summarise(wetHa = sum(wetHa)) %>%     
          ungroup() %>% 
          left_join(sites, by = c('idPoly', 'siteName'))
  
  # Summarize by latitude -----
  
    siteWater02 <- siteHydro %>% 
      group_by(siteName, Latitude, Longitude, year, ecoHydro, period) %>% 
      summarize(wetSum = sum(wetHa)) %>% 
      ungroup()
  
  # Clean and join ibis data -----  
  
    ibis02 <- ibis %>%
        rename('siteName' = 'Site Category',
               'birdNum' = 'Num Breeding Birds',
               'year' = 'Year') %>% 
        select('siteName', 'Latitude', 'Longitude', 'birdNum', 'year') %>% 
        right_join(siteWater02, by = c('Latitude', 'Longitude', 'year', 'siteName')) %>% 
        na.omit()
  
#===============================================================================    

  # Plots -----  
  
    # ibis X water -----
  
      ggplot(ibis01, aes(x = wetSum, y = birdNum, color = ecoHydro)) +
        geom_point()
    
  # Try filtering high colony numbers out -----
  
    ibis03 <- ibis01 %>% 
      filter(birdNum <10000)
    
     ggplot(ibis02, aes(x = wetSum, y = birdNum, color = ecoHydro)) +
      geom_point()
   
  # Try filtering high colony numbers and high water out  ----
   
    ibis04 <- ibis02 %>% 
      filter(wetSum < 100000)
    
    ggplot(ibis03, aes(x = wetSum, y = birdNum, color = ecoHydro)) +
      geom_point()
     
  # ibis X semiPerm
     
    water02 <- ibis02 %>% 
      # filter(period == 'semi') %>% 
      filter(wetSum < 20000)
    
    ggplot(water02, aes(x = wetSum, y = birdNum, color = ecoHydro)) +
      geom_point()+
      ggtitle('semiPerm')
  