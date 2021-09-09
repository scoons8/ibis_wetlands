################################################################################
# Task: Making some summary tables of overall wetland data
# 
# Jan 11 2021
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('leaflet')
  library('ggridges')
  library('gridExtra')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }
  
# Read in data -----
  
    hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv')
  
# Color Palette -----
  #3A2139 = 'dark purple'
  #0B646E = 'ming' (dark cerulean)
  #23C9B6 = 'medium turquoise'
  #FED179 = 'orange-yellow crayola'
  #C07C6B = 'blast off bronze'
  
#===============================================================================

#  Create T1 and T2 -----
  
  hydroTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 't1',                                    # creates a column with filled with 'p1'
              term = replace(term, year >2003, 't2')) %>%     # replaces 't1' with 't2' when year is >2002
      na.omit()
  
#-------------------------------------------------------------------------------
# Find the mean wetHa for T1 and T2 for each region plus SD -----
  
  hydro01 <- hydroTerm %>% 
      group_by(ecoHydro, year, term) %>%                   # grouping
      summarise(wetHa = sum(wetHa)) %>%                    # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(term, ecoHydro) %>%                         # group again
      summarise(wetMean = mean(wetHa)) %>%                 # take the mean of summed wetHa
      spread(term, wetMean) %>%                            # spread the data so you can do some column math
      mutate(perDif = ((t1-t2)/t1)*-1,                     # calculate difference between t1 and t2 as percent
             change = (t2-t1))                             # calculate the difference in wetHa between T1 and T2            
      
# Calculate Standard deviation -----
  
  hydro02 <- hydroTerm %>% 
      group_by(ecoHydro, year, term) %>% 
      summarise(wetHa = sum(wetHa)) %>%
      ungroup() %>% 
      group_by(ecoHydro, term) %>% 
      summarise(SD = sd(wetHa)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 't1',
             SD2 = 't2')
    
# Wilcoxon Test -----
  
  hydro03 <- hydroTerm %>%
      group_by(term, ecoHydro, year) %>%                    # group by term, region, and year - function will run across these groups
      summarise(wetSum = sum(wetHa)) %>%                    # summarise the above groups by sum - the result is the total wetHa for each year for each region and term
      split(.$ecoHydro) %>%                                 # '.' shorthand for the dataframe - splits the data by region
      map(~wilcox.test(wetSum ~ term, data = .x)) %>%       # iterates whole process over each region (calculates the mean and tests for difference)
      map_df(broom::tidy, .id = 'ecoHydro')    
  
# Site Results -----
  
    sites01 <- fread('03_output/table_site_wilcox.csv')
  
    # Total Sites by region -----
    sites02 <- sites01 %>% 
      filter(ecoHydro == "Great Basin-Colorado Plateau")
  
    nrow(sites02)
  
    # Significantly declining sites -----
    sites03 <- sites02 %>% 
      subset(p.value <= 0.05 & change < 0)
  
    nrow(sites03)
   
    # Significantly increasing sites -----
    sites04 <- sites02 %>% 
      subset(p.value <= 0.05 & change > 0)
      
    nrow(sites04)
    
    # GB: total = 87; decrease = 55; increase = 0
    # MR: total = 11; decrease = 6; increase = 0
    # MD: total = 1; decrease = 0; increase = 0
    # NP: total = 15; decrease = 6; increase = 4
    # NR: total = 3; decrease = 3; increase = 0
    # PNW: total = 7; decrease = 3; increase = 0
    # SP: total = 6; decrease = 6; increase = 0
    # SR: total = 23; decrease = 12; increase = 0
    
  # make a dataframe of the fractions:
    
    ecoHydro <- c('Great Basin-Colorado Plateau', 'Mojave-Sonoran Deserts',
                  'Middle Rockies', 'Northern Plains', 'Northern Rockies',
                  'Pacific NW', 'Southern Plains', 'Southern Rockies and Basins')
    
    sigFrac <- c('55/87', '0/1', '6/11', '6/15', '3/3', '3/7', '6/6', '12/23')
    
    sites05 <- data.frame(ecoHydro, sigFrac)
  
# Join tables -----
  
  hydro04 <- hydro01 %>% 
    full_join(hydro02, by = 'ecoHydro') %>% 
    full_join(hydro03) %>% 
    left_join(sites05, by = 'ecoHydro') %>% 
    mutate(perDif = 100*perDif) %>% 
    rename('Region' = 'ecoHydro',
           '1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif',
           'Num p sig' = 'sigFrac')
  
  colOrder <-c('Region', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'Num p sig')
  
  hydro05 <- hydro04[ , colOrder]
    
# Write Table -----
  
  fwrite(hydro05, '03_output/summaryTable_wetlands.csv')
    
                
   
  
  
     
  
  
  
  