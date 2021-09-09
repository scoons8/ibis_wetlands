################################################################################
# Task: Making summary table of irrigation data
# 
# Jan 13 2021
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('gridExtra')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }

#===============================================================================
# Irrigation  summarized by HUC4-----
  
  # Read in data -----
  
    RF<-fread('01_data/11_RF_vars_clean/RFvars05.csv')
  
  # Reformat -----
  
    RF01 <- RF %>%
      na.omit() %>% 
      filter(year > 1987) %>% 
      select(siteName, ecoHydro, HUC4, irr_ha, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
# Test by HUC4 -----

  RF02 <- RF01 %>% 
    select(HUC4, irr_ha, year, term) %>% 
    distinct()
  
  irrWilcoxTest <- RF02 %>%
    group_by(term, HUC4, year) %>%                   
    # summarise(wetSum = sum(wetHa)) %>%                   
    split(.$HUC4) %>%                                 
    map(~wilcox.test(irr_ha ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'HUC4')
    
# Calculate percent difference -----

  irr01 <- RF02 %>% 
    group_by(term, HUC4) %>% 
    summarise(irrMean = mean(irr_ha)) %>% 
    spread(term, irrMean) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1),
           HUC4 = as.character(HUC4))
    
# Calculate Standard deviation -----
  
  irr02 <- RF02 %>% 
      group_by(HUC4, year, term) %>% 
      group_by(HUC4, term) %>% 
      summarise(SD = sd(irr_ha)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 't1',
             SD2 = 't2') %>% 
      mutate(HUC4 = as.character(HUC4))
    
# Join tables -----
  
  irr03 <- irr01 %>% 
    full_join(irr02, by = 'HUC4') %>% 
    full_join(irrWilcoxTest) %>% 
    rename('1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif')
  
  colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  irr04 <- irr03[ , colOrder]
    
# Write Table -----
  
  fwrite(irr04, '03_output/summaryTable_irrHa_HUC4.csv')
  
#===============================================================================
# Irrigation  summarized by ecoRegion -----
  
# Test by HUC4 -----

  RF03 <- RF01 %>% 
    # mutate(ecoHydro = case_when(HUC4 == 1003 ~ 'Northern Plains',
    #                             HUC4 == 1007 ~ 'Northern Plains', 
    #                             HUC4 == 1019 ~ 'Southern Plains',
    #                             HUC4 == 1102 ~ 'Southern Plains',
    #                             HUC4 == 1601 ~ 'Great Basin-Colorado Plateau',
    #                             HUC4 == 1701 ~ 'Northern Rockies',
    #                             HUC4 == 1704 ~ 'Great Basin-Colorado Plateau',
    #                             TRUE ~ ecoHydro)) %>% 
    select(HUC4, ecoHydro, irr_ha, year, term) %>% 
    distinct()
  
  irrWilcoxTest <- RF03 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(irrSum = sum(irr_ha)) %>%
    split(.$ecoHydro) %>%                                 
    map(~wilcox.test(irrSum ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'ecoHydro')
    
# Calculate percent difference -----

  RF04 <- RF03 %>% 
    group_by(term, ecoHydro, year) %>%                   
    summarise(irrSum = sum(irr_ha)) %>%
    group_by(term, ecoHydro) %>% 
    summarise(irrMean = mean(irrSum)) %>% 
    spread(term, irrMean) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1))
    
# Calculate Standard deviation -----
  
  RF05 <- RF03 %>% 
    group_by(term, ecoHydro, year) %>%                   
    summarise(irrSum = sum(irr_ha)) %>%
    group_by(term, ecoHydro) %>% 
      summarise(SD = sd(irrSum)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 't1',
             SD2 = 't2')
    
# Join tables -----
  
  RF06 <- RF04 %>% 
    full_join(RF05) %>% 
    full_join(irrWilcoxTest) %>% 
    rename('1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif')
  
  colOrder <-c('ecoHydro', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  RF07 <- RF06[ , colOrder]
    
# Write Table -----
  
  fwrite(irr04, '03_output/summaryTable_irrHa_HUC4.csv')
  
  
  
            
    