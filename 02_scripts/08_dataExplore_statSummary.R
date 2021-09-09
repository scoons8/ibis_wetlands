################################################################################
# Task: Data Exploration - statistical summaries
# 
# Nov 19 2020
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

# Read in data -----

  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin01.csv')

#===============================================================================
# Test for serial autocorrelation (temporal autocorrelation) by region -----
  
  # Great Basin -----
  
    hydro01 <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 't1',                                    # creates a column with filled with 'p1'
              term = replace(term, year >2003, 't2')) %>%     # replaces 't1' with 't2' when year is >2002
      na.omit()
  
    hydro02 <- hydro01 %>%
      group_by(term, ecoHydro, year) %>%                 # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                 # summarise the grouped columns by summing wetHa
      ungroup() 
    
    GBhydro <- hydro02 %>% 
      filter(ecoHydro == "Great Basin-Colorado Plateau")
    gbReg <- lm(wetSum ~ year, data = GBhydro)
    gbRes <- resid(gbReg)
    plot(GBhydro$year, gbRes)
    qqnorm(gbRes)
    plot(density(gbRes))
    plot(GBhydro$wetSum, gbRes)
    res<-cor.test(GBhydro$year, GBhydro$wetSum, method="kendall")
    res
    
    SRhydro <- hydro02 %>% 
      filter(ecoHydro == "Southern Rockies and Basins")
    srReg <- lm(wetSum ~ year, data = SRhydro)
    srRes <- resid(srReg)
    plot(SRhydro$year, srRes)
    qqnorm(srRes)
    plot(density(srRes))
    resSR<-cor.test(SRhydro$year, SRhydro$wetSum, method="kendall")
    resSR
  
    NPhydro <- hydro02 %>% 
      filter(ecoHydro == "Northern Plains")
    npReg <- lm(wetSum ~ year, data = NPhydro)
    npRes <- resid(npReg)
    plot(NPhydro$year, npRes)
    
    SPhydro <- hydro02 %>% 
      filter(ecoHydro == "Southern Plains")
    spReg <- lm(wetSum ~ year, data = SPhydro)
    spRes <- resid(spReg)
    plot(SPhydro$year, spRes) 
   
    PNWhydro <- hydro02 %>% 
      filter(ecoHydro == "Pacific NW")
    pnwReg <- lm(wetSum ~ year, data = PNWhydro)
    pnwRes <- resid(pnwReg)
    plot(PNWhydro$year, pnwRes) 
    resPNW<-cor.test(PNWhydro$year, PNWhydro$wetSum, method="kendall")
    resPNW
    
    abline(0, 0)

#===============================================================================
# Find mean wetHa in each region
    
  blob <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 't1',                                    
              term = replace(term, year >2003, 't2')) %>% 
      na.omit()
    
  blob2 <- blob %>%
    group_by(term, ecoHydro, year) %>%                 
      summarise(wetSum = sum(wetHa)) %>%                 
      ungroup() %>% 
    group_by(ecoHydro) %>% 
    summarize(wetMean = mean(wetSum))
  