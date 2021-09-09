#===============================================================================
# Task: Proportion of private/public for each hydroperiod type
#
# Date: 7/15/2021
#===============================================================================

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('ggridges')
  library('gridExtra')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }
  
# Read in data -----
  
  # Water Data -----
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv')
 
  # IDpoly's linked to Blob ID's
  polyBlob <-fread("01_data/24_siteBlobIDs/polyBlobID.csv")
  
  # Get rid of NA's and blanks -----
  # polyBlob <- polyBlob %>% 
  #   na.omit()
  
  # poly <- polyBlob[!(is.na(polyBlob$state) | polyBlob$state == ""), ]
  
#===============================================================================
# Delete repetitive columns in the polyBlob data -----
  
  polyBlob2 <- polyBlob[ , c(2, 6, 7)] #%>% 
   # na.omit()
  
# Summarize the water data and join blob ID info -----
  
  hydroTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 'T1',                                    
              term = replace(term, year >2003, 'T2')) %>%
      na.omit()
  
  stateHydro <- hydroTerm %>% 
        left_join(polyBlob2, by = c('idPoly'))
  
# Find the average wetHa for each hydroperiod by ownership for each state -----
  
  hydro01 <-stateHydro %>% 
    group_by(state, ownAg, period, month, year, term) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(state, ownAg, period, year, term) %>% 
    summarise(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(state, ownAg, period, term) %>% 
    summarise(wetMean = mean(wetMean))

# What is the breakdown of ownership of hydroperiod for each state for T2? -----
  
  hydro02 <- hydro01 %>% 
    filter(term == 'T2' & period == "semi") %>% 
    select(!term)
  
  hydro03 <- hydro01 %>% 
    filter(term == 'T2' & period != "semi") %>% 
    group_by(state, ownAg) %>% 
    summarise(wetMean = sum(wetMean),
              period = "temp/seas") %>% 
    full_join(hydro02, by= c("state", "ownAg", "period", "wetMean"))
  
  fwrite(hydro03, "03_output/stateOwnAg_hydroper.csv")
  
#===============================================================================
# Make a four panel plot depicting ownership for each hydroperiod + overall -----
  
  hydro04 <- hydro01 %>% 
    filter(term == 'T2') %>% 
    select(!term) %>% 
    spread(ownAg, wetMean) %>% 
    mutate(PercentPublic = (Public/(Public+Private))*100) %>% 
    mutate(period  = case_when(str_detect(period, 'temp') ~ 'Temporary',
                             str_detect(period, 'seasonal') ~ 'Seasonal',
                             str_detect(period, 'semi') ~ 'Semi-permanent',
                             TRUE ~ 'OTHER'))
  
  hydro05 <- stateHydro %>% 
    group_by(state, ownAg, month, year, term) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(state, ownAg, year, term) %>% 
    summarise(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(state, ownAg, term) %>% 
    summarize(wetMean = mean(wetMean)) %>% 
    filter(term == 'T2') %>% 
    select(!term) %>% 
    spread(ownAg, wetMean) %>% 
    mutate(PercentPublic = (Public/(Public+Private))*100) %>% 
    mutate(period = "Overall") %>% 
    full_join(hydro04, by = c("state", "Private", "Public", "PercentPublic", "period"))
  
  hydro05$period_f <- factor(hydro05$period, levels = c("Overall", "Temporary", "Seasonal", "Semi-permanent"))
    
  
  ggplot(hydro05, aes(x = state, y = PercentPublic, fill = state)) +  
      geom_bar(stat = 'identity') +
      scale_fill_manual(values=c("#96C5B0", "#66806D", "#343330", "#45462A","#7E5920",
                                 "#DC851F", "#FFA737", "#ECBB66")) +  # fill color for box plots ("#23C9B6", "#fed179")
      facet_wrap(. ~ period_f, ncol = 2) +          # creates plot for each region
      # scale_y_continuous(labels = thousands) +
      xlab("State") +
      ylab("Percent Public") +
      ylim(0, 100) +
      theme_classic() +
      theme(legend.position = "none")
      # theme(legend.position = "bottom", legend.title = element_blank())

  ggsave("ownAgProportions01.png", plot = last_plot(), width = 7, height = 6, units = "in", device='png', dpi=300)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  