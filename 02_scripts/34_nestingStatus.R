################################################################################
# Task: Develop categories for nesting status. 
# 
# 6.25.21
# Update: 7.1.21:
# Make more bins for nesting status - base on 5-10 yr chunks
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }
  
# Read data -----
  
dat <- fread("01_data/23_ibisData/IbisData_May11.csv")

#===============================================================================
#===============================================================================
# Add values to data based on nesting status -----
  
  # Assign a value for nesting status -----
  # Based on the Western Colonial Waterbird Survey (2009-2011) -----
  
    dat01 <- dat %>% 
      mutate(nestStat = case_when(Nesting == 'Yes' & Year == 2009:2011 ~ 2, # if confirmed nesting in 2009-2011, then 2
                               Nesting == 'No' & Year == 2009:2011 ~ 1,  # if no nesting confirmed in 2009-2011, then 1
                               TRUE ~ 0))                          # if unknown nesting in, then 0


# Take out the 2009:2011 data and select the most recent year for each site -----
  
  dat02 <- dat01 %>%
    filter(!(Year == 2009:2011)) %>% 
    group_by(Latitude) %>% 
    top_n(1, Year)

# Select all of the 2011 data (which the nest status is based off of) -----
  
  dat03 <- dat01 %>% 
    filter(Year == 2009:2011)

# Combine the two datasets together and select the highest nestStat value -----
  # Anything with a 1 or 2, which could only occur for 2011 data, will be selected
  
  dat04 <- union(dat02, dat03) %>% 
    group_by(Latitude) %>% 
    top_n(1, nestStat) %>% 
    top_n(1, Year)

  fwrite(dat04, 'nestingStatus.csv')
    
    
#===============================================================================   
  # Select the most recent year for each site so that I have a 
  # single entry for each location with nesting status -----
  
    # dat02 <- dat01 %>% 
    #   group_by(Latitude) %>% 
    #   top_n(1, Year) 
  
  # test2 <- dat01 %>% 
  #   filter(Year > 2011) %>% 
  #   group_by(Latitude) %>% 
  #   slice(which.min(Year))  
  
#=============================================================================== 
#=============================================================================== 
# Bin nesting status by 5 year periods (i.e., active within the last 5 years,
# 10 years, 15 years, etc) 
  
  year5 <- 2015:2019
  year10 <- 2010:2014
  year15 <- 2005:2009
  year20 <- 2000:2004
  year30 <- 1995:1999
  year40 <- 1985:1994
  
  dat01 <- dat %>% 
    mutate(nestStat = case_when(Nesting == 'Yes' & Year %in% year5 ~ 6, # if confirmed nesting in 2009-2011, then 2
                                Nesting == 'Yes' & Year %in% year10 ~ 5,  # if no nesting confirmed in 2009-2011, then 1
                                Nesting == 'Yes' & Year %in% year15 ~ 4,
                                Nesting == 'Yes' & Year %in% year20 ~ 3,
                                Nesting == 'Yes' & Year %in% year30 ~ 2,
                                Nesting == 'Yes' & Year %in% year40 ~ 1,
                                TRUE ~ 0)) %>% 
    group_by(Latitude) %>% 
    slice(which.max(nestStat))
  
  fwrite(dat01, 'nestingStatus02.csv')

  
  
  
  
  
  
  
  