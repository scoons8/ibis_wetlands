################################################################################
# Task: Recreate the annual hydroperiod table that is given by GEE using the 
# other tables downloaded from the GEE wetland model.
# This code will be relevant to years where one month needs an extended mean
#
# Sept 16 2020
################################################################################

setwd("/Users/sc148852/Box/R/ibisSites/rubyLakeBlob")

# Packages -----

library("tidyverse")
library("dplyr")
library('readr')
library('ggplot2')

# Read in data -----

  wetAreaFin <- read_csv("wetAreaFinal.csv")
  hydroGraFin <- read_csv("hydroGraFinal.csv")
  hydroPerFin <- read_csv("hydroPerFinal.csv")

# Make the same data that is in annual table using other tables 
  
  hydroGraFin01 <- hydroGraFin %>% 
    group_by(idPoly, siteName, ownAg, wetType, year, period2) %>% 
    summarise(wetHa = sum(wetHa)) %>% 
    ungroup()
