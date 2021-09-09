################################################################################
# Task: Combine all of the climate and irrigation variable summaries into 
# a single summary table.
#
# March 8 2021
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
# Read in all of the summary tables -----
  
  aet <- fread('03_output/05_Tables/aetTable.csv')
  pr <- fread('03_output/05_Tables/prTable.csv')
  swe <- fread('03_output/05_Tables/sweTable.csv')  
  ro <- fread('03_output/05_Tables/roTable.csv')  
  tmin <- fread('03_output/05_Tables/tminTable.csv')  
  irr <- fread('03_output/05_Tables/irrTable.csv')

# Bind tables -----
  
  summary <- rbind(aet, pr, swe, ro, tmin, irr) %>% 
    mutate
  
# Save table -----
  
  fwrite(summary, '03_output/RFvarsSummary.csv')

# Make some changes - make the %Dif column actuall percentages -----
  
  summary01 <- fread('03_output/05_Tables/RFvarsSummary.csv')
  
  summary02 <- summary01 %>% 
    rename("PerDif" = "% Dif") %>% 
    mutate(PerDif = PerDif * 100)

  fwrite(summary02, '03_output/RFvarsSummary.csv')  
  