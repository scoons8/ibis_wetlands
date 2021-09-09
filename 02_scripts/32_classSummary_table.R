################################################################################
# Task: Make Data summary tables for ownAg, wetType, and Period for each region
# 
# Mar 15 2020
################################################################################
# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('leaflet')
  # library('sf')
  # library('sp')
  library('ggridges')
  library('gridExtra')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }

#-------------------------------------------------------------------------------
# Read in data -----
  
  ownAg <- fread('03_output/05_Tables/01_WilcoxTables/table_ownAg_region_wilcox.csv')
  wetType <- fread('03_output/05_Tables/01_WilcoxTables/table_wetType_region_wilcox.csv')
  hydroPer <- fread('03_output/05_Tables/01_WilcoxTables/table_hydroperiod_region_wilcox.csv')
  
# Fix col names to make join easier -----
  
  colnames(ownAg) <- c('Region', 'Attribute', 'T1', 'SD1', 'T2', 'SD2', 'Change', 'perDif', "p.value")
  colnames(wetType) <- c('Region', 'Attribute', 'T1', 'SD1', 'T2', 'SD2', 'Change', 'perDif', "p.value")
  colnames(hydroPer) <- c('Region', 'Attribute', 'T1', 'SD1', 'T2', 'SD2', 'Change', 'perDif', "p.value")
  
  allWat <- ownAg %>%
    full_join(wetType) %>% 
    full_join(hydroPer)
  
# Subset by region -----
  
  gb <- subset(allWat, Region == 'Great Basin-Colorado Plateau') %>% 
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    # mutate_if(is.numeric, ~round(.,0))
  fwrite(gb, '03_output/05_Tables/05_RegionAttributes/table_gb_types.csv')
  
  mr <- subset(allWat, Region == 'Middle Rockies') %>% 
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    # mutate_if(is.numeric, ~round(.,0))
  fwrite(mr, '03_output/05_Tables/05_RegionAttributes/table_mr_types.csv')
  
  md <- subset(allWat, Region == 'Mojave-Sonoran Deserts') %>% 
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    #mutate_if(is.numeric, ~round(.,0))
  fwrite(md, '03_output/05_Tables/05_RegionAttributes/table_md_types.csv')
  
  np <- subset(allWat, Region == 'Northern Plains') %>%
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    #mutate_if(is.numeric, ~round(.,0))
  fwrite(np, '03_output/05_Tables/05_RegionAttributes/table_np_types.csv')
  
  nr <- subset(allWat, Region == 'Northern Rockies') %>%
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    #mutate_if(is.numeric, ~round(.,0))
  fwrite(nr, '03_output/05_Tables/05_RegionAttributes/table_nr_types.csv')
  
  pnw <- subset(allWat, Region == 'Pacific NW') %>% 
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    #mutate_if(is.numeric, ~round(.,0))
  fwrite(pnw, '03_output/05_Tables/05_RegionAttributes/table_pnw_types.csv')
  
  sp <- subset(allWat, Region == 'Southern Plains') %>% 
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) %>% 
   #mutate_if(is.numeric, ~round(.,0))
  fwrite(sp, '03_output/05_Tables/05_RegionAttributes/table_sp_types.csv')
  
  sr <- subset(allWat, Region == 'Southern Rockies and Basins') %>% 
    select(Region, Attribute, T1, SD1, T2, SD2, Change, perDif, p.value) #%>% 
    # mutate(perDif = 100*(perDif)) #%>% 
    #mutate_if(is.numeric, ~round(.,0))
  fwrite(sr, '03_output/05_Tables/05_RegionAttributes/table_sr_types.csv')
    
  