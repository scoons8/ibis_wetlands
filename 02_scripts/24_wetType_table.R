################################################################################
# Task: Calculate area of polygons by wetType field. Also create table showing
#       sample of raw data used for analysis.
# 
# Mar 01 2021
# Update: June 16 2021
# I am remaking the wet type table using the wet hectares and the footprint 
# hectares from QGIS
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }
  
# Read in data -----
  
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin01.csv')
  
#===============================================================================
# Find the average wetHa per wetland type -----
  
  hydroGra <- hydroGra %>% 
      na.omit()
  
  typeWet <- hydroGra %>% 
      group_by(wetType, year) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      mutate(year = as.numeric(year)) %>%
      summarise(Avg_wetHa = mean(wetHa))
  
  hydroper <- hydroGra %>% 
    group_by(period, year) %>% 
    summarise(wetHa = sum(wetHa)) %>% 
    mutate(year = as.numeric(year)) %>%
    summarise(Avg_wetHa = mean(wetHa))
  
  owner <- hydroGra %>% 
    group_by(ownAg, year) %>% 
    summarise(wetHa = sum(wetHa)) %>% 
    mutate(year = as.numeric(year)) %>%
    summarise(Avg_wetHa = mean(wetHa))
  
# join together and export -----
  
  totals <- typeWet %>% 
    full_join(hydroper, by = "Avg_wetHa") %>% 
    full_join(owner, by = "Avg_wetHa")
  
  fwrite(totals, "05_Tables/wetAttribute_summary.csv")
  
################################################################################  
# Read in data -----
  
  polyArea <- fread('01_data/22_wetPoly_area/wetPoly_area_meters.csv')
  
#===============================================================================   
# Calculate the polygon area (hectares) by wetType -----
  
  # Group by 'wetType' and sum -----
  
    polyArea02 <- polyArea %>% 
      filter(wetType != '') %>% 
      filter(wetType != 'Ag') %>% 
      group_by(wetType) %>% 
      summarize(areaTot = sum(Area_m)/10000) # sum and convert to hectares
  
  # Make descriptions -----
  
    def <- c('Reservoir: Large bodies of water including lakes and reservoirs.',
             'Riverine: Riperian and riverine systems.',
             'Natural wetland systems.',
             'Wet Agriculture: Wetlands associated with flood irrigated agriculture,
                including those found in riparian floodplains and hay meadows.',
             'Managed Wetlands: Wetlands managed specifically for wildlife, 
                including public refuges.')
  
  # Bind descriptions to original dataframe -----
  
    polyArea03 <- cbind(polyArea02, def)
  
  # Reorder columns -----
  
    polyArea04 <- polyArea03[c(1, 3, 2)]
  
  # Change column names -----
  
    colnames(polyArea04) <- c('Wetland Type', 'Description', 'Total Area (Hectares)')
  
  # Write table -----
  
    fwrite(polyArea04, '03_output/wetType_table.csv')

#===============================================================================
# Create table showing sample of raw data: 
  
  # Read in data -----
  
    hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin01.csv')
  
  # Sample rows -----
  
    hydroSample <- hydroGra[sample(nrow(hydroGra), 10), ]
  
  # Save table -----
  
    fwrite(hydroSample, '03_output/rawDataSample_table.csv')
  

  
    