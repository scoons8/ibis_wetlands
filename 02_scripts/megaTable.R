################################################################################
# Task: Build MEGA table of all wetland data 
# Join HUC4 and ecohydroregion data and clean up the table
# Nov 12 2020
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

# Working directory -----

  setwd("/Users/sc148852/Box/R/ibisSites/tablesFinal") # change site folder here

# Read in data -----

  # ecoHUC <- read_csv("HUCsites.csv") # ecohydroregion and HUC4 data
  # wetArea <- fread("wetAreaAll.csv") # wet area
  # hydroGra <- fread("hydroGraAll.csv") # hydrograph
  # hydroPer <- fread("hydroPerAll.csv") # hydroperiod
  
  # Look at data -----
  
    # str(wetArea)
    # nrow(wetArea)
    # head(wetArea)
    # 
    # head(hydroGra)
    # str(hydroGra)
    # nrow(hydroGra)
    # 
    # head(hydroPer)  

# Join ecohydroregion/huc data to water tables -----
  
  # wetArea01 <- wetArea %>% 
  #   select('idPoly','siteName','ownAg','wetType', 'year', 'month', 'wetHa') %>% 
  #   left_join(ecoHUC, by = "idPoly")
  # 
  # hydroGra01 <- hydroGra %>% 
  #    select('idPoly','siteName','ownAg','wetType', 'year', 'month', 'period', 'wetHa') %>% 
  #    left_join(ecoHUC, by = "idPoly")
  
  # hydroPer01 <- hydroPer %>%
  #   select('idPoly','siteName','ownAg','wetType', 'year', 'period', 'wetHa') %>%
  #   left_join(ecoHUC, by = "idPoly")

# Write joined .csv files -----  
  
  # fwrite(wetArea01, "wetAreaEco.csv")  
  # fwrite(hydroGra01, "hydroGraEco.csv")

#-------------------------------------------------------------------------------
# More data cleaning -----
# Fix spelling errors and fix ownAg errors -----  
  
  # Read in data -----
  
    wetArea <- read_csv("wetAreaEco.csv")
    hydroGra <- read_csv("hydroGraEco.csv")
  
  # Look at data -----
    
    str(wetArea)
    nrow(wetArea)
    head(wetArea)

    head(hydroGra)
    str(hydroGra)
    nrow(hydroGra)
  
# Fix spelling errors -----
    
  # How many of these incorrect entries are there? -----
    
    table(wetArea$ownAg)
    table(hydroGra$ownAg)
    
    # How many different 'idPoly' are there? -----
    
      AreaWet <- filter(wetArea, ownAg == 'wet')
      Hydrowet <- filter(hydroGra, ownAg == 'wet')
      
      table(AreaWet$idPoly)
      table(Hydrowet$idPoly)
    
    # Looks like there's only two polygons that have been mislabeled and have
    # been iterated over time to get so many entries. I cross-checked the 'idPoly
    # with QGIS and they aren't essential polygons (they are in the same location)
    # so I think it's okay to delete them.
  
  # Filter out the mislabeled entries and fix the spelling errors -----
    
    wetArea01 <- wetArea %>%        # using the df wetArea
      filter(ownAg != 'wet') %>%    # keep everything in df except for the rows where ownAg = 'wet'
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public',  # When the string 'Pub' is detected in the column 'ownAg', replace it with 'Public'
                             str_detect(ownAg, 'Pri') ~ 'Private',
                             TRUE ~ 'OTHER'))
    
    hydroGra01 <- hydroGra %>%
      filter(ownAg != 'wet') %>%
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public', 
                             str_detect(ownAg, 'Pri') ~ 'Private',
                             TRUE ~ 'OTHER'))
    # check your work -----
      unique(wetArea01$ownAg)
      unique(hydroGra01$ownAg)
      # table(wetArea$ownAg)
    
# Write the spelling-corrected .csv's -----
      
    fwrite(wetArea01, "wetAreaSp.csv")
    fwrite(hydroGra01, "hydroGraSp.csv")

#-------------------------------------------------------------------------------
# Fix additional spelling errors, change 'wetType' values 
# based on 'idPoly', remove 'Ag' -----
    
  # Read in data -----
  
    wetArea <- fread("wetAreaSp.csv")
    hydroGra <- fread("hydroGraSp.csv")
    
  # Replace the 'wetType' value based on 'idPoly'
    
    # List of 'idPoly's to change from 'wetMan' to 'res' -----
   
      polys <- c(73121, 73523, 73528, 73528, 73529, 73522, 73124, 73119,  # Lake Helena WMA
                 73388, 73118, 73527, 73340, 73526,                       # Lake Helena WMA
                 63232, 63000, 63002, 63003, 63049, 63229,                # Freezeout Lake
                 63533,                                                   # Grass Lake NWR
                 90390,                                                   # Red Rock Lakes NWR
                 90846, 90845,                                            # Spidel WPA
                 # 8153, 8154, 1535,                                        # Tule and Klamath Lake
                 # 13680, 13758,                                            # Fairchild Swamp
                 99502)                                                   # Canvasback Club                                    
               
    # Remove 'wetType' == 'Ag'. 
    # Change 'wetType' to 'res' based on list of 'idPoly' values.
    # Change 'Middel Rockies' to 'Middle Rockies' -----
    
    wetArea01 <- wetArea %>% 
        filter(wetType != 'Ag') %>% 
        mutate(wetType = ifelse(idPoly %in% polys, 'res', wetType),
               ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro))
    
   hydroGra01 <- hydroGra %>% 
      filter(wetType != 'Ag') %>% 
      mutate(wetType = ifelse(idPoly %in% polys, 'res', wetType),
             ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro))
  
  # write .csv's
   
    fwrite(wetArea01, "wetAreaFin.csv")
    fwrite(hydroGra01, "hydroGraFin.csv")
   
#-------------------------------------------------------------------------------
# For future reference, I can consolidate the previous two sections into the 
# the following code. -----
    
  # Read in data ----
    
  # ecoHUC <- read_csv("HUCsites.csv") # ecohydroregion and HUC4 data
  # wetArea <- fread("wetAreaAll.csv") # wet area
  # hydroGra <- fread("hydroGraAll.csv") # hydrograph
  # hydroPer <- fread("hydroPerAll.csv") # hydroperiod
    
  # List of 'idPoly's to change from 'wetMan' to 'res' -----
 
    polys <- c(73121, 73523, 73528, 73528, 73529, 73522, 73124, 73119,  # Lake Helena WMA
               73388, 73118, 73527, 73340, 73526,                       # Lake Helena WMA
               63232, 63000, 63002, 63003, 63049, 63229,                # Freezeout Lake
               63533,                                                   # Grass Lake NWR
               90390,                                                   # Red Rock Lakes NWR
               90846, 90845,                                            # Spidel WPA
               # 8153, 8154, 1535,                                        # Tule and Klamath Lake
               # 13680, 13758,                                            # Fairchild Swamp
               99502)                                                   # Canvasback Club
    
  # fix spelling errors, change wetType based on idPoly, remove Ag -----
    
    wetArea01 <- wetArea %>%   
      select('idPoly','siteName','ownAg','wetType', 'year', 'month', 'wetHa') %>% 
      left_join(ecoHUC, by = "idPoly") %>% 
      filter(ownAg != 'wet' | wetType != 'Ag') %>%   
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public',
                             str_detect(ownAg, 'Pri') ~ 'Private',
                             TRUE ~ 'OTHER'),
             wetType = ifelse(idPoly %in% polys, 'res', wetType),
             ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro))
    
    
    hydroGra01 <- hydroGra %>%
      select('idPoly','siteName','ownAg','wetType', 'year', 'month', 'wetHa') %>% 
      left_join(ecoHUC, by = "idPoly") %>% 
      filter(ownAg != 'wet' | wetType != 'Ag') %>%   
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public',
                             str_detect(ownAg, 'Pri') ~ 'Private',
                             TRUE ~ 'OTHER'),
             wetType = ifelse(idPoly %in% polys, 'res', wetType),
             ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro))
    
#-------------------------------------------------------------------------------    
  # Visualize differences in ownship per ecoHydroregion depending on removal of
  # 'wetMan'. I want to understand why public is more than private in most cases.
  
    # Group -----
     
      wetArea02 <- (wetArea01) %>%
        filter(wetType != 'wetMan') %>%
          group_by(ownAg, ecoHydro, year) %>%
          summarise(wetHa = sum(wetHa)) %>%
          ungroup() %>%
          mutate(year = as.numeric(year))
      
        wetArea02 <- (wetArea01) %>%
          group_by(ownAg, ecoHydro, year) %>%
          summarise(wetHa = sum(wetHa)) %>%
          ungroup() %>%
          mutate(year = as.numeric(year))
      
    # plot -----
        
      wetPlot02 <- ggplot(wetArea02, aes(x = year, y = wetHa, color = ownAg)) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("without tule and klamath")
  
#-------------------------------------------------------------------------------    
# Data Visualization -----
  
  # Read in data -----
  
    wetArea <- read_csv("wetAreaFin.csv")
    hydroGra <- read_csv("hydroGraFin.csv")
  
  # Look at data -----
    
    str(wetArea)
    nrow(wetArea)
    head(wetArea)

    head(hydroGra)
    str(hydroGra)
    nrow(hydroGra)    
# Group, Sum, Plot -----
  
  # wetHa for ecohydroregion over time -----
  
    # Group -----
    wetArea01 <- (wetArea) %>% 
      group_by(ecoHydro, year) %>% 
      summarise(wetHa=sum(wetHa)) %>% 
      ungroup() %>%
      mutate(year = as.numeric(year))
  
    # plot -----
      wetPlot01 <- ggplot(wetArea01, aes(x = year, y = wetHa, color = ecoHydro)) +
        # scale_color_manual(values=c("#83e49b", "#d9408a", "#7127d3", "#5ca3c8", "#0f25cc", "#c86c66", "#88d268", "#d03ad2", "#d4e186")) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("wetHa grouped by month for each site over time")
  
    # Check to make sure that data from the hydroGra plots the same as wetArea
    # Group -----
      hydroGra01 <- (hydroGra) %>% 
        group_by(ecoHydro, year) %>% 
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        mutate(year = as.numeric(year))
    
    # plot -----
      hydroPlot01 <- ggplot(hydroGra01, aes(x = year, y = wetHa)) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 2, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("wetHa grouped by month for each site over time")

#-------------------------------------------------------------------------------  
    
  # Private vs Public for Ecohydroregion over time -----
    
    # Group -----
      wetArea02 <- (wetArea) %>% 
        group_by(ownAg, ecoHydro, year) %>% 
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        mutate(year = as.numeric(year))
    
    # plot -----
      wetPlot02 <- ggplot(wetArea02, aes(x = year, y = wetHa, color = ownAg)) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("wetHa grouped by month for each site over time")
  
    # Group -----
      hydroGra02 <- (hydroGra) %>% 
        group_by(ownAg, ecoHydro, year) %>% 
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        mutate(year = as.numeric(year))
    
    # plot -----
      hydroPlot02 <- ggplot(hydroGra02, aes(x = year, y = wetHa, color = ownAg)) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("wetHa grouped by month for each site over time")
    
#-------------------------------------------------------------------------------    
    # month over time -----
    
    # Group -----
      wetArea03 <- (wetArea) %>% 
        group_by(month, year) %>% 
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        mutate(year = as.numeric(year))
    
    # plot -----
      wetPlot03 <- ggplot(wetArea03, aes(x = year, y = wetHa)) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~month, ncol = 3, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("wetHa grouped by month for each site over time")

#-------------------------------------------------------------------------------
  # hydroperiod for ecoregion 
    
    # Group -----
      hydroGra03 <- (hydroGra) %>% 
        group_by(wetType, ecoHydro, year) %>% 
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        mutate(year = as.numeric(year))
    
    # plot -----
      hydroPlot03 <- ggplot(hydroGra03, aes(x = year, y = wetHa, color = wetType)) +
        geom_line(size = 1, alpha = .5) + 
        geom_smooth(method = 'lm', size=.2) + 
        scale_y_continuous(labels = thousands) +
        theme_light() +
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom') +
        ggtitle("wetHa grouped by month for each site over time")
    
#-------------------------------------------------------------------------------
# first period of time vs second period of time -----
    
    wetArea04 <- wetArea %>% 
     mutate(year = as.numeric(year),
             term = 't1',                                # creates a column with filled with 'p1'
             term = replace(term, year >2003, 't2'))

    # calc difference between p1 and p1 as %
    
    wetChange <- wetArea04 %>%
      group_by(term, ecoHydro, year) %>%
      summarise(wetSum = sum(wetHa)) %>%
      ungroup() %>%
      group_by(term, ecoHydro) %>% 
      summarise(wetMean = mean(wetSum)) %>%
      spread(term, wetMean) %>% 
      mutate(change = ((t1-t2)/t1)*-1)
    
  # make box plots to show distibutions between p1 nad p2 surface water
  
    wetBox <- wetArea04 %>%
      group_by(term, ecoHydro, year) %>%
      summarise(wetSum = sum(wetHa))
    
    wetBoxPlot <- ggplot(wetBox, aes(x = term, y = wetSum, group = term)) +
      geom_boxplot() +
      facet_wrap(. ~ ecoHydro, scales = 'free') +
      scale_y_continuous(labels = thousands) +
      xlab("") +
      ylab("Inundated hectares x 1000") +
      theme_bw()    
    
     wetWilcox <- wetArea04 %>%
      group_by(term, ecoHydro, year) %>%
      summarise(wetSum = sum(wetHa)) %>%
      split(.$ecoHydro) %>%                                 # '.' shorthand for the dataframe
      map(~wilcox.test(wetSum ~ term, data = .x)) %>%   # iterates whole process over each region
      map_df(broom::tidy, .id = 'ecoHydro')                 # kicks out dataframe with p-valuves by region and season
  
  # combine wilcoxon results with p1, p2 diff results -----
    
    wetChangeWY <- wetChange %>% 
      full_join(wetWilcox) %>%                            # full_join is safer and preffered to bind_cols  
      select(ecoHydro, t1, t2, change, p.value)
    