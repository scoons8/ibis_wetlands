################################################################################
# Task: clean up climate data 
# 
# Dec 10 2020
################################################################################

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
  
  climVars <- fread("01_data/10_climate_vars_raw/climateVars.csv")
  tmin <- fread("01_data/10_climate_vars_raw/tmin.csv")
  
################################################################################

# Clean data -----
  
  temp <- tmin %>% 
    select('HUC4', 'mean', 'year') %>%                              # select the columns you want to keep
    rename('tmin' = mean)                                           # rename the column 'mean' to 'tmin'
  
  clim <- climVars %>% 
    select('HUC4', 'NAME', 'aet', 'pr', 'ro', 'swe', 'year') %>% 
    left_join(temp, by = c('HUC4', 'year')) %>%                     # join the temperature data with the other climate variables (join by HUC4 and year)
    gather(climateVar, value, "aet" : "swe", "tmin")                # make wide table long: rearranges climate variables into one column
                                                                    # gather arguments (name of new column #1, name of new column #2, columns being gathered into new column #1)
  
# Save the data -----
  
  fwrite(clim, "01_data/11_climate_vars_clean/climateVars.csv")
  
################################################################################
# Clean seasonal data for RF analysis ------
  
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin01.csv')
  
  hydro01 <- hydroGra %>% 
    group_by(siteName, ecoHydro, HUC4, year) %>% 
    summarise(wetSum = sum(wetHa)) %>% 
    ungroup()
  
  fwrite(hydro01, '01_data/11_RF_vars_clean/ibisSeas02.csv')
  
################################################################################
  
# Quick Plots -----
  
  # plot everthing together? 
  # Temp is in Celsius and everything else is in mm
    
    clim01 <- clim %>%
      filter(climateVar != 'tmin') %>% 
      filter(HUC4 == '1712') #%>% 
      #filter(climateVar != 'aet')
  
  clim02 <- clim %>% 
    filter(climateVar == 'tmin') %>% 
    filter(HUC4 == '1712')
  
  clim03 <- clim %>%
    filter(HUC4 == '1712')
  
  ggplot(clim03, aes(x = year, y = value, color = climateVar)) +                   # colors of lines (made to match the map I made in QGIS)
        geom_line(size = 1, alpha = .5) +                   # divides units by 1000
        theme_light() +                                             # color theme
        #ylab('units = celcius') + 
        facet_wrap(~climateVar, ncol = 3, scales = 'free') #+          # makes a plot for each region; 3 columns; y-axis different for each graph
        # theme(strip.background =element_rect(fill="#7A7A7A")) +     # color of graph title boxes
        # theme( axis.title.y=element_text(size=8),                   # text size
        #        axis.title.x=element_text(size=8),
        #        axis.text=element_text(size=8)) + 
        # ggtitle("climate vars")  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  