################################################################################
# Task: 
# Merge GEE water files and add in extended means for Mar and Apr. 
# Apply to each ibis blob data (55 total) extracted from GEE
# 
# Date:
# Sept 16 2020
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')


# Set working directory  -----

# For work computer:

  setwd("01_data/site_water_files/washoeLakeBlob") # change site folder here


# GEE Water files -----
# Read in data -----

  wetArea <- read_csv("wetArea.csv")
  hydroGra <- read_csv("hydroGra.csv")

# Tidy up the data -----

  # wetArea -----
  
    wetArea01 <- tbl_df(wetArea) %>%  
      filter(wetType != 'res') %>%  # filter out wetTypes classified as 'res'
      rename('3' = band_2) %>%      # Rename bands to appropriate month 
      rename('4' = band_2_1) %>%    # Rename 'band_2_1' to '4' which corresponds to April
      rename('5' = band_2_2) %>%
      rename('6' = band_2_3) %>%
      rename('7' = band_2_4) %>%
      rename('8' = band_2_5) %>%
      rename('9' = band_2_6) %>%
      rename('10' = band_2_7) %>%
      select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% # select the columns that you want
      gather(month, wetHa,'3':'10')                                      # reorganize data from wide to long. 
                                                                         # Gather columns into rows (arguments: new col name, new col name, data being gathered)
  # Hydrograph -----

    hydro01 <- tbl_df(hydroGra) %>%
      filter(wetType != 'res') %>%   # remove wetType = 'res' from data frame
      select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>% 
      gather(period, wetHa,'seasonal03':'temp10') %>% 
      mutate(month = ifelse(grepl("03", period), '3', 
                      ifelse(grepl("04", period), '4', 
                      ifelse(grepl("05", period), '5',
                      ifelse(grepl("06", period), '6',
                      ifelse(grepl("07", period), '7',
                      ifelse(grepl("08", period), '8',
                      ifelse(grepl("09", period), '9',
                      ifelse(grepl("10", period), '10', '0')))))))),
             period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
      select(!'period') %>%
      rename(period = period2)

 ################################################################################  
# Upload extended means for Mar and Apr and replace poor data in OG datasets
  # Skip this if no years needed extensions

  # Read in data -----

  # March -----  
    wetAreaExtMar <- read_csv("wetAreaExtMar.csv")
    hydroGraExtMar <- read_csv("hydroGraExtMar.csv")
  
  # April -----
    wetAreaExtApr <- read_csv("wetAreaExtApr.csv")
    hydroGraExtApr <- read_csv("hydroGraExtApr.csv")

  # Tidy up data -----

    # March wetAreaExt -----
    
      wetAreaExtMar01 <- tbl_df(wetAreaExtMar) %>%
        filter(wetType != 'res') %>%
        rename('3' = band_2) %>%
        rename('4' = band_2_1) %>%
        rename('5' = band_2_2) %>%
        rename('6' = band_2_3) %>%
        rename('7' = band_2_4) %>%
        rename('8' = band_2_5) %>%
        rename('9' = band_2_6) %>%
        rename('10' = band_2_7) %>%
        select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
        gather(month, wetHa,'3':'10')
    
    # April wetAreaExt -----
    
      wetAreaExtApr01 <- tbl_df(wetAreaExtApr) %>%
        filter(wetType != 'res') %>%
        rename('3' = band_2) %>%
        rename('4' = band_2_1) %>%
        rename('5' = band_2_2) %>%
        rename('6' = band_2_3) %>%
        rename('7' = band_2_4) %>%
        rename('8' = band_2_5) %>%
        rename('9' = band_2_6) %>%
        rename('10' = band_2_7) %>%
        select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
        gather(month, wetHa,'3':'10')
    
    # March HydrographExt -----
    
      hydroExtMar01 <- tbl_df(hydroGraExtMar) %>%
        filter(wetType != 'res') %>%
        select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>% 
        gather(period, wetHa,'seasonal03':'temp10') %>% 
        mutate(month = ifelse(grepl("03", period), '3', 
                        ifelse(grepl("04", period), '4', 
                        ifelse(grepl("05", period), '5',
                        ifelse(grepl("06", period), '6',
                        ifelse(grepl("07", period), '7',
                        ifelse(grepl("08", period), '8',
                        ifelse(grepl("09", period), '9',
                        ifelse(grepl("10", period), '10', '0')))))))),
               period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>% 
        select(!'period') %>%
        rename(period = period2)
    
     # April HydrographExt -----
    
      hydroExtApr01 <- tbl_df(hydroGraExtApr) %>%
        filter(wetType != 'res') %>%
        select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>% 
        gather(period, wetHa,'seasonal03':'temp10') %>% 
        mutate(month = ifelse(grepl("03", period), '3',       # makes new column labeled 'month'
                        ifelse(grepl("04", period), '4',      # if '04' is in the column 'period',
                        ifelse(grepl("05", period), '5',      # then add the value '4' to the column 'month'
                        ifelse(grepl("06", period), '6',      # Do the same for the rest of the values and add
                        ifelse(grepl("07", period), '7',      # the value '0' for anything that isn't defined
                        ifelse(grepl("08", period), '8',
                        ifelse(grepl("09", period), '9',
                        ifelse(grepl("10", period), '10', '0')))))))),
               period2 = ifelse(grepl("perm", period), 'perm',            # if 'perm' is in the column 'period',
                          ifelse(grepl("semi", period), 'semi',           # add the value 'perm' to the column 'period2'
                          ifelse(grepl("seasonal", period), 'seasonal',   # repeat for semi and seasonal and temp
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>% 
        select(!'period') %>%
        rename(period = period2)


    # Extract the Year + Months that need the extended means -----
    # Change the years depending on the site
    
    # Fill variables with years that have extended means -----
    
      mar <- c(2006)
      apr <- c(2006)

  # wetArea -----
    # March -----
    wetAreaExtMar02 <- wetAreaExtMar01 %>%
      filter(month == 3 & year %in% mar)
    
    # April -----
    wetAreaExtApr02 <- wetAreaExtApr01 %>% 
      filter(month == 4 & year %in% apr)
  
  # hydrograph -----
    # March -----
    hydroExtMar02 <- tbl_df(hydroExtMar01) %>%
      filter(month == 3 & year %in% mar)
    
    # April -----
    hydroExtApr02 <- tbl_df(hydroExtApr01) %>%
      filter(month == 4 & year %in% apr)

# Remove the bad years + months from the OG dataset and 
# add in the new data with the extended years -----

  # wetArea -----
  
    wetArea02 <- wetArea01 %>%
      filter(!(month == 3 & year %in% mar)) %>%
      filter(!(month == 4 & year %in% apr)) %>%
      bind_rows(wetAreaExtMar02) %>%
      bind_rows(wetAreaExtApr02)
  
  # hydrograph -----
  
    hydro02 <- hydro01 %>%
      filter(!(month == 3 & year %in% mar)) %>%
      filter(!(month == 4 & year %in% apr)) %>%
      bind_rows(hydroExtMar02) %>%
      bind_rows(hydroExtApr02)
  
  # Annual hydroperiod -----
  
    Annual02 <- hydro02 %>%  # change to hydro01 if no extensions were used, otherwise use hydro02
      group_by(idPoly, siteName, ownAg, wetType, year, period) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      ungroup()


# Write the data as .csv's -> will join to polygons in QGIS -----

write.csv(wetArea02, file = "wetAreaCompiled.csv")  # change to wetArea01 if no extensions were used, otherwise use wetArea02
write.csv(hydro02, file = "hydroGraCompiled.csv")   # change to hydro01 if no extensions were used, otherwise use hydro02
write.csv(Annual02, file = "hydroPerCompiled.csv")

# wetAreaFin <- read_csv("wetAreaFinal.csv")
# hydroGraFin <- read_csv("hydroGraFinal.csv")


################################################################################
options(scipen=999)         
thousands <- function(x){   
  x/1000
}

Annual03 <- (Annual02) %>% 
  group_by(period, wetType, year) %>% 
  summarise(wetHa = sum(wetHa)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

wetPlot <- ggplot(Annual03, aes(x = year, y = wetHa, color = wetType)) +
    geom_line(size = 1, alpha = .5) +
    scale_y_continuous(labels = thousands) +
    theme_light() +
    ylab('Inundated hectares x 1000') +
    facet_wrap(~period, ncol = 2, scales = 'free') +
    theme( axis.title.y=element_text(size=8),
           axis.title.x=element_text(size=8),
           axis.text=element_text(size=8),
           legend.title = element_blank(),
           legend.position = 'bottom') +
    ggtitle("wetHa grouped by month for each site over time")


################################################################################
################################################################################
################################################################################
# For those sites where I had to exclude March -----
  # Remember that the bands have different names: 
  # April is now band_2 instead of March. 

# RED ROCK LAKES BLOB:

setwd("/Users/sc148852/Box/R/ibisSites/redRockLakesBlob") # change site folder here


# GEE Water files -----
# Read in data -----

  wetArea <- read_csv("wetArea.csv")
  hydroGra <- read_csv("hydroGra.csv")

# Tidy up the data -----

  # wetArea -----
  
    wetArea01 <- tbl_df(wetArea) %>%
      filter(wetType != 'res') %>%
      rename('4' = band_2) %>%
      rename('5' = band_2_1) %>%
      rename('6' = band_2_2) %>%
      rename('7' = band_2_3) %>%
      rename('8' = band_2_4) %>%
      rename('9' = band_2_5) %>%
      rename('10' = band_2_6) %>%
      select('idPoly','4':'10','ownAg','siteName','wetType', 'year') %>% 
      gather(month, wetHa,'4':'10')

  # Hydrograph -----

    hydro01 <- tbl_df(hydroGra) %>%
      filter(wetType != 'res') %>%
      select('idPoly','seasonal04':'semi10','temp04':'temp10','siteName','ownAg','wetType','year') %>% 
      gather(period, wetHa,'seasonal04':'temp10') %>% 
      mutate(month = ifelse(grepl("04", period), '4', 
                      ifelse(grepl("05", period), '5',
                      ifelse(grepl("06", period), '6',
                      ifelse(grepl("07", period), '7',
                      ifelse(grepl("08", period), '8',
                      ifelse(grepl("09", period), '9',
                      ifelse(grepl("10", period), '10', '0'))))))),
             period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
      select(!'period') %>%
      rename(period = period2)

 ################################################################################  
# Upload extended means for Mar and Apr and replace poor data in OG datasets
  # Skip this if no years needed extensions

  # Read in data -----

  # March -----  
    # wetAreaExtMar <- read_csv("wetAreaExtMar.csv")
    # hydroGraExtMar <- read_csv("hydroGraExtMar.csv")
  
  # April -----
    wetAreaExtApr <- read_csv("wetAreaExtApr.csv")
    hydroGraExtApr <- read_csv("hydroGraExtApr.csv")

  # Tidy up data -----

    # March wetAreaExt -----
    
      # wetAreaExtMar01 <- tbl_df(wetAreaExtMar) %>%
      #   filter(wetType != 'res') %>%
      #   rename('3' = band_2) %>%
      #   rename('4' = band_2_1) %>%
      #   rename('5' = band_2_2) %>%
      #   rename('6' = band_2_3) %>%
      #   rename('7' = band_2_4) %>%
      #   rename('8' = band_2_5) %>%
      #   rename('9' = band_2_6) %>%
      #   rename('10' = band_2_7) %>%
      #   select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
      #   gather(month, wetHa,'3':'10')
    
    # April wetAreaExt -----
    
      wetAreaExtApr01 <- tbl_df(wetAreaExtApr) %>%
        filter(wetType != 'res') %>%
        rename('4' = band_2) %>%
        rename('5' = band_2_1) %>%
        rename('6' = band_2_2) %>%
        rename('7' = band_2_3) %>%
        rename('8' = band_2_4) %>%
        rename('9' = band_2_5) %>%
        rename('10' = band_2_6) %>%
        select('idPoly','4':'10','ownAg','siteName','wetType', 'year') %>% 
        gather(month, wetHa,'4':'10')
    
    # March HydrographExt -----
    
      # hydroExtMar01 <- tbl_df(hydroGraExtMar) %>%
      #   filter(wetType != 'res') %>%
      #   select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>% 
      #   gather(period, wetHa,'seasonal03':'temp10') %>% 
      #   mutate(month = ifelse(grepl("03", period), '3', 
      #                   ifelse(grepl("04", period), '4', 
      #                   ifelse(grepl("05", period), '5',
      #                   ifelse(grepl("06", period), '6',
      #                   ifelse(grepl("07", period), '7',
      #                   ifelse(grepl("08", period), '8',
      #                   ifelse(grepl("09", period), '9',
      #                   ifelse(grepl("10", period), '10', '0')))))))),
      #          period2 = ifelse(grepl("perm", period), 'perm',
      #                     ifelse(grepl("semi", period), 'semi',
      #                     ifelse(grepl("seasonal", period), 'seasonal',
      #                     ifelse(grepl("temp", period), 'temp', '0'))))) %>% 
      #   select(!'period') %>%
      #   rename(period = period2)
    
     # April HydrographExt -----
    
      hydroExtApr01 <- tbl_df(hydroGraExtApr) %>%
        filter(wetType != 'res') %>%
        select('idPoly','seasonal04':'semi10','temp04':'temp10','siteName','ownAg','wetType','year') %>% 
        gather(period, wetHa,'seasonal04':'temp10') %>% 
        mutate(month = ifelse(grepl("04", period), '4', 
                        ifelse(grepl("05", period), '5',
                        ifelse(grepl("06", period), '6',
                        ifelse(grepl("07", period), '7',
                        ifelse(grepl("08", period), '8',
                        ifelse(grepl("09", period), '9',
                        ifelse(grepl("10", period), '10', '0'))))))),
               period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>% 
        select(!'period') %>%
        rename(period = period2)


    # Extract the Year + Months that need the extended means -----
    # Change the years depending on the site
    
    # Fill variables with years that have extended means -----
    
      # mar <- c(2006)
      apr <- c(1992, 1993, 1994, 2012, 2013, 2014)

  # wetArea -----
    # March -----
    # wetAreaExtMar02 <- wetAreaExtMar01 %>%
    #   filter(month == 3 & year %in% mar)
    
    # April -----
    wetAreaExtApr02 <- wetAreaExtApr01 %>% 
      filter(month == 4 & year %in% apr)
  
  # hydrograph -----
    # March -----
    # hydroExtMar02 <- tbl_df(hydroExtMar01) %>%
    #   filter(month == 3 & year %in% mar)
    
    # April -----
    hydroExtApr02 <- tbl_df(hydroExtApr01) %>%
      filter(month == 4 & year %in% apr)

# Remove the bad years + months from the OG dataset and 
# add in the new data with the extended years -----

  # wetArea -----
  
    wetArea02 <- wetArea01 %>%
      # filter(!(month == 3 & year %in% mar)) %>%
      filter(!(month == 4 & year %in% apr)) %>%
      # bind_rows(wetAreaExtMar02) %>%
      bind_rows(wetAreaExtApr02)
  
  # hydrograph -----
  
    hydro02 <- hydro01 %>%
      # filter(!(month == 3 & year %in% mar)) %>%
      filter(!(month == 4 & year %in% apr)) %>%
      # bind_rows(hydroExtMar02) %>%
      bind_rows(hydroExtApr02)
  
  # Annual hydroperiod -----
  
    Annual02 <- hydro02 %>%  # change to hydro01 if no extensions were used, otherwise use hydro02
      group_by(idPoly, siteName, ownAg, wetType, year, period) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      ungroup()


# Write the data as .csv's -> will join to polygons in QGIS -----

write.csv(wetArea02, file = "wetAreaCompiled.csv")  # change to wetArea01 if no extensions were used, otherwise use wetArea02
write.csv(hydro02, file = "hydroGraCompiled.csv")   # change to hydro01 if no extensions were used, otherwise use hydro02
write.csv(Annual02, file = "hydroPerCompiled.csv")

# wetAreaFin <- read_csv("wetAreaFinal.csv")
# hydroGraFin <- read_csv("hydroGraFinal.csv")

################################################################################
################################################################################
################################################################################

# For those sites where I had to exclude March -----
  # Remember that the bands have different names: 
  # April is now band_2 instead of March. 

# ROCK CREEK BLOB:
# Some .csv's with no March and some with March included - there's a bit more 
# code here since I have to do the same process twice, then bind them together. 

setwd("/Users/sc148852/Box/R/ibisSites/rockCreekBlob") # change site folder here

# 'No March' files first -----
# GEE Water files -----
# Read in data -----

  wetArea <- read_csv("wetArea.csv")
  hydroGra <- read_csv("hydroGra.csv")
  wetAreaBands <- read_csv("wetAreaBands.csv")
  hydroGraBands <- read_csv("hydroGraBands.csv")
  

# Tidy up the data -----
  
   # wetArea (with March) -----
  
    wetArea01 <- tbl_df(wetArea) %>%
      filter(wetType != 'res') %>%
      rename('3' = band_2) %>%
      rename('4' = band_2_1) %>%
      rename('5' = band_2_2) %>%
      rename('6' = band_2_3) %>%
      rename('7' = band_2_4) %>%
      rename('8' = band_2_5) %>%
      rename('9' = band_2_6) %>%
      rename('10' = band_2_7) %>%
      select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
      gather(month, wetHa,'3':'10')

  # Hydrograph (with March) -----

    hydro01 <- tbl_df(hydroGra) %>%
      filter(wetType != 'res') %>%
      select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>% 
      gather(period, wetHa,'seasonal03':'temp10') %>% 
      mutate(month = ifelse(grepl("03", period), '3', 
                      ifelse(grepl("04", period), '4', 
                      ifelse(grepl("05", period), '5',
                      ifelse(grepl("06", period), '6',
                      ifelse(grepl("07", period), '7',
                      ifelse(grepl("08", period), '8',
                      ifelse(grepl("09", period), '9',
                      ifelse(grepl("10", period), '10', '0')))))))),
             period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
      select(!'period') %>%
      rename(period = period2)

  # wetArea (no March) -----
  
    wetArea03 <- tbl_df(wetAreaBands) %>%
      filter(wetType != 'res') %>%
      rename('4' = band_2) %>%
      rename('5' = band_2_1) %>%
      rename('6' = band_2_2) %>%
      rename('7' = band_2_3) %>%
      rename('8' = band_2_4) %>%
      rename('9' = band_2_5) %>%
      rename('10' = band_2_6) %>%
      select('idPoly','4':'10','ownAg','siteName','wetType', 'year') %>% 
      gather(month, wetHa,'4':'10')

  # Hydrograph (no March) -----

    hydro03 <- tbl_df(hydroGraBands) %>%
      filter(wetType != 'res') %>%
      select('idPoly','seasonal04':'semi10','temp04':'temp10','siteName','ownAg','wetType','year') %>% 
      gather(period, wetHa,'seasonal04':'temp10') %>% 
      mutate(month = ifelse(grepl("04", period), '4', 
                      ifelse(grepl("05", period), '5',
                      ifelse(grepl("06", period), '6',
                      ifelse(grepl("07", period), '7',
                      ifelse(grepl("08", period), '8',
                      ifelse(grepl("09", period), '9',
                      ifelse(grepl("10", period), '10', '0'))))))),
             period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
      select(!'period') %>%
      rename(period = period2)

 ################################################################################  
# Upload extended means for Mar and Apr and replace poor data in OG datasets
  # Skip this if no years needed extensions

  # Read in data -----

  # March -----  
    wetAreaExtMar <- read_csv("wetAreaExtMar.csv")
    hydroGraExtMar <- read_csv("hydroGraExtMar.csv")
  
  # April -----
    # wetAreaExtApr <- read_csv("wetAreaExtApr.csv")
    # hydroGraExtApr <- read_csv("hydroGraExtApr.csv")

  # Tidy up data -----

    # March wetAreaExt -----
    
      wetAreaExtMar01 <- tbl_df(wetAreaExtMar) %>%
        filter(wetType != 'res') %>%
        rename('3' = band_2) %>%
        rename('4' = band_2_1) %>%
        rename('5' = band_2_2) %>%
        rename('6' = band_2_3) %>%
        rename('7' = band_2_4) %>%
        rename('8' = band_2_5) %>%
        rename('9' = band_2_6) %>%
        rename('10' = band_2_7) %>%
        select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>%
        gather(month, wetHa,'3':'10')
    
    # April wetAreaExt -----
    
      # wetAreaExtApr01 <- tbl_df(wetAreaExtApr) %>%
      #   filter(wetType != 'res') %>%
      #   rename('4' = band_2) %>%
      #   rename('5' = band_2_1) %>%
      #   rename('6' = band_2_2) %>%
      #   rename('7' = band_2_3) %>%
      #   rename('8' = band_2_4) %>%
      #   rename('9' = band_2_5) %>%
      #   rename('10' = band_2_6) %>%
      #   select('idPoly','4':'10','ownAg','siteName','wetType', 'year') %>% 
      #   gather(month, wetHa,'4':'10')
    
    # March HydrographExt -----
    
      hydroExtMar01 <- tbl_df(hydroGraExtMar) %>%
        filter(wetType != 'res') %>%
        select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>%
        gather(period, wetHa,'seasonal03':'temp10') %>%
        mutate(month = ifelse(grepl("03", period), '3',
                        ifelse(grepl("04", period), '4',
                        ifelse(grepl("05", period), '5',
                        ifelse(grepl("06", period), '6',
                        ifelse(grepl("07", period), '7',
                        ifelse(grepl("08", period), '8',
                        ifelse(grepl("09", period), '9',
                        ifelse(grepl("10", period), '10', '0')))))))),
               period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
        select(!'period') %>%
        rename(period = period2)
    
     # April HydrographExt -----
    
      # hydroExtApr01 <- tbl_df(hydroGraExtApr) %>%
      #   filter(wetType != 'res') %>%
      #   select('idPoly','seasonal04':'semi10','temp04':'temp10','siteName','ownAg','wetType','year') %>% 
      #   gather(period, wetHa,'seasonal04':'temp10') %>% 
      #   mutate(month = ifelse(grepl("04", period), '4', 
      #                   ifelse(grepl("05", period), '5',
      #                   ifelse(grepl("06", period), '6',
      #                   ifelse(grepl("07", period), '7',
      #                   ifelse(grepl("08", period), '8',
      #                   ifelse(grepl("09", period), '9',
      #                   ifelse(grepl("10", period), '10', '0'))))))),
      #          period2 = ifelse(grepl("perm", period), 'perm',
      #                     ifelse(grepl("semi", period), 'semi',
      #                     ifelse(grepl("seasonal", period), 'seasonal',
      #                     ifelse(grepl("temp", period), 'temp', '0'))))) %>% 
      #   select(!'period') %>%
      #   rename(period = period2)


    # Extract the Year + Months that need the extended means -----
    # Change the years depending on the site
    
    # Fill variables with years that have extended means -----
    
      mar <- c(1988, 1989, 1990)
      # apr <- c(1992, 1993, 1994, 2012, 2013, 2014)

  # wetArea -----
    # March -----
    wetAreaExtMar02 <- wetAreaExtMar01 %>%
      filter(month == 3 & year %in% mar)
    
    # April -----
    # wetAreaExtApr02 <- wetAreaExtApr01 %>% 
    #   filter(month == 4 & year %in% apr)
  
  # hydrograph -----
    # March -----
    hydroExtMar02 <- tbl_df(hydroExtMar01) %>%
      filter(month == 3 & year %in% mar)
    
    # April -----
    # hydroExtApr02 <- tbl_df(hydroExtApr01) %>%
    #   filter(month == 4 & year %in% apr)

# Remove the bad years + months from the OG dataset and 
# add in the new data with the extended years -----

  # wetArea -----
  
    wetArea02 <- wetArea01 %>%
      bind_rows(wetAreaExtMar02) %>%
      bind_rows(wetArea03)
  
  # hydrograph -----
  
    hydro02 <- hydro01 %>%
      bind_rows(hydroExtMar02) %>%
      bind_rows(hydro03)
  
  # Annual hydroperiod -----
  
    Annual02 <- hydro02 %>%  # change to hydro01 if no extensions were used, otherwise use hydro02
      group_by(idPoly, siteName, ownAg, wetType, year, period) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      ungroup()


# Write the data as .csv's -> will join to polygons in QGIS -----

write.csv(wetArea02, file = "wetAreaCompiled.csv")  # change to wetArea01 if no extensions were used, otherwise use wetArea02
write.csv(hydro02, file = "hydroGraCompiled.csv")   # change to hydro01 if no extensions were used, otherwise use hydro02
write.csv(Annual02, file = "hydroPerCompiled.csv")

# wetAreaFin <- read_csv("wetAreaFinal.csv")
# hydroGraFin <- read_csv("hydroGraFinal.csv")


################################################################################
################################################################################
################################################################################

# For those sites where I had to exclude March -----
  # Remember that the bands have different names: 
  # April is now band_2 instead of March. 

# WILLOW CREEK RES BLOB:
# Some .csv's with no March and some with March included - there's a bit more 
# code here since I have to do the same process twice, then bind them together. 

setwd("/Users/sc148852/Box/R/ibisSites/willowCreekResBlob") # change site folder here

# 'No March' files first -----
# GEE Water files -----
# Read in data -----

  wetArea <- read_csv("wetArea.csv")
  hydroGra <- read_csv("hydroGra.csv")
  wetAreaBands <- read_csv("wetAreaBands.csv")
  hydroGraBands <- read_csv("hydroGraBands.csv")
  

# Tidy up the data -----
  
   # wetArea (with March) -----
  
    wetArea01 <- tbl_df(wetArea) %>%
      filter(wetType != 'res') %>%
      rename('3' = band_2) %>%
      rename('4' = band_2_1) %>%
      rename('5' = band_2_2) %>%
      rename('6' = band_2_3) %>%
      rename('7' = band_2_4) %>%
      rename('8' = band_2_5) %>%
      rename('9' = band_2_6) %>%
      rename('10' = band_2_7) %>%
      select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
      gather(month, wetHa,'3':'10')

  # Hydrograph (with March) -----

    hydro01 <- tbl_df(hydroGra) %>%
      filter(wetType != 'res') %>%
      select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>% 
      gather(period, wetHa,'seasonal03':'temp10') %>% 
      mutate(month = ifelse(grepl("03", period), '3', 
                      ifelse(grepl("04", period), '4', 
                      ifelse(grepl("05", period), '5',
                      ifelse(grepl("06", period), '6',
                      ifelse(grepl("07", period), '7',
                      ifelse(grepl("08", period), '8',
                      ifelse(grepl("09", period), '9',
                      ifelse(grepl("10", period), '10', '0')))))))),
             period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
      select(!'period') %>%
      rename(period = period2)

  # wetArea (no March) -----
  
    wetArea03 <- tbl_df(wetAreaBands) %>%
      filter(wetType != 'res') %>%
      rename('4' = band_2) %>%
      rename('5' = band_2_1) %>%
      rename('6' = band_2_2) %>%
      rename('7' = band_2_3) %>%
      rename('8' = band_2_4) %>%
      rename('9' = band_2_5) %>%
      rename('10' = band_2_6) %>%
      select('idPoly','4':'10','ownAg','siteName','wetType', 'year') %>% 
      gather(month, wetHa,'4':'10')

  # Hydrograph (no March) -----

    hydro03 <- tbl_df(hydroGraBands) %>%
      filter(wetType != 'res') %>%
      select('idPoly','seasonal04':'semi10','temp04':'temp10','siteName','ownAg','wetType','year') %>% 
      gather(period, wetHa,'seasonal04':'temp10') %>% 
      mutate(month = ifelse(grepl("04", period), '4', 
                      ifelse(grepl("05", period), '5',
                      ifelse(grepl("06", period), '6',
                      ifelse(grepl("07", period), '7',
                      ifelse(grepl("08", period), '8',
                      ifelse(grepl("09", period), '9',
                      ifelse(grepl("10", period), '10', '0'))))))),
             period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
      select(!'period') %>%
      rename(period = period2)

 ################################################################################  
# Upload extended means for Mar and Apr and replace poor data in OG datasets
  # Skip this if no years needed extensions

  # Read in data -----

  # March -----  
    wetAreaExtMar <- read_csv("wetAreaExtMar.csv")
    hydroGraExtMar <- read_csv("hydroGraExtMar.csv")
  
  # April -----
    wetAreaExtApr <- read_csv("wetAreaExtApr.csv")
    hydroGraExtApr <- read_csv("hydroGraExtApr.csv")

  # Tidy up data -----

    # March wetAreaExt -----
    
      wetAreaExtMar01 <- tbl_df(wetAreaExtMar) %>%
        filter(wetType != 'res') %>%
        rename('3' = band_2) %>%
        rename('4' = band_2_1) %>%
        rename('5' = band_2_2) %>%
        rename('6' = band_2_3) %>%
        rename('7' = band_2_4) %>%
        rename('8' = band_2_5) %>%
        rename('9' = band_2_6) %>%
        rename('10' = band_2_7) %>%
        select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>%
        gather(month, wetHa,'3':'10')
    
    # April wetAreaExt -----
    
      wetAreaExtApr01 <- tbl_df(wetAreaExtApr) %>%
        filter(wetType != 'res') %>%
        rename('4' = band_2) %>%
        rename('5' = band_2_1) %>%
        rename('6' = band_2_2) %>%
        rename('7' = band_2_3) %>%
        rename('8' = band_2_4) %>%
        rename('9' = band_2_5) %>%
        rename('10' = band_2_6) %>%
        select('idPoly','4':'10','ownAg','siteName','wetType', 'year') %>%
        gather(month, wetHa,'4':'10')
    
    # March HydrographExt -----
    
      hydroExtMar01 <- tbl_df(hydroGraExtMar) %>%
        filter(wetType != 'res') %>%
        select('idPoly','seasonal03':'semi10','temp03':'temp10','siteName','ownAg','wetType','year') %>%
        gather(period, wetHa,'seasonal03':'temp10') %>%
        mutate(month = ifelse(grepl("03", period), '3',
                        ifelse(grepl("04", period), '4',
                        ifelse(grepl("05", period), '5',
                        ifelse(grepl("06", period), '6',
                        ifelse(grepl("07", period), '7',
                        ifelse(grepl("08", period), '8',
                        ifelse(grepl("09", period), '9',
                        ifelse(grepl("10", period), '10', '0')))))))),
               period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
        select(!'period') %>%
        rename(period = period2)
    
     # April HydrographExt -----
    
      hydroExtApr01 <- tbl_df(hydroGraExtApr) %>%
        filter(wetType != 'res') %>%
        select('idPoly','seasonal04':'semi10','temp04':'temp10','siteName','ownAg','wetType','year') %>%
        gather(period, wetHa,'seasonal04':'temp10') %>%
        mutate(month = ifelse(grepl("04", period), '4',
                        ifelse(grepl("05", period), '5',
                        ifelse(grepl("06", period), '6',
                        ifelse(grepl("07", period), '7',
                        ifelse(grepl("08", period), '8',
                        ifelse(grepl("09", period), '9',
                        ifelse(grepl("10", period), '10', '0'))))))),
               period2 = ifelse(grepl("perm", period), 'perm',
                          ifelse(grepl("semi", period), 'semi',
                          ifelse(grepl("seasonal", period), 'seasonal',
                          ifelse(grepl("temp", period), 'temp', '0'))))) %>%
        select(!'period') %>%
        rename(period = period2)


    # Extract the Year + Months that need the extended means -----
    # Change the years depending on the site
    
    # Fill variables with years that have extended means -----
    
      mar <- c(1991, 1992, 1993, 1998, 2004, 2005, 2006)
      apr <- c(2004)

  # wetArea -----
    # March -----
    wetAreaExtMar02 <- wetAreaExtMar01 %>%
      filter(month == 3 & year %in% mar)
    
    # April -----
    wetAreaExtApr02 <- wetAreaExtApr01 %>%
      filter(month == 4 & year %in% apr)
  
  # hydrograph -----
    # March -----
    hydroExtMar02 <- tbl_df(hydroExtMar01) %>%
      filter(month == 3 & year %in% mar)
    
    # April -----
    hydroExtApr02 <- tbl_df(hydroExtApr01) %>%
      filter(month == 4 & year %in% apr)

# Remove the bad years + months from the OG dataset and 
# add in the new data with the extended years -----

  # wetArea -----
  
    wetArea02 <- wetArea01 %>%
      filter(!(month == 3 & year %in% mar)) %>%
      filter(!(month == 4 & year %in% apr)) %>%
      bind_rows(wetAreaExtMar02) %>%
      bind_rows(wetAreaExtApr02) %>%
      bind_rows(wetArea03)
  
  # hydrograph -----
  
    hydro02 <- hydro01 %>%
      filter(!(month == 3 & year %in% mar)) %>%
      filter(!(month == 4 & year %in% apr)) %>%
      bind_rows(hydroExtMar02) %>%
      bind_rows(hydroExtApr02) %>%
      bind_rows(hydro03)
  
  # Annual hydroperiod -----
  
    Annual02 <- hydro02 %>%  # change to hydro01 if no extensions were used, otherwise use hydro02
      group_by(idPoly, siteName, ownAg, wetType, year, period) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      ungroup()


# Write the data as .csv's -> will join to polygons in QGIS -----

write.csv(wetArea02, file = "wetAreaCompiled.csv")  # change to wetArea01 if no extensions were used, otherwise use wetArea02
write.csv(hydro02, file = "hydroGraCompiled.csv")   # change to hydro01 if no extensions were used, otherwise use hydro02
write.csv(Annual02, file = "hydroPerCompiled.csv")

# wetAreaFin <- read_csv("wetAreaFinal.csv")
# hydroGraFin <- read_csv("hydroGraFinal.csv")


