################################################################################
# Task: 
# Merge GEE water files and add in extended means for the sites with 
# zones (malheur and saltlake blobs)
#
# Date:
# Nov 2020
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')


# Set working directory and load libraries -----

# For work computer:
 
  setwd("01_data/site_water_files/saltLakeBlob") # change site folder here


# GEE Water files -----
# Read in data -----

  wetArea <- read_csv("wetArea.csv")
  hydroGra <- read_csv("hydroGra.csv")

# Tidy up the data -----

  # wetArea -----
  
  # Example: 
  
  wetArea$year <- gsub("_", "", wetArea$year)
  
    wetArea01 <- tbl_df(wetArea) %>%
      filter(wetType != 'res') %>%
      rename('3' = '03MAR') %>%
      rename('4' = '04APR') %>%
      rename('5' = '05MAY') %>%
      rename('6' = '06JUN') %>%
      rename('7' = '07JUL') %>%
      rename('8' = '08AUG') %>%
      rename('9' = '09SEP') %>%
      rename('10' = '10OCT') %>%
      select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
      gather(month, wetHa,'3':'10')

  # Hydrograph -----
    
    hydroGra$year <- gsub("_", "", hydroGra$year)

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
    
  # Remove the underscores that precede the years -----

    wetAreaExtMar$year <- gsub("_", "", wetAreaExtMar$year)
    wetAreaExtApr$year <- gsub("_", "", wetAreaExtApr$year)
    hydroGraExtMar$year <- gsub("_", "", hydroGraExtMar$year)
    hydroGraExtApr$year <- gsub("_", "", hydroGraExtApr$year)
    
  # Tidy up data -----

    # March wetAreaExt -----
    
      wetAreaExtMar01 <- tbl_df(wetAreaExtMar) %>%
        filter(wetType != 'res') %>%
        rename('3' = '03MAR') %>%
        rename('4' = '04APR') %>%
        rename('5' = '05MAY') %>%
        rename('6' = '06JUN') %>%
        rename('7' = '07JUL') %>%
        rename('8' = '08AUG') %>%
        rename('9' = '09SEP') %>%
        rename('10' = '10OCT') %>%
        select('idPoly','3':'10','ownAg','siteName','wetType', 'year') %>% 
        gather(month, wetHa,'3':'10')
    
    # April wetAreaExt -----
    
      wetAreaExtApr01 <- tbl_df(wetAreaExtApr) %>%
        filter(wetType != 'res') %>%
        rename('3' = '03MAR') %>%
        rename('4' = '04APR') %>%
        rename('5' = '05MAY') %>%
        rename('6' = '06JUN') %>%
        rename('7' = '07JUL') %>%
        rename('8' = '08AUG') %>%
        rename('9' = '09SEP') %>%
        rename('10' = '10OCT') %>%
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


    # Extract the Year + Months that need the extended means -----
    # Change the years depending on the site
    
    # Fill variables with years that have extended means -----
    
      mar <- c(1992, 1998, 2002, 2003, 2009, 2010, 2013)
      apr <- c(1993)

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

write.csv(wetArea02, file = "wetAreaFinal.csv")  # change to wetArea01 if no extensions were used, otherwise use wetArea02
write.csv(hydro02, file = "hydroGraFinal.csv")   # change to hydro01 if no extensions were used, otherwise use hydro02
write.csv(Annual02, file = "hydroPerFinal.csv")

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


