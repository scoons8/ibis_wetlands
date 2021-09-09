################################################################################
# Task: compile, reformat, and merge 2020 wet files
# Aug 12 2020
################################################################################

# Load Packages -----

  library('plyr')
  library("tidyverse")
  library('readr')
  library('data.table')
  library('stringr')
  options(scipen=999)

#===============================================================================
# Load in sites that need extensions for March and replace March data -----

  # Bowdoin -----
  # Load data -----
  
    bowdoinHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/bowdoin2020.csv')
    bowdoinMarHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/bowdoinMarExt2020.csv')
    bowdoinWet <- fread('01_data/14_2020data/wetArea2020MarExt/bowdoin2020.csv')
    bowdoinMarWet <- fread('01_data/14_2020data/wetArea2020MarExt/bowdoinMarExt2020.csv')
    
  # Clean data -----
    
    # wetArea -----
  
      wet01 <- tbl_df(bowdoinWet) %>%  
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
                                            
  
      wetExt01 <- tbl_df(bowdoinMarWet) %>%  
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
          gather(month, wetHa,'3':'10')            
      
    # Hydrograph -----

      hydro01 <- tbl_df(bowdoinHydro) %>%
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
      
      hydroExt01 <- tbl_df(bowdoinMarHydro) %>%
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
      
    # Extract only March from the extended data -----
     
      wetExt02 <- wetExt01 %>%
        filter(month == 3)

      hydroExt02 <- hydroExt01 %>%
        filter(month == 3)
      
    # Remove the bad months from the OG dataset and 
    # add in the new data with the extended years -----

      # wetArea -----
      
        wet02 <- wet01 %>%
          filter(!(month == 3)) %>%
          bind_rows(wetExt02)
      
      # hydrograph -----
      
        hydro02 <- hydro01 %>%
          filter(!(month == 3 )) %>% 
          bind_rows(hydroExt02)

#-------------------------------------------------------------------------------  
  # Goose Lake -----
  # Load data -----
  
    gooseLakeHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/gooseLake2020.csv')
    gooseLakeMarHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/gooseLakeMarExt2020.csv')
    gooseLakeWet <- fread('01_data/14_2020data/wetArea2020MarExt/gooseLake2020.csv')
    gooseLakeMarWet <- fread('01_data/14_2020data/wetArea2020MarExt/gooseLakeMarExt2020.csv')
    
  # Clean data -----
    
    # wetArea -----
  
      wet03 <- tbl_df(gooseLakeWet) %>%  
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
                                            
  
      wetExt03 <- tbl_df(gooseLakeMarWet) %>%  
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
          gather(month, wetHa,'3':'10')            
      
    # Hydrograph -----

      hydro03 <- tbl_df(gooseLakeHydro) %>%
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
      
      hydroExt03 <- tbl_df(gooseLakeMarHydro) %>%
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
      
    # Extract only March from the extended data -----
     
      wetExt04 <- wetExt03 %>%
        filter(month == 3)

      hydroExt04 <- hydroExt03 %>%
        filter(month == 3)
      
    # Remove the bad months from the OG dataset and 
    # add in the new data with the extended years -----

      # wetArea -----
      
        wet04 <- wet03 %>%
          filter(!(month == 3)) %>%
          bind_rows(wetExt04)
      
      # hydrograph -----
      
        hydro04 <- hydro03 %>%
          filter(!(month == 3 )) %>% 
          bind_rows(hydroExt04)  
      
#-------------------------------------------------------------------------------  
  # Medicine Lake -----
  # Load data -----
  
    medLakeHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/medicineLake2020.csv')
    medLakeMarHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/medicineLakeMarExt2020.csv')
    medLakeWet <- fread('01_data/14_2020data/wetArea2020MarExt/medicineLake2020.csv')
    medLakeMarWet <- fread('01_data/14_2020data/wetArea2020MarExt/medicineLakeMarExt2020.csv')
    
  # Clean data -----
    
    # wetArea -----
  
      wet05 <- tbl_df(medLakeWet) %>%  
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
                                            
  
      wetExt05 <- tbl_df(medLakeMarWet) %>%  
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
          gather(month, wetHa,'3':'10')            
      
    # Hydrograph -----

      hydro05 <- tbl_df(medLakeHydro) %>%
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
      
      hydroExt05 <- tbl_df(medLakeMarHydro) %>%
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
      
    # Extract only March from the extended data -----
     
      wetExt06 <- wetExt05 %>%
        filter(month == 3)

      hydroExt06 <- hydroExt05 %>%
        filter(month == 3)
      
    # Remove the bad months from the OG dataset and 
    # add in the new data with the extended years -----

      # wetArea -----
      
        wet06 <- wet05 %>%
          filter(!(month == 3)) %>%
          bind_rows(wetExt06)
      
      # hydrograph -----
      
        hydro06 <- hydro05 %>%
          filter(!(month == 3 )) %>% 
          bind_rows(hydroExt06)  

#-------------------------------------------------------------------------------  
  # Utah Lake -----
  # Load data -----
  
    utahLakeHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/utahLake2020.csv')
    utahLakeMarHydro <- fread('01_data/14_2020data/hydroGraph2020MarExt/utahLakeMarExt2020.csv')
    utahLakeWet <- fread('01_data/14_2020data/wetArea2020MarExt/utahLake2020.csv')
    utahLakeMarWet <- fread('01_data/14_2020data/wetArea2020MarExt/utahLakeMarExt2020.csv')
    
  # Clean data -----
    
    # wetArea -----
  
      wet07 <- tbl_df(utahLakeWet) %>%  
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
                                            
  
      wetExt07 <- tbl_df(utahLakeMarWet) %>%  
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
          gather(month, wetHa,'3':'10')            
      
    # Hydrograph -----

      hydro07 <- tbl_df(utahLakeHydro) %>%
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
      
      hydroExt07 <- tbl_df(utahLakeMarHydro) %>%
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
      
    # Extract only March from the extended data -----
     
      wetExt08 <- wetExt07 %>%
        filter(month == 3)

      hydroExt08 <- hydroExt07 %>%
        filter(month == 3)
      
    # Remove the bad months from the OG dataset and 
    # add in the new data with the extended years -----

      # wetArea -----
      
        wet08 <- wet07 %>%
          filter(!(month == 3)) %>%
          bind_rows(wetExt08)
      
      # hydrograph -----
      
        hydro08 <- hydro07 %>%
          filter(!(month == 3 )) %>% 
          bind_rows(hydroExt08)
      
#-------------------------------------------------------------------------------  
  # Ruby Lake -----
  # Load data -----
  
    rubyLakeHydro <- fread('01_data/14_2020data/hydroGraph2020AprExt/rubyLake2020.csv')
    rubyLakeMarHydro <- fread('01_data/14_2020data/hydroGraph2020AprExt/rubyLakeAprExt2020.csv')
    rubyLakeWet <- fread('01_data/14_2020data/wetArea2020AprExt/rubyLake2020.csv')
    rubyLakeMarWet <- fread('01_data/14_2020data/wetArea2020AprExt/rubyLakeAprExt2020.csv')
    
  # Clean data -----
    
    # wetArea -----
  
      wet09 <- tbl_df(rubyLakeWet) %>%  
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
                                            
  
      wetExt09 <- tbl_df(rubyLakeMarWet) %>%  
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
          gather(month, wetHa,'3':'10')            
      
    # Hydrograph -----

      hydro09 <- tbl_df(rubyLakeHydro) %>%
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
      
      hydroExt09 <- tbl_df(rubyLakeMarHydro) %>%
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
      
    # Extract only March from the extended data -----
     
      wetExt10 <- wetExt09 %>%
        filter(month == 4)

      hydroExt10 <- hydroExt09 %>%
        filter(month == 4)
      
    # Remove the bad months from the OG dataset and 
    # add in the new data with the extended years -----

      # wetArea -----
      
        wet10 <- wet09 %>%
          filter(!(month == 4)) %>%
          bind_rows(wetExt10)
      
      # hydrograph -----
      
        hydro10 <- hydro09 %>%
          filter(!(month == 4)) %>% 
          bind_rows(hydroExt10)  
      
#===============================================================================
# Compile, clean, and format sites with zones -----

  # Malheur -----
  # Compile -----
    
    # Define folders -----
  
      malheurHydroFolder = '01_data/14_2020data/hydroGraph2020zones/malheur'
      malheurWetFolder = '01_data/14_2020data/wetArea2020zones/malheur'
      
   
    # Import all csv files in 'folder' and bind rows using dplyr and sapply

      files <- list.files(path = malheurHydroFolder, pattern = "*.csv", full.names = T)
      malheurHydro <- sapply(files, read_csv, simplify=FALSE) %>% 
      bind_rows(.id = "id")
      
      files <- list.files(path = malheurWetFolder, pattern = "*.csv", full.names = T)
      malheurWet <- sapply(files, read_csv, simplify=FALSE) %>% 
      bind_rows(.id = "id")
      
  # Tidy data -----    

    # wetArea -----
  
      malheurWet$year <- gsub("_", "", malheurWet$year)
      
        malheurWet01 <- tbl_df(malheurWet) %>%
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
          gather(month, wetHa,'3':'10') %>% 
          mutate(year = as.numeric(year))

    # Hydrograph -----
      
      malheurHydro$year <- gsub("_", "", malheurHydro$year)
  
      malheurHydro01 <- tbl_df(malheurHydro) %>%
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
        rename(period = period2) %>% 
        mutate(year = as.numeric(year))
#------------------------------------------------------------------------------- 
  # salt lake -----
  # Compile -----
    
    # Define folders -----
  
      saltHydroFolder = '01_data/14_2020data/hydroGraph2020zones/saltLake'
      saltWetFolder = '01_data/14_2020data/wetArea2020zones/saltLake'
      
   
    # Import all csv files in 'folder' and bind rows using dplyr and sapply

      files <- list.files(path = saltHydroFolder, pattern = "*.csv", full.names = T)
      saltHydro <- sapply(files, read_csv, simplify=FALSE) %>% 
      bind_rows(.id = "id")
      
      files <- list.files(path = saltWetFolder, pattern = "*.csv", full.names = T)
      saltWet <- sapply(files, read_csv, simplify=FALSE) %>% 
      bind_rows(.id = "id")
      
  # Tidy data -----    

    # wetArea -----
  
      saltWet$year <- gsub("_", "", saltWet$year)
    
      saltWet01 <- tbl_df(saltWet) %>%
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
        gather(month, wetHa,'3':'10') %>% 
        mutate(year = as.numeric(year))

    # Hydrograph -----
      
      saltHydro$year <- gsub("_", "", saltHydro$year)
  
      saltHydro01 <- tbl_df(saltHydro) %>%
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
        rename(period = period2) %>% 
        mutate(year = as.numeric(year))
      
#===============================================================================      
# Compile non-zone hydrograph files -----
  
  # Define folder -----
  
    hydrofolder = '01_data/14_2020data/hydroGraph2020'
   
  # Import all csv files in 'folder' and bind rows using dplyr and sapply

    files <- list.files(path = hydrofolder, pattern = "*.csv", full.names = T)
    hydroGra2020 <- sapply(files, read_csv, simplify=FALSE) %>% 
    bind_rows(.id = "id")

#-------------------------------------------------------------------------------     
# Compile non-zone wetArea files -----
  
  # Define folder -----
  
    wetfolder = '01_data/14_2020data/wetArea2020'
   
  # Import all csv files in 'folder' and bind rows using dplyr and sapply

    files <- list.files(path = wetfolder, pattern = "*.csv", full.names = T)
    wetArea2020 <- sapply(files, read_csv, simplify=FALSE) %>% 
    bind_rows(.id = "id")

#-------------------------------------------------------------------------------
# Tidy up the data -----

  # wetArea -----
  
    wetArea01 <- tbl_df(wetArea2020) %>%  
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
                                            
  
    # Hydrograph -----

    hydroGra01 <- tbl_df(hydroGra2020) %>%
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
#===============================================================================
# Bind all of the cleaned files together -----
    
  # Wet areas -----
    
    wetAll <- wetArea01 %>% 
      full_join(malheurWet01) %>% 
      full_join(saltWet01) %>% 
      full_join(wet02) %>% 
      full_join(wet04) %>% 
      full_join(wet06) %>% 
      full_join(wet08) %>% 
      full_join(wet10)
    
  # Hydrographs -----
    
    hydroAll <- hydroGra01 %>% 
        full_join(malheurHydro01) %>% 
        full_join(saltHydro01) %>% 
        full_join(hydro02) %>% 
        full_join(hydro04) %>% 
        full_join(hydro06) %>% 
        full_join(hydro08) %>% 
        full_join(hydro10)
    
#===============================================================================
# add ecoregion and huc info to tables -----  
    
  # Read in data -----
    
    ecoHUC <- fread('01_data/06_ecoHydro_HUCS/HUCsites.csv')
    
  # Join ecohydroregion/huc data to water tables -----
  
    wetAll02 <- wetAll %>%
      select('idPoly','siteName','ownAg','wetType', 'year', 'month', 'wetHa') %>%
      left_join(ecoHUC, by = "idPoly")
  
    hydroAll02 <- hydroAll %>%
      select('idPoly','siteName','ownAg','wetType', 'year', 'month', 'period', 'wetHa') %>%
      left_join(ecoHUC, by = "idPoly")

#===============================================================================    
# More data cleaning -----
# Fix spelling errors and fix ownAg errors -----  
  
  # Look at data -----
    
    str(wetAll02)
    nrow(wetAll02)
    head(wetAll02)

    head(hydroAll02)
    str(hydroAll02)
    nrow(hydroAll02)
  
  # Fix spelling errors -----
      
    # How many of these incorrect entries are there? -----
      
      table(wetAll02$ownAg)
      table(hydroAll02$ownAg)
      
      # How many different 'idPoly' are there? -----
      
        AreaWet <- filter(wetAll02, ownAg == 'wet')
        Hydrowet <- filter(hydroAll02, ownAg == 'wet')
        
        table(AreaWet$idPoly)
        table(Hydrowet$idPoly)
      
    # Looks like there's only two polygons that have been mislabeled and have
    # been iterated over time to get so many entries. I cross-checked the 'idPoly
    # with QGIS and they aren't essential polygons (they are in the same location)
    # so I think it's okay to delete them.
  
  # Filter out the mislabeled entries and fix the spelling errors -----
    
    wetAll03 <- wetAll02 %>%        # using the df wetArea
      filter(ownAg != 'wet') %>%    # keep everything in df except for the rows where ownAg = 'wet'
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public',  # When the string 'Pub' is detected in the column 'ownAg', replace it with 'Public'
                             str_detect(ownAg, 'Pri') ~ 'Private',
                             TRUE ~ 'OTHER'))
    
    hydroAll03 <- hydroAll02 %>%
      filter(ownAg != 'wet') %>%
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public', 
                             str_detect(ownAg, 'Pri') ~ 'Private',
                             TRUE ~ 'OTHER'))
    # check your work -----
      unique(wetAll03$ownAg)
      unique(hydroAll03$ownAg)
      # table(wetArea$ownAg)
           
#===============================================================================    
# Even more cleaning -----
# Fix additional spelling errors, change 'wetType' values 
# based on 'idPoly', remove 'Ag' -----
    
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
    
      wetAll04 <- wetAll03 %>% 
          filter(wetType != 'Ag') %>% 
          filter(wetType != 'res') %>% 
          mutate(wetType = ifelse(idPoly %in% polys, 'res', wetType),
                 ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro)) %>% 
          filter(wetType != 'res') %>% 
          mutate(month = as.numeric(month))
          
      
       hydroAll04 <- hydroAll03 %>% 
          filter(wetType != 'Ag') %>% 
          filter(wetType != 'res') %>% 
          mutate(wetType = ifelse(idPoly %in% polys, 'res', wetType),
                 ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro)) %>% 
          filter(wetType != 'res') %>% 
          mutate(month = as.numeric(month))
 
#===============================================================================    
# Remove 2020 data from OG data frame and replace with new 2020 data -----
  
  # Read in OG data -----
       
    wetOG <- fread('01_data/08_final_hydro_data/wetAreaFin02.csv')
    hydroOG <- fread('01_data/08_final_hydro_data/hydroGraFin02.csv')
    
  # Compare 2020 data? -----
    
    # wetBad <- wetOG %>% 
    #   filter(year == '2020')
    
  # Remove 2020 from OG data and add in new 2020
    
    wet2020 <- wetOG %>% 
      filter(year != '2020') %>% 
      full_join(wetAll04)

    hydro2020 <- hydroOG %>% 
      filter(year != '2020') %>% 
      full_join(hydroAll04)
    
#===============================================================================    
# Remove March data from sites that didn't have March imagery -----    
  
  # Sites that didn't have March imagery -----
    
    sites <- c('Red Rock Lake NWR', 'Grays Lake NWR', 'Walden Reservoir', 'Bear Lake NWR')
    
  # Remove sites without March imagery from the datasets -----
    
    wetFinal <- wet2020[!(wet2020$month == 3 & wet2020$siteName %in% sites)]
    
    hydroFinal <- hydro2020[!(hydro2020$month == 3 & hydro2020$siteName %in% sites)]
    
  # Write data and pray that I don't have other mistakes to clean up -----
    
    fwrite(wetFinal, "01_data/08_final_hydro_data/wetAreaFin03.csv")
    fwrite(hydroFinal, "01_data/08_final_hydro_data/hydroGraFin03.csv")
    
#===============================================================================
# I want to change the ecoregion and HUC to align with where the centroid of the
# colony circle lies and not the region that that the individual polygons lie. 
# This might simplify some things...
   
  # Read in data tables -----
    
    wet01 <- fread("01_data/08_final_hydro_data/wetAreaFin03.csv")
    hydro01 <- fread("01_data/08_final_hydro_data/hydroGraFin03.csv")
     
  # Correct ecoregions -----
  # Some sites encompass multiple ecoregions
  # Change to one region per site - pick the region in which the colony coordinates lie -----
    
    GBsites <- c('Blackfoot Reservoir', 'Chester Hill Reservoir', 'Chewaucan Marshes',
                 'Honey Lake', 'Silvies River', 'Swan Lake', 'Washoe Lake', 'Summer Lake WMA')
      
    PNWsites <- c('Goose Lake')
    
    SRsites <- c('Bear Lake NWR')
    
    MRsites <- c('Grays Lake NWR', 'Lake Helena WMA')
    
    wetEco <- wet01 %>% 
      mutate(ecoHydro = ifelse(siteName %in% GBsites, 'Great Basin-Colorado Plateau', ecoHydro),
             ecoHydro = ifelse(siteName %in% PNWsites, 'Pacific NW', ecoHydro),
             ecoHydro = ifelse(siteName %in% MRsites, 'Middle Rockies', ecoHydro),
             ecoHydro = ifelse(siteName %in% SRsites, 'Southern Rockies and Basins', ecoHydro))
    
    hydroEco <- hydro01 %>% 
      mutate(ecoHydro = ifelse(siteName %in% GBsites, 'Great Basin-Colorado Plateau', ecoHydro),
             ecoHydro = ifelse(siteName %in% PNWsites, 'Pacific NW', ecoHydro),
             ecoHydro = ifelse(siteName %in% MRsites, 'Middle Rockies', ecoHydro),
             ecoHydro = ifelse(siteName %in% SRsites, 'Southern Rockies and Basins', ecoHydro))
  
  # Same problem as above, but with HUCS
  # I went into QGIS and repeated the spatial join with the HUCS and sites so
  # there's only one HUC per site. 
  # Load in the new HUC data and fix the data table-----
    
    siteHUCS <- fread('01_data/17_sites_HUCS/siteNames_HUCS.csv')
    
    wetHUC <- wetEco %>%
      rename(HUCbad = HUC4) %>%
      rename(NAMEbad = NAME) %>% 
      left_join(siteHUCS, by = 'siteName') %>% 
      select('idPoly', 'siteName', 'ownAg', 'wetType', 'year', 'month', 'wetHa', 'ecoHydro', 'HUC4')
    
    hydroHUC <- hydroEco %>%
      rename(HUCbad = HUC4) %>%
      rename(NAMEbad = NAME) %>% 
      left_join(siteHUCS, by = 'siteName') %>% 
      select('idPoly', 'siteName', 'ownAg', 'wetType', 'year', 'month', 'period', 'wetHa', 'ecoHydro', 'HUC4')
    
  # Write data and pray that I don't have other mistakes to clean up -----
    
    fwrite(wetHUC, "01_data/08_final_hydro_data/wetAreaFin04.csv")
    fwrite(hydroHUC, "01_data/08_final_hydro_data/hydroGraFin04.csv") 

#===============================================================================
# Red Rock Lakes NWR has been duplicated with spelling errors.
# Are all of the polygons duplicated as well? -----
  
  # Read data -----
    
    hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin04.csv')
    wetArea <- fread('01_data/08_final_hydro_data/hydroGraFin04.csv')
    
  # Subset -----
    
    rrlNWR <- hydroGra %>%
      filter(siteName == 'Red Rock Lake NWR' | siteName == 'Red Rock Lakes NWR') %>% 
      group_by(idPoly, siteName, year) %>% 
      summarize(wetSum = sum(wetHa)) %>%
      ungroup()
    
  # Compare -----
  
    rrlNWR01 <- rrlNWR %>%
      filter(siteName == 'Red Rock Lake NWR')  
    
    rrlNWR02 <- rrlNWR %>%
      filter(siteName == 'Red Rock Lakes NWR')  
    
    # Notes:
    # Looks like there's about 11 more polygons in the 'Red Rock Lake NWR'
    # compared to the 'Red Rock Lakes NWR'; all others look like duplicates.
    # I'll remove the 'Red Rock Lakes NWR' from the dataset. 
    
      hydroGra01 <- hydroGra %>% 
        filter(siteName != 'Red Rock Lakes NWR')
      
      wetArea01 <- wetArea %>% 
         filter(siteName != 'Red Rock Lakes NWR')
      
  # Write the data -----
      
    fwrite(hydroGra01, "01_data/08_final_hydro_data/hydroGraFin05.csv")
    fwrite(wetArea01, "01_data/08_final_hydro_data/wetAreaFin05.csv")
    
#===============================================================================
# Blitzen River and Malheur Lake have the same coordinates and therefore have
# duplicated the polygon and results - need to delete one of them.
    
    hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin05.csv')
    wetArea <- fread('01_data/08_final_hydro_data/hydroGraFin05.csv')
    
  # Subset -----
    
    dups <- hydroGra %>%
      filter(siteName == 'Blitzen River' | siteName == 'Malheur Lake') %>% 
      group_by(idPoly, siteName, year) %>% 
      summarize(wetSum = sum(wetHa)) %>%
      ungroup()
    
  # Compare -----
  
    br <- dups %>%
      filter(siteName == 'Blitzen River')  
    
    ml <- dups %>%
      filter(siteName == 'Malheur Lake')  
    
    # Notes:
    # There's the same number of polygons with the same values for wetHa in 
    # both sites. I'm going to delete Blitzen River.  
    
      hydroGra01 <- hydroGra %>% 
        filter(siteName != 'Blitzen River')
      
      wetArea01 <- wetArea %>% 
         filter(siteName != 'Blitzen River')
      
  # Write the data -----
      
    fwrite(hydroGra01, "01_data/08_final_hydro_data/hydroGraFin06.csv")
    fwrite(wetArea01, "01_data/08_final_hydro_data/wetAreaFin06.csv")
    
#===============================================================================
# Fix Mojave desert spelling error -----
    
  hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin06.csv') 
    
  hydroGra01 <- hydroGra %>% 
    mutate(ecoHydro = ifelse(ecoHydro == 'Mojave-Sanoran Deserts', 'Mojave-Sonoran Deserts', ecoHydro)) 
  
  fwrite(hydroGra01, '01_data/08_final_hydro_data/hydroGraFin07.csv')
  
#===============================================================================
# Format Seasonal wetHa for RF model ----- 
  
  hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin07.csv')
    
  hydroGra01 <- hydroGra %>% 
    group_by(siteName, ecoHydro, HUC4, year) %>% 
      summarize(wetSum = sum(wetHa)) %>%
      ungroup()
    
  fwrite(hydroGra01, '01_data/11_RF_vars_clean/ibisSeas.csv')  
    
    
    
    
    
    
    
    
    
       
    
   