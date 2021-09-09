################################################################################
# Task: clean and format annual wetland data for RF model
# 
# Jan 02 2020
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('leaflet')
  library('sf')
  library('sp')
  library('ggridges')
  library('gridExtra')
  
#===============================================================================
# Compile Zone files
  
  # Define folder -----

    # folder = '01_data/15_wetAnnual/saltLakeAnnual'                            # un-comment whichever folder you want to run and rename var below appropriately
    # folder = '01_data/15_wetAnnual/malheurAnnual'
    folder = '01_data/15_wetAnnual/ibisAnnual'
  
  # Import all csv files in 'folder' and bind rows using dplyr and sapply -----
  
    files <- list.files(path = folder, pattern = "*.csv", full.names = T)
    sitesAnnual <- sapply(files, read_csv, simplify=FALSE) %>%                   # change the name here: 'saltLake', 'malheur', or 'sitesAnnual'
    bind_rows(.id = "id")
    
    saltLake <- tbl_df(saltLake)
    malheur <- tbl_df(malheur)
    sitesAnnual <- tbl_df(sitesAnnual)

#===============================================================================
# Tidy data ----
    
  # Join newly merged csv files and reformat -----
    
    annualAll <- sitesAnnual %>% 
      full_join(saltLake) %>% 
      full_join(malheur) %>% 
      select('idPoly','siteName', 'ownAg', 'wetType', '1984':'2020') %>% 
      gather(year, wetHa, '1984':'2020')
    
  # Clean up data ----
    
    polys <- c(73121, 73523, 73528, 73528, 73529, 73522, 73124, 73119,  # Lake Helena WMA
                 73388, 73118, 73527, 73340, 73526,                       # Lake Helena WMA
                 63232, 63000, 63002, 63003, 63049, 63229,                # Freezeout Lake
                 63533,                                                   # Grass Lake NWR
                 90390,                                                   # Red Rock Lakes NWR
                 90846, 90845,                                            # Spidel WPA
                 # 8153, 8154, 1535,                                        # Tule and Klamath Lake
                 # 13680, 13758,                                            # Fairchild Swamp
                 99502)                                                   # Canvasback Club                                    
               
    
    annualClean <- annualAll %>%
      filter(ownAg != 'wet') %>%                                                # I accidently mislabeled two polygons (identified in previous scripts and QGIS - they are small and can be deleted)
      mutate(ownAg = case_when(str_detect(ownAg, 'Pub') ~ 'Public',             # When the string 'Pub' is detected in the column 'ownAg', replace it with 'Public'
                             str_detect(ownAg, 'Pri') ~ 'Private',              # I mispelled 'public' a few times
                             TRUE ~ 'OTHER')) %>% 
      mutate(wetType = ifelse(idPoly %in% polys, 'res', wetType)) %>%           # re-classify specific polygons to 'res'  
      filter(wetType != 'res') %>%                                              # remove all the reservoirs
      filter(wetType != 'Ag')                                                   # remove the dry agriculture 
        
      

    # Remove 'wetType' == 'Ag'. 
    # Change 'wetType' to 'res' based on list of 'idPoly' values.
    # Change 'Middel Rockies' to 'Middle Rockies' -----
    
      wetAll04 <- wetAll03 %>% 
          filter(wetType != 'Ag') %>% 
          filter(wetType != 'res') %>% 
          mutate(wetType = ifelse(idPoly %in% polys, 'res', wetType),
                 ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro)) %>% 
          filter(wetType != 'res') %>%                                          # remove all the reservoirs
          filter(wetType != 'Ag')                                               # remove the dry agriculture 
        
     
#===============================================================================
# Join ecoregion / HUC data to annual table
   
  # Read in ecoregion / HUC data -----
    
    HUCs <- fread('01_data/06_ecoHydro_HUCS/HUCsites.csv')
    
  # Join to annual wetland data -----
    
    ecoWet <- annualClean %>% 
      left_join(HUCs, by = "idPoly") %>% 
      mutate(ecoHydro = ifelse(ecoHydro == 'Middel Rockies', 'Middle Rockies', ecoHydro))
    
    test <- siteWat %>% 
      filter(siteName == "Washoe Lake")
    
  # Some sites encompass multiple ecoregions
  # Change to one region per site - pick the region in which the colony coordinates lie -----
    
    GBsites <- c('Blackfoot Reservoir', 'Chester Hill Reservoir', 'Chewaucan Marshes',
                 'Honey Lake', 'Silvies River', 'Swan Lake', 'Washoe Lake', 'Summer Lake WMA')
      
    PNWsites <- c('Goose Lake')
    
    SRsites <- c('Bear Lake NWR')
    
    MRsites <- c('Grays Lake NWR', 'Lake Helena WMA')
    
    ecoWet02 <- ecoWet %>% 
      mutate(ecoHydro = ifelse(siteName %in% GBsites, 'Great Basin-Colorado Plateau', ecoHydro),
             ecoHydro = ifelse(siteName %in% PNWsites, 'Pacific NW', ecoHydro),
             ecoHydro = ifelse(siteName %in% MRsites, 'Middle Rockies', ecoHydro),
             ecoHydro = ifelse(siteName %in% SRsites, 'Southern Rockies and Basins', ecoHydro))
    
  # Same problem as above, but with HUCS
  # I went into QGIS and repeated the spatial join with the HUCS and sites so
  # there's only one HUC per site. 
  # Time to load in the new HUC data and fix the data table-----
    
    siteHUCS <- fread('01_data/17_sites_HUCS/siteNames_HUCS.csv')
    
    ecoWet03 <- ecoWet02 %>%
      rename(HUCbad = HUC4) %>%
      rename(NAMEbad = NAME) %>% 
      left_join(siteHUCS, by = 'siteName') %>% 
      select('idPoly', 'siteName', 'ownAg', 'wetType', 'year', 'wetHa', 'ecoHydro', 'HUC4')
      
  # partition and sum over different groups for each year -----
    
    # Overall wetHa per site -----
    
      siteWat <- ecoWet03 %>% 
        group_by(siteName, year, ecoHydro, HUC4) %>% 
        summarize(wetHa=sum(wetHa))

    # WetHa per site by ownership -----
    
      ownWat <- ecoWet03 %>% 
        group_by(siteName, year, ownAg, ecoHydro, HUC4) %>% 
        summarize(wetHa=sum(wetHa))
      
      pubWat <- ownWat %>% 
        filter(ownAg == "Public") %>% 
        spread(ownAg, wetHa)
      
      privWat <- ownWat %>% 
        filter(ownAg == "Private") %>% 
        spread(ownAg, wetHa)
      
    # Add each of the sums as column in new data frame -----
      
      pubWat02 <- pubWat %>% 
        select(siteName, year, Public)
      
      privWat02 <- privWat %>% 
        select(siteName, year, Private)
      
      annualFin <- siteWat %>% 
        left_join(pubWat02, by = c('siteName', 'year', 'ecoHydro')) %>% 
        left_join(privWat02, by = c('siteName', 'year', 'ecoHydro'))
      
    # write files -----
      
      fwrite(annualFin, '01_data/16_wetAnnual_clean/wetAnnual.csv')
      
      
      
  
  
  
  
  
  
  
  
  
  