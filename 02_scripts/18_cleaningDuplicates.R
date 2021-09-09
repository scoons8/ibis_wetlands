################################################################################
# Task: Delete the duplicated polygons from the data. I cleaned up the sites in 
# QGIS - I deleted all of the polygons that overlapped and/or were duplicates.
# 
# Jan 16 2020
# Update 7/9/2021: used the wetPolyClean03 file instead of the wetPolyClean01 
# file the wetPolyClean03 file has a way for dealing with the Klamath polygons
# Nevermind!! The polygons with an id of 0 were removed at some point so this
# whole effort has been a moot point.
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')

#===============================================================================
# Clean new table with corrected idPolys -----

  # Read in data -----

    #Deprecated: datClean <- fread('01_data/18_duplicatesCleaned/wetPolyClean01.csv')
    datClean <- fread('01_data/18_duplicatesCleaned/wetPolyClean03.csv')
    test <- datClean[(datClean$idPoly == 0), ]
    
  # Remove polygons classfied as Ag or Res.
    
    datClean01 <- datClean %>%
      filter(wetType != 'res') %>% 
      filter(wetType != 'Ag') %>% 
      select(idPoly, siteName, ownAg, wetType)
    
  # Updated Cleaning -----
    
    datClean02 <- datClean01[(datClean01$idPoly == 0), ]
    datClean03 <- datClean02[3, ]
    datClean04 <- datClean01[!(datClean01$idPoly == 0), ] %>% 
      rbind(datClean03)
    
#===============================================================================
# Filter old data table to only include the siteIDs found in the new data table.
# The resulting dataframe is the one I will use for regional analyses. I will 
# need to make another one for site-level analyses (in which case, a polygon may
# be repeated for every site that is in). -----
    
  # Read in hydrograph data -----
    
    hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin04.csv')

# Add the missing year for Rock Creek -----
  # read data -----

    rc <- fread('01_data/20_rockCreek2015/rockCreek2015.csv')

    rc1 <- rc %>%
      filter(wetType != 'res') %>%
      filter(wetType != 'ag') %>%
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
      mutate(ecoHydro = 'Great Basin-Colorado Plateau',
              HUC4 = 1604,
              month = as.integer(month))

    # join data ----

    hydroGra2 <- hydroGra %>%
      full_join(rc1)

  # Clean, format, bind data -----
    
    hydro01 <- hydroGra2 %>%                     
      distinct() %>%                               # Removes any duplicated rows
      semi_join(datClean04, by = 'idPoly')         # removes all rows that do not match 'idPoly' in datClean (Was datClean01 in the original code)
    
#===============================================================================
# Additional cleaning -----
    
  # Red Rock Lakes is spelled multiple ways 
  # and Mojave region is spelled wrong -----
    
    hydro02 <- hydro01 %>% 
    mutate(siteName = ifelse(siteName == 'Red Rock Lake NWR', 
                                         'Red Rock Lakes NWR', siteName),
            ecoHydro = ifelse(ecoHydro == 'Mojave-Sanoran Deserts', 
                                          'Mojave-Sonoran Deserts', ecoHydro)) 
    
# Write the files -----
    
  #Deprecated: fwrite(hydro02, '01_data/19_hydroCleaned/hydroGraBlobFin02.csv')
  fwrite(hydro02, '01_data/19_hydroCleaned/hydroGraBlobFin03.csv')

#===============================================================================
# Filter old data table to only include the siteIDs found in the new data table.
# The resulting dataframe is the one I will use for site-level analyses. 
    
  # Read in data -----
    
    # hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin04.csv')          # old hydrograph data
    sites <- fread('01_data/18_duplicatesCleaned/singleSites.csv')              # cleaned sites (only a few places needed to be cleaned up)
    
  # Remove polygons classfied as Ag or Res in the new table -----
    
    sites02 <- sites %>%
      filter(wetType != 'res') %>% 
      filter(wetType != 'Ag') %>% 
      select(idPoly, siteName, ownAg, wetType)
    
  # Clean, formatt, bind data -----
    
    hydro01 <- hydroGra2 %>% 
      distinct() %>% 
      mutate(siteName = ifelse(siteName == 'Red Rock Lake NWR', 
                                         'Red Rock Lakes NWR', siteName),
            ecoHydro = ifelse(ecoHydro == 'Mojave-Sanoran Deserts', 
                                          'Mojave-Sonoran Deserts', ecoHydro)) %>% 
      semi_join(sites, by = 'idPoly')
    
  # write the data -----
    
    #Deprecated: fwrite(hydro01, '01_data/19_hydroCleaned/hydroGraSiteFin02.csv')
    fwrite(hydro01, '01_data/19_hydroCleaned/hydroGraSiteFin03.csv')
    
#===============================================================================



    
    
    
    
    
    
    
    
    
    
    
    