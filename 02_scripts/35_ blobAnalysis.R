#===============================================================================
# Task: Re-run the analysis for blobs
#
# Date: 7/6/2021
#===============================================================================

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
  
  # Water Data -----
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin02.csv')
  
  # IDpoly's linked to Blob ID's
  polyBlob <-fread("01_data/24_siteBlobIDs/polyBlobID.csv")
  
#===============================================================================
# Delete repetitive columns in the polyBlob data -----
  
  polyBlob2 <- polyBlob[ , c(1:2, 6:7)]
  
# Summarize the water data and join blob ID info -----
  
  hydroTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 'T1',                                    
              term = replace(term, year >2003, 'T2')) %>%
      na.omit()
  
  BlobHydro <- hydroTerm %>% 
        group_by(idPoly, siteName, year, term, ecoHydro) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(polyBlob2, by = c('idPoly', 'siteName'))
  
# Group and sum the water by blob -----
  
  blobChange <- BlobHydro %>% 
    group_by(term, blobID, year) %>% 
    summarise(wetSum = sum(wetHa)) %>%
    ungroup() %>%                   
    group_by(term, blobID) %>%              
    summarise(wetMean = mean(wetSum)) %>% 
    spread(term, wetMean) %>%                          
    mutate(change = ((T1-T2)/T1)*-1) %>% 
    na.omit()
  
  BlobHydro2 <- as.data.frame(BlobHydro)
  
# Wilcox test by BlobID -----
  
  blobWilcoxTest <- BlobHydro2 %>%
      group_by(term, blobID, year) %>%
      summarise(wetSum = sum(wetHa)) %>%   
      split(.$blobID) %>%  
      map(~wilcox.test(wetSum ~ term, data = .x, conf.int = T)) %>%       
      map_df(broom::tidy, .id = 'blobID')
    
# Combine wilcoxon results with p1, p2 diff results -----
  
    blobWilcoxTest$blobID <- as.numeric(as.character(blobWilcoxTest$blobID))
    
    blobWilcoxChange <- blobChange %>% 
      full_join(blobWilcoxTest) %>%                      
      select(blobID, T1, T2, change, p.value) 
    
    blobCat <- blobWilcoxChange %>% 
      mutate(ChangeCat = case_when(change < 0 & p.value < 0.05 ~ 'SigDecrease',
                                   change < 0 & p.value > 0.05 ~ 'NonSigDecrease',
                                   change > 0 ~ 'Stable_Increase')) %>% 
      rename('pValue' = 'p.value') %>% 
      mutate(colorCode = case_when(ChangeCat == 'SigDecrease' ~ 2,
                                   ChangeCat == 'NonSigDecrease' ~ 1,
                                   ChangeCat == 'Stable_Increase'~ 0))

    fwrite(blobCat, "03_output/blobChange.csv")
  
  