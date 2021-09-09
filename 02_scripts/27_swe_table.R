################################################################################
# Task: Making summary table of evapotranspiration data
# 
# March 7 2021
################################################################################

# Packages -----

  library("tidyverse")
  library('readr')
  library('ggplot2')
  library('data.table')
  library('stringr')
  library('gridExtra')
  library('lme4')
  library('broom')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }

#===============================================================================
# Evapotranspiration summarized by HUC4-----
  
  # Read in data -----
  
    RF<-fread('01_data/11_RF_vars_clean/RFvars05.csv')
  
  # Reformat -----
  
    swe01 <- RF %>%
      na.omit() %>% 
      filter(year > 1982) %>% 
      select(siteName, ecoHydro, HUC4, swe, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    swe02 <- swe01 %>% 
      select(HUC4, swe, year, term) %>% 
      distinct()
    
    sweWilcox <- swe02 %>%
      group_by(term, HUC4, year) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(swe ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    swe03 <- swe02 %>% 
      group_by(term, HUC4) %>% 
      summarise(sweMean = mean(swe)) %>% 
      spread(term, sweMean) %>% 
      mutate(perDif = ((t1-t2)/t1)*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    swe04 <- swe02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(swe)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    swe05 <- swe03 %>% 
      full_join(swe04, by = 'HUC4') %>% 
      full_join(sweWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    swe06 <- swe05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
  # make linear model into a dataframe -----
  
    dataList <-split(swe02, swe02$HUC4)

    # Run the linear model for each HUC and make dataframe of p-values -----
    
      lmResults <- lapply(dataList,function(x){
           y <- summary(lm(swe ~ year,data = x))
           pValue <- y$coefficients[2,4]
           data.frame(pValue)
      })
      lmResultTable <- do.call(rbind,lmResults)
      
      # add HUC4 indicators -----
      
        lmResultTable$HUC4 <- names(dataList)
        
        lmResultTable 
        
    # clean up lm table -----
        
      lmTable <- lmResultTable %>% 
          select('pValue', 'HUC4')
      colnames(lmTable) <- c('pValue_lm', 'HUC4')
      
    # Bind to Wilcoxon table -----
      
      swe07 <- swe06 %>% 
        full_join(lmTable, by = 'HUC4')

#------------------------------------
# Make table summarizing data by ecoregion -----
# Will essentially need to take the means of swe06 table
      
  # Make table with ecoregions and HUCS-----
  
    swe08 <- swe01 %>% 
      select(HUC4, ecoHydro) %>% 
      distinct() %>% 
      mutate(HUC4 = as.character(HUC4))
      
  # Bind tables together -----
      
    swe09 <- swe08 %>% 
      full_join(swe07, by = 'HUC4')
  
  # summarize data (take means and count) -----
      
    swe10 <- swe09[, 2:8]
      
    swe11<- aggregate(.~ecoHydro, data = swe10, mean)
    
  # number of HUCs with p < 0.05 Wilxon results by ecoregion -----
    
    wSig <-swe09 %>% 
      filter(p.value < 0.05)  # 0 significant results
    
    lSig <- swe09 %>% 
      filter(pValue_lm < 0.05) # 2 significant results
    
  # Number of total HUCs in each ecoregion ------
    
    # a <- nrow(swe09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    # b <- nrow(swe09[ecoHydro == 'Middle Rockies']) # 5
    # c <- nrow(swe09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    # d <- nrow(swe09[ecoHydro == 'Northern Plains']) # 5
    # e <- nrow(swe09[ecoHydro == 'Northern Rockies']) # 1
    # f <- nrow(swe09[ecoHydro == 'Pacific NW']) # 2
    # g <- nrow(swe09[ecoHydro == 'Southern Plains']) # 2
    # h <- nrow(swe09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
  # Fraction of significant results for HUCs -----
  
    # Wilcoxon -----
    
      nrow(subset(swe09, p.value < 0.05)) #& ecoHydro == 'Middle Rockies'))
      wilSig <- c('0/12', '0/5', '0/1', '0/5', '0/1', '0/2', '0/2', '0/7')
  
      # GB: 0/12
      # MR: 0/5
      # MD: 0/1
      # NP: 0/5
      # NR: 0/1
      # PNW: 0/2
      # SP: 0/2
      # SR: 0/7
    
    # Linear model -----
    
      nrow(subset(swe09, pValue_lm < 0.05))# & ecoHydro == 'Middle Rockies'))
      lmSig <- c('0/12', '0/5', '0/1', '0/5', '0/1', '0/2', '0/2', '0/7')
    
      # GB: 0/12
      # MR: 0/5
      # MD: 0/1
      # NP: 0/5
      # NR: 0/1
      # PNW: 0/2
      # SP: 0/2
      # SR: 0/7
      
  # Join fractions to main table -----
      
    swe12 <- cbind(swe11, wilSig, lmSig)
      
  # Label the rows as belonging to swe so I can bind it together with other 
  # vars later -----
      
    climVar <- c('swe')
    swe13 <- cbind(climVar, swe12)
    
  # write table -----
    
    fwrite(swe13, '03_output/sweTable.csv')
    
#------------------------------------
# Graph variable over time -----
  
  test <- swe02 %>%
      group_by(term, HUC4, year) %>%                   
      summarise(sweSum = sum(swe)) %>% 
      ungroup() %>% 
      filter(HUC4 == '1102') 
      
  ggplot(test, aes(x = year, y = sweSum)) +
          geom_line(size = 1, alpha = .5) +
          geom_smooth(method = 'lm', size=.2)
  
  