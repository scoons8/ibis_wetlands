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
  
    RF<-fread('01_data/11_RF_vars_clean/RFvars06.csv')
  
  # Reformat -----
  
    tmin01 <- RF %>%
      na.omit() %>% 
      filter(year > 1982) %>% 
      select(siteName, ecoHydro, HUC4, tmin, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    tmin02 <- tmin01 %>% 
      select(HUC4, tmin, year, term) %>% 
      distinct()
    
    tminWilcox <- tmin02 %>%
      group_by(term, HUC4, year) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(tmin ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    tmin03 <- tmin02 %>% 
      group_by(term, HUC4) %>% 
      summarise(tminMean = mean(tmin)) %>% 
      spread(term, tminMean) %>% 
      mutate(perDif = ((t1-t2)/abs(t1))*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    tmin04 <- tmin02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(tmin)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    tmin05 <- tmin03 %>% 
      full_join(tmin04, by = 'HUC4') %>% 
      full_join(tminWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    tmin06 <- tmin05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
  # make linear model into a dataframe -----
  
    dataList <-split(tmin02, tmin02$HUC4)

    # Run the linear model for each HUC and make dataframe of p-values -----
    
      lmResults <- lapply(dataList,function(x){
           y <- summary(lm(tmin ~ year,data = x))
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
      
      tmin07 <- tmin06 %>% 
        full_join(lmTable, by = 'HUC4')

#------------------------------------
# Make table summarizing data by ecoregion -----
# Will essentially need to take the means of tmin06 table
      
  # Make table with ecoregions and HUCS-----
  
    tmin08 <- tmin01 %>% 
      select(HUC4, ecoHydro) %>% 
      distinct() %>% 
      mutate(HUC4 = as.character(HUC4))
      
  # Bind tables together -----
      
    tmin09 <- tmin08 %>% 
      full_join(tmin07, by = 'HUC4')
  
  # summarize data (take means and count) -----
      
    tmin10 <- tmin09[, 2:8]
      
    tmin11<- aggregate(.~ecoHydro, data = tmin10, mean)
    
  # number of HUCs with p < 0.05 Wilxon results by ecoregion -----
    
    wSig <-tmin09 %>% 
      filter(p.value < 0.05)  # 9 significant results
    
    lSig <- tmin09 %>% 
      filter(pValue_lm < 0.05) # 18 significant results
    
  # Number of total HUCs in each ecoregion ------
    
    # a <- nrow(tmin09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    # b <- nrow(tmin09[ecoHydro == 'Middle Rockies']) # 5
    # c <- nrow(tmin09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    # d <- nrow(tmin09[ecoHydro == 'Northern Plains']) # 5
    # e <- nrow(tmin09[ecoHydro == 'Northern Rockies']) # 1
    # f <- nrow(tmin09[ecoHydro == 'Pacific NW']) # 2
    # g <- nrow(tmin09[ecoHydro == 'Southern Plains']) # 2
    # h <- nrow(tmin09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
  # Fraction of significant results for HUCs -----
  
    # Wilcoxon -----
    
      nrow(subset(tmin09, p.value < 0.05)) #& ecoHydro == 'Middle Rockies'))
      wilSig <- c('7/12', '1/5', '1/1', '0/5', '0/1', '0/2', '0/2', '0/7')
  
      # GB: 7/12
      # MR: 1/5
      # MD: 1/1
      # NP: 0/5
      # NR: 0/1
      # PNW: 0/2
      # SP: 0/2
      # SR: 0/7
    
    # Linear model -----
    
      nrow(subset(tmin09, pValue_lm < 0.05))# & ecoHydro == 'Middle Rockies'))
      lmSig <- c('9/12', '1/5', '1/1', '0/5', '0/1', '2/2', '2/2', '3/7')
    
      # GB: 9/12
      # MR: 1/5
      # MD: 1/1
      # NP: 0/5
      # NR: 0/1
      # PNW: 2/2
      # SP: 2/2
      # SR: 3/7
      
  # Join fractions to main table -----
      
    tmin12 <- cbind(tmin11, wilSig, lmSig)
      
  # Label the rows as belonging to tmin so I can bind it together with other 
  # vars later -----
      
    climVar <- c('tmin')
    tmin13 <- cbind(climVar, tmin12)
    
  # write table -----
    
    fwrite(tmin13, '03_output/tminTable.csv')
    
#------------------------------------
# Graph variable over time -----
  
  test <- tmin02 %>%
      group_by(term, HUC4, year) %>%                   
      summarise(tminSum = sum(tmin)) %>% 
      ungroup() %>% 
      filter(HUC4 == '1102') 
      
  ggplot(test, aes(x = year, y = tminSum)) +
          geom_line(size = 1, alpha = .5) +
          geom_smooth(method = 'lm', size=.2)
  
  