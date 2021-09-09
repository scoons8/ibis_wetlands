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
  
    pr01 <- RF %>%
      na.omit() %>% 
      filter(year > 1982) %>% 
      select(siteName, ecoHydro, HUC4, pr, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    pr02 <- pr01 %>% 
      select(HUC4, pr, year, term) %>% 
      distinct()
    
    prWilcox <- pr02 %>%
      group_by(term, HUC4, year) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(pr ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    pr03 <- pr02 %>% 
      group_by(term, HUC4) %>% 
      summarise(prMean = mean(pr)) %>% 
      spread(term, prMean) %>% 
      mutate(perDif = ((t1-t2)/t1)*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    pr04 <- pr02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(pr)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    pr05 <- pr03 %>% 
      full_join(pr04, by = 'HUC4') %>% 
      full_join(prWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    pr06 <- pr05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
  # make linear model into a dataframe -----
  
    dataList <-split(pr02, pr02$HUC4)

    # Run the linear model for each HUC and make dataframe of p-values -----
    
      lmResults <- lapply(dataList,function(x){
           y <- summary(lm(pr ~ year,data = x))
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
      
      pr07 <- pr06 %>% 
        full_join(lmTable, by = 'HUC4')

#------------------------------------
# Make table summarizing data by ecoregion -----
# Will essentially need to take the means of pr06 table
      
  # Make table with ecoregions and HUCS-----
  
    pr08 <- pr01 %>% 
      select(HUC4, ecoHydro) %>% 
      distinct() %>% 
      mutate(HUC4 = as.character(HUC4))
      
  # Bind tables together -----
      
    pr09 <- pr08 %>% 
      full_join(pr07, by = 'HUC4')
  
  # summarize data (take means and count) -----
      
    pr10 <- pr09[, 2:8]
      
    pr11<- aggregate(.~ecoHydro, data = pr10, mean)
    
  # number of HUCs with p < 0.05 Wilxon results by ecoregion -----
    
    wSig <-pr09 %>% 
      filter(p.value < 0.05)  # 0 significant results
    
    lSig <- pr09 %>% 
      filter(pValue_lm < 0.05) # 2 significant results
    
  # Number of total HUCs in each ecoregion ------
    
    # a <- nrow(pr09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    # b <- nrow(pr09[ecoHydro == 'Middle Rockies']) # 5
    # c <- nrow(pr09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    # d <- nrow(pr09[ecoHydro == 'Northern Plains']) # 5
    # e <- nrow(pr09[ecoHydro == 'Northern Rockies']) # 1
    # f <- nrow(pr09[ecoHydro == 'Pacific NW']) # 2
    # g <- nrow(pr09[ecoHydro == 'Southern Plains']) # 2
    # h <- nrow(pr09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
  # Fraction of significant results for HUCs -----
  
    # Wilcoxon -----
    
      nrow(subset(pr09, p.value < 0.05))# & ecoHydro == 'Middle Rockies'))
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
    
      nrow(subset(pr09, pValue_lm < 0.05))# & ecoHydro == 'Middle Rockies'))
      lmSig <- c('0/12', '0/5', '0/1', '0/5', '0/1', '0/2', '1/2', '1/7')
    
      # GB: 0/12
      # MR: 0/5
      # MD: 0/1
      # NP: 0/5
      # NR: 0/1
      # PNW: 0/2
      # SP: 1/2
      # SR: 1/7
      
  # Join fractions to main table -----
      
    pr12 <- cbind(pr11, wilSig, lmSig)
      
  # Label the rows as belonging to pr so I can bind it together with other 
  # vars later -----
      
    climVar <- c('pr')
    pr13 <- cbind(climVar, pr12)
    
  # write table -----
    
    fwrite(pr13, '03_output/prTable.csv')
    
#------------------------------------
# Graph variable over time -----
  
  test <- pr02 %>%
      group_by(term, HUC4, year) %>%                   
      summarise(prSum = sum(pr)) %>% 
      ungroup() %>% 
      filter(HUC4 == '1102') 
      
  ggplot(test, aes(x = year, y = prSum)) +
          geom_line(size = 1, alpha = .5) +
          geom_smooth(method = 'lm', size=.2)
  