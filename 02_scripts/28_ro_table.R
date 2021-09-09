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
  
    ro01 <- RF %>%
      na.omit() %>% 
      filter(year > 1982) %>% 
      select(siteName, ecoHydro, HUC4, ro, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    ro02 <- ro01 %>% 
      select(HUC4, ro, year, term) %>% 
      distinct()
    
    roWilcox <- ro02 %>%
      group_by(term, HUC4, year) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(ro ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    ro03 <- ro02 %>% 
      group_by(term, HUC4) %>% 
      summarise(roMean = mean(ro)) %>% 
      spread(term, roMean) %>% 
      mutate(perDif = ((t1-t2)/t1)*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    ro04 <- ro02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(ro)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    ro05 <- ro03 %>% 
      full_join(ro04, by = 'HUC4') %>% 
      full_join(roWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    ro06 <- ro05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
  # make linear model into a dataframe -----
  
    dataList <-split(ro02, ro02$HUC4)

    # Run the linear model for each HUC and make dataframe of p-values -----
    
      lmResults <- lapply(dataList,function(x){
           y <- summary(lm(ro ~ year,data = x))
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
      
      ro07 <- ro06 %>% 
        full_join(lmTable, by = 'HUC4')

#------------------------------------
# Make table summarizing data by ecoregion -----
# Will essentially need to take the means of ro06 table
      
  # Make table with ecoregions and HUCS-----
  
    ro08 <- ro01 %>% 
      select(HUC4, ecoHydro) %>% 
      distinct() %>% 
      mutate(HUC4 = as.character(HUC4))
      
  # Bind tables together -----
      
    ro09 <- ro08 %>% 
      full_join(ro07, by = 'HUC4')
  
  # summarize data (take means and count) -----
      
    ro10 <- ro09[, 2:8]
      
    ro11<- aggregate(.~ecoHydro, data = ro10, mean)
    
  # number of HUCs with p < 0.05 Wilxon results by ecoregion -----
    
    wSig <-ro09 %>% 
      filter(p.value < 0.05)  # 0 significant results
    
    lSig <- ro09 %>% 
      filter(pValue_lm < 0.05) # 0 significant results
    
  # Number of total HUCs in each ecoregion ------
    
    # a <- nrow(ro09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    # b <- nrow(ro09[ecoHydro == 'Middle Rockies']) # 5
    # c <- nrow(ro09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    # d <- nrow(ro09[ecoHydro == 'Northern Plains']) # 5
    # e <- nrow(ro09[ecoHydro == 'Northern Rockies']) # 1
    # f <- nrow(ro09[ecoHydro == 'Pacific NW']) # 2
    # g <- nrow(ro09[ecoHydro == 'Southern Plains']) # 2
    # h <- nrow(ro09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
  # Fraction of significant results for HUCs -----
  
    # Wilcoxon -----
    
      nrow(subset(ro09, p.value < 0.05)) #& ecoHydro == 'Middle Rockies'))
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
    
      nrow(subset(ro09, pValue_lm < 0.05))# & ecoHydro == 'Middle Rockies'))
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
      
    ro12 <- cbind(ro11, wilSig, lmSig)
      
  # Label the rows as belonging to ro so I can bind it together with other 
  # vars later -----
      
    climVar <- c('ro')
    ro13 <- cbind(climVar, ro12)
    
  # write table -----
    
    fwrite(ro13, '03_output/roTable.csv')
    
#------------------------------------
# Graph variable over time -----
  
  test <- ro02 %>%
      group_by(term, HUC4, year) %>%                   
      summarise(roSum = sum(ro)) %>% 
      ungroup() %>% 
      filter(HUC4 == '1102') 
      
  ggplot(test, aes(x = year, y = roSum)) +
          geom_line(size = 1, alpha = .5) +
          geom_smooth(method = 'lm', size=.2)
  