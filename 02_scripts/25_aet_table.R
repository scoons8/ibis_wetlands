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
  
    aet01 <- RF %>%
      na.omit() %>% 
      filter(year > 1982) %>% 
      select(siteName, ecoHydro, HUC4, aet, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    aet02 <- aet01 %>% 
      select(HUC4, aet, year, term) %>% 
      distinct()
    
    aetWilcox <- aet02 %>%
      group_by(term, HUC4, year) %>%                   
      # summarise(wetSum = sum(wetHa)) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(aet ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    aet03 <- aet02 %>% 
      group_by(term, HUC4) %>% 
      summarise(aetMean = mean(aet)) %>% 
      spread(term, aetMean) %>% 
      mutate(perDif = ((t1-t2)/t1)*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    aet04 <- aet02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(aet)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    aet05 <- aet03 %>% 
      full_join(aet04, by = 'HUC4') %>% 
      full_join(aetWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    aet06 <- aet05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
    # aet07 <- lmList(aet ~ year | HUC4, data = aet02)
    # summary(aet07)
    # test <- subset(aet02, HUC4 == '1006')
    # test01 <- lm (aet ~ year, data = test)
    # summary(test01)
    
  # make linear model into a dataframe -----
  
    dataList <-split(aet02, aet02$HUC4)
    
    # Test code from stackoverflow that uses lapply to apply linear model across
    # a data frame and then extract coefficients into a table -----
    
      # lmResults <- lapply(dataList,function(x){
      #    y <- summary(lm(aet ~ year,data = x))
      #    Intercept <- y$coefficients[1,1]
      #    Slope <- y$coefficients[2,1]
      #    rSquared <- y$r.squared
      #    adjRSquared <- y$adj.r.squared
      #    f <- y$fstatistic[1]
      #    pValue <- pf(y$fstatistic[1],y$fstatistic[2],y$fstatistic[3],lower.tail = FALSE)
      #    data.frame(Slope,Intercept,rSquared,adjRSquared,pValue)
      # })
      # lmResultTable <- do.call(rbind,lmResults)
      # 
      #   # add HUC4 labesl
      #   lmResultTable$HUC4 <- names(dataList)
  
        # lmResultTable 


    # Run the linear model for each HUC and make dataframe of p-values -----
    
      lmResults <- lapply(dataList,function(x){
           y <- summary(lm(aet ~ year,data = x))
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
      
      aet07 <- aet06 %>% 
        full_join(lmTable, by = 'HUC4')

#------------------------------------
# Make table summarizing data by ecoregion -----
# Will essentially need to take the means of aet06 table
      
  # Make table with ecoregions and HUCS-----
  
    aet08 <- aet01 %>% 
      select(HUC4, ecoHydro) %>% 
      distinct() %>% 
      mutate(HUC4 = as.character(HUC4))
      
  # Bind tables together -----
      
    aet09 <- aet08 %>% 
      full_join(aet07, by = 'HUC4')
  
  # summarize data (take means and count) -----
      
    aet10 <- aet09[, 2:8]
      
    aet11<- aggregate(.~ecoHydro, data = aet10, mean)
    
  # number of HUCs with p < 0.05 Wilxon results by ecoregion -----
    
    wSig <-aet09 %>% 
      filter(p.value < 0.05)  # 0 significant results
    
    lSig <- aet09 %>% 
      filter(pValue_lm < 0.05) # 2 significant results
    
  # Number of total HUCs in each ecoregion ------
    
    # a <- nrow(aet09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    # b <- nrow(aet09[ecoHydro == 'Middle Rockies']) # 5
    # c <- nrow(aet09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    # d <- nrow(aet09[ecoHydro == 'Northern Plains']) # 5
    # e <- nrow(aet09[ecoHydro == 'Northern Rockies']) # 1
    # f <- nrow(aet09[ecoHydro == 'Pacific NW']) # 2
    # g <- nrow(aet09[ecoHydro == 'Southern Plains']) # 2
    # h <- nrow(aet09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
  # Fraction of significant results for HUCs -----
  
    # Wilcoxon -----
    
      nrow(subset(aet09, p.value < 0.05 & ecoHydro == 'Middle Rockies'))
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
    
      nrow(subset(aet09, pValue_lm < 0.05 & ecoHydro == 'Middle Rockies'))
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
      
    aet12 <- cbind(aet11, wilSig, lmSig)
      
  # Label the rows as belonging to aet so I can bind it together with other 
  # vars later -----
      
    climVar <- c('ET')
    aet13 <- cbind(climVar, aet12)
    
  # write table -----
    
    fwrite(aet13, '03_output/aetTable.csv')
    
#------------------------------------
# Graph variable over time -----
  
  test <- aet02 %>%
      group_by(term, HUC4, year) %>%                   
      summarise(aetSum = sum(aet)) %>% 
      ungroup() %>% 
      filter(HUC4 == '1102') 
      
  ggplot(test, aes(x = year, y = aetSum)) +
          geom_line(size = 1, alpha = .5) +
          geom_smooth(method = 'lm', size=.2)
  
  