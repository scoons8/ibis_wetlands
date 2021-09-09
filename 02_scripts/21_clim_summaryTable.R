################################################################################
# Task: Making summary table of climate data
# 
# Jan 13 2021
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
  
    aet01 <- RF %>%
      na.omit() %>% 
      filter(year > 1987) %>% 
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
    
    a <- nrow(aet09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    b <- nrow(aet09[ecoHydro == 'Middle Rockies']) # 5
    c <- nrow(aet09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    d <- nrow(aet09[ecoHydro == 'Northern Plains']) # 5
    e <- nrow(aet09[ecoHydro == 'Northern Rockies']) # 1
    f <- nrow(aet09[ecoHydro == 'Pacific NW']) # 2
    g <- nrow(aet09[ecoHydro == 'Southern Plains']) # 2
    h <- nrow(aet09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
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
      lmSig <- c('0/12', '0/5', '0/1', '0/5', '0/1', '0/2', '1/2', '2/7')
    
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
      
    climVar <- c('aet')
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
  

#===============================================================================
# Precipitation summarized by HUC4-----
  
  # Reformat -----
  
    pr01 <- RF %>%
      na.omit() %>% 
      filter(year > 1987) %>% 
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
      # summarise(wetSum = sum(wetHa)) %>%                   
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
    
    pr09 <- lmList(pr ~ year | HUC4, data = pr02)
    summary(pr09)
     
#===============================================================================    
# SWE summarized by HUC4-----
  
  # Reformat -----
  
    swe01 <- RF %>%
      na.omit() %>% 
      filter(year > 1987) %>% 
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
      # summarise(wetSum = sum(wetHa)) %>%                   
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
    
    swe09 <- lmList(swe ~ year | HUC4, data = swe02)
    summary(swe09)
         
#===============================================================================
# RO summarized by HUC4-----
  
  # Reformat -----
  
    ro01 <- RF %>%
      na.omit() %>% 
      filter(year > 1987) %>% 
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
      # summarise(wetSum = sum(wetHa)) %>%                   
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
    
    ro09 <- lmList(ro ~ year | HUC4, data = ro02)
    summary(ro09)
        
#===============================================================================
# RO summarized by HUC4-----
  
  # Reformat -----
  
    t01 <- RF %>%
      na.omit() %>% 
      filter(year > 1987) %>% 
      select(siteName, ecoHydro, HUC4, tmin, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    t02 <- t01 %>% 
      select(HUC4, tmin, year, term) %>% 
      distinct()
    
    tWilcox <- t02 %>%
      group_by(term, HUC4, year) %>%                   
      # summarise(wetSum = sum(wetHa)) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(tmin ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    t03 <- t02 %>% 
      group_by(term, HUC4) %>% 
      summarise(tMean = mean(tmin)) %>% 
      spread(term, tMean) %>% 
      mutate(perDif = ((t1-t2)/t1)*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    t04 <- t02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(tmin)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    t05 <- t03 %>% 
      full_join(t04, by = 'HUC4') %>% 
      full_join(tWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    t06 <- t05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
    t09 <- lmList(tmin ~ year | HUC4, data = t02)
    summary(t09)
        
        
#===============================================================================  
#===============================================================================
# AET By ecoregion -----
  
# Test by ecoregion -----

  aet07 <- aet01 %>% 
    # mutate(ecoHydro = case_when(HUC4 == 1003 ~ 'Northern Plains',
    #                             HUC4 == 1007 ~ 'Northern Plains', 
    #                             HUC4 == 1019 ~ 'Southern Plains',
    #                             HUC4 == 1102 ~ 'Southern Plains',
    #                             HUC4 == 1601 ~ 'Great Basin-Colorado Plateau',
    #                             HUC4 == 1701 ~ 'Northern Rockies',
    #                             HUC4 == 1704 ~ 'Great Basin-Colorado Plateau',
    #                             TRUE ~ ecoHydro)) %>% 
    select(ecoHydro, HUC4, aet, year, term) %>% 
    distinct()
  
  aetWilcox02 <- aet07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(aetSum = sum(aet)) %>%                   
    split(.$ecoHydro) %>%                                 
    map(~wilcox.test(aetSum ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'ecoHydro')
    
# Calculate percent difference -----

  aet08 <- aet07 %>% 
    group_by(term, ecoHydro, year) %>% 
    summarise(aetSum = sum(aet)) %>% 
    ungroup() %>% 
    group_by(term, ecoHydro) %>% 
    summarise(aetMean = mean(aetSum)) %>% 
    spread(term, aetMean) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1))
    
# Calculate Standard deviation -----
  
  aet09 <- aet07 %>% 
    group_by(term, ecoHydro, year) %>%                   
    summarise(aetSum = sum(aet)) %>%  
    ungroup() %>% 
    group_by(ecoHydro, term) %>% 
    summarise(SD = sd(aetSum)) %>% 
    ungroup() %>% 
    spread(term, SD) %>% 
    rename(SD1 = 't1',
           SD2 = 't2') 
    
# Join tables -----
  
  ecoHydro <- c('Great Basin-Colorado Plateau', 'Mojave-Sonoran Deserts',
                  'Middle Rockies', 'Northern Plains', 'Northern Rockies',
                  'Pacific NW', 'Southern Plains', 'Southern Rockies and Basins')
    
    Wiln Frac <- c('55/87', '0/1', '6/11', '6/15', '3/3', '3/7', '6/6', '12/23')
  
  aet10 <- aet08 %>% 
    full_join(aet09, by = 'ecoHydro') %>% 
    full_join(aetWilcox02) %>% 
    mutate(perDif = 100(perDif)) %>% 
    rename('Ecoregion' = 'ecoHydro',
           '1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif') %>% 
    mutate()
  
  colOrder <-c('Ecoregion', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  aet011 <- aet10[ , colOrder]
  
fwrite(aet011, '03_output/summaryTable_aet.csv')

# Plot -----
test <- aet07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(aetSum = sum(aet)) %>% 
    ungroup() %>% 
    filter(ecoHydro == 'Great Basin-Colorado Plateau') 
    
ggplot(test, aes(x = year, y = aetSum)) +
        geom_line(size = 1, alpha = .5) +
        geom_smooth(method = 'lm', size=.2)
  
#===============================================================================  
# PR by ecoRegion
  
# Reformat -----
  
  pr01 <- RF %>%
    na.omit() %>% 
    filter(year > 1987) %>% 
    select(siteName, ecoHydro, HUC4, pr, year) %>% 
    mutate(year = as.numeric(year),                 
             term = 't1',                               
             term = replace(term, year >2003, 't2')) %>% 
  na.omit()  
  
# Test by ecoregion -----

  pr07 <- pr01 %>% 
    mutate(ecoHydro = case_when(HUC4 == 1003 ~ 'Northern Plains',
                                HUC4 == 1007 ~ 'Northern Plains', 
                                HUC4 == 1019 ~ 'Southern Plains',
                                HUC4 == 1102 ~ 'Southern Plains',
                                HUC4 == 1601 ~ 'Great Basin-Colorado Plateau',
                                HUC4 == 1701 ~ 'Northern Rockies',
                                HUC4 == 1704 ~ 'Great Basin-Colorado Plateau',
                                TRUE ~ ecoHydro)) %>% 
    select(ecoHydro, HUC4, pr, year, term) %>% 
    distinct()
  
  prWilcox02 <- pr07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(prSum = sum(pr)) %>%                   
    split(.$ecoHydro) %>%                                 
    map(~wilcox.test(prSum ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'ecoHydro')
    
# Calculate percent difference -----

  pr08 <- pr07 %>% 
    group_by(term, ecoHydro, year) %>% 
    summarise(prSum = sum(pr)) %>% 
    ungroup() %>% 
    group_by(term, ecoHydro) %>% 
    summarise(prMean = mean(prSum)) %>% 
    spread(term, prMean) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1))
    
# Calculate Standard deviation -----
  
  pr09 <- pr07 %>% 
      group_by(term, ecoHydro, year) %>%                   
      summarise(prSum = sum(pr)) %>% 
      ungroup() %>% 
      group_by(ecoHydro, term) %>% 
      summarise(SD = sd(prSum)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 't1',
             SD2 = 't2') 
    
# Join tables -----
  
  pr10 <- pr08 %>% 
    full_join(pr09, by = 'ecoHydro') %>% 
    full_join(prWilcox02) %>% 
    rename('Ecoregion' = 'ecoHydro',
           '1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif')
  
  colOrder <-c('Ecoregion', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  pr011 <- pr10[ , colOrder]
  
 fwrite(pr011, '03_output/summaryTable_pr.csv') 

# Plot -----
 
 test <- pr07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(prSum = sum(pr)) %>% 
    ungroup() %>% 
    filter(ecoHydro == 'Great Basin-Colorado Plateau') 
    
ggplot(test, aes(x = year, y = prSum)) +
        geom_line(size = 1, alpha = .5) +
        geom_smooth(method = 'lm', size=.2)
  
#===============================================================================  
# RO by ecoRegion
  
# Reformat -----
  
  ro01 <- RF %>%
    na.omit() %>% 
    filter(year > 1987) %>% 
    select(siteName, ecoHydro, HUC4, ro, year) %>% 
    mutate(year = as.numeric(year),                 
             term = 't1',                               
             term = replace(term, year >2003, 't2')) %>% 
  na.omit()  
  
# Test by ecoregion -----

  ro07 <- ro01 %>% 
    mutate(ecoHydro = case_when(HUC4 == 1003 ~ 'Northern Plains',
                                HUC4 == 1007 ~ 'Northern Plains', 
                                HUC4 == 1019 ~ 'Southern Plains',
                                HUC4 == 1102 ~ 'Southern Plains',
                                HUC4 == 1601 ~ 'Great Basin-Colorado Plateau',
                                HUC4 == 1701 ~ 'Northern Rockies',
                                HUC4 == 1704 ~ 'Great Basin-Colorado Plateau',
                                TRUE ~ ecoHydro)) %>% 
    select(ecoHydro, HUC4, ro, year, term) %>% 
    distinct()
  
  roWilcox02 <- ro07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(roSum = sum(ro)) %>%                   
    split(.$ecoHydro) %>%                                 
    map(~wilcox.test(roSum ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'ecoHydro')
    
# Calculate percent difference -----

  ro08 <- ro07 %>% 
    group_by(term, ecoHydro, year) %>% 
    summarise(roSum = sum(ro)) %>% 
    ungroup() %>% 
    group_by(term, ecoHydro) %>% 
    summarise(roMean = mean(roSum)) %>% 
    spread(term, roMean) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1))
    
# Calculate Standard deviation -----
  
  ro09 <- ro07 %>% 
    group_by(term, ecoHydro, year) %>%                   
    summarise(roSum = sum(ro)) %>%  
    ungroup() %>% 
    group_by(ecoHydro, term) %>% 
    summarise(SD = sd(roSum)) %>% 
    ungroup() %>% 
    spread(term, SD) %>% 
    rename(SD1 = 't1',
           SD2 = 't2') 
    
# Join tables -----
  
  ro10 <- ro08 %>% 
    full_join(ro09, by = 'ecoHydro') %>% 
    full_join(roWilcox02) %>% 
    rename('Ecoregion' = 'ecoHydro',
           '1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif')
  
  colOrder <-c('Ecoregion', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  ro011 <- ro10[ , colOrder]
  
  fwrite(ro011, '03_output/summaryTable_ro.csv')
  
test <- ro07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(roSum = sum(ro)) %>% 
    ungroup() %>% 
    filter(ecoHydro == 'Great Basin-Colorado Plateau')
    
ggplot(test, aes(x = year, y = roSum)) +
        geom_line(size = 1, alpha = .5) +
        geom_smooth(method = 'lm', size=.2)
  
#===============================================================================  
# SWE by ecoRegion
  
# Reformat -----
  
  swe01 <- RF %>%
    na.omit() %>% 
    filter(year > 1987) %>% 
    select(siteName, ecoHydro, HUC4, swe, year) %>% 
    mutate(year = as.numeric(year),                 
             term = 't1',                               
             term = replace(term, year >2003, 't2')) %>% 
  na.omit()  
  
# Test by ecoregion -----

  swe07 <- swe01 %>% 
    mutate(ecoHydro = case_when(HUC4 == 1003 ~ 'Northern Plains',
                                HUC4 == 1007 ~ 'Northern Plains',
                                HUC4 == 1019 ~ 'Southern Plains',
                                HUC4 == 1102 ~ 'Southern Plains',
                                HUC4 == 1601 ~ 'Great Basin-Colorado Plateau',
                                HUC4 == 1701 ~ 'Northern Rockies',
                                HUC4 == 1704 ~ 'Great Basin-Colorado Plateau',
                                TRUE ~ ecoHydro)) %>%
    select(ecoHydro, HUC4, swe, year, term) %>% 
    distinct()
  
  sweWilcox02 <- swe07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(sweSum = sum(swe)) %>%                   
    split(.$ecoHydro) %>%                                 
    map(~wilcox.test(sweSum ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'ecoHydro')
    
# Calculate percent difference -----

  swe08 <- swe07 %>% 
    group_by(term, ecoHydro, year) %>% 
    summarise(sweSum = sum(swe)) %>% 
    ungroup() %>% 
    group_by(term, ecoHydro) %>% 
    summarise(sweMean = mean(sweSum)) %>% 
    spread(term, sweMean) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1))
    
# Calculate Standard deviation -----
  
  swe09 <- swe07 %>% 
      group_by(term, ecoHydro, year) %>%                   
      summarise(sweSum = sum(swe)) %>%  
      group_by(ecoHydro, term) %>% 
      summarise(SD = sd(sweSum)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 't1',
             SD2 = 't2') 
    
# Join tables -----
  
  swe10 <- swe08 %>% 
    full_join(swe09, by = 'ecoHydro') %>% 
    full_join(sweWilcox02) %>% 
    rename('Ecoregion' = 'ecoHydro',
           '1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif')
  
  colOrder <-c('Ecoregion', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  swe011 <- swe10[ , colOrder]
  
  fwrite(swe011, '03_output/summaryTable_swe.csv')

test <- swe07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(sweSum = sum(swe)) %>% 
    ungroup() %>% 
    filter(ecoHydro == 'Great Basin-Colorado Plateau')
    
ggplot(test, aes(x = year, y = sweSum)) +
        geom_line(size = 1, alpha = .5) +
        geom_smooth(method = 'lm', size=.2)
#===============================================================================  
# tmin by ecoRegion
  
# Reformat -----
  
  tmin01 <- RF %>%
    na.omit() %>% 
    filter(year > 1987) %>% 
    select(siteName, ecoHydro, HUC4, tmin, year) %>% 
    mutate(year = as.numeric(year),                 
             term = 't1',                               
             term = replace(term, year >2003, 't2')) %>% 
  na.omit()  
  
# Test by ecoregion -----

  tmin07 <- tmin01 %>% 
    mutate(ecoHydro = case_when(HUC4 == 1003 ~ 'Northern Plains',
                                HUC4 == 1007 ~ 'Northern Plains', 
                                HUC4 == 1019 ~ 'Southern Plains',
                                HUC4 == 1102 ~ 'Southern Plains',
                                HUC4 == 1601 ~ 'Great Basin-Colorado Plateau',
                                HUC4 == 1701 ~ 'Northern Rockies',
                                HUC4 == 1704 ~ 'Great Basin-Colorado Plateau',
                                TRUE ~ ecoHydro)) %>% 
    select(ecoHydro, HUC4, tmin, year, term) %>% 
    distinct()
  
  tminWilcox02 <- tmin07 %>%
    group_by(term, ecoHydro, year) %>%                   
    summarise(tminMean = mean(tmin)) %>%                   
    split(.$ecoHydro) %>%                                 
    map(~wilcox.test(tminMean ~ term, data = .x)) %>%     
    map_df(broom::tidy, .id = 'ecoHydro')
    
# Calculate percent difference -----

  tmin08 <- tmin07 %>% 
    group_by(term, ecoHydro, year) %>% 
    summarise(tminMean = mean(tmin)) %>% 
    ungroup() %>% 
    group_by(term, ecoHydro) %>% 
    summarise(tminAvg = mean(tminMean)) %>% 
    spread(term, tminAvg) %>% 
    mutate(perDif = ((t1-t2)/t1)*-1,                     
           change = (t2-t1))
    
# Calculate Standard deviation -----
  
  tmin09 <- tmin07 %>% 
      group_by(ecoHydro, year, term) %>% 
      summarise(tminMean = mean(tmin)) %>% 
      ungroup() %>% 
      group_by(ecoHydro, term) %>% 
      summarise(SD = sd(tminMean)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 't1',
             SD2 = 't2') 
    
# Join tables -----
  
  tmin10 <- tmin08 %>% 
    full_join(tmin09, by = 'ecoHydro') %>% 
    full_join(tminWilcox02) %>% 
    rename('Ecoregion' = 'ecoHydro',
           '1983-2003 (T1)' = 't1',
           'SD T1' = 'SD1',
           '2004-2020 (T2)' = 't2',
           'SD T2' = 'SD2',
           'Change' = 'change',
           '% Dif' = 'perDif')
  
  colOrder <-c('Ecoregion', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
           'Change', '% Dif', 'p.value')
  
  tmin011 <- tmin10[ , colOrder]
  
  fwrite(tmin011, '03_output/summaryTable_tmin.csv')
  
# plot -----
  
  #-------------------------------------
  # Linear regression -----
    
    tmin12 <- lmList(tmin ~ year | ecoHydro, data = tmin07)
    summary(tmin12)
    
  
test <- tmin07 %>% 
    group_by(term, ecoHydro, year) %>% 
    summarise(tminMean = mean(tmin)) %>% 
    ungroup() %>% 
    filter(ecoHydro == 'Northern Rockies')

test02<-lm(tminMean~year, data = test)
summary(test02)

 ggplot(test, aes(x = year, y = tminMean)) +
        geom_line(size = 1, alpha = .5) +
        geom_smooth(method = 'lm', size=.2)

 