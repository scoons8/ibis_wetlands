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
  
    irr_ha01 <- RF %>%
      na.omit() %>% 
      filter(year > 1985) %>% 
      select(siteName, ecoHydro, HUC4, irr_ha, year) %>% 
      mutate(year = as.numeric(year),                 
               term = 't1',                               
               term = replace(term, year >2003, 't2')) %>% 
    na.omit()
  
  # Test by HUC4 -----
  
    irr_ha02 <- irr_ha01 %>% 
      select(HUC4, irr_ha, year, term) %>% 
      distinct()
    
    irr_haWilcox <- irr_ha02 %>%
      group_by(term, HUC4, year) %>%                   
      split(.$HUC4) %>%                                 
      map(~wilcox.test(irr_ha ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'HUC4')
      
   # Calculate percent difference -----
  
    irr_ha03 <- irr_ha02 %>% 
      group_by(term, HUC4) %>% 
      summarise(irr_haMean = mean(irr_ha)) %>% 
      spread(term, irr_haMean) %>% 
      mutate(perDif = ((t1-t2)/abs(t1))*-1,                     
             change = (t2-t1),
             HUC4 = as.character(HUC4))
      
  # Calculate Standard deviation -----
    
    irr_ha04 <- irr_ha02 %>% 
        group_by(HUC4, term) %>% 
        summarise(SD = sd(irr_ha)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 't1',
               SD2 = 't2') %>% 
        mutate(HUC4 = as.character(HUC4))
      
  # Join tables -----
    
    irr_ha05 <- irr_ha03 %>% 
      full_join(irr_ha04, by = 'HUC4') %>% 
      full_join(irr_haWilcox) %>% 
      rename('1983-2003 (T1)' = 't1',
             'SD T1' = 'SD1',
             '2004-2020 (T2)' = 't2',
             'SD T2' = 'SD2',
             'Change' = 'change',
             '% Dif' = 'perDif')
    
    colOrder <-c('HUC4', '1983-2003 (T1)', 'SD T1', '2004-2020 (T2)', 'SD T2', 
             'Change', '% Dif', 'p.value')
    
    irr_ha06 <- irr_ha05[ , colOrder]
    
#-------------------------------------
  # Linear regression -----
    
  # make linear model into a dataframe -----
  
    dataList <-split(irr_ha02, irr_ha02$HUC4)

    # Run the linear model for each HUC and make dataframe of p-values -----
    
      lmResults <- lapply(dataList,function(x){
           y <- summary(lm(irr_ha ~ year,data = x))
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
      
      irr_ha07 <- irr_ha06 %>% 
        full_join(lmTable, by = 'HUC4')

#------------------------------------
# Make table summarizing data by ecoregion -----
# Will essentially need to take the means of irr_ha06 table
      
  # Make table with ecoregions and HUCS-----
  
    irr_ha08 <- irr_ha01 %>% 
      select(HUC4, ecoHydro) %>% 
      distinct() %>% 
      mutate(HUC4 = as.character(HUC4))
      
  # Bind tables together -----
      
    irr_ha09 <- irr_ha08 %>% 
      full_join(irr_ha07, by = 'HUC4')
  
  # summarize data (take means and count) -----
      
    irr_ha10 <- irr_ha09[, 2:8]
      
    irr_ha11<- aggregate(.~ecoHydro, data = irr_ha10, mean)
    
  # number of HUCs with p < 0.05 Wilxon results by ecoregion -----
    
    wSig <-irr_ha09 %>% 
      filter(p.value < 0.05)  # 9 significant results
    
    lSig <- irr_ha09 %>% 
      filter(pValue_lm < 0.05) # 18 significant results
    
  # Number of total HUCs in each ecoregion ------
    
    # a <- nrow(irr_ha09[ecoHydro == 'Great Basin-Colorado Plateau']) # 12
    # b <- nrow(irr_ha09[ecoHydro == 'Middle Rockies']) # 5
    # c <- nrow(irr_ha09[ecoHydro == 'Mojave-Sonoran Deserts']) # 1
    # d <- nrow(irr_ha09[ecoHydro == 'Northern Plains']) # 5
    # e <- nrow(irr_ha09[ecoHydro == 'Northern Rockies']) # 1
    # f <- nrow(irr_ha09[ecoHydro == 'Pacific NW']) # 2
    # g <- nrow(irr_ha09[ecoHydro == 'Southern Plains']) # 2
    # h <- nrow(irr_ha09[ecoHydro == 'Southern Rockies and Basins']) # 7
    
  # Fraction of significant results for HUCs -----
  
    # Wilcoxon -----
    
      nrow(subset(irr_ha09, p.value < 0.05)) #& ecoHydro == 'Middle Rockies'))
      wilSig <- c('6/12', '0/5', '0/1', '0/5', '0/1', '1/2', '0/2', '2/7')
  
      # GB: 6/12
      # MR: 0/5
      # MD: 0/1
      # NP: 0/5
      # NR: 0/1
      # PNW: 1/2
      # SP: 0/2
      # SR: 2/7
    
    # Linear model -----
    
      nrow(subset(irr_ha09, pValue_lm < 0.05))# & ecoHydro == 'Middle Rockies'))
      lmSig <- c('8/12', '1/5', '1/1', '2/5', '0/1', '1/2', '0/2', '3/7')
    
      # GB: 8/12
      # MR: 1/5
      # MD: 1/1
      # NP: 2/5
      # NR: 0/1
      # PNW: 1/2
      # SP: 0/2
      # SR: 3/7
      
  # Join fractions to main table -----
      
    irr_ha12 <- cbind(irr_ha11, wilSig, lmSig)
      
  # Label the rows as belonging to irr_ha so I can bind it together with other 
  # vars later -----
      
    climVar <- c('irr_ha')
    irr_ha13 <- cbind(climVar, irr_ha12)
    
  # write table -----
    
    fwrite(irr_ha13, '03_output/irrTable.csv')
    
#------------------------------------
# Graph variable over time -----
  
  test <- irr_ha02 %>%
      group_by(term, HUC4, year) %>%                   
      summarise(irr_haSum = sum(irr_ha)) %>% 
      ungroup() %>% 
      filter(HUC4 == '1102') 
      
  ggplot(test, aes(x = year, y = irr_haSum)) +
          geom_line(size = 1, alpha = .5) +
          geom_smooth(method = 'lm', size=.2)
  
  