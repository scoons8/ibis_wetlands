#===============================================================================
# Task: Re-run the analysis for blobs
#
# Date: /6/2021
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
  
  acres <- function(x){
    round((x*2.471)/1000, 0)
  }
  
# Read in data -----
  
  # Water Data -----
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv')
 
  # IDpoly's linked to Blob ID's
  polyBlob <-fread("01_data/24_siteBlobIDs/polyBlobID.csv")
  
  # Get rid of NA's and blanks -----
  # polyBlob <- polyBlob %>% 
  #   na.omit()
  
  # poly <- polyBlob[!(is.na(polyBlob$state) | polyBlob$state == ""), ]
  
#===============================================================================
# Delete repetitive columns in the polyBlob data -----
  
  polyBlob2 <- polyBlob[ , c(2, 6, 7)] #%>% 
   # na.omit()
  
# Summarize the water data and join blob ID info -----
  
  hydroTerm <- hydroGra %>% 
    filter(month > 3, month < 9) %>%                                            # I only want the data for April-August
    mutate(year = as.numeric(year),                                             # make year a number
            term = 'T1',                                    
            term = replace(term, year >2003, 'T2')) %>%                         # create a column for the two terms where term 2 is 2004-2020
    na.omit()
  
  stateHydro <- hydroTerm %>% 
    group_by(idPoly, siteName, year, month, term, ecoHydro) %>%                 # calculate the total water for each polygon during each month     
    summarise(wetHa = sum(wetHa)) %>%     
    ungroup() %>% 
    left_join(polyBlob2, by = c('idPoly')) %>%                                  # join the data that includes state designations for each polygon so I can sum by state
    group_by(state, month, year, term) %>%                                      # Sum the polygons --> find the total amount of monthly water in each state
    summarise(wetHa = sum(wetHa)) %>%
    ungroup() %>% 
    group_by(state, year, term) %>%                                             # Find the averaage annual surface water by taking the mean of the monthly water
    summarise(wetHa = mean(wetHa)) %>% 
    ungroup()
    
# Group and sum the water by state -----
  
  overallState <- stateHydro %>% 
    group_by(state, term) %>%                                                   # Take the mean of the two terms
    summarise(wetMean = mean(wetHa)) %>% 
    spread(term, wetMean) %>% 
    mutate(Change = (T2 - T1),                                                  # Take the difference between the two time periods
            PerDif = ((T1-T2)/T1)*-100)                                         # Calculate the percent change

# Calculate Standard deviation -----
  
  stateSD <- stateHydro %>% 
      group_by(state, term) %>% 
      summarise(SD = sd(wetHa)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 'T1',
             SD2 = 'T2')
  
# Wilcox test by BlobID -----
  
  stateWilcoxTest <- stateHydro %>%
      split(.$state) %>%  
      map(~wilcox.test(wetHa ~ term, data = .x, conf.int = T)) %>%       
      map_df(broom::tidy, .id = 'state')
    
# Combine wilcoxon results with p1, p2 diff results -----
  
    # stateWilcoxTest$blobID <- as.numeric(as.character(blobWilcoxTest$blobID))
    
    stateWilcoxChange <- overallState %>% 
      full_join(stateWilcoxTest, by = "state") %>%  
      full_join(stateSD, by = "state") %>% 
      select(state, T1, SD1, T2, SD2, Change, PerDif, p.value) 

# Write data -----
  
  fwrite(stateWilcoxChange, '03_output/05_Tables/stateLevel_table.csv')
  
# Box Plot -----
  
    # stateBox <- stateHydro %>%    
    #   group_by(term, state, year) %>%
    #   summarise(wetSum = sum(wetHa))
  
    stateBox <- stateHydro %>% 
      mutate(wetHa = 2.471*wetHa) #<-------- Converted everything to acres
    
    ggplot(stateBox, aes(x = term, y = wetHa, group = term, fill = term)) +  
      geom_boxplot() +
      scale_fill_manual(values=c("grey", "#FED179")) +  # fill color for box plots ("#23C9B6", "#fed179")
      facet_wrap(. ~ state, ncol = 4, scales = 'free') +          # creates plot for each region
      scale_y_continuous(labels = thousands) +
      xlab("Time Period") +
      ylab("Inundated acres x 1000") +
      # labs(fill = "Time Period") +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank())
    
    ggsave("overallStateBoxplot02.png", plot = last_plot(), width = 9.5, height = 5, units = "in", device='png', dpi=300)
    
#===============================================================================
# How many sites per state are decreasing?
  
  siteTrends <- fread("03_output/05_Tables/siteTrendValues.csv")
  siteState <- fread("03_output/05_Tables/siteState.csv")
  
  siteTrends02 <- siteTrends %>%  
  mutate(siteName = ifelse(siteName == 'Red Rock Lake NWR', 
                                         'Red Rock Lakes NWR', siteName))
  
  site <- siteTrends02 %>% 
    full_join(siteState, by = c("siteName", "Latitude", "Longitude"))

  site02 <- site %>%
    filter(STUSPS == 'MT') %>% 
    filter(cat == 'Sig. Decrease')
  nrow(site02)
  
  state <- c("CA", "CO", "NV", "WY", "ID", "OR", "UT", "MT")
  sigFrac <- c("3/5", "17/22", "11/28", "2/8", "5/13", "21/30", "17/19", "15/28")
  frac <- data.frame(state, sigFrac)
  
  stateWilcoxChange02 <- stateWilcoxChange %>% 
    full_join(frac, by = "state")
  
  fwrite(stateWilcoxChange02, '03_output/05_Tables/stateLevel_table.csv')
  
  
  # CA: 3/5
  # CO: 17/22
  # NV: 11/28
  # WY: 2/8
  # ID: 5/13
  # OR: 21/30
  # UT: 17/19
  # MT: 15/28
  
#===============================================================================
# Monthly Changes -----
  
  hydroTerm02 <- hydroGra %>% 
      mutate(year = as.numeric(year),
              term = 'T1',                                    
              term = replace(term, year >2003, 'T2')) %>%
      mutate(month = case_when(month == '3' ~ 'Mar',
                               month == '4' ~ 'Apr',
                               month == '5' ~ 'May',
                               month == '6' ~ 'Jun',
                               month == '7' ~ 'Jul',
                               month == '8' ~ 'Aug',
                               month == '9' ~ 'Sept',
                               month == '10' ~ 'Oct')) %>%
      na.omit()
  
  stateHydro02 <- hydroTerm02 %>%
        left_join(polyBlob2, by = c('idPoly'))

# Summarize by month -----
  
  stateMonth <- stateHydro02 %>% 
    group_by(term, state, year, month) %>% 
    summarise(wetSum = sum(wetHa)) %>% 
    na.omit()
  
  stateMonth02 <- stateMonth %>% 
    group_by(term, month, state) %>% 
    summarise(wetMean = mean(wetSum))
  
  monthPercentChange <- stateMonth02 %>%              
    spread(term, wetMean) %>%                        
    mutate(change = ((T1-T2)/T1)*-1) 
  
# Wilcox test for each month -----      
   
  marWilcox <-  stateMonth %>%    
        filter(month == 'Mar') %>%                                            # filter data to only include private
        group_by(term, state, year) %>% 
        split(.$state) %>%                                                 # '.' shorthand for the dataframe
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%                       # iterates whole process over each region
        map_df(broom::tidy, .id = 'state') %>%                             # kicks out dataframe with p-valuves by region and season
        mutate(month = 'Mar')
    
  aprWilcox <-  stateMonth %>%    
        filter(month == 'Apr') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'Apr')
    
  mayWilcox <-  stateMonth %>%    
        filter(month == 'May') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'May')
    
  junWilcox <-  stateMonth %>%    
        filter(month == 'Jun') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'Jun')

  julWilcox <-  stateMonth %>%    
        filter(month == 'Jul') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'Jul')
  
  augWilcox <-  stateMonth %>%    
        filter(month == 'Aug') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'Aug')
  
  septWilcox <-  stateMonth %>%    
        filter(month == 'Sept') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'Sept')
  
  octWilcox <-  stateMonth %>%    
        filter(month == 'Oct') %>%                      
        group_by(term, state, year) %>% 
        split(.$state) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'state') %>% 
        mutate(month = 'Oct')

# Join monthly Wilcoxon tests together -----
    
    monthChange <- marWilcox %>% 
        full_join(aprWilcox) %>% 
        full_join(mayWilcox) %>%
        full_join(junWilcox) %>%
        full_join(julWilcox) %>%
        full_join(augWilcox) %>%
        full_join(septWilcox) %>%
        full_join(octWilcox) %>%
        full_join(monthPercentChange) %>% 
        select(state, month, T1, T2, change, p.value)  
    
    fwrite(monthChange, '03_output/stateMonth_Wilcox.csv')

#===============================================================================
# OwnAg Changes -----
# Summarize by ownership -----
    
  stateHydro03 <- hydroTerm %>%
        left_join(polyBlob2, by = c('idPoly')) %>% 
      na.omit()
  
  ownHydro <- stateHydro03 %>% 
    group_by(state, ownAg, month, year, term) %>%           
    summarise(wetHa = sum(wetHa)) %>% 
    ungroup()
  
  ownHydro02 <- ownHydro %>% 
    group_by(state, ownAg, year, term) %>% 
    summarise(wetHa = mean(wetHa)) %>% 
    ungroup() %>% 
    group_by(term, ownAg, state) %>%
    summarise(wetMean = mean(wetHa)) %>%
    spread(term, wetMean) %>% 
    mutate(perDif = ((T1-T2)/T1)*-1,
          Change = T2-T1)
    
# Wilcoxon Test based on ownAg -----
  
  # Private -----
  
    privWilcox <-  ownHydro %>%    
      filter(ownAg == 'Private') %>%                                            # filter data to only include private
      group_by(state, ownAg, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                                                        # '.' shorthand for the dataframe
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%                          # iterates whole process over each region
      map_df(broom::tidy, .id = 'state') %>%                                    # kicks out dataframe with p-valuves by region and season
      mutate(ownAg = 'Private')                                                 # make new column with with ownership value ('Private')
  
  # Public -----
    pubWilcox <- ownHydro %>%    
      filter(ownAg == 'Public') %>%                                            # filter data to only include private
      group_by(state, ownAg, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>%
      split(.$state) %>%                                                        # '.' shorthand for the dataframe
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%                           # iterates whole process over each region
      map_df(broom::tidy, .id = 'state') %>%                                    # kicks out dataframe with p-valuves by region and season
      mutate(ownAg = 'Public')                                                  # make new column with with ownership value ('Public')

# Merge Wilcoxon test with data table -----   

  ownChange <- privWilcox %>% 
    full_join(pubWilcox) %>%                            
    full_join(ownHydro02) %>% 
    select(state, ownAg, T1, T2, Change, perDif, p.value)
  
  # fwrite(ownChange, '03_output/stateOwnAg_Wilcox.csv')

  
# Box plot -----
  
  # ownStateBox <- stateHydro03 %>%    
  #       group_by(term, ownAg, state, year) %>%               
  #       summarise(wetSum = sum(wetHa))
  
    ownStateBox <- stateHydro03 %>%    
      group_by(term, ownAg, state, year) %>%               
      summarise(wetSum = sum(wetHa)) %>% 
      mutate(wetSum = 2.471*wetSum)
    
  ggplot(ownStateBox, aes(x = ownAg, y = wetSum, fill = term)) +  
    geom_boxplot() +
    scale_fill_manual(values=c("grey", "#FED179")) +  # fill color for box plots
    facet_wrap(. ~ state, ncol = 4, scales = 'free') +          # creates plot for each region
    scale_y_continuous(labels = thousands) +
    xlab("Time Period") +
    ylab("Inundated acres x 1000") +
    theme_classic() +
    # labs(fill = 'Ownership')
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
   ggsave("ownAgStateBoxplot02.png", plot = last_plot(), width = 10, height = 5, units = "in", device='png', dpi=300)
#===============================================================================
# Hydroperiod Changes -----
# Summarize by hydroperiod ------
  
  hydroPer <- stateHydro03 %>% 
    group_by(state, period, month, year, term) %>%           
    summarise(wetHa = sum(wetHa)) %>% 
    ungroup() 
   
  hydroPer02 <- hydroPer %>% 
    group_by(term, period, year, state) %>%
    summarise(wetMean = mean(wetHa)) %>%
    ungroup() %>% 
    group_by(term, period, state) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    spread(term, wetMean) %>% 
    mutate(perDif = ((T1-T2)/T1)*-1,
          Change = T2-T1)

# Wilcoxon based on Hydroperiod -----
  
  # temp -----
  
    tempWilcox <-  hydroPer %>%    
      filter(period == 'temp') %>%                      
      group_by(state, period, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(period = 'temp')
  
  # seasonal -----
  
    seasWilcox <-  hydroPer %>%    
      filter(period == 'seasonal') %>%                      
      group_by(state, period, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(period = 'seasonal')
  
  # seasonal -----
  
    semiWilcox <-  hydroPer %>%    
      filter(period == 'semi') %>%                      
      group_by(state, period, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(period = 'semi')
  
# Merge Wilcoxon test with data table -----   
  
  perChange <- tempWilcox %>% 
    full_join(seasWilcox) %>% 
    full_join(semiWilcox) %>% 
    full_join(hydroPer02) %>%
    select(state, period, T1, T2, Change, perDif, p.value)
    
# Write table -----
  
  fwrite(perChange, '03_output/stateHydroper_Wilcox.csv')  
  
#===============================================================================
# Wetland Type Change -----
# Summarize by wetland type -----
  
  wetHydro <- stateHydro03 %>% 
    group_by(state, wetType, month, year, term) %>%           
    summarise(wetHa = sum(wetHa)) %>% 
    ungroup() 
  
  wetHydro02 <- wetHydro %>% 
    group_by(term, wetType, year, state) %>%
    summarise(wetMean = mean(wetHa)) %>% 
    ungroup() %>% 
    group_by(term, wetType, state) %>% 
    summarise(wetMean = mean(wetMean)) %>%
    spread(term, wetMean) %>% 
    mutate(perDif = ((T1-T2)/T1)*-1,
          Change = T2-T1)
  
# Wilcoxon based on wetland type -----
  
  # riverine -----
        
    rivWilcox <- wetHydro %>%    
      filter(wetType == 'riv') %>%                      
      group_by(term, state, year, wetType) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(wetType = 'riv')
        
  # wetlands -----
  
    wetWilcox <- wetHydro %>%    
      filter(wetType == 'wet') %>%                      
      group_by(term, state, year, wetType) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(wetType = 'wet')
  
  # flooded ag -----
  
    wetAgWilcox <- wetHydro %>%    
      filter(wetType == 'wetAg') %>%                      
      group_by(term, state, year, wetType) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(wetType = 'wetAg')

  # managed wetlands -----
  
    wetManWilcox <- wetHydro %>%    
      filter(wetType == 'wetMan') %>%                      
      group_by(term, state, year, wetType) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      split(.$state) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'state') %>%
      mutate(wetType = 'wetMan')
        
  # Merge Wilcoxon test with data table -----   
    
    wetTypeChange <- rivWilcox %>% 
      full_join(wetWilcox) %>% 
      full_join(wetAgWilcox) %>%
      full_join(wetManWilcox) %>%
      full_join(wetHydro02) %>%
      select(state, wetType, T1, T2, Change, perDif, p.value)
          
  # Write table -----
    
    fwrite(wetTypeChange, '03_output/stateWetType_Wilcox.csv')
  
  
  
  
  
  
  