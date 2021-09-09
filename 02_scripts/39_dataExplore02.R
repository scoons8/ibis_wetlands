################################################################################
# Task: Data summary and visualization and exploration
# 
# Nov 19 2020
################################################################################

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


# Color Palette -----
  #3A2139 = 'dark purple'
  #0B646E = 'ming' (dark cerulean)
  #23C9B6 = 'medium turquoise'
  #FED179 = 'orange-yellow crayola'
  #C07C6B = 'blast off bronze'
  

# Read data -----
 
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv')
    
#===============================================================================
#-------------------------------------------------------------------------------
# Changes for overall water by region -----
  
  # Create column that sorts years among two periods -----
   
    wetTerm <- hydroGra %>%  
      filter(month > 3, month < 9) %>%                                          # Filter data to April - August     
      mutate(year = as.numeric(year),                                           # Makes the column 'year' numeric
              term = 'T1',                                                      # creates a column with filled with 'p1'
              term = replace(term, year >2003, 'T2')) %>%                       # replaces 't1' with 't2' when year is >2002
      na.omit()
  
    wetTerm02 <- wetTerm %>% 
      group_by(year, month, ecoHydro, term) %>%                                 # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                                        # summarise the grouped columns by summing the wetHa found with in each month
      ungroup() %>%                                                             # Ungroup: the result is the monthly total of water for each year and region
      group_by(ecoHydro, year, term) %>%                                        # Group the columns that you want to run a function over
      summarise(wetMean = mean(wetSum)) %>%                                     # Summarise the grouped columns by taking the mean of the monthly totals to get a mean annual value for each region
      ungroup()
    
  # calc difference between p1 and p1 as % -----
    
    wetChange <- wetTerm02 %>%                                                  
      group_by(term, ecoHydro) %>%                                              # group again by term and region
      summarise(wetMean = mean(wetMean)) %>%                                    # summarise the grouped columns by the average wetHa - this gives the avg wetHa across each term for each region
      spread(term, wetMean) %>%                                                 # spread is opposite of gather: there is now a column for t1 and one for t2
      mutate(Change = T2-T1,                                                    # create column 'change' by doing column math with columns 't1' and 't2'
             PerDif = ((T1-T2)/T1)*-100)                                        # create column 'perDif' by calculating the percent differenct between T1 and T2                         
                                                                                # The final result is a table with a column with the percent change from t1 to t2
  # Wilcoxon Test -----
  
    wetWilcoxTest <- wetTerm02 %>%                                              
      split(.$ecoHydro) %>%                                                     # '.' shorthand for the dataframe - splits the data by region
      map(~wilcox.test(wetMean ~ term, data = .x, conf.int = T)) %>%            # iterates whole process over each region (calculates the mean and tests for difference)
      map_df(broom::tidy, .id = 'ecoHydro')                                     # kicks out dataframe with p-valuves by region and season
       
  # Calculate Standard Deviations -----
  
    wetSD <- wetTerm02 %>% 
      group_by(term, ecoHydro) %>% 
      summarise(SD = sd(wetMean)) %>% 
      ungroup() %>% 
      spread(term, SD) %>% 
      rename(SD1 = 'T1',
             SD2 = 'T2')
  
  # combine wilcoxon results with p1, p2 diff results -----
    
    wetWilcoxChange <- wetChange %>% 
      full_join(wetWilcoxTest) %>%                                              # merge the Wilcoxon test results to the wetChange df     
      full_join(wetSD) %>%                                                      # merge the standard deviation to the other df's
      select(ecoHydro, T1, SD1, T2, SD2, Change, PerDif, p.value)               # select the columns that you want to keep
    
    # fwrite(wetWilcoxChange, '03_output/table_region_wilcox.csv')
    
  # Make box plots to show distibutions between t1 nad t2 surface water -----
    
    ggplot(wetTerm02, aes(x = term, y = wetMean, group = term, fill = term)) +  # group and fill color based on the T1 or T2
      geom_boxplot() +
      scale_fill_manual(values=c("grey", "#FED179")) +                          # fill color for box plots ("#23C9B6", "#fed179")
      facet_wrap(. ~ ecoHydro, ncol = 4, scales = 'free') +                     # creates plot for each region
      scale_y_continuous(labels = thousands) +                                  # use the 'thousands' function created above
      xlab("Time Period") +
      ylab("Inundated hectares x 1000") +
      labs(fill = "Time Period") +
      theme_classic() 
    
    ggsave("overallBoxplot.png", plot = last_plot(), width = 9.5, height = 5, 
           units = "in", device='png', dpi=300)
    
  # Line Plots -----
    
    ggplot(wetTerm02, aes(x = year, y = wetMean)) +
        geom_line(size = 1, alpha = .7) +
        geom_smooth(method = 'lm', size=.2, color = 'black') +
        scale_y_continuous(labels = thousands) +
        theme_classic() +
        ylab('Inundated hectares x 1000') +
        xlab('Year') +
        facet_wrap(~ecoHydro, ncol = 4, scales = 'free') +
        theme( axis.title.y=element_text(size=8),
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8),
               legend.title = element_blank(),
               legend.position = 'bottom')
    
    ggsave("overallLinePlot.png", plot = last_plot(), width = 9.5, height = 5, 
           units = "in", device='png', dpi=300)

#-------------------------------------------------------------------------------
# Monthly water by region -----
   
  # Add term to dataframe; sum over region, year, month, and term
  # This gives the total wetHa for each month in each year for each region -----
  
    wetTermMonth <- hydroGra %>%  
        mutate(year = as.numeric(year),                                         # makes the column 'year' numeric
               term = 'T1',                                                     # creates a column called 'term' with filled with 't1'
               term = replace(term, year >2003, 'T2')) %>%                      # labels everything in 'term' as 't2' if 'year' is greater than 2003
      group_by(term, ecoHydro, year, month) %>%                                 # Group the columns that you want to summarise over
      summarise(wetSum = sum(wetHa)) %>% 
      # mutate(month = case_when(month == '3' ~ 'Mar',
        # month == '4' ~ 'Apr',
        # month == '5' ~ 'May',
        # month == '6' ~ 'Jun',
        # month == '7' ~ 'Jul',
        # month == '8' ~ 'Aug',
        # month == '9' ~ 'Sept',
        # month == '10' ~ 'Oct')) %>%
      na.omit()
  
  # Plot -----
      
    # Ridgeline Plots -----
      
      ggplot(wetTermMonth, aes(x = year, y = as.factor(month), height = wetSum, fill = month)) +
          geom_density_ridges(stat = "identity", alpha = 0.5, scale = 2 ) +
          scale_y_discrete(limits = rev) +
          facet_wrap(~ ecoHydro, ncol = 4) +
          theme_ridges(center_axis_labels = TRUE) +
          theme_classic() +
          theme(strip.text = element_text(size = 10)) +
          xlab("Year") +
          ylab("Month") +
          theme(legend.position = "none")
    
    ggsave("monthRidgeLinePlot.png", plot = last_plot(), width = 9.5, height = 5, 
           units = "in", device='png', dpi=300)
      
# How has monthly water changed from t1 to t2? -----
  
  # Find the percent change for each month from t1 to t2 -----

    wetMonth <- wetTermMonth %>% 
      group_by(term, month, ecoHydro) %>%                                       # group by month, term, and region
      summarise(wetMean = mean(wetSum)) %>%                                     # find the mean wetHa for each month in each region for each term
      spread(term, wetMean) %>%                        
      mutate(change = T2-T1,
            PerDif = ((T1-T2)/T1)*-100)

# Wilcox test for each month -----      
   
  marWilcox <-  wetTermMonth %>%    
        filter(month == 'Mar') %>%                                              # filter data to only include private
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                                                   # '.' shorthand for the dataframe
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%                         # iterates whole process over each region
        map_df(broom::tidy, .id = 'ecoHydro') %>%                               # kicks out dataframe with p-valuves by region and season
        mutate(month = 'Mar')
    
  aprWilcox <-  wetTermMonth %>%    
        filter(month == 'Apr') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
        mutate(month = 'Apr')
    
  mayWilcox <-  wetTermMonth %>%    
        filter(month == 'May') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
        mutate(month = 'May')
    
  junWilcox <-  wetTermMonth %>%    
        filter(month == 'Jun') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
        mutate(month = 'Jun')

  julWilcox <-  wetTermMonth %>%    
        filter(month == 'Jul') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
        mutate(month = 'Jul')
  
  augWilcox <-  wetTermMonth %>%    
        filter(month == 'Aug') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
        mutate(month = 'Aug')
  
  septWilcox <-  wetTermMonth %>%    
        filter(month == 'Sept') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
        mutate(month = 'Sept')
  
  octWilcox <-  wetTermMonth %>%    
        filter(month == 'Oct') %>%                      
        group_by(term, ecoHydro, year) %>% 
        split(.$ecoHydro) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%   
        map_df(broom::tidy, .id = 'ecoHydro') %>% 
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
        full_join(wetMonth) %>% 
        select(ecoHydro, month, T1, T2, change, PerDif, p.value)  
    
    fwrite(monthChange, '03_output/table_month_wilcox.csv')
    
  # select all of the significant results
    
    sigMonth <- monthChange %>% 
      filter(p.value <= 0.05 & change < 0)
#-------------------------------------------------------------------------------
# Private vs public -----
# Find the avg amt of water for public/private in each ecohydroregion -----
# Compare proportions of time period 1 to 2  -----
 
  # Calculate the proportinal differences between the mean private and public 
  # wetHa for each ecoHydroregion -----
  
    ownHydro <- wetTerm %>% 
      group_by(ecoHydro, ownAg, month, year, term) %>%                          # group columns that you want to run the summarizing function over
      summarise(wetSum = sum(wetHa)) %>%                                        # sum the wetHa to get the total monthly water for each ownership
      ungroup() %>%                                                             # ungroup
      group_by(ecoHydro, ownAg, year, term) %>%                                 # group again
      summarise(wetMean = mean(wetSum)) %>%                                     # take the mean of the monthly totals to get the average annual values for water on public and private lands
      ungroup()
    
    ownHydro02 <- ownHydro %>% 
      group_by(term, ownAg, ecoHydro) %>% 
      summarise(wetMean = mean(wetMean)) %>% 
      spread(term, wetMean) %>%                                                 # spread the data so you can do some column math
      mutate(perDif = ((T1-T2)/T1)*-100,                                        # Calculate the percent differences between time periods
            Change = T2-T1)                                                     # Calculate the change between the time periods

  # Calculate Standard deviation -----

    SDown <- ownHydro %>% 
        group_by(ecoHydro, ownAg, term) %>% 
        summarise(SD = sd(wetMean)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 'T1',
               SD2 = 'T2')            
  
  # Wilcoxon Test based on ownAg -----
    
    # Split data by ownAg  and run test-----
      
      # Private -----
      
        privWilcox <-  ownHydro %>%    
          filter(ownAg == 'Private') %>%                                        # filter data to only include private
          split(.$ecoHydro) %>%                                                 # '.' shorthand for the dataframe
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%                      # iterates whole process over each region
          map_df(broom::tidy, .id = 'ecoHydro') %>%                             # kicks out dataframe with p-valuves by region and season
          mutate(ownAg = 'Private')                                             # make new column with with ownership value ('Private')
      
        pubWilcox <- ownHydro %>% 
          filter(ownAg == 'Public') %>%                                         # filter data to only include public
          split(.$ecoHydro) %>%                                                 # '.' shorthand for the dataframe
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%                      # iterates whole process over each region
          map_df(broom::tidy, .id = 'ecoHydro') %>%                             # kicks out dataframe with p-valuves by region and season
          mutate(ownAg = 'Public')                                              # make new column with with ownership value ('Public')
    
    # Merge Wilcoxon test with data table -----   
  
      ownChange <- privWilcox %>% 
        full_join(pubWilcox) %>%                            
        full_join(ownHydro02) %>% 
        full_join(SDown) %>% 
        select(ecoHydro, ownAg, T1, SD1, T2, SD2, Change, perDif, p.value)
      
      # fwrite(ownChange, '03_output/table_ownAg_region_wilcox.csv')
  
  # Plots -----
      
    # Box Plot -----
    
      ggplot(ownHydro, aes(x = ownAg, y = wetMean, fill = term)) +  
        geom_boxplot() +
        scale_fill_manual(values=c("grey", "#FED179")) +                        # fill color for box plots
        facet_wrap(. ~ ecoHydro, ncol = 4, scales = 'free') +                   # creates plot for each region
        scale_y_continuous(labels = thousands) +
        # xlab("Time Period") +
        ylab("Inundated hectares x 1000") +
        theme_classic() +
        theme(legend.position = "bottom", legend.title = element_blank(), 
              axis.title.x = element_blank())
        
      
       ggsave("ownAgBoxplot.png", plot = last_plot(), width = 10, height = 5, units = "in", device='png', dpi=300)
      
    # Ownership trend lines -----
    
      ggplot(ownHydro, aes(x = year, y = wetMean, color = ownAg)) +
        scale_color_manual(values=c("#369499", "#98554E")) +                    # colors of the lines
        geom_line(size = 1, alpha = .5) +                                       # line size and transparency
        geom_smooth(method = 'lm', size=.2) +                                   # trend line and error
        scale_y_continuous(labels = thousands) +                                # divides units by 1000
        theme_light() +                                                         # color theme
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +                      # makes a plot for each region; 3 columns; y-axis different for each graph
        theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
        theme( axis.title.y=element_text(size=8),                               # text size
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8)) + 
        ggtitle("Regional Ownership Water Trends")

#-------------------------------------------------------------------------------
# Hydroperiod -----
  
  # Overall Hydroperiod trends per Ecohydroregion -----
      
    wetPeriod <- wetTerm %>% 
      group_by(ecoHydro, period, month, year, term) %>%                          # group columns that you want to run the summarizing function over
      summarise(wetSum = sum(wetHa)) %>%                                        # sum the wetHa to get the total monthly water for each ownership
      ungroup() %>%                                                             # ungroup
      group_by(ecoHydro, period, year, term) %>%                                 # group again
      summarise(wetMean = mean(wetSum)) %>%                                     # take the mean of the monthly totals to get the average annual values for water on public and private lands
      ungroup()
       
    wetPeriod02 <- wetPeriod %>% 
      group_by(term, period, ecoHydro) %>% 
      summarise(wetMean = mean(wetMean)) %>% 
      spread(term, wetMean) %>%                                                 # spread the data so you can do some column math
      mutate(perDif = ((T1-T2)/T1)*-100,                                        # Calculate the percent differences between time periods
            Change = T2-T1)                                                     # Calculate the change between the time periods

    # Plot -----
      
      ggplot(wetPeriod, aes(x = year, y = wetMean, color = period)) +
        geom_line(size = 1, alpha = .5) +                                       # line size and transparency
        scale_y_continuous(labels = thousands) +                                # divides units by 1000
        theme_light() +                                                         # color theme
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8)) + 
        ggtitle("Water Surface Area Trend by Hydroperiod") 
        
    # Test -----
      
      # Calculate Standard deviation -----
  
        SDperiod <- wetPeriod %>% 
            group_by(ecoHydro, period, term) %>% 
            summarise(SD = sd(wetMean)) %>% 
            ungroup() %>% 
            spread(term, SD) %>% 
            rename(SD1 = 'T1',
                   SD2 = 'T2')
        
      # Split data by period and run test-----
        
        # temp -----
        
          tempWilcox <-  wetPeriod %>%    
            filter(period == 'temp') %>%                      
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(period = 'temp')
        
        # seasonal -----
        
          seasWilcox <-  wetPeriod %>%    
            filter(period == 'seasonal') %>%                      
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(period = 'seasonal')
        
        # semi-perm -----
        
          semiWilcox <-  wetPeriod %>%    
            filter(period == 'semi') %>%                      
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(period = 'semi')
        
      # Merge Wilcoxon test with data table -----   
        
        perChange <- tempWilcox %>% 
          full_join(seasWilcox) %>% 
          full_join(semiWilcox) %>% 
          full_join(wetPeriod02) %>% 
          full_join(SDperiod) %>% 
          select(ecoHydro, period, T1, SD1, T2, SD2, Change, perDif, p.value)
          
      # Write table -----
        
        fwrite(perChange, '03_output/table_hydroperiod_region_wilcox.csv')

    # Boxplot -----
  
      ggplot(wetPeriod, aes(x = term, y = wetMean, fill = period)) +  
        geom_boxplot() +
        scale_fill_manual(values=c("#23C9B6", "#FED179", "#C07C6B")) +  # fill color for box plots
        facet_wrap(. ~ ecoHydro, scales = 'free', ncol = 4) +          # creates plot for each region
        scale_y_continuous(labels = thousands) +
        xlab("Time Period") +
        ylab("Inundated hectares x 1000") +
        labs(fill = 'Hydroperiod') +
        theme_classic() 
        
#-------------------------------------------------------------------------------      
# Hydroperiod vs ownAg -----
# What is the proportion of each hydroperiod that is private/public? -----
    
    wetPeriod03 <- wetTerm %>% 
      group_by(ecoHydro, ownAg, period, year, month, term) %>%                  # grouping
      summarise(wetHa = sum(wetHa)) %>%                                         # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(ecoHydro, ownAg, period, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      group_by(term, ownAg, ecoHydro, period) %>%                               # group again
      summarise(wetMean = mean(wetMean)) %>%                                    # take the mean of summed wetHa
      spread(ownAg, wetMean) %>%                                                # spread the data so you can do some column math
      mutate(Pub = (Public/(Public + Private)*100),                             # column giving the proportion of public land for each hydroperiod
             Pri = (Private/(Public + Private)*100))                            # column giving the proportion of private land for each hydroperiod

    wetPeriod04 <- wetPeriod03 %>% 
      gather(owner, proportion, 'Pri', 'Pub') %>%                               # gather the 'Pri' and 'Pub' columns into one column named 'proportion'
      select(term, ecoHydro, period, owner, proportion)
  
  # Write table -----

    fwrite(wetPeriod03, '03_output/table_hydroperiod_ownAg_portion.csv')
    
    # Plot -----
    # T1 -----
    
      ggplot(
        subset(wetPeriod04, term == 'T1'),
        aes(x = period, y = proportion, fill = owner)) +
        geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
        ylim(0, 100) +
        facet_wrap(. ~ ecoHydro, scales = 'fixed') +
        scale_fill_manual(values=c("#369499", "#98554E")) +
        ggtitle("Hydroperiod and Ownership, T1 (1988-2003)") +
        xlab("Hydroperiod") +
        ylab("Proportion of Surface Water") +
        theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 
    
    # T2 -----
    
      ggplot(
          subset(wetPeriod04, term == 'T2'),
          aes(x = period, y = proportion, fill = owner)) +
          geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
          ylim(0, 100) +
          facet_wrap(. ~ ecoHydro, scales = 'fixed') +
          scale_fill_manual(values=c("#369499", "#98554E")) +
          ggtitle("Hydroperiod and Ownership, T2 (2004-2020)") +
          xlab("Hydroperiod") +
          ylab("Proportion of Surface Water") +
          theme(
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(size = 0.5, linetype = "solid",
                                       colour = "black")) 

#-------------------------------------------------------------------------------      
# Hydroperiod vs wetType -----
# What is the proportion of each hydroperiod for each wetType? -----
    
    wetPeriod05 <- wetTerm %>% 
      group_by(ecoHydro, wetType, period, year, month, term) %>%                
      summarise(wetHa = sum(wetHa)) %>%                                         
      ungroup() %>% 
      group_by(ecoHydro, wetType, period, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      group_by(term, wetType, ecoHydro, period) %>%                             
      summarise(wetMean = mean(wetMean)) %>%                                    # gives the mean acreage of each hydroperiod for each wetland type
      spread(period, wetMean) %>%                                               # spread the data so you can do some column math
      mutate(temporary = (temp/(temp + seasonal + semi)*100),                   # Columns giving the percentage of temp, semi, and seasonal
             seas = (seasonal/(temp + seasonal + semi)*100),
             semipermanent = (semi/(temp + seasonal + semi)*100))
      

    wetPeriod06 <- wetPeriod05 %>% 
      gather(hydroPer, proportion, 'temporary', 'seas', 'semipermanent') %>%    # gather the listed columns so that one column (hydroPer) describes hydroperiod type and the other column (proportion) gives the proportion of the hydroperiod for the given wetland type 
      select(term, ecoHydro, wetType, hydroPer, proportion)
    
    # Write table -----

       fwrite(wetPeriod05, '03_output/table_hydroperiod_wetType_portion.csv')

#-------------------------------------------------------------------------------
# wetType -----
  
  # Overall wetType trends per Ecohydroregion -----
      
  typeWet <- wetTerm %>% 
    group_by(ecoHydro, wetType, month, year, term) %>% 
    summarise(wetSum = sum(wetHa)) %>% 
    ungroup() %>%
    mutate(year = as.numeric(year)) %>% 
    group_by(ecoHydro, wetType, year, term) %>%                                
    summarise(wetMean = mean(wetSum)) %>%                                    
    ungroup()
    
       
  wetType02 <- typeWet %>% 
    group_by(term, wetType, ecoHydro) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    spread(term, wetMean) %>%                                                   # spread the data so you can do some column math
    mutate(perDif = ((T1-T2)/T1)*-100,                                          # Calculate the percent differences between time periods
          Change = T2-T1)                                                       # Calculate the change between the time periods

      
  # Plot -----
    
    ggplot(typeWet, aes(x = year, y = wetMean, color = wetType)) +
      geom_line(size = 1, alpha = .5) +                                         # line size and transparency
      scale_y_continuous(labels = thousands) +                                  # divides units by 1000
      theme_light() +                                                           # color theme
      ylab('Inundated hectares x 1000') + 
      facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
      theme(strip.background =element_rect(fill="#7A7A7A")) +                   # color of graph title boxes
      theme( axis.title.y=element_text(size=8), 
             axis.title.x=element_text(size=8),
             axis.text=element_text(size=8)) + 
      ggtitle("Water Surface Area Trend by wetType")
    
  # Test -----
  
    # Calculate Standard deviation -----

      SDwetType <- typeWet %>% 
        group_by(ecoHydro, wetType, term) %>% 
        summarise(SD = sd(wetMean)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 'T1',
               SD2 = 'T2')
  
    # Split data by period and run test-----
      
      # riverine -----
      
        rivWilcox <-  typeWet %>%    
          filter(wetType == 'riv') %>%                      
          split(.$ecoHydro) %>%                              
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
          map_df(broom::tidy, .id = 'ecoHydro') %>%
          mutate(wetType = 'riv')
      
      # wetlands -----
      
        wetWilcox <-  typeWet %>%    
          filter(wetType == 'wet') %>%                      
          split(.$ecoHydro) %>%                              
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
          map_df(broom::tidy, .id = 'ecoHydro') %>%
          mutate(wetType = 'wet')
      
      # flooded ag -----
      
        wetAgWilcox <-  typeWet %>%    
          filter(wetType == 'wetAg') %>%                      
          split(.$ecoHydro) %>%                              
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
          map_df(broom::tidy, .id = 'ecoHydro') %>%
          mutate(wetType = 'wetAg')
  
      # managed wetlands -----
      
        wetManWilcox <-  typeWet %>%    
          filter(wetType == 'wetMan') %>%                      
          split(.$ecoHydro) %>%                              
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
          map_df(broom::tidy, .id = 'ecoHydro') %>%
          mutate(wetType = 'wetMan')
      
    # Merge Wilcoxon test with data table -----   
      
      wetTypeChange <- rivWilcox %>% 
        full_join(wetWilcox) %>% 
        full_join(wetAgWilcox) %>%
        full_join(wetManWilcox) %>%
        full_join(wetType02) %>% 
        full_join(SDwetType) %>% 
        select(ecoHydro, wetType, T1, SD1, T2, SD2, Change, perDif, p.value)
        
    # Write table -----
      
      fwrite(wetTypeChange, '03_output/table_wetType_region_wilcox.csv')
        
    
#-------------------------------------------------------------------------------      
# Ownership vs wetType -----
# What is the proportion of each hydroperiod for each wetType? -----
    
    wetPeriod07 <- wetTerm %>% 
      group_by(ecoHydro, wetType, ownAg, year, month, term) %>%                
      summarise(wetHa = sum(wetHa)) %>%                                         
      ungroup() %>% 
      group_by(ecoHydro, wetType, ownAg, year, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      group_by(term, wetType, ecoHydro, ownAg) %>%                             
      summarise(wetMean = mean(wetMean)) %>%                                    # gives the mean acreage of each ownAg for each wetland type
      spread(ownAg, wetMean) %>%                                                # spread the data so you can do some column math
      mutate_all(~replace(., is.na(.), 0)) %>%                                  # make NA's zeros (some places didn't have private WetMan, for example)
      mutate(Pub = (Public/(Public + Private)),  
             Pri = (Private/(Public + Private)))
      
    wetPeriod08 <- wetPeriod07 %>% 
      gather(owner, proportion, 'Pri', 'Pub') %>%                               # Gives the proportion of each wetland type that is either private or public
      select(term, ecoHydro, wetType, owner, proportion)
    
  # Write table -----
        
        fwrite(wetPeriod07, '03_output/table_wetType_ownAg_portion.csv')
    
#-------------------------------------------------------------------------------
# What's the total area covered by each wetland attribute across all regions?
   
  # By land ownership ----- 
    
    ownTotal <- wetTerm %>% 
      group_by(ownAg, year, month, term) %>%                                    # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                                        # summarise the grouped columns by summing the wetHa found with in each month
      ungroup() %>%                                                             # Ungroup: the result is the monthly total of water for each year and region
      group_by(ownAg, year, term) %>%                                           # Group the columns that you want to run a function over
      summarise(wetMean = mean(wetSum)) %>%                                     # Summarise the grouped columns by taking the mean of the monthly totals to get a mean annual value for each region
      ungroup() %>% 
      group_by(ownAg, term) %>% 
      summarise(wetMean = mean(wetMean))
    
  # By Hydroperiod -----
    
    hydroTotal <- wetTerm %>% 
      group_by(period, year, month, term) %>%                                    # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                                        # summarise the grouped columns by summing the wetHa found with in each month
      ungroup() %>%                                                             # Ungroup: the result is the monthly total of water for each year and region
      group_by(period, year, term) %>%                                           # Group the columns that you want to run a function over
      summarise(wetMean = mean(wetSum)) %>%                                     # Summarise the grouped columns by taking the mean of the monthly totals to get a mean annual value for each region
      ungroup() %>% 
      group_by(period, term) %>% 
      summarise(wetMean = mean(wetMean))
    
  # By wetland type ----
    
    typeTotal <- wetTerm %>% 
      group_by(wetType, year, month, term) %>%                                    # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                                        # summarise the grouped columns by summing the wetHa found with in each month
      ungroup() %>%                                                             # Ungroup: the result is the monthly total of water for each year and region
      group_by(wetType, year, term) %>%                                           # Group the columns that you want to run a function over
      summarise(wetMean = mean(wetSum)) %>%                                     # Summarise the grouped columns by taking the mean of the monthly totals to get a mean annual value for each region
      ungroup() %>% 
      group_by(wetType, term) %>% 
      summarise(wetMean = mean(wetMean))
    
    
  