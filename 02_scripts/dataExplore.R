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
  library('leaflet')
  library('sf')
  library('sp')
  library('ggridges')
  library('gridExtra')

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }

# Working directory -----

  # setwd("/Users/sc148852/Box/R/ibisSites/tablesFinal") # change site folder here
  
# Read in data -----
  
  # wetArea <- read_csv("01_data/08_final_hydro_data/wetAreaFin.csv")
  # hydroGra <- read_csv("01_data/08_final_hydro_data/hydroGraFin.csv")
  
#-------------------------------------------------------------------------------
# Extra data-cleaning that has arisen as I explore the code -----

  # Filter out wetType = 'res'  -----
  
    wetArea <- wetArea %>%
      filter(wetType != 'res')
  
    hydroGra <- hydroGra %>%
      filter(wetType != 'res')
    
  # write files to folder -----
  
    fwrite(wetArea, '01_data/08_final_hydro_data/wetAreaFin02.csv')
    fwrite(hydroGra, '01_data/08_final_hydro_data/hydroGraFin02.csv')
    
  # read back in newest / cleanest data -----
 
    wetArea <- fread('01_data/08_final_hydro_data/wetAreaFin02.csv')
    hydroGra <- fread('01_data/08_final_hydro_data/hydroGraFin02.csv')
    
#-------------------------------------------------------------------------------
# wetHa for ecohydroregion over time -----
  
  # Group, Sum, Plot -----
    
    # Group -----
  
      wetArea01 <- (wetArea) %>%            # name variable
        filter(month > 3, month < 9) %>%         # filter to April through August
        group_by(ecoHydro, year) %>%        # select the columns you want to sum across
        summarise(wetHa=sum(wetHa)) %>%     # sum the wetHa -> result is the total wetHa for each year in each region
        ungroup() %>%                       # ungroup
        mutate(year = as.numeric(year))
   
    # plot -----
  
      wetPlot01 <- ggplot(wetArea01, aes(x = year, y = wetHa, color = ecoHydro)) +  
        scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",  # 
                                    "#1654E3", "#E35D16", "#8FB00B", "#E556EC", 
                                    "#DCB102")) +                   # colors of lines (made to match the map I made in QGIS)
        geom_line(size = 1, alpha = .5) +                           # line size and transparency
        geom_smooth(method = 'lm', size=.2) +                       # trend line and error
        scale_y_continuous(labels = thousands) +                    # divides units by 1000
        theme_light() +                                             # color theme
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +          # makes a plot for each region; 3 columns; y-axis different for each graph
        theme(strip.background =element_rect(fill="#7A7A7A")) +     # color of graph title boxes
        theme( axis.title.y=element_text(size=8),                   # text size
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8)) + 
        ggtitle("Water Surface Area Trend") +
        guides(color = FALSE)                                       # removes legend
    
  
# Check to make sure that data from the hydroGra plots the same as wetArea -----
  
    # Group -----
  
    #   hydroGra01 <- (hydroGra) %>% 
    #     group_by(ecoHydro, year) %>% 
    #     summarise(wetHa = sum(wetHa)) %>%
    #     ungroup() %>% 
    #     mutate(year = as.numeric(year))
    # 
    # # plot -----
    # 
    #   hydroPlot01 <- ggplot(hydroGra01, aes(x = year, y = wetHa)) +
    #     geom_line(size = 1, alpha = .5) + 
    #     geom_smooth(method = 'lm', size=.2) + 
    #     scale_y_continuous(labels = thousands) +
    #     theme_light() +
    #     ylab('Inundated hectares x 1000') + 
    #     facet_wrap(~ecoHydro, ncol = 2, scales = 'free') +
    #     theme( axis.title.y=element_text(size=8), 
    #            axis.title.x=element_text(size=8),
    #            axis.text=element_text(size=8),
    #            legend.title = element_blank(),
    #            legend.position = 'bottom') +
    #     ggtitle("wetHa grouped by month for each site over time")

#-------------------------------------------------------------------------------
# first period of time vs second period of time for overall water per ecoHydro -----
  
  # Create column that sorts years among two periods -----
    
    wetTerm <- wetArea %>% 
      filter(month > 3, month < 9)                       # filter data to April - August
      mutate(year = as.numeric(year),                    # makes the column 'year' numeric
             term = 't1',                                # creates a column called 'term' with filled with 't1'
             term = replace(term, year >2003, 't2'))     # labels everything in 'term' as 't2' if 'year' is greater than 2003

  # calc difference between p1 and p1 as % -----
    
    wetChange <- wetTerm %>%
      group_by(term, ecoHydro, year) %>%                 # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                 # summarise the grouped columns by summing wetHa
      ungroup() %>%                                      # ungroup; the result is the total wetHa for each year, term, and region
      group_by(term, ecoHydro) %>%                       # group again by term and region
      summarise(wetMean = mean(wetSum)) %>%              # summarise the grouped columns by the average wetHa - this gives the avg wetHa across each term for each region
      spread(term, wetMean) %>%                          # spread is opposite of gather: there is now a column for t1 and one for t2
      mutate(change = ((t1-t2)/t1)*-1)                   # create column 'change' by doing column math with columns 't1' and 't2'
                                                         # The final result is a table with a column with the percent change from t1 to t2
    wetWilcoxTest <- wetTerm %>%
      group_by(term, ecoHydro, year) %>%                    # group by term, region, and year - function will run across these groups
      summarise(wetSum = sum(wetHa)) %>%                    # summarise the above groups by sum - the result is the total wetHa for each year for each region and term
      split(.$ecoHydro) %>%                                 # '.' shorthand for the dataframe - splits the data by region
      map(~wilcox.test(wetSum ~ term, data = .x)) %>%       # iterates whole process over each region (calculates the mean and tests for difference)
      map_df(broom::tidy, .id = 'ecoHydro')                 # kicks out dataframe with p-valuves by region and season
    
  # combine wilcoxon results with p1, p2 diff results -----
    
    wetWilcoxChange <- wetChange %>% 
      full_join(wetWilcoxTest) %>%                      # merge the Wilcoxon test results to the wetChange df     
      select(ecoHydro, t1, t2, change, p.value)         # select the columns that you want to keep
    
  # make box plots to show distibutions between t1 nad t2 surface water -----
  
    wetBox <- wetTerm %>%    
      group_by(term, ecoHydro, year) %>%                # data for the box plot -> wetHa summed for each year and each region with term labels
      summarise(wetSum = sum(wetHa))
    
    wetBoxPlot <- ggplot(wetBox, aes(x = term, y = wetSum, group = term, fill = term)) +  
      geom_boxplot() +
      scale_fill_manual(values=c("#369499", "#98554E")) +  # fill color for box plots
      facet_wrap(. ~ ecoHydro, scales = 'free') +          # creates plot for each region
      scale_y_continuous(labels = thousands) +
      xlab("Term") +
      ylab("Inundated hectares x 1000") +
      theme_bw() 
    
#-------------------------------------------------------------------------------
# Monthly water by region -----
   
  # Add term to dataframe; sum over region, year, month, and term
  # This gives the total wetHa for each month in each year for each region -----
  
    wetTermMonth <- wetArea %>%  
        mutate(year = as.numeric(year),                      # makes the column 'year' numeric
               term = 't1',                                  # creates a column called 'term' with filled with 't1'
               term = replace(term, year >2003, 't2')) %>%   # labels everything in 'term' as 't2' if 'year' is greater than 2003
      group_by(term, ecoHydro, year, month) %>%              # Group the columns that you want to summarise over
      summarise(wetSum = sum(wetHa)) %>% 
      mutate(month = case_when(month == '3' ~ 'Mar',
                               month == '4' ~ 'Apr',
                               month == '5' ~ 'May',
                               month == '6' ~ 'Jun',
                               month == '7' ~ 'Jul',
                               month == '8' ~ 'Aug',
                               month == '9' ~ 'Sept',
                               month == '10' ~ 'Oct'))
      
  # Create separate table for each ecohydroregion -----
    # (an alternative to this looping is to subset the data when making the ggplot)
    
    for(i in unique(wetTermMonth$ecoHydro)) {                # for each unique value in the 'ecoHydro' column...
      nam <- paste("df", i, sep = ".")                       # create variable 'nam' and rename to 'df.i' where i corresponds to a unique value in 'ecoHydro'
      assign(nam, wetTermMonth[wetTermMonth$ecoHydro==i,])   # assign to 'nam' a subset of the original dataframe. The subset is for a unique value of 'ecoHydro'
    }
    
    # rename these dataframes so they are easier to call -----
    
      greatBasin <- `df.Great Basin-Colorado Plateau`
      midRockies <- `df.Middle Rockies`
      mojaveDes <- `df.Mojave-Sanoran Deserts`
      norPlains <- `df.Northern Plains`
      norRockies <- `df.Northern Rockies`
      pacificNW <- `df.Pacific NW`
      pacificSW <- `df.Pacific SW`
      soPlains <- `df.Southern Plains`
      soRockies <- `df.Southern Rockies and Basins`
      
  # Plot -----
      
    # Ridgeline Plots -----
      
     GBridge <- greatBasin %>%                                                  # Call on greatBasin df
        arrange(wetSum) %>%                                                     # arrange the data according to wetSum (not sure this step is necessary)
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>%   # change the order of month column for plotting
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +          # I may not need the 'as.factor' now that I've changed the months from integers to strings
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Great Basin") +
                  xlab("wet hectares") +
                  facet_wrap(~term, ncol = 1, scales = 'fixed')   
      
      MRridge <- midRockies %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Middle Rockies") +
                  xlab("wet hectares") +
                  facet_wrap(~term, ncol = 1, scales = 'fixed') 
      
      NPridge <- norPlains %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Northern Plains") +
                  xlab("wet hectares") +
                  facet_wrap(~term, ncol = 1, scales = 'fixed') 
                
      NRridge <- norRockies %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Northern Rockies") +
                  xlab("wet hectares") +
                  facet_wrap(~term, ncol = 1, scales = 'fixed') 
      
      PNWridge <- pacificNW %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                    geom_density_ridges() +
                    theme_ridges() +
                    theme(legend.position = "none") +
                    labs(title = "Pacific NW") +
                    xlab("wet hectares") +
                    facet_wrap(~term, ncol = 1, scales = 'fixed') 
      
      SPridge <- soPlains %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                    geom_density_ridges() +
                    theme_ridges() +
                    theme(legend.position = "none") +
                    labs(title = "South Plains") +
                    xlab("wet hectares") +
                    facet_wrap(~term, ncol = 1, scales = 'fixed')
      
      SRridge <- soRockies %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                    geom_density_ridges() +
                    theme_ridges() +
                    theme(legend.position = "none") +
                    labs(title = "South Rockies") +
                    xlab("wet hectares") +
                    facet_wrap(~term, ncol = 1, scales = 'fixed')
      
      MSridge <- mojaveDes %>% 
        arrange(wetSum) %>% 
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", "Jul", "Jun", "May", "Apr", "Mar"))) %>% 
         ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +
                    geom_density_ridges() +
                    theme_ridges() +
                    theme(legend.position = "none") +
                    labs(title = "Mojave-Sonoran Desert") +
                    xlab("wet hectares") +
                    facet_wrap(~term, ncol = 1, scales = 'fixed')
      
      # Plot all of the ridge plots on one panel -----
      
        grid.arrange(arrangeGrob(GBridge, NRridge, MRridge, SRridge, ncol = 4),
                     arrangeGrob(NPridge, SPridge, PNWridge, MSridge, ncol = 4),
                     nrow = 2)
    
    # Line Graph -----
    
      ggplot(soRockies, # --> change the regional df here so I don't have to duplicate this code. 
             aes(x = year, y = wetSum, color = as.factor(month))) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
        scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                      "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
          geom_line(size = 1, alpha = .5) +                                       # line size and transparency
          geom_smooth(method = 'lm', size=.2) +                                   # trend line and error
          scale_y_continuous(labels = thousands) +                                # divides units by 1000
          theme_light() +                                                         # color theme
          ylab('Inundated hectares x 1000') + 
          facet_wrap(~month, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
          theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
          theme( axis.title.y=element_text(size=8),                               # text size
                 axis.title.x=element_text(size=8),
                 axis.text=element_text(size=8)) + 
          ggtitle("Southern Rockies \n Monthly Surface Area Trend") + # --> change title to mathch region
          guides(color = FALSE)  

# How has monthly water changed from t1 to t2? -----
  
  # find the mean amount of water for t1 and for t2 for each month -----   
   
    wetMonth <- wetTermMonth %>% 
        group_by(term, month, ecoHydro) %>%         # group by month, term, and region
        summarise(wetMean = mean(wetSum))           # find the mean wetHa for each month in each region for each term
    
  # Plot time series of average monthly water for t1 and t2 -----
      
    ggplot(wetMonth, 
         aes(x = month, y = wetMean, color = (term))) +               
      scale_color_manual(values=c("#369499", "#98554E")) +                      # colors of the lines
      geom_line(size = 1, alpha = .5) +                                         # line size and transparency
      # geom_smooth(method = 'lm', size=.2) +                                   # trend line and error
      scale_y_continuous(labels = thousands) +                                  # divides units by 1000
      theme_light() +                                                           # color theme
      ylab('Inundated hectares x 1000') + 
      facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +                        # makes a plot for each region; 3 columns; y-axis different for each graph
      theme(strip.background =element_rect(fill="#7A7A7A")) +                   # color of graph title boxes
      theme( axis.title.y=element_text(size=8),                                 # text size
             axis.title.x=element_text(size=8),
             axis.text=element_text(size=8)) + 
      ggtitle("Regional Monthly Surface Area Trend") 
    
  # Find the percent change for each month from t1 to t2 -----

    monthPercentChange <- wetMonth %>%              
      spread(term, wetMean) %>%                        
      mutate(change = ((t1-t2)/t1)*-1)  
    
#-------------------------------------------------------------------------------
# Private vs public -----
# Find the avg amt of water for public/private in each ecohydroregion -----
# Compare proportions of time period 1 to 2  -----
 
  # Create Term ----- 
  
    hydroTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 't1',                                # creates a column with filled with 'p1'
              term = replace(term, year >2003, 't2'))     # replaces 't1' with 't2' when year is >2002
    
  # Calculate the proportinal differences between the mean private and public 
  # wetHa for each ecoHydroregion -----
  
    ownHydro <- hydroTerm %>% 
      group_by(ecoHydro, ownAg, year, term) %>%           # grouping
      summarise(wetHa = sum(wetHa)) %>%                   # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(term, ownAg, ecoHydro) %>%                 # group again
      summarise(wetMean = mean(wetHa)) %>%                # take the mean of summed wetHa
      spread(term, wetMean) %>%                           # spread the data so you can do some column math
      mutate(change = ((t1-t2)/t1)*-1)                    # calculate difference between t1 and t2 as percent
  
  # Wilcoxon Test based on ownAg -----
    
    # Split data by ownAg  and run test-----
      
      # Private -----
      
        privWilcox <-  hydroTerm %>%    
          filter(ownAg == 'Private') %>%                      # filter data to only include private
          group_by(term, ecoHydro, year) %>% 
          summarise(wetSum = sum(wetHa)) %>% 
          split(.$ecoHydro) %>%                               # '.' shorthand for the dataframe
          map(~wilcox.test(wetSum ~ term, data = .x)) %>%     # iterates whole process over each region
          map_df(broom::tidy, .id = 'ecoHydro') %>%           # kicks out dataframe with p-valuves by region and season
          mutate(ownAg = 'Private')                           # make new column with with ownership value ('Private')
      
        pubWilcox <- hydroTerm %>% 
          filter(ownAg == 'Public') %>%                       # filter data to only include public
          group_by(term, ecoHydro, year) %>% 
          summarise(wetSum = sum(wetHa)) %>% 
          split(.$ecoHydro) %>%                               # '.' shorthand for the dataframe
          map(~wilcox.test(wetSum ~ term, data = .x)) %>%     # iterates whole process over each region
          map_df(broom::tidy, .id = 'ecoHydro') %>%           # kicks out dataframe with p-valuves by region and season
          mutate(ownAg = 'Public')                            # make new column with with ownership value ('Public')
    
    # Merge Wilcoxon test with data table    
  
      ownChange <- privWilcox %>% 
        full_join(pubWilcox) %>%                            
        full_join(ownHydro) %>% 
        select(ecoHydro, ownAg, t1, t2, change, p.value)
  
  # Summary Stats ----- 
    # Can use for making error bars 
    
    # total wetHa -----
    
      wetTotal <- hydroTerm %>% 
        group_by(ecoHydro, ownAg, year, term) %>%           
        summarise(wetHa = sum(wetHa)) %>% 
        group_by(ownAg, ecoHydro, term) %>% 
        summarise(wetMean = mean(wetHa),
                  wetSD = sd(wetHa),
                  n_change = n())
  
  # Plots -----
      
    # Box Plot -----
      
      ownBox <- hydroTerm %>%    
        group_by(term, ownAg, ecoHydro, year) %>%               
        summarise(wetSum = sum(wetHa))
    
      ownBoxPlot <- ggplot(ownBox, aes(x = term, y = wetSum, fill = ownAg)) +  
        geom_boxplot() +
        scale_fill_manual(values=c("#369499", "#98554E")) +  # fill color for box plots
        facet_wrap(. ~ ecoHydro, scales = 'free') +          # creates plot for each region
        scale_y_continuous(labels = thousands) +
        xlab("Term") +
        ylab("Inundated hectares x 1000") +
        theme_bw() 
      
    # Private water by term -----
      
      ggplot(
        subset(wetTotal, ownAg == "Private"),
        aes(x = wetMean, y = ecoHydro, fill = term)) +
        geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
        geom_errorbar(aes(xmin = wetMean - wetSD, xmax = wetMean + wetSD), 
                      width = .3, position = position_dodge(0.9), 
                      color =  "#505050") +
        scale_fill_manual(values=c("#369499", "#98554E")) +
        # xlim(0, 1600000) +
        ggtitle("Private Water by Term") +
        xlab("wetHa") +
        ylab("Ecohydroregion") +
        theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 
    
      # Public water by term -----
      
        ggplot(
          subset(wetTotal, ownAg == "Public"),
          aes(x = wetMean, y = ecoHydro, fill = term)) +
          geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
          geom_errorbar(aes(xmin = wetMean - wetSD, xmax = wetMean + wetSD), 
                        width = .3, position = position_dodge(0.9), 
                        color =  "#505050") +
          scale_fill_manual(values=c("#369499", "#98554E")) +
          ggtitle("Public Water by Term") +
          xlab("wetHa") +
          ylab("Ecohydroregion") +
          theme(
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(size = 0.5, linetype = "solid",
                                       colour = "black")) 
    
    # Percent change by ownership -----
    
      ownHydroChange <- ownChange %>% 
        gather(Term, wetHa, 't1', 't2')
      
      ggplot(ownHydroChange, aes(x = change, y = ecoHydro, fill = ownAg)) +
        geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
        scale_fill_manual(values=c("#369499", "#98554E")) +
        ggtitle("Percent Change of Water by Ownership") +
        xlab("Percent Change") +
        ylab("Ecohydroregion") +
        theme(legend.title = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 
      
      # Ownership trend lines -----
      
        ownTrend <- hydroTerm %>% 
          group_by(ecoHydro, ownAg, year, term) %>%           # grouping
          summarise(wetHa = sum(wetHa)) %>%                   # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
          ungroup()
      
        ggplot(ownTrend, aes(x = year, y = wetHa, color = ownAg)) +
          scale_color_manual(values=c("#369499", "#98554E")) +                      # colors of the lines
          geom_line(size = 1, alpha = .5) +                                         # line size and transparency
          # geom_smooth(method = 'lm', size=.2) +                                   # trend line and error
          scale_y_continuous(labels = thousands) +                                  # divides units by 1000
          theme_light() +                                                           # color theme
          ylab('Inundated hectares x 1000') + 
          facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +                        # makes a plot for each region; 3 columns; y-axis different for each graph
          theme(strip.background =element_rect(fill="#7A7A7A")) +                   # color of graph title boxes
          theme( axis.title.y=element_text(size=8),                                 # text size
                 axis.title.x=element_text(size=8),
                 axis.text=element_text(size=8)) + 
          ggtitle("Regional Ownership Water Trends")

#-------------------------------------------------------------------------------
# Hydroperiod -----
  
  # Overall Hydroperiod trends per Ecohydroregion -----
      
    # Group -----
      
    wetPeriod <- hydroTerm %>% 
      group_by(ecoHydro, period, year) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      ungroup() %>%
      mutate(year = as.numeric(year))
      
    # Plot -----
      
      ggplot(wetPeriod, aes(x = year, y = wetHa, color = period)) +
        geom_line(size = 1, alpha = .5) +                           # line size and transparency
        scale_y_continuous(labels = thousands) +                    # divides units by 1000
        theme_light() +                                             # color theme
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme(strip.background =element_rect(fill="#7A7A7A")) +     # color of graph title boxes
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8)) + 
        ggtitle("Water Surface Area Trend by Hydroperiod") 
      
#-------------------------------------------------------------------------------      
# Hydroperiod vs ownAg -----
# What is the proportion of each hydroperiod that is private/public? -----
    
    wetPeriod02 <- hydroTerm %>% 
      group_by(ecoHydro, ownAg, period, year, term) %>%           # grouping
      summarise(wetHa = sum(wetHa)) %>%                   # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(term, ownAg, ecoHydro, period) %>%                 # group again
      summarise(wetMean = mean(wetHa)) %>%                # take the mean of summed wetHa
      spread(ownAg, wetMean) %>%                           # spread the data so you can do some column math
      mutate(Pub = (Public/(Public + Private)),
             Pri = (Private/(Public + Private)))
      
    wetPeriod03 <- wetPeriod02 %>% 
      # gather(ownAg, wetHa, 'Private', 'Public') %>% 
      gather(owner, proportion, 'Pri', 'Pub') %>% 
      select(term, ecoHydro, period, owner, proportion)
    
    # Plot -----
    # t1 -----
    
      ggplot(
        subset(wetPeriod03, term == 't1'),
        aes(x = period, y = proportion, fill = owner)) +
        geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
        ylim(0, 1.0) +
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
    
    # t2 -----
    
      ggplot(
          subset(wetPeriod03, term == 't2'),
          aes(x = period, y = proportion, fill = owner)) +
          geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
          ylim(0, 1.0) +
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
    
    wetPeriod04 <- hydroTerm %>% 
      group_by(ecoHydro, wetType, period, year, term) %>%           
      summarise(wetHa = sum(wetHa)) %>%                   # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(term, wetType, ecoHydro, period) %>%                 
      summarise(wetMean = mean(wetHa)) %>%                 # take the mean of summed wetHa
      spread(period, wetMean) %>%                           # spread the data so you can do some column math
      mutate(temporary = (temp/(temp + seasonal + semi)),            # Columns giving the percentage of temp, semi, and seasonal
             seasonal = (seasonal/(temp + seasonal + semi)),
             semipermanent = (semi/(temp + seasonal + semi)))
      
    wetPeriod05 <- wetPeriod04 %>% 
      # gather(ownAg, wetHa, 'Private', 'Public') %>% 
      gather(hydroPer, proportion, 'temporary', 'seasonal', 'semipermanent') %>% 
      select(term, ecoHydro, wetType, hydroPer, proportion)
    
    # Plot -----
    # t1 -----
    
      ggplot(
        subset(wetPeriod05, term == 't1'),
        aes(x = wetType, y = proportion, fill = hydroPer)) +
        geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
        ylim(0, 1.0) +
        facet_wrap(. ~ ecoHydro, scales = 'fixed') +
        scale_fill_manual(values=c("#369499", "#98554E", "#D7E569")) +
        ggtitle("Hydroperiod and Wetland Type, T1 (1988-2003)") +
        xlab("Hydroperiod") +
        ylab("Proportion of Surface Water") +
        theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 
    
    # t2 -----
    
      ggplot(
          subset(wetPeriod05, term == 't2'),
          aes(x = wetType, y = proportion, fill = hydroPer)) +
          geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
          ylim(0, 1.0) +
          facet_wrap(. ~ ecoHydro, scales = 'fixed') +
          scale_fill_manual(values=c("#369499", "#98554E", "#D7E569")) +
          ggtitle("Hydroperiod and Wetland Type, T2 (2004-2020)") +
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
# wetType -----
  
  # Overall wetType trends per Ecohydroregion -----
      
    # Group -----
      
    typeWet <- hydroTerm %>% 
      group_by(ecoHydro, wetType, year) %>% 
      summarise(wetHa = sum(wetHa)) %>% 
      ungroup() %>%
      mutate(year = as.numeric(year))
      
    # Plot -----
      
      ggplot(typeWet, aes(x = year, y = wetHa, color = wetType)) +
        geom_line(size = 1, alpha = .5) +                           # line size and transparency
        scale_y_continuous(labels = thousands) +                    # divides units by 1000
        theme_light() +                                             # color theme
        ylab('Inundated hectares x 1000') + 
        facet_wrap(~ecoHydro, ncol = 3, scales = 'free') +
        theme(strip.background =element_rect(fill="#7A7A7A")) +     # color of graph title boxes
        theme( axis.title.y=element_text(size=8), 
               axis.title.x=element_text(size=8),
               axis.text=element_text(size=8)) + 
        ggtitle("Water Surface Area Trend by wetType")   
    
#-------------------------------------------------------------------------------      
# Ownership vs wetType -----
# What is the proportion of each hydroperiod for each wetType? -----
    
    wetPeriod06 <- hydroTerm %>% 
      group_by(ecoHydro, wetType, ownAg, year, term) %>%           
      summarise(wetHa = sum(wetHa)) %>%                   # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(term, wetType, ecoHydro, ownAg) %>%                 
      summarise(wetMean = mean(wetHa)) %>%                 # take the mean of summed wetHa
      spread(ownAg, wetMean) %>%                          # spread the data so you can do some column math
      mutate_all(~replace(., is.na(.), 0)) %>%            # make NA's zeros (some places didn't have private WetMan, for example)
      mutate(Pub = (Public/(Public + Private)),  
             Pri = (Private/(Public + Private)))
      
    wetPeriod07 <- wetPeriod06 %>% 
      gather(owner, proportion, 'Pri', 'Pub') %>% 
      select(term, ecoHydro, wetType, owner, proportion)
    
    # Plot -----
    
    # Set y- axis for each facet wrap
      
      # scales_y <- list(
      #   `Great Basin-Colorado Plateau` = scale_y_continuous(limits = c(0, ), breaks = seq(5, 25, 5)),
      #   `f` = scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)),
      #   `r` = scale_y_continuous(limits = c(10, 20), breaks = seq(10, 20, 2))
      # )
    
    # t1 -----
    
      ggplot(
        subset(wetPeriod07, term == 't1'),
        aes(x = wetType, y = proportion, fill = owner)) +
        geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
        facet_wrap(. ~ ecoHydro, scales = 'fixed') +
        scale_fill_manual(values=c("#369499", "#98554E")) +
        ggtitle("Ownership and WetType, T1") +
        xlab("Wetland Type") +
        ylab("Proportion of Surface Water") +
        theme(
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 
    
    # t2 -----
    
      ggplot(
          subset(wetPeriod07, term == 't2'),
          aes(x = wetType, y = proportion, fill = owner)) +
          geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
          facet_wrap(. ~ ecoHydro, scales = 'fixed') +
          scale_fill_manual(values=c("#369499", "#98554E")) +
          ggtitle("Ownership and WetType, T2") +
          xlab("Wetland Type") +
          ylab("Proportion of Surface Water") +
          theme(
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(size = 0.5, linetype = "solid",
                                       colour = "black")) 
#-------------------------------------------------------------------------------
# sites and water: What is the overall trend at individual ibis blobs (as opposed to region)?
  
  # Read in site data -----
    
    sites <- fread("01_data/09_site_names/siteNamesID.csv")
    
  # summarize data before join -----
      
    siteHydro <- hydroTerm %>% 
        group_by(idPoly, year, term) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = 'idPoly')
  
  # Join the data -----
    
    siteHydro01 <- siteHydro %>% 
        group_by(siteName, Latitude, Longitude, year, term) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(siteName, Latitude, Longitude, term) %>% 
        summarise(wetMean = mean(wetHa)) %>% 
        spread(term, wetMean) %>%                           # spread the data so you can do some column math
        mutate(change = ((t1-t2)/t1)*-1)                    # calculate percent change and add as a new column
  
  # Wilcoxon test ----- 
      
    siteWilcox <-  siteHydro %>%    
      group_by(term, siteName, Latitude, Longitude, year) %>% 
      summarise(wetSum = sum(wetHa)) %>% 
      split(.$siteName) %>%                               # '.' shorthand for the dataframe
      map(~wilcox.test(wetSum ~ term, data = .x)) %>%     # iterates whole process over each region
      map_df(broom::tidy, .id = 'siteName')               # kicks out dataframe with p-valuves by region and season
    
    # Merge Wilcoxon test with data table    
  
      siteChange <- siteHydro01 %>% 
        full_join(siteWilcox) %>%
        select(siteName, t1, t2, change, p.value)
    
  # Map -----
    
    # Create a factor by which to color code the sites by -----
    # In this case, decreasing water will be red and increasing will be blue
      
      siteCat <- siteChange %>% 
        mutate(cat = case_when(change < 0 & p.value < 0.05 ~ "sigDec",
                               change < 0 & p.value > 0.05 ~ "noDec",
                               change > 0 & p.value < 0.05 ~ "sigInc",
                               change > 0 & p.value > 0.05 ~ "noInc"))
    
    # Define the color palette -----
      
      pal <- colorFactor(palette = c("#369499", "#98554E", "#00eaf2","#cc0a00"), 
                         levels = c("noInc","noDec", "sigInc", "sigDec"))  
    
    # Read in ecohydroRegion shapefile -----
    
      ecoHydroBounds <- st_read('/Users/sc148852/Box/Qgis/Thesis/EcoHydro Regions/ecoHydroWest_UTM12.shp')
   
      # Transform CRS -----
    
        ecoHydrodWGS <- st_transform(ecoHydroBounds, CRS("+init=epsg:4326"))
    
    # Create the leaflet map with the site locations -----
    
      leaflet(data = siteCat) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%     
      setView(lng = -121.7373, lat = 41.9875, zoom = 5) %>% 
      addPolygons(data = ecoHydrodWGS,
                color = "slategray", weight = 2, smoothFactor = 0.2,
                opacity = 0.3, fillOpacity = 0.5,
                fillColor = 'transparent',
                stroke = T) %>% 
      addCircleMarkers(label = ~siteName, 
                       labelOptions = labelOptions(noHide = F, 
                                                   direction = 'left',    
                                                   textOnly = T,          
                                                   offset=c(-10,-5)),    
                       radius = 6,
                       stroke = FALSE,
                       color = ~pal(cat),
                       fillOpacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values = ~cat, title = "Trend")

#-------------------------------------------------------------------------------
# sites and hydroperiod: 
# What are the hydroperiod trends at individual ibis blobs?

  # summarize data and then join -----
      
    sitePer <- hydroTerm %>% 
        group_by(idPoly, year, period, term) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = 'idPoly')
  
  # calculate mean amt of water per hydroperiod per term per site plus % change -----
    
    sitePer01 <- sitePer %>% 
        group_by(siteName, Latitude, Longitude, year, period, term) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(siteName, Latitude, Longitude, period, term) %>% 
        summarise(wetMean = mean(wetHa)) %>% 
        spread(term, wetMean) %>%                          
        mutate(change = ((t1-t2)/t1)*-1)
 
  # Wilcoxon test for each hydro period  ----- 
    
    # Temporary wetlands -----  
      
      tempWilcox <- sitePer %>%
        filter(period == 'temp') %>%                       
        group_by(term, Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                          # Used 'Latitude' instead of 'siteName' b/c all latitudes are unique but some siteNames are duplicated 
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(period = 'temp',
               Latitude = as.numeric(Latitude))                           

    # Seasonal wetlands ----- 
      
      seasWilcox <- sitePer %>%
        filter(period == 'seasonal') %>%                     
        group_by(term,  Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
        map_df(broom::tidy, .id = 'Latitude') %>%          
        mutate(period = 'seasonal',
                Latitude = as.numeric(Latitude))
      
    # semi-perm wetlands ----- 

      semiWilcox <- sitePer %>%
        filter(period == 'semi') %>%                       
        group_by(term, Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                               
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(period = 'semi',
                Latitude = as.numeric(Latitude))                          

    # Merge Wilcoxon test with data table 
    
      perChange <- tempWilcox %>%
        full_join(seasWilcox) %>%
        full_join(semiWilcox) %>%
        full_join(sitePer01, by = c('Latitude', 'period')) %>%  # join by both latitude and period
        select(siteName, Latitude, Longitude, period, t1, t2, change, p.value)
      
      perChange01 <- na.omit(perChange)      # remove the rows that have NA's (I don't know where these rows came from but their values are small)
      
  # Map -----
    
    # Create a factor by which to color code the sites by -----
    # In this case, decreasing water will be red and increasing will be blue
      
      perCat <- perChange01 %>% 
        mutate(cat = case_when(change < 0 & p.value < 0.05 ~ "sigDec",
                               change < 0 & p.value > 0.05 ~ "noDec",
                               change > 0 & p.value < 0.05 ~ "sigInc",
                               change > 0 & p.value > 0.05 ~ "noInc"))
    
    # Define the color palette -----
      
      pal02 <- colorFactor(palette = c("#369499", "#98554E", "#00eaf2","#cc0a00"), 
                         levels = c("noInc","noDec", "sigInc", "sigDec"))
    
    # Create the leaflet map with the site locations -----
      
      tempCat <- perCat %>% 
        filter(period =='temp')
      
      seasCat <- perCat %>% 
        filter(period == 'seasonal')
      
      permCat <- perCat %>% 
        filter(period == 'semi')
    
      leaflet(data = permCat) %>%  # <-- change the hydroperiod being used here
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%     
      setView(lng = -121.7373, lat = 41.9875, zoom = 5) %>% 
      addPolygons(data = ecoHydrodWGS,
                color = "slategray", weight = 2, smoothFactor = 0.2,
                opacity = 0.3, fillOpacity = 0.5,
                fillColor = 'transparent',
                stroke = T) %>% 
      addCircleMarkers(label = ~siteName, 
                       labelOptions = labelOptions(noHide = F, 
                                                   direction = 'left',    
                                                   textOnly = T,          
                                                   offset=c(-10,-5)),    
                       radius = 6,
                       stroke = FALSE,
                       color = ~pal02(cat),
                       fillOpacity = 0.8) %>% 
      addLegend("bottomright", pal = pal, values = ~cat, title = "Trend")

      
      
    
    
  