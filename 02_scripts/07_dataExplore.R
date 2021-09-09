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

# Working directory -----

  # setwd("/Users/sc148852/Box/R/ibisSites/tablesFinal") # change site folder here
  
# Read in data -----
  
  # wetArea <- read_csv("01_data/08_final_hydro_data/wetAreaFin.csv")
  # hydroGra <- read_csv("01_data/08_final_hydro_data/hydroGraFin.csv")
  
# Color Palette -----
  #3A2139 = 'dark purple'
  #0B646E = 'ming' (dark cerulean)
  #23C9B6 = 'medium turquoise'
  #FED179 = 'orange-yellow crayola'
  #C07C6B = 'blast off bronze'
  
#-------------------------------------------------------------------------------
# Extra data-cleaning that has arisen as I explore the code -----

  # Filter out wetType = 'res'  -----
  
  #   wetArea <- wetArea %>%
  #     filter(wetType != 'res')
  # 
  #   hydroGra <- hydroGra %>%
  #     filter(wetType != 'res')
  #   
  # # write files to folder -----
  # 
  #   fwrite(wetArea, '01_data/08_final_hydro_data/wetAreaFin02.csv')
  #   fwrite(hydroGra, '01_data/08_final_hydro_data/hydroGraFin02.csv')
    
  # read back in newest / cleanest data -----
 
    # wetArea <- fread('01_data/08_final_hydro_data/wetAreaFin06.csv')
    hydroGra <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv')
    
#===============================================================================
#-------------------------------------------------------------------------------
# wetHa for ecohydroregion over time -----
  
  # Group, Sum, Plot -----
    
    # Group -----

      # wetArea01 <- (wetArea) %>%            # name variable
      #   filter(month > 3, month < 9) %>%         # filter to April through August
      #   group_by(ecoHydro, year) %>%        # select the columns you want to sum across
      #   summarise(wetHa=sum(wetHa)) %>%     # sum the wetHa -> result is the total wetHa for each year in each region
      #   ungroup() %>%                       # ungroup
      #   mutate(year = as.numeric(year))

    # plot -----

    # colors that I've used before:
    # scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",  #
    #                                 "#1654E3", "#E35D16", "#8FB00B", "#E556EC",
    #                                 "#DCB102")) +

      # ggplot(wetArea01, aes(x = year, y = wetHa)) +
      #   # scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",  #
      #   #                             "#1654E3", "#E35D16", "#8FB00B", "#E556EC",
      #   #                             "#DCB102")) +                   # colors of lines (made to match the map I made in QGIS)
      #   geom_line(size = 1, alpha = .5) +                           # line size and transparency
      #   geom_smooth(method = 'lm', size=.2, color = 'black') +                       # trend line and error
      #   scale_y_continuous(labels = thousands) +                    # divides units by 1000
      #   theme_light() +                                             # color theme
      #   ylab('Inundated hectares x 1000') +
      #   facet_wrap(~ecoHydro, ncol = 2, scales = 'free') +          # makes a plot for each region; 3 columns; y-axis different for each graph
      #   theme(strip.background =element_rect(fill="#7A7A7A")) +     # color of graph title boxes
      #   theme( axis.title.y=element_text(size=8),                   # text size
      #          axis.title.x=element_text(size=8),
      #          axis.text=element_text(size=8)) +
      #   ggtitle("Water Surface Area Trend") +
      #   guides(color = FALSE)                                       # removes legend

  
# Check to make sure that data from the hydroGra plots the same as wetArea -----
  
    # Group -----

      hydroGra01 <- (hydroGra) %>%
        filter(month > 3, month < 9) %>%
        group_by(ecoHydro, year) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>%
        mutate(year = as.numeric(year))

    # plot -----

      ggplot(hydroGra01, aes(x = year, y = wetHa)) +
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
               legend.position = 'bottom') #+
        #ggtitle("wetHa grouped by month for each site over time")
  
    hydroGra02 <- hydroGra01 %>% 
      filter(ecoHydro == 'Southern Rockies and Basins')
    
    ggplot(hydroGra02, aes(x = year, y = wetHa)) +
        geom_line(size = 3, color = '#A5D0A8') +
        # geom_smooth(method = 'lm', size=.2) +
        scale_y_continuous(labels = thousands) +
        theme_minimal() +
        ylab('Inundated hectares x 1000') +
        theme( axis.title.y=element_text(size=20, color = '#c8c8c8'),
               axis.title.x=element_text(size=20, color = '#c8c8c8'),
               axis.text=element_text(size=12, color = '#c8c8c8'),
               legend.title = element_blank(),
               legend.position = 'bottom') +
       theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
      # theme(plot.background = element_rect(fill = "#170A1C")) +
      # theme(panel.background = element_rect(fill = "#170A1C")) +
      # theme(panel.border = element_blank())

#-------------------------------------------------------------------------------
# first period of time vs second period of time for overall water per ecoHydro -----
  
  # Create column that sorts years among two periods -----
    
    # wetTerm <- wetArea %>% 
    #   filter(month > 3, month < 9) %>%                   # filter data to April - August
    #   mutate(year = as.numeric(year),                    # makes the column 'year' numeric
    #          term = 't1',                                # creates a column called 'term' with filled with 't1'
    #          term = replace(term, year >2003, 't2'))     # labels everything in 'term' as 't2' if 'year' is greater than 2003
   
    wetTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 'T1',                                    # creates a column with filled with 'p1'
              term = replace(term, year >2003, 'T2')) %>%     # replaces 't1' with 't2' when year is >2002
      na.omit()
    
  # calc difference between p1 and p1 as % -----
    
    wetChange <- wetTerm %>%
      group_by(term, ecoHydro, year) %>%                 # Group the columns that you want to run a function over
      summarise(wetSum = sum(wetHa)) %>%                 # summarise the grouped columns by summing wetHa
      ungroup() %>%                                      # ungroup; the result is the total wetHa for each year, term, and region
      group_by(term, ecoHydro) %>%                       # group again by term and region
      summarise(wetMean = mean(wetSum)) %>%              # summarise the grouped columns by the average wetHa - this gives the avg wetHa across each term for each region
      spread(term, wetMean) %>%                          # spread is opposite of gather: there is now a column for t1 and one for t2
      mutate(change = ((T1-T2)/T1)*-1)                   # create column 'change' by doing column math with columns 't1' and 't2'
                                                         # The final result is a table with a column with the percent change from t1 to t2
    wetWilcoxTest <- wetTerm %>%
      group_by(term, ecoHydro, year) %>%                    # group by term, region, and year - function will run across these groups
      summarise(wetSum = sum(wetHa)) %>%                    # summarise the above groups by sum - the result is the total wetHa for each year for each region and term
      split(.$ecoHydro) %>%                                 # '.' shorthand for the dataframe - splits the data by region
      map(~wilcox.test(wetSum ~ term, data = .x, conf.int = T)) %>%       # iterates whole process over each region (calculates the mean and tests for difference)
      map_df(broom::tidy, .id = 'ecoHydro')                 # kicks out dataframe with p-valuves by region and season
    
  # combine wilcoxon results with p1, p2 diff results -----
    
    wetWilcoxChange <- wetChange %>% 
      full_join(wetWilcoxTest) %>%                      # merge the Wilcoxon test results to the wetChange df     
      select(ecoHydro, T1, T2, change, p.value)         # select the columns that you want to keep
    
    # fwrite(wetWilcoxChange, '03_output/table_region_wilcox.csv')
    
  # make box plots to show distibutions between t1 nad t2 surface water -----
  
    wetBox <- wetTerm %>%    
      group_by(term, ecoHydro, year) %>%                # data for the box plot -> wetHa summed for each year and each region with term labels
      summarise(wetSum = sum(wetHa))
    
    ggplot(wetBox, aes(x = term, y = wetSum, group = term, fill = term)) +  
      geom_boxplot() +
      scale_fill_manual(values=c("grey", "#FED179")) +  # fill color for box plots ("#23C9B6", "#fed179")
      facet_wrap(. ~ ecoHydro, ncol = 4, scales = 'free') +          # creates plot for each region
      scale_y_continuous(labels = thousands) +
      xlab("Time Period") +
      ylab("Inundated hectares x 1000") +
      labs(fill = "Time Period") +
      theme_classic() 
    
    ggsave("overallBoxplot.png", plot = last_plot(), width = 9.5, height = 5, units = "in", device='png', dpi=300)
    
  # Make bar plot illustrating changes in wetHa -----
    
    wetHaChange <- wetWilcoxChange %>% 
      mutate(wetHaDif = (T2-T1),
             type = ifelse(wetHaDif > 0, 'positive', 'negative'))
    
    ggplot(wetHaChange, aes(x = wetHaDif, y = ecoHydro, fill = type)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_fill_manual(values=c("dark gray", "light gray")) +
        scale_y_discrete(position = 'right') +
        # ggtitle("Percent Change of Water by Ownership") +
        xlab("Wetland Loss (hectares)") +
        ylab("Ecoregion") +
        theme(legend.title = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 
    
#-------------------------------------------------------------------------------
# Monthly water by region -----
   
  # Add term to dataframe; sum over region, year, month, and term
  # This gives the total wetHa for each month in each year for each region -----
  
    wetTermMonth <- hydroGra %>%  
        mutate(year = as.numeric(year),                      # makes the column 'year' numeric
               term = 'T1',                                  # creates a column called 'term' with filled with 't1'
               term = replace(term, year >2003, 'T2')) %>%   # labels everything in 'term' as 't2' if 'year' is greater than 2003
      group_by(term, ecoHydro, year, month) %>%              # Group the columns that you want to summarise over
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
    
    test <- wetTermMonth %>% 
      filter(ecoHydro == 'Middle Rockies', month == 10) %>% 
      filter(year == '2020')
  # Create separate table for each ecohydroregion -----
    # (an alternative to this looping is to subset the data when making the ggplot) -----
    
    for(i in unique(wetTermMonth$ecoHydro)) {                # for each unique value in the 'ecoHydro' column...
      nam <- paste("df", i, sep = ".")                       # create variable 'nam' and rename to 'df.i' where i corresponds to a unique value in 'ecoHydro'
      assign(nam, wetTermMonth[wetTermMonth$ecoHydro==i,])   # assign to 'nam' a subset of the original dataframe. The subset is for a unique value of 'ecoHydro'
    }
    
    # rename these dataframes so they are easier to call -----
    
      greatBasin <- `df.Great Basin-Colorado Plateau`
      midRockies <- `df.Middle Rockies`
      mojaveDes <- `df.Mojave-Sonoran Deserts`
      norPlains <- `df.Northern Plains`
      norRockies <- `df.Northern Rockies`
      pacificNW <- `df.Pacific NW`
      soPlains <- `df.Southern Plains`
      soRockies <- `df.Southern Rockies and Basins`
      
    # Write .csv of above data -----
      
      # fwrite(greatBasin, '01_data/12_greatBasin/greatBasin.csv')
  
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
      
    # Testing out some monthly hydroperiod stuff; doesn't really work -----
        
          test <- hydroGra %>%  
            mutate(year = as.numeric(year),                      
                   term = 'T1',                                 
                   term = replace(term, year >2003, 'T2')) %>%   
            group_by(term, ecoHydro, year, month, period) %>%       
            summarise(wetSum = sum(wetHa)) %>% 
            ungroup() %>% 
            filter(ecoHydro == "Great Basin-Colorado Plateau") %>% 
            group_by(period, month) %>% 
            summarize(wetMean = mean(wetSum)) %>% 
            ungroup() %>% 
            na.omit()
          
          ggplot(test, aes(x = month, y = period, height = wetMean)) +
            geom_density_ridges(stat = "identity", alpha = 0.5) +
            theme_ridges(center_axis_labels = TRUE)
   
        
     
      # Thoughts on hydroperiods -----
        # Compare seasonal hydrograph for each hydroperiod. Semi tends to be 
        # stable through the whole season. Both seasonal and temporary decline
        # and are reduced by the fall. Seasonal seems to extend further into
        # the fall. There's also more inter-annual variability in temp and 
        # seasonal wetlands than there are for semi-perm wetlands. 
      
        wetPeriod <- hydroTerm %>% 
          group_by(ecoHydro, period, month, year) %>% 
          summarise(wetHa = sum(wetHa)) %>% 
          ungroup() %>%
          mutate(year = as.numeric(year))
        
        gbHydro <- wetPeriod %>% 
          filter(ecoHydro == 'Great Basin-Colorado Plateau' & period == 'temp')
        
         ggplot(gbHydro, aes(x = year, y = as.factor(month), height = wetHa)) +
            geom_density_ridges(stat = "identity", alpha = 0.5, fill = 'blue') +
            theme_ridges(center_axis_labels = TRUE)
          
          yrs <- c(1990, 1995, 2000, 2005, 2010, 2015, 2020)
          gbHydro2 <- gbHydro %>%
            filter(year %in% yrs)
          
          ggplot(gbHydro2, aes(x = month, y = as.factor(year), height = wetHa)) + # month should be a numerical
             geom_density_ridges(stat = "identity", alpha = 0.5) +
            theme_ridges(center_axis_labels = TRUE)
          
        # another monthly hydroperiod plot -----
          
          wetTermMonth2 <- hydroGra %>%  
            mutate(year = as.numeric(year),                      # makes the column 'year' numeric
                   term = 'T1',                                  # creates a column called 'term' with filled with 't1'
                   term = replace(term, year >2003, 'T2')) %>%   # labels everything in 'term' as 't2' if 'year' is greater than 2003
            group_by(term, ecoHydro, year, month, period) %>%              # Group the columns that you want to summarise over
            summarise(wetSum = sum(wetHa)) %>% 
            na.omit()
          
          test <- wetTermMonth2 %>% 
             filter(ecoHydro == 'Northern Rockies')
          
          test2 <- test %>% 
            group_by(period, month) %>% 
            summarize(wetMean = mean(wetSum)) %>% 
            ungroup()
          
          ggplot(test2, aes(x = month, y = wetMean, color = period, fill = period)) +
            geom_line(size = 1, alpha = .7) +
          geom_area(position=position_identity(), alpha=.90)
            # geom_area(aes(alpha = 0.5, fill = period))
    
    # Line Graphs -----
      
      # If I can figure out how to label with percent decline -----
      # f_labels <- data.frame(month = c("Aug", "Apr", "Jul", "Jun", "Mar", "May", "Oct", "Sept"), 
      #                    label = c('1', '2', '3', '4', '5', '6', '7', '8'))
      
      # This will order the months correctly but mess up the above labels -----
      
        greatBasin$month_f <- factor(greatBasin$month, levels = c('Mar', 'Apr', 'May', 
                                                                  'Jun', 'Jul', 'Aug',
                                                                  'Sept','Oct'))
      
      # Plots -----
    
      MRplot <- ggplot(midRockies, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                           
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Middle Rockies") + # --> change title to mathch region
                  guides(color = FALSE) 
      
      GBplot <- ggplot(greatBasin, 
                  aes(x = year, y = wetSum)) +               
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",  
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                 
                  scale_y_continuous(labels = thousands) +                              
                  theme_light() +                                                     
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                        
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +             
                  theme( axis.title.y=element_text(size=8),                           
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Great Basin") + # --> change title to mathch region
                  guides(color = FALSE) 
      
       
      MDplot <- ggplot(mojaveDes, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                               # text size
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Mojave Desert") + # --> change title to mathch region
                  guides(color = FALSE) 
      
      NPplot <- ggplot(norPlains, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                               # text size
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Northern Plains") + # --> change title to mathch region
                  guides(color = FALSE) 
      
      NRplot <- ggplot(norRockies, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                               # text size
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Northern Rockies") + # --> change title to mathch region
                  guides(color = FALSE) 
      
       PNWplot <- ggplot(pacificNW, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                               # text size
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Pacific NW") + # --> change title to mathch region
                  guides(color = FALSE) 
                  
       SRplot <- ggplot(soRockies, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                               # text size
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Southern Rockies") + # --> change title to mathch region
                  guides(color = FALSE)
       
       SPplot <- ggplot(soPlains, # --> change the regional df here so I don't have to duplicate this code. 
                  aes(x = year, y = wetSum)) +               # make month a factor so I can change the colors (it was previously viewed as a continues variable)
              #scale_color_manual(values=c("#3AD394", "#932462", "#7525E1", "#17C1C1",   # new colors for each month
                                           # "#1654E3", "#E35D16", "#8FB00B", "#E556EC")) +                   
                  geom_line(size = 1, alpha = .5) +                                       # line size and transparency
                  geom_smooth(method = 'lm', size=.2, color = 'black') +                                   # trend line and error
                  scale_y_continuous(labels = thousands) +                                # divides units by 1000
                  theme_light() +                                                         # color theme
                  ylab('Inundated hectares x 1000') + 
                  facet_wrap(~month_f, ncol = 8, scales = 'fixed') +                         # makes a plot for each region; 3 columns; y-axis different for each graph
                  #geom_text(x = 2015, y = 37000, aes(label = label,  color = NULL), data=f_labels, inherit.aes = FALSE) +
                  theme(strip.background =element_rect(fill="#7A7A7A")) +                 # color of graph title boxes
                  theme( axis.title.y=element_text(size=8),                               # text size
                         axis.title.x=element_text(size=8),
                         axis.text=element_text(size=8)) + 
                  ggtitle("Southern Plains") + # --> change title to mathch region
                  guides(color = FALSE)
      
      
      grid.arrange(arrangeGrob(GBplot, ncol = 1),
                   arrangeGrob(MDplot, ncol = 1),
                   arrangeGrob(MRplot, ncol = 1),
                   arrangeGrob(NPplot, ncol = 1),
                   arrangeGrob(NRplot, ncol = 1),
                   arrangeGrob(PNWplot, ncol = 1),
                   arrangeGrob(SPplot, ncol = 1),
                   arrangeGrob(SRplot, ncol = 1),
                   nrow = 8)
      
      

# How has monthly water changed from t1 to t2? -----
  
  # find the mean amount of water for t1 and for t2 for each month -----   
      
    wetTerm02 <- hydroGra %>%
      mutate(year = as.numeric(year),                    # makes the column 'year' numeric
             term = 'T1',                                # creates a column called 'term' with filled with 't1'
             term = replace(term, year >2003, 'T2')) %>% 
      group_by(term, ecoHydro, year, month) %>%              # Group the columns that you want to summarise over
      summarise(wetSum = sum(wetHa))
   
    wetMonth <- wetTerm02 %>% 
        group_by(term, month, ecoHydro) %>%         # group by month, term, and region
        summarise(wetMean = mean(wetSum))           # find the mean wetHa for each month in each region for each term
    
  # Plot time series of average monthly water for t1 and t2 -----
      
    ggplot(wetMonth, 
         aes(x = month, y = wetMean, color = (term))) +               
      scale_color_manual(values=c("#369499", "#98554E"), name = 'Time Period') +                      # colors of the lines
      geom_line(size = 1, alpha = .7) +                                         # line size and transparency
      # geom_smooth(method = 'lm', size=.2) +                                   # trend line and error
      scale_y_continuous(labels = thousands) +                                  # divides units by 1000                                                           # color theme
      ylab('Inundated hectares x 1000') + 
      xlab('Month') +
      facet_wrap(~ecoHydro, ncol = 4, scales = 'free') +                        # makes a plot for each region; 3 columns; y-axis different for each graph
      # theme(strip.background =element_rect(fill="#7A7A7A")) +                   # color of graph title boxes
      theme( axis.title.y=element_text(size=8),                                 # text size
             axis.title.x=element_text(size=8),
             axis.text=element_text(size=8)) + 
      theme_classic()
      # ggtitle("Regional Monthly Surface Area Trend") 
    
  # Find the percent change for each month from t1 to t2 -----

     wetMonth02 <- wetTermMonth %>% 
        group_by(term, month, ecoHydro) %>%         # group by month, term, and region
        summarise(wetMean = mean(wetSum))           # find the mean wetHa for each month in each region for each term
    

    monthPercentChange <- wetMonth02 %>%              
      spread(term, wetMean) %>%                        
      mutate(change = ((T1-T2)/T1)*-1) #%>%
      # select(ecoHydro, month, change) %>%
      # spread(month, change)
    
    fwrite(monthPercentChange, '03_output/table_month_percentChange_wide.csv')

# Wilcox test for each month -----      
   
    marWilcox <-  wetTermMonth %>%    
          filter(month == 'Mar') %>%                                            # filter data to only include private
          group_by(term, ecoHydro, year) %>% 
          split(.$ecoHydro) %>%                                                 # '.' shorthand for the dataframe
          map(~wilcox.test(wetSum ~ term, data = .x)) %>%                       # iterates whole process over each region
          map_df(broom::tidy, .id = 'ecoHydro') %>%                             # kicks out dataframe with p-valuves by region and season
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
        full_join(monthPercentChange) %>% 
        select(ecoHydro, month, T1, T2, change, p.value)  
    
    fwrite(monthChange, '03_output/table_month_wilcox.csv')
    
  # select all of the significant results
    
    sigMonth <- monthChange %>% 
      filter(p.value <= 0.05 & change < 0)
#-------------------------------------------------------------------------------
# Private vs public -----
# Find the avg amt of water for public/private in each ecohydroregion -----
# Compare proportions of time period 1 to 2  -----
 
  # Create Term ----- 
  
    hydroTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 'T1',                                    # creates a column with filled with 'p1'
              term = replace(term, year >2003, 'T2')) %>%     # replaces 't1' with 't2' when year is >2002
      na.omit()
    
  # Calculate the proportinal differences between the mean private and public 
  # wetHa for each ecoHydroregion -----
  
    ownHydro <- hydroTerm %>% 
      group_by(ecoHydro, ownAg, year, term) %>%           # grouping
      summarise(wetHa = sum(wetHa)) %>%                   # sum wetHa = total amt of H20 by ownership, year, and ecohydroregion
      ungroup() %>% 
      group_by(term, ownAg, ecoHydro) %>%                 # group again
      summarise(wetMean = mean(wetHa)) %>%                # take the mean of summed wetHa
      spread(term, wetMean) %>%                           # spread the data so you can do some column math
      mutate(perDif = ((T1-T2)/T1)*-1,
            Change = T2-T1)
        
  # Calculate Standard deviation -----

    SDown <- hydroTerm %>% 
        group_by(ecoHydro, year, ownAg, term) %>% 
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(ecoHydro, ownAg, term) %>% 
        summarise(SD = sd(wetHa)) %>% 
        ungroup() %>% 
        spread(term, SD) %>% 
        rename(SD1 = 'T1',
               SD2 = 'T2')            
  
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
    
    # Merge Wilcoxon test with data table -----   
  
      ownChange <- privWilcox %>% 
        full_join(pubWilcox) %>%                            
        full_join(ownHydro) %>% 
        full_join(SDown) %>% 
        select(ecoHydro, ownAg, T1, SD1, T2, SD2, Change, perDif, p.value)
      
      fwrite(ownChange, '03_output/table_ownAg_region_wilcox.csv')
  
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
    
      ggplot(ownBox, aes(x = term, y = wetSum, fill = ownAg)) +  
        geom_boxplot() +
        scale_fill_manual(values=c("grey", "#FED179")) +  # fill color for box plots
        facet_wrap(. ~ ecoHydro, ncol = 4, scales = 'free') +          # creates plot for each region
        scale_y_continuous(labels = thousands) +
        xlab("Time Period") +
        ylab("Inundated hectares x 1000") +
        theme_classic() +
        labs(fill = 'Ownership')
      
       ggsave("ownAgBoxplot.png", plot = last_plot(), width = 10, height = 5, units = "in", device='png', dpi=300)
      
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
        scale_y_discrete(position = 'right') +
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
          geom_smooth(method = 'lm', size=.2) +                                   # trend line and error
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
        
    # Test -----
        
      # Group and summarize -----
        
        wetPeriod <- hydroTerm %>% 
          group_by(ecoHydro, period, year, term) %>% 
          summarise(wetHa = sum(wetHa)) %>% 
          ungroup() %>%
          mutate(year = as.numeric(year)) %>% 
          group_by(term, period, ecoHydro) %>% 
          summarize(wetMean = mean(wetHa)) %>% 
          spread(term, wetMean) %>% 
          mutate(perDif = ((T1-T2)/T1)*-1,
                 Change = T2-T1)
        
      # Calculate Standard deviation -----
  
        SDperiod <- hydroTerm %>% 
            group_by(ecoHydro, year, period, term) %>% 
            summarise(wetHa = sum(wetHa)) %>%
            ungroup() %>% 
            group_by(ecoHydro, period, term) %>% 
            summarise(SD = sd(wetHa)) %>% 
            ungroup() %>% 
            spread(term, SD) %>% 
            rename(SD1 = 'T1',
                   SD2 = 'T2')
        
      # Split data by period and run test-----
        
        # temp -----
        
          tempWilcox <-  hydroTerm %>%    
            filter(period == 'temp') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(period = 'temp')
        
        # seasonal -----
        
          seasWilcox <-  hydroTerm %>%    
            filter(period == 'seasonal') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(period = 'seasonal')
        
        # seasonal -----
        
          semiWilcox <-  hydroTerm %>%    
            filter(period == 'semi') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(period = 'semi')
        
      # Merge Wilcoxon test with data table -----   
        
        perChange <- tempWilcox %>% 
          full_join(seasWilcox) %>% 
          full_join(semiWilcox) %>% 
          full_join(wetPeriod) %>% 
          full_join(SDperiod) %>% 
          select(ecoHydro, period, T1, SD1, T2, SD2, Change, perDif, p.value)
          
      # Write table -----
        
        fwrite(perChange, '03_output/table_hydroperiod_region_wilcox.csv')
        
    # Plots -----
        
      # Boxplot -----
        
        perBox <- hydroTerm %>%    
          group_by(term, period, ecoHydro, year) %>%               
          summarise(wetSum = sum(wetHa))
    
        ggplot(perBox, aes(x = term, y = wetSum, fill = period)) +  
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
  
  # Write table -----

    fwrite(wetPeriod03, '03_output/table_hydroperiod_ownAg_portion.csv')
    
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
    
    # Write table -----

       fwrite(wetPeriod05, '03_output/table_hydroperiod_wetType_portion.csv')
    
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
    
    # Test -----
        
      # Group and summarize -----
        
        wetTypePeriod <- hydroTerm %>% 
          group_by(ecoHydro, wetType, year, term) %>% 
          summarise(wetHa = sum(wetHa)) %>% 
          ungroup() %>%
          mutate(year = as.numeric(year)) %>% 
          group_by(term, wetType, ecoHydro) %>% 
          summarize(wetMean = mean(wetHa)) %>% 
          spread(term, wetMean) %>% 
          mutate(perDif = ((T1-T2)/T1)*-1,
                 Change = T2-T1)
    
      # Calculate Standard deviation -----
  
        SDwetType <- hydroTerm %>% 
            group_by(ecoHydro, year, wetType, term) %>% 
            summarise(wetHa = sum(wetHa)) %>%
            ungroup() %>% 
            group_by(ecoHydro, wetType, term) %>% 
            summarise(SD = sd(wetHa)) %>% 
            ungroup() %>% 
            spread(term, SD) %>% 
            rename(SD1 = 'T1',
                   SD2 = 'T2')
    
        
      # Split data by period and run test-----
        
        # riverine -----
        
          rivWilcox <-  hydroTerm %>%    
            filter(wetType == 'riv') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(wetType = 'riv')
        
        # wetlands -----
        
          wetWilcox <-  hydroTerm %>%    
            filter(wetType == 'wet') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(wetType = 'wet')
        
        # flooded ag -----
        
          wetAgWilcox <-  hydroTerm %>%    
            filter(wetType == 'wetAg') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(wetType = 'wetAg')
    
        # managed wetlands -----
        
          wetManWilcox <-  hydroTerm %>%    
            filter(wetType == 'wetMan') %>%                      
            group_by(term, ecoHydro, year) %>% 
            summarise(wetSum = sum(wetHa)) %>% 
            split(.$ecoHydro) %>%                              
            map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
            map_df(broom::tidy, .id = 'ecoHydro') %>%
            mutate(wetType = 'wetMan')
        
      # Merge Wilcoxon test with data table -----   
        
        wetTypeChange <- rivWilcox %>% 
          full_join(wetWilcox) %>% 
          full_join(wetAgWilcox) %>%
          full_join(wetManWilcox) %>%
          full_join(wetTypePeriod) %>% 
          full_join(SDwetType) %>% 
          select(ecoHydro, wetType, T1, SD1, T2, SD2, Change, perDif, p.value)
          
      # Write table -----
        
        fwrite(wetTypeChange, '03_output/table_wetType_region_wilcox.csv')
        
    
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
    
  # Write table -----
        
        fwrite(wetPeriod07, '03_output/table_wetType_ownAg_portion.csv')
    
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

      
    
    
  