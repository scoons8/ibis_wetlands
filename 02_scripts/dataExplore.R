################################################################################
# Task: Data summary and visualization 
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

# Functions -----
  
  options(scipen=999)         
  thousands <- function(x){   
    x/1000
  }

# Working directory -----

  # setwd("/Users/sc148852/Box/R/ibisSites/tablesFinal") # change site folder here
  
# Read in data -----
  
  wetArea <- fread("01_data/08_final_hydro_data/wetAreaFin.csv")
  hydroGra <- fread("01_data/08_final_hydro_data/hydroGraFin.csv")
  
#-------------------------------------------------------------------------------
# wetHa for ecohydroregion over time -----
  
  # Group, Sum, Plot -----
    
    # Group -----
  
      wetArea01 <- (wetArea) %>%            # name variable
        filter(month > 3, month < 9) %>%    # filter to April through August
        group_by(ecoHydro, year) %>%        # select the columns you want to sum across
        summarise(wetHa=sum(wetHa)) %>%     # sum the wetHa -> result is the total wetHa for each year in each region
        ungroup() %>%                       # ungroup
        mutate(year = as.numeric(year))     # make the column 'year' numeric
  
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
      filter(month > 3, month < 9) %>%                   # filter data to April - August
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
      scale_fill_manual(values=c("#56B4E9", "#E69F00")) +  # fill color for box plots
      facet_wrap(. ~ ecoHydro, scales = 'free') +          # creates plot for each region
      scale_y_continuous(labels = thousands) +
      xlab("Term") +
      ylab("Inundated hectares x 1000") +
      theme_bw() 

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
  
    ownWilcox <- hydroTerm %>%
      group_by(term, ownAg, ecoHydro, year) %>%
      summarise(wetSum = sum(wetHa)) %>%
      split(.$ecoHydro) %>%                               # '.' shorthand for the dataframe
      map(~wilcox.test(wetSum ~ term, data = .x)) %>%     # iterates whole process over each region
      map_df(broom::tidy, .id = 'ecoHydro')               # kicks out dataframe with p-valuves by region and season
  
    ownChange <- ownHydro %>% 
      full_join(ownWilcox) %>%                            # combine wilcoxon results with t1, t2 diff results
      select(ecoHydro, t1, t2, change, p.value)
  
  # Summary Stats -----  
    
    # total wetHa -----
    
      wetTotal <- hydroTerm %>% 
        group_by(ecoHydro, ownAg, year, term) %>%           
        summarise(wetHa = sum(wetHa)) %>% 
        group_by(ownAg, ecoHydro, term) %>% 
        summarise(wetMean = mean(wetHa),
                  wetSD = sd(wetHa),
                  n_change = n())
  
  # Plot -----
    
    # Private water by term -----
    ggplot(
      subset(wetTotal, ownAg == "Private"),
      aes(x = wetMean, y = ecoHydro, fill = term)) +
      geom_bar(stat="identity", position=position_dodge(), color = "#505050") +
      geom_errorbar(aes(xmin = wetMean - wetSD, xmax = wetMean + wetSD), 
                    width = .3, position = position_dodge(0.9), 
                    color =  "#505050") +
      scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
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
      scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
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
    
  # Plot -----
    # Percent change by ownership -----
    
      ownHydroChange <- ownChange %>% 
        gather(Term, wetHa, 't1', 't2')
      
      ggplot(ownHydroChange, aes(x = change, y = ecoHydro, fill = ownAg)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
        ggtitle("Percent Change of Water by Ownership") +
        xlab("Percent Change") +
        ylab("Ecohydroregion") +
        scale_x_reverse() +
        theme(legend.title = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",
                                     colour = "black")) 

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
        geom_bar(stat="identity", position=position_dodge()) +
        facet_wrap(. ~ ecoHydro, scales = 'free') +
        scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
        ggtitle("Hydroperiod and Ownership, T1") +
        xlab("wetHa") +
        ylab("Ecohydroregion") +
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
          geom_bar(stat="identity", position=position_dodge()) +
          facet_wrap(. ~ ecoHydro, scales = 'free') +
          scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
          ggtitle("Hydroperiod and Ownership, T2") +
          xlab("wetHa") +
          ylab("Ecohydroregion") +
          theme(
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(size = 0.5, linetype = "solid",
                                       colour = "black")) 
        
#-------------------------------------------------------------------------------
# sites and water
  
  # Read in site data ----
    
  sites <- fread("siteNamesID.csv")
    
  # summarize data before join ----
      
  siteHydro <- hydroTerm %>% 
      group_by(idPoly, year, term) %>%           
      summarise(wetHa = sum(wetHa)) %>%     
      ungroup() %>% 
      left_join(sites, by = 'idPoly')
    
  siteHydro01 <- siteHydro %>% 
      group_by(siteName, Latitude, Longitude, year, term) %>%
      summarise(wetHa = sum(wetHa)) %>%
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, term) %>% 
      summarise(wetMean = mean(wetHa)) %>% 
      spread(term, wetMean) %>%                           # spread the data so you can do some column math
      mutate(change = ((t1-t2)/t1)*-1)                    # calculate percent change and add as a new column
  
  # Map -----
    
    # Create a factor by which to color code the sites by -----
    # In this case, decreasing water will be red and increasing will be blue
    
      siteCat <- siteHydro01 %>% 
        mutate(cat = ifelse(change < 0, 'decrease', 'increase'),
               cat = as.factor(cat),
               sig = ifelse(p.value < 0.05, 'yes', 'no'),
               sig = as.factor(sig))
    
    # Define the color palette -----
      
      pal <- colorFactor(palette = c("#0E98A9", "#D1501C"), 
                         levels = c("increase","decrease"))  
    
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
  

    
    
  