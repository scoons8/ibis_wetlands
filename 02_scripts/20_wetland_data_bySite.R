################################################################################
# Task: Data analysis and visualization of wetHa by site
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
  
# Read data -----

  # wetArea <- fread('01_data/08_final_hydro_data/wetAreaFin06.csv')
  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraSiteFin01.csv')

#===============================================================================

# Group and summarize hydrograph data -----
  
  hydroTerm <- hydroGra %>% 
      filter(month > 3, month < 9) %>% 
      mutate(year = as.numeric(year),
              term = 't1',                                # creates a column with filled with 'p1'
              term = replace(term, year >2003, 't2')) %>% # replaces 't1' with 't2' when year is >2002
      na.omit()
    
#===============================================================================
# sites and water: What is the overall trend at individual ibis blobs? -----
  
  # Read in site data -----
    
    sites <- fread("01_data/09_site_names/siteNamesID.csv")
    
  # summarize data before join -----
      
    siteHydro <- hydroTerm %>% 
        group_by(idPoly, siteName, year, term, ecoHydro) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = c('idPoly', 'siteName'))
  
  # Join the data -----
    
    siteHydro01 <- siteHydro %>% 
        group_by(siteName, Latitude, Longitude, year, term, ecoHydro) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(siteName, Latitude, Longitude, term, ecoHydro) %>% 
        summarise(wetMean = mean(wetHa)) %>% 
        spread(term, wetMean) %>%                           
        mutate(change = ((t1-t2)/t1)*-1) %>%      
        na.omit()
  
  # Wilcoxon test ----- 
      
    siteWilcox <-  siteHydro %>%    
      group_by(term, siteName, Latitude, Longitude, year) %>% 
      summarise(wetSum = sum(wetHa)) %>% 
      split(.$Latitude) %>%                              
      map(~wilcox.test(wetSum ~ term, data = .x)) %>%     
      map_df(broom::tidy, .id = 'Latitude') %>% 
      mutate(Latitude = as.numeric(Latitude))
    
    # Merge Wilcoxon test with data table -----  
  
      siteChange <- siteHydro01 %>% 
        full_join(siteWilcox, by = 'Latitude') %>%
        select(siteName, ecoHydro, t1, t2, change, p.value) %>% 
        na.omit()
      
      fwrite(siteChange, '03_output/table_site_wilcox.csv')
      
      # Site data -----
        numSite <- subset(siteChange, ecoHydro == 'Northern Plains')
        nrow(numSite)
      
        # Great Basin = 87
        # Middle Rockies = 11
        # Mojave-Sonoran Deserts = 1
        # Northern Plains = 15
        # Northern Rockies = 3
        # Pacific Northwest = 7
        # Southern Plains = 6
        # Southern Rockies and Basins = 23
        # Total = 153

#===============================================================================
# Make a table for regions and sites -----
  
  ecoregion <- c('Great Basin-Colorado Plateau', 'Middle Rockies', 
                 'Mojave-Sonoran Deserts', 'Northern Plains', 'Northern Rockies',
                 'Pacific Northwest', 'Southern Plains', 'Southern Rockies and Basins', 'Total')
        
  numbers <- c(87, 11, 1, 15, 3, 7, 6, 23, 153)
  
  ecoTable <- data.frame(ecoregion, numbers)
  colnames(ecoTable) <- c('Ecoregion', 'NumColonies')
  
  fwrite(ecoTable, '03_output/ecoTable.csv')
  
  # Plot -----
  
    ecoTable %>%
      filter(NumColonies < 150) %>% 
      arrange(NumColonies) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      mutate(Ecoregion=factor(Ecoregion, levels=Ecoregion)) %>%   # This trick update the factor levels
      ggplot( aes(x=Ecoregion, y= NumColonies)) +
      # geom_segment( aes(xend=Ecoregion, yend=0)) +
      geom_point( size=10, color="orange") +
      coord_flip() +
      geom_text(aes(label=NumColonies), vjust= 0.4, color = 'white', size=3.5) +
      xlab('Region') +
      ylab("Number of Ibis Breeding Colonies") +
      # ggtitle("Breeding Colonies by Region") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))
  
    ggsave("sitePlot.png", plot = last_plot(), width = 6, height = 5, units = "in", device='png', dpi=300)
      
#===============================================================================       
    # How many sites have significant values ( p < 0.05)? -----
      
      sigSites <- subset(siteChange, p.value < 0.05)
      nrow(sigSites)
      nrow(siteChange)
      
      npSig <- sigSites %>% 
        filter(ecoHydro == "Northern Plains")
        # 95/153 sites have significant changes. How many are negative? -----
      
          sigNegSites <- subset(sigSites, change < 0)
          nrow(sigNegSites)
          
          sigPosSites <- subset(sigSites, change > 0)
          
        # 91 / 153 sites have significant declines in wetland flooding -----
        # How many sites are almost significant?
          
          almost <- subset(siteChange, p.value >= 0.05 & p.value <0.08)
          nrow(almost)
          
          almostNeg <- subset(almost, change < 0)
          nrow(almostNeg)
          
    # Regional patterns -----
          
      # How many sites total in GB and how many significant?
          
        sub02 <- subset(siteChange, ecoHydro == "Great Basin-Colorado Plateau")
        nrow(sub02) #87 sites out of total = GB
        sub <- subset(sigSites, ecoHydro == "Great Basin-Colorado Plateau")
        nrow(sub) # 55 sites in GB have sig results
        sub03 <- subset(sigNegSites, ecoHydro == "Great Basin-Colorado Plateau")
        nrow(sub03) # 55 sites = GB
        
    
    
   # Map -----
    
    # Create a factor by which to color code the sites by -----
    # In this case, decreasing water will be red and increasing will be blue
      
      siteCat <- siteChange %>% 
        mutate(cat = case_when(change < 0 & p.value < 0.05 ~ "Significant Decrease",
                               change < 0 & p.value > 0.05 ~ "Nonsignificant Decrease",
                               change > 0 & p.value < 0.05 ~ "Significant Increase",
                               change > 0 & p.value > 0.05 ~ "Nonsignicant Increase"))
    
    # Define the color palette -----
      
      pal <- colorFactor(palette = c("#369499", "#98554E", "#00eaf2","#cc0a00"), 
                         levels = c("Nonsignicant Increase","Nonsignificant Decrease", 
                                    "Significant Increase", "Significant Decrease"))  
    
    # Read in ecohydroRegion shapefile -----
    
      ecoHydroBounds <- st_read('/Users/sc148852/Box/Qgis/Thesis/06_EcoHydro Regions/ecoHydroWest_UTM12.shp')
   
      # Transform CRS -----
    
        ecoHydrodWGS <- st_transform(ecoHydroBounds, CRS("+init=epsg:4326"))
    
    # Create the leaflet map with the site locations -----
    
      leaflet(data = siteCat) %>%
      addProviderTiles(providers$Thunderforest.MobileAtlas) %>%     
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
# Sites and ownAg: What are the trends in priv/pub wetlands by site?  -----

  # summarize data and then join -----
      
    siteOwn <- hydroTerm %>% 
        group_by(idPoly, siteName, ecoHydro, year, ownAg, term) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = c('idPoly', 'siteName'))
        
  # calculate mean amt of water per priv/pub per term per site plus % change -----
    
    siteOwn01 <- siteOwn %>% 
        group_by(siteName, ecoHydro, Latitude, Longitude, year, ownAg, term) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(siteName, Latitude, Longitude, ecoHydro, ownAg, term) %>% 
        summarise(wetMean = mean(wetHa)) %>% 
        spread(term, wetMean) %>%                          
        mutate(change = ((t1-t2)/t1)*-1) %>% 
        na.omit()
        
  # Split data by ownAg and run test-----
      
      # Private -----
        
        test <- subset(siteOwn01, ownAg == "Private")
        table(test$siteName)
        nrow(test)
      
        privSiteWilcox <- siteOwn %>%    
          filter(ownAg == 'Private') %>%                      # filter data to only include private
          group_by(term, siteName, Latitude, Longitude, year) %>% 
          summarise(wetSum = sum(wetHa)) %>% 
          split(.$Latitude) %>%                               # '.' shorthand for the dataframe
          map(~wilcox.test(wetSum ~ term, data = .x)) %>%     # iterates whole process over each region
          map_df(broom::tidy, .id = 'Latitude') %>%           # kicks out dataframe with p-valuves by region and season
          mutate(ownAg = 'Private',
                 Latitude = as.double(Latitude))                           # make new column with with ownership value ('Private')
      
        pubSiteWilcox <- siteOwn %>% 
          filter(ownAg == 'Public') %>%                       # filter data to only include public
          group_by(term, Latitude, Longitude, siteName, year) %>% 
          summarise(wetSum = sum(wetHa)) %>% 
          split(.$Latitude) %>%                               # '.' shorthand for the dataframe
          map(~wilcox.test(wetSum ~ term, data = .x)) %>%     # iterates whole process over each region
          map_df(broom::tidy, .id = 'Latitude') %>%           # kicks out dataframe with p-valuves by region and season
           mutate(ownAg = 'Public',
                 Latitude = as.double(Latitude))                       # make new column with with ownership value ('Public')
        
    # Merge Wilcoxon test with data table -----   
        
      OwnSiteChange <- privSiteWilcox %>%
        full_join(pubSiteWilcox) %>%
        full_join(siteOwn01, by = c('Latitude', 'ownAg')) %>%  # join by both latitude and ownAg
        select(siteName, Latitude, Longitude, ecoHydro, ownAg, t1, t2, change, p.value)
        
        fwrite(OwnSiteChange, '03_output/table_site_ownAg_wilcox.csv')
        
  # Summaries -----
        
    # How many public and private sites are significant?----
        
      privSiteSig <- subset(OwnSiteChange, p.value < 0.05 & ownAg == "Private")
        nrow(privSiteSig) # 80 private sites with significant results
      privSiteNeg <- privSiteSig %>% 
        filter(change <0)
        nrow(privSiteNeg) # 78 private sites with significant declines (78 / 153 sites with private wetlands)
      privSitePos <- privSiteSig %>% 
        filter(change > 0)
      nrow(privSitePos)
      
      pubSiteSig <- subset(OwnSiteChange, p.value < 0.05 & ownAg == "Public")
        nrow(pubSiteSig) # 101 public sites with significant results 
      pubSiteNeg <- pubSiteSig %>% 
        filter(change <0)
      nrow(pubSiteNeg) # 97 public sites with significant declines (97 / 151 sites with public wetlands)
      pubSitePos <- pubSiteSig %>% 
        filter(change > 0)
      nrow(pubSitePos)
      
      overlapOwn <- privSiteNeg %>% 
        inner_join(pubSiteNeg, by = c('siteName', 'Latitude'))
      nrow(overlapOwn)
        
    # by region -----
      regSig <- 
        # subset(OwnSiteChange, p.value < 0.05 & ownAg == "Private") %>%
        subset(OwnSiteChange, p.value < 0.05 & ownAg == "Public") %>%
        filter(ecoHydro == 'Southern Rockies and Basins')
      regNeg <- regSig %>% 
        filter(change < 0)
      regPos <- regSig %>% 
        filter(change > 0)
      nrow(regSig)
      nrow(regNeg)
      nrow(regPos)
      
      test <- subset(OwnSiteChange, p.value < 0.05 & ecoHydro == 'Southern Rockies and Basins') %>% 
        filter(change < 0)
      testPub <- test %>% 
        filter(ownAg == 'Public')
      testPri <- test %>% 
        filter(ownAg == 'Private')
      test02 <- testPub %>% 
        inner_join(testPri, by = c('siteName', 'Latitude'))
      nrow(test02)
      
      # Private:
        # GB = 46 decrease, 1 increase;   41 sites decreasing across both private and public (5 just private, 17 just public)
        # MR = 4 decrease, 0 increase;    4 sites decreasing across both private and public (5 just public)
        # MD = 0 decrease, 0 increase;    0  
        # NP = 6 decrease, 0 increase;    5 sites decreasing across both private and public (1 just private)
        # NR = 3 decrease, 0 increase;    3 sites decreasing across both private and public
        # PNW = 2 decrease, 1 increase;   2 sites decreasing across both private and public (1 just public)
        # SP = 6 decrease, 0 increase;    5 sites decreasing across both private and public (1 just private)
        # SR = 11 decrease, 0 increase;   9 sites decreasing across both private and public (2 just private, 5 just public)
      
      # Public:
        # GB = 58 decrease, 0 increase
        # MR = 9 decrease, 0 increase
        # MD = 0 decrease, 0 increase
        # NP = 5 decrease, 4 increase
        # NR = 3 decrease, 0 increase
        # PNW = 3 decrease, 0 increase
        # SP = 5 decrease, 0 increase
        # SR = 14 decrease, 0 increase
      
#-------------------------------------------------------------------------------        
# sites and hydroperiod: 
# What are the hydroperiod trends at individual ibis blobs? -----

  # summarize data and then join -----
      
    sitePer <- hydroTerm %>% 
        group_by(idPoly, siteName, year, period, term) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = c('idPoly', 'siteName'))
  
  # calculate mean amt of water per hydroperiod per term per site plus % change -----
    
    sitePer01 <- sitePer %>% 
        group_by(siteName, Latitude, Longitude, year, period, term) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(siteName, Latitude, Longitude, period, term) %>% 
        summarise(wetMean = mean(wetHa)) %>% 
        spread(term, wetMean) %>%                          
        mutate(change = ((t1-t2)/t1)*-1)%>% 
        na.omit()
 
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

    # Merge Wilcoxon test with data table -----
    
      perChange <- tempWilcox %>%
        full_join(seasWilcox) %>%
        full_join(semiWilcox) %>%
        full_join(sitePer01, by = c('Latitude', 'period')) %>%  # join by both latitude and period
        select(siteName, Latitude, Longitude, period, t1, t2, change, p.value)
      
    # Summaries -----
      
      # How many sites for each hydroperiod? -----
      
        tempSites <- subset(perChange, period == 'temp')
          nrow(tempSites)
          
        seasSites <- subset(perChange, period == 'seasonal')
          nrow(seasSites)
          
        semiSites <- subset(perChange, period == 'semi')
          nrow(semiSites)
      
      # How many sites show significance? -----
      
        tempSiteSig <- subset(perChange, p.value < 0.05 & period == "temp")
          nrow(tempSiteSig) # 58 sites with significant results in temp wetlands
        tempSiteNeg <- tempSiteSig %>% 
          filter(change <0)
          nrow(tempSiteNeg) # 31 sites with significant declines in temp wetlands
        tempSitePos <- tempSiteSig %>% 
          filter(change > 0)
        nrow(tempSitePos) # 27 sites with significant increases in temp wetlands
        
        seasSiteSig <- subset(perChange, p.value < 0.05 & period == "seasonal")
          nrow(seasSiteSig) # 89 sites with significant results in seasonal wetlands
        seasSiteNeg <- seasSiteSig %>% 
          filter(change <0)
        nrow(seasSiteNeg) # 70 sites with significant declines in seasonal wetlands
        seasSitePos <- seasSiteSig %>% 
          filter(change > 0)
        nrow(seasSitePos) # 19 sites with significant increases in seasonal wetlands
        
        semiSiteSig <- subset(perChange, p.value < 0.05 & period == "semi")
          nrow(semiSiteSig) # 109 sites with significant results in semi-permanent wetlands
        semiSiteNeg <- semiSiteSig %>% 
          filter(change <0)
        nrow(semiSiteNeg) # 96 sites with significant declines in semi-permanent wetlands
        semiSitePos <- semiSiteSig %>% 
          filter(change > 0)
        nrow(semiSitePos) # 13 sites with significant increases in semi-permanent wetlands
        
        overlapPer <- tempSiteNeg %>% 
          inner_join(seasSiteNeg, by = c('siteName', 'Latitude')) %>%
          inner_join(semiSiteNeg, by = c('siteName', 'Latitude'))
          # 19 sites are experiencing declines in all hydroperiods
          # 25 sites are experiencing declines in both temp and seasonal wetlands
          # 52 sites are experiencing declines in both seasonal and semi-permanent wetlands
          # 19 sites are experiencing declines in both temp and semi-permanent wetlands
        
        # Overlap between decreasing semi and increasing temp?
        
        overlapPerPos <- tempSitePos %>% 
          # inner_join(seasSiteNeg, by = c('siteName', 'Latitude')) %>%
          inner_join(semiSiteNeg, by = c('siteName', 'Latitude'))
        # 19 sites are experiencing declines in semi-perm with increases in temp
        # 2 sites are experiencing declines in seasonal with increases in temp
        # 15 sites are experiencing declines in semi-perm with increases in seasonal
        # 13 sites are experiencing declines in semi-perm with increases in both seasonal and temp
        # 13 sites are experiencing increases in both seasonal and temp
      
      
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

      
      
#-------------------------------------------------------------------------------        
# sites and wetland type: 
# What are the wetland type trends at individual ibis blobs? -----

  # summarize data and then join -----
      
    siteWetType <- hydroTerm %>% 
        group_by(idPoly, siteName, year, wetType, term) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = c('idPoly', 'siteName'))
  
  # calculate mean amt of water per hydroperiod per term per site plus % change -----
    
    siteType01 <- siteWetType %>% 
        group_by(siteName, Latitude, Longitude, year, wetType, term) %>%
        summarise(wetHa = sum(wetHa)) %>%
        ungroup() %>% 
        group_by(siteName, Latitude, Longitude, wetType, term) %>% 
        summarise(wetMean = mean(wetHa)) %>% 
        spread(term, wetMean) %>%                          
        mutate(change = ((t1-t2)/t1)*-1)%>% 
        na.omit()
 
  # Wilcoxon test for each hydro period  ----- 
      
    # Riverine wetlands -----  
      
      rivWilcox <- siteWetType %>%
        filter(wetType == 'riv') %>%                       
        group_by(term, Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'riv',
               Latitude = as.numeric(Latitude))
      
    # wet wetlands -----  
      
      wetWilcox <- siteWetType %>%
        filter(wetType == 'wet') %>%                       
        group_by(term, Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'wet',
               Latitude = as.numeric(Latitude)) 
      
    # wetAg wetlands -----  
      
      agWilcox <- siteWetType %>%
        filter(wetType == 'wetAg') %>%                       
        group_by(term, Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'wetAg',
               Latitude = as.numeric(Latitude)) 
      
    # wetAg wetlands -----  
      
      manWilcox <- siteWetType %>%
        filter(wetType == 'wetMan') %>%                       
        group_by(term, Latitude, Longitude, siteName, year) %>%
        summarise(wetSum = sum(wetHa)) %>%
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetSum ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'wetMan',
               Latitude = as.numeric(Latitude)) 
      
  # Merge Wilcoxon test with data table -----
    
      typeChange <- rivWilcox %>%
        full_join(wetWilcox) %>%
        full_join(agWilcox) %>%
        full_join(manWilcox) %>%
        full_join(siteType01, by = c('Latitude', 'wetType')) %>%  # join by both latitude and period
        select(siteName, Latitude, Longitude, wetType, t1, t2, change, p.value) %>% 
        na.omit()
      
      # Summaries -----
      
      # How many sites for each wetland type? -----
      
        rivSites <- subset(typeChange, wetType == 'riv')
          nrow(rivSites) # 96 sites
          
        wetSites <- subset(typeChange, wetType == 'wet')
          nrow(wetSites) # 151 sites
          
        agSites <- subset(typeChange, wetType == 'wetAg')
          nrow(agSites) # 134 sites
          
        manSites <- subset(typeChange, wetType == 'wetMan')
          nrow(manSites) # 103 sites
      
      # How many sites show significance? -----
      
        rivSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'riv')
          nrow(rivSitesSig) # 50 sites with significant results in riv wetlands
        rivSitesNeg <- rivSitesSig %>% 
          filter(change <0)
          nrow(rivSitesNeg) # 41 sites with significant declines in riv wetlands
        rivSitesPos <- rivSitesSig %>% 
          filter(change > 0)
        nrow(rivSitesPos) # 9 sites with significant increases in riv wetlands
        
        wetSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'wet')
          nrow(wetSitesSig) # 92 sites with significant results in wet wetlands
        wetSitesNeg <- wetSitesSig %>% 
          filter(change <0)
        nrow(wetSitesNeg) # 86 sites with significant declines in wet wetlands
        wetSitesPos <- wetSitesSig %>% 
          filter(change > 0)
        nrow(wetSitesPos) # 6 sites with significant increases in wet wetlands
        
        agSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'wetAg')
          nrow(agSitesSig) # 75 sites with significant results in wetAg wetlands
        agSitesNeg <- agSitesSig %>% 
          filter(change <0)
        nrow(agSitesNeg) # 67 sites with significant declines in wetAg wetlands
        agSitesPos <- agSitesSig %>% 
          filter(change > 0)
        nrow(agSitesPos) # 8 sites with significant increases in wetAg wetlands
        
        manSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'wetMan')
          nrow(manSitesSig) # 77 sites with significant results in managed wetlands
        manSitesNeg <- manSitesSig %>% 
          filter(change <0)
        nrow(manSitesNeg) # 73 sites with significant declines in managed wetlands
        manSitesPos <- manSitesSig %>% 
          filter(change > 0)
        nrow(manSitesPos) # 4 sites with significant increases in managed wetlands
        
        overlapType <- rivSitesNeg %>% 
          inner_join(wetSitesNeg, by = c('siteName', 'Latitude')) %>%
          inner_join(agSitesNeg, by = c('siteName', 'Latitude')) %>% 
          inner_join(manSitesNeg, by = c('siteName', 'Latitude'))
        
#===============================================================================        
# Calculate the mean amount of water in each site by region ------  
        
  test <- siteHydro %>% 
    group_by(siteName, Latitude, Longitude, year, term, ecoHydro) %>%
    summarise(wetHa = sum(wetHa)) %>%
    ungroup() %>% 
    group_by(siteName, Latitude, Longitude, ecoHydro) %>% 
    summarise(wetMean = mean(wetHa)) %>% 
    na.omit()

  test2 <- test %>% 
    group_by(ecoHydro) %>% 
    summarise(meanWet = mean(wetMean)) %>% 
    ungroup()

  test3 <- test %>%
    filter(ecoHydro == 'Southern Rockies and Basins')
    # filter(ecoHydro == 'Great Basin-Colorado Plateau') %>% 
   # filter(siteName != 'Bear River MBR') %>% 
     # filter(wetMean < 100000)
     # 
  test4 <- test3 %>% 
    group_by(ecoHydro) %>% 
     summarise(meanWet = mean(wetMean))

  ggplot(test3, aes(x = Latitude, y = wetMean)) +
    geom_point()
  
  