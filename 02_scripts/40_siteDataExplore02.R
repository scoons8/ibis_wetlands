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

  hydroGra <- fread('01_data/19_hydroCleaned/hydroGraSiteFin03.csv')

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
    
  # summarize data before join and then join site data -----
      
    siteHydro <- hydroTerm %>% 
        group_by(idPoly, siteName, month, year, term, ecoHydro) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = c('idPoly', 'siteName'))
  
  # Summarize data -----
    
    siteHydro01 <- siteHydro %>% 
      group_by(siteName, Latitude, Longitude, month, year, term, ecoHydro) %>%
      summarise(wetSum = sum(wetHa)) %>% 
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, year, term, ecoHydro) %>% 
      summarise(wetMean = mean(wetSum)) %>% 
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, term, ecoHydro) %>% 
      summarise(wetMean = mean(wetMean)) %>% 
      spread(term, wetMean) %>%                           
      mutate(change = ((t1-t2)/t1)*-100) %>%      
      na.omit()
  
  # Wilcoxon test ----- 
      
    siteWilcox <-  siteHydro %>%    
      group_by(siteName, Latitude, Longitude, month, year, term, ecoHydro) %>%
      summarise(wetSum = sum(wetHa)) %>% 
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, year, term, ecoHydro) %>% 
      summarise(wetMean = mean(wetSum)) %>% 
      ungroup() %>% 
      split(.$Latitude) %>%                              
      map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
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
      nrow(siteChange) # <-- 96 of 153 sites show significant change
      
      npSig <- sigSites %>% 
        filter(ecoHydro == "Northern Plains") # <-- 10 sites w/ sig results (6 decrease and 4 increase)
      
    # How many sites have significant results that are negative? -----
      
      sigNegSites <- subset(sigSites, change < 0)
      nrow(sigNegSites) # <-- 92 sites have negative results
      
      sigPosSites <- subset(sigSites, change > 0)
      nrow(sigPosSites) # <-- 4 sites have positive results (they are all in the Northern Plains)
          
    # Regional patterns -----
          
      # How many sites total in GB and how many significant?
          
        sub02 <- subset(siteChange, ecoHydro == "Southern Rockies and Basins") # <-- Change the region here
        nrow(sub02) 
        sub <- subset(sigSites, ecoHydro == "Southern Rockies and Basins")     # <-- Change the region here
        nrow(sub) 
        sub03 <- subset(sigNegSites, ecoHydro == "Southern Rockies and Basins")# <-- Change the region here
        nrow(sub03) 
        
        
        # Regional results:
          # GB: 55/87 sites decreasing
          # MR: 6/11 sites decreasing
          # MD: 0/1 sites decreasing
          # NP: 6/15 sites decreasing; 4/15 sites increasing
          # NR: 3/3 sites decreasing
          # PNW: 3/7 sites decreasing
          # SP: 6/6 sites decreasing
          # SR: 13/23 sites decreasing
    
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
      group_by(idPoly, siteName, ecoHydro, month, year, ownAg, term) %>%           
      summarise(wetHa = sum(wetHa)) %>%     
      ungroup() %>% 
      left_join(sites, by = c('idPoly', 'siteName'))
        
  # calculate mean amt of water per priv/pub per term per site plus % change -----
    
    siteOwn01 <- siteOwn %>% 
      group_by(siteName, ecoHydro, Latitude, Longitude, month, year, ownAg, term) %>%
      summarise(wetHa = sum(wetHa)) %>%
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, ecoHydro, ownAg, year, term) %>% 
      summarise(wetMean = mean(wetHa))
        
    siteOwn02 <- siteOwn01 %>% 
      group_by(siteName, Latitude, Longitude, ownAg, term, ecoHydro) %>% 
      summarise(wetMean = mean(wetMean)) %>% 
      spread(term, wetMean) %>%                          
      mutate(change = ((t1-t2)/t1)*-100) %>% 
      na.omit()

  # Split data by ownAg and run test-----
      
      # Private -----
      
        privSiteWilcox <- siteOwn01 %>%    
          filter(ownAg == 'Private') %>%                                        # filter data to only include private
          split(.$Latitude) %>%                                                 # '.' shorthand for the dataframe
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%                      # iterates whole process over each region
          map_df(broom::tidy, .id = 'Latitude') %>%                             # kicks out dataframe with p-valuves by region and season
          mutate(ownAg = 'Private',
                 Latitude = as.double(Latitude))                                # make new column with with ownership value ('Private')
      
        pubSiteWilcox <- siteOwn01 %>% 
          filter(ownAg == 'Public') %>%                                         # filter data to only include public
          split(.$Latitude) %>%                                                 # '.' shorthand for the dataframe
          map(~wilcox.test(wetMean ~ term, data = .x)) %>%                      # iterates whole process over each region
          map_df(broom::tidy, .id = 'Latitude') %>%                             # kicks out dataframe with p-valuves by region and season
           mutate(ownAg = 'Public',
                 Latitude = as.double(Latitude))                                # make new column with with ownership value ('Public')
        
    # Merge Wilcoxon test with data table -----   
        
      OwnSiteChange <- privSiteWilcox %>%
        full_join(pubSiteWilcox) %>%
        full_join(siteOwn02, by = c('Latitude', 'ownAg')) %>%                   # join by both latitude and ownAg
        select(siteName, Latitude, Longitude, ecoHydro, ownAg, t1, t2, change, p.value)
        
        fwrite(OwnSiteChange, '03_output/table_site_ownAg_wilcox.csv')
        
  # Summaries -----
        
    # How many public and private sites are significant?----
        
      privSiteSig <- subset(OwnSiteChange, p.value < 0.05 & ownAg == "Private")
        nrow(privSiteSig) # <-- 81 private sites with significant results
      privSiteNeg <- privSiteSig %>% 
        filter(change <0)
        nrow(privSiteNeg) # <-- 79 private sites with significant declines (78 / 153 sites with private wetlands)
      privSitePos <- privSiteSig %>% 
        filter(change > 0)
      nrow(privSitePos) # <-- 2 sites with significant increases
      
      pubSiteSig <- subset(OwnSiteChange, p.value < 0.05 & ownAg == "Public")
        nrow(pubSiteSig) # 101 public sites with significant results 
      pubSiteNeg <- pubSiteSig %>% 
        filter(change <0)
      nrow(pubSiteNeg) # 97 public sites with significant declines (97 / 151 sites with public wetlands)
      pubSitePos <- pubSiteSig %>% 
        filter(change > 0)
      nrow(pubSitePos) # <-- 4 sites with significant increases
      
      overlapOwn <- privSiteNeg %>% 
        inner_join(pubSiteNeg, by = c('siteName', 'Latitude'))
      nrow(overlapOwn)
      
#-------------------------------------------------------------------------------        
# sites and hydroperiod: 
# What are the hydroperiod trends at individual ibis blobs? -----

  # summarize data and then join -----
      
    sitePer <- hydroTerm %>% 
        group_by(idPoly, siteName, month, year, period, term) %>%           
        summarise(wetHa = sum(wetHa)) %>%     
        ungroup() %>% 
        left_join(sites, by = c('idPoly', 'siteName'))
  
  # calculate mean amt of water per hydroperiod per term per site plus % change -----
    
    sitePer01 <- sitePer %>% 
      group_by(siteName, Latitude, Longitude, month, year, period, term) %>%
      summarise(wetHa = sum(wetHa)) %>%
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, period, year, term) %>% 
      summarise(wetMean = mean(wetHa))
        
    sitePer02 <- sitePer01 %>% 
      group_by(siteName, Latitude, Longitude, period, term) %>% 
      summarise(wetMean = mean(wetMean)) %>% 
      spread(term, wetMean) %>%                          
      mutate(change = ((t1-t2)/t1)*-100)%>% 
      na.omit()

  # Wilcoxon test for each hydro period  ----- 
    
    # Temporary wetlands -----  
      
      tempWilcox <- sitePer01 %>%
        filter(period == 'temp') %>% 
        split(.$Latitude) %>%                                                   # Used 'Latitude' instead of 'siteName' b/c all latitudes are unique but some siteNames are duplicated 
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(period = 'temp',
               Latitude = as.numeric(Latitude))                           

    # Seasonal wetlands ----- 
      
      seasWilcox <- sitePer01 %>%
        filter(period == 'seasonal') %>% 
        split(.$Latitude) %>%                               
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%     
        map_df(broom::tidy, .id = 'Latitude') %>%          
        mutate(period = 'seasonal',
                Latitude = as.numeric(Latitude))
      
    # semi-perm wetlands ----- 

      semiWilcox <- sitePer01 %>%
        filter(period == 'semi') %>% 
        split(.$Latitude) %>%                               
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(period = 'semi',
                Latitude = as.numeric(Latitude))                          

    # Merge Wilcoxon test with data table -----
    
      perChange <- tempWilcox %>%
        full_join(seasWilcox) %>%
        full_join(semiWilcox) %>%
        full_join(sitePer02, by = c('Latitude', 'period')) %>%  # join by both latitude and period
        select(siteName, Latitude, Longitude, period, t1, t2, change, p.value)
      
    # Summaries -----
      
      # How many sites for each hydroperiod? -----
      
        tempSites <- subset(perChange, period == 'temp')
          nrow(tempSites) # <-- all sites have temp habitat
          
        seasSites <- subset(perChange, period == 'seasonal')
          nrow(seasSites) # <-- all sites have seasonal habitat
          
        semiSites <- subset(perChange, period == 'semi')
          nrow(semiSites) # <-- all sites have semi perm habitat
      
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
          nrow(semiSiteSig) # 110 sites with significant results in semi-permanent wetlands
        semiSiteNeg <- semiSiteSig %>% 
          filter(change <0)
        nrow(semiSiteNeg) # 97 sites with significant declines in semi-permanent wetlands
        semiSitePos <- semiSiteSig %>% 
          filter(change > 0)
        nrow(semiSitePos) # 13 sites with significant increases in semi-permanent wetlands

#-------------------------------------------------------------------------------        
# sites and wetland type: 
# What are the wetland type trends at individual ibis blobs? -----

  # summarize data and then join -----
      
    siteWetType <- hydroTerm %>% 
      group_by(idPoly, siteName, month, year, wetType, term) %>%           
      summarise(wetHa = sum(wetHa)) %>%     
      ungroup() %>% 
      left_join(sites, by = c('idPoly', 'siteName'))
  
  # calculate mean amt of water per hydroperiod per term per site plus % change -----
    
    siteType01 <- siteWetType %>% 
      group_by(siteName, Latitude, Longitude, month, year, wetType, term) %>%
      summarise(wetHa = sum(wetHa)) %>%
      ungroup() %>% 
      group_by(siteName, Latitude, Longitude, wetType, year, term) %>% 
      summarise(wetMean = mean(wetHa))
        
    siteType02 <- siteType01 %>% 
      group_by(siteName, Latitude, Longitude, wetType, term) %>% 
      summarise(wetMean = mean(wetMean)) %>% 
      spread(term, wetMean) %>%                          
      mutate(change = ((t1-t2)/t1)*-100) %>% 
      na.omit()
 
  # Wilcoxon test for each hydro period  ----- 
      
    # Riverine wetlands -----  
      
      rivWilcox <- siteType01 %>%
        filter(wetType == 'riv') %>% 
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'riv',
               Latitude = as.numeric(Latitude))
      
    # wet wetlands -----  
      
      wetWilcox <- siteType01 %>%
        filter(wetType == 'wet') %>% 
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'wet',
               Latitude = as.numeric(Latitude)) 
      
    # wetAg wetlands -----  
      
      agWilcox <- siteType01 %>%
        filter(wetType == 'wetAg') %>% 
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'wetAg',
               Latitude = as.numeric(Latitude)) 
      
    # wetAg wetlands -----  
      
      manWilcox <- siteType01 %>%
        filter(wetType == 'wetMan') %>%
        split(.$Latitude) %>%                         
        map(~wilcox.test(wetMean ~ term, data = .x)) %>%    
        map_df(broom::tidy, .id = 'Latitude') %>%           
        mutate(wetType = 'wetMan',
               Latitude = as.numeric(Latitude)) 
      
  # Merge Wilcoxon test with data table -----
    
      typeChange <- rivWilcox %>%
        full_join(wetWilcox) %>%
        full_join(agWilcox) %>%
        full_join(manWilcox) %>%
        full_join(siteType02, by = c('Latitude', 'wetType')) %>%            
        select(siteName, Latitude, Longitude, wetType, t1, t2, change, p.value) %>% 
        na.omit()
      
      # Summaries -----
      
      # How many sites for each wetland type? -----
      
        rivSites <- subset(typeChange, wetType == 'riv')
          nrow(rivSites) # <-- 96 sites
          
        wetSites <- subset(typeChange, wetType == 'wet')
          nrow(wetSites) # <-- 151 sites
          
        agSites <- subset(typeChange, wetType == 'wetAg')
          nrow(agSites) # <-- 134 sites
          
        manSites <- subset(typeChange, wetType == 'wetMan')
          nrow(manSites) # <-- 103 sites
      
      # How many sites show significance? -----
      
        rivSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'riv')
          nrow(rivSitesSig) # <-- 51 sites with significant results in riv wetlands
        rivSitesNeg <- rivSitesSig %>% 
          filter(change <0)
          nrow(rivSitesNeg) # <-- 42 sites with significant declines in riv wetlands
        rivSitesPos <- rivSitesSig %>% 
          filter(change > 0)
        nrow(rivSitesPos) # <-- 9 sites with significant increases in riv wetlands
        
        wetSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'wet')
          nrow(wetSitesSig) # <-- 93 sites with significant results in wet wetlands
        wetSitesNeg <- wetSitesSig %>% 
          filter(change <0)
        nrow(wetSitesNeg) # <-- 87 sites with significant declines in wet wetlands
        wetSitesPos <- wetSitesSig %>% 
          filter(change > 0)
        nrow(wetSitesPos) # <-- 6 sites with significant increases in wet wetlands
        
        agSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'wetAg')
          nrow(agSitesSig) # <-- 76 sites with significant results in wetAg wetlands
        agSitesNeg <- agSitesSig %>% 
          filter(change <0)
        nrow(agSitesNeg) # <-- 68 sites with significant declines in wetAg wetlands
        agSitesPos <- agSitesSig %>% 
          filter(change > 0)
        nrow(agSitesPos) # <-- 8 sites with significant increases in wetAg wetlands
        
        manSitesSig <- subset(typeChange, p.value < 0.05 & wetType == 'wetMan')
          nrow(manSitesSig) # <--77 sites with significant results in managed wetlands
        manSitesNeg <- manSitesSig %>% 
          filter(change <0)
        nrow(manSitesNeg) # <-- 73 sites with significant declines in managed wetlands
        manSitesPos <- manSitesSig %>% 
          filter(change > 0)
        nrow(manSitesPos) # <-- 4 sites with significant increases in managed wetlands
        

  
  