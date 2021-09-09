


# Packages -----
  
    library(tidyverse)
    library(data.table)
    # library(raster)
    # library(rgdal)
    library(sp)
    library(ggplot2)
    library(sf)
    library(leaflet)
    library(rgeos)

# Create map illustrating the magnitude 
# and direction of change for each region -----

# Read data -----

  change <- fread('03_output/table_region_wilcox.csv')
  
# Read in spatial data -----
  
  eco <- st_read(
      "/Users/sc148852/Box/Qgis/Thesis/06_EcoHydro Regions/ecoHydroWest_UTM12.shp")
    ecoWGS <- st_transform(eco, CRS("+init=epsg:4326"))
    
  # Delete the ecoregions that I don't need -----
    
    eco02 <- ecoWGS %>% 
      filter(ecoHydro != 'Pacific SW') %>% 
      filter(ecoHydro != 'Madrean Mts-Chihuahuan Desert')
  
# create factor by which to color code direction of change -----
  
  change01 <- change %>% 
      mutate(wetHaDif = (t2-t1),
             type = ifelse(wetHaDif > 0, 'positive', 'negative'),
             perChange = (change * 100),
             perChange = round(perChange))
    
# Join percent change data to spatial data -----
  # I don't think that I actually needed to do this step. -----
    
  ecoChange <- eco02 %>% 
      full_join(change01, by = 'ecoHydro')
    
# Add coordinates to the polygons -----
    
  ecoCoords <- fread(
    "/Users/sc148852/Box/Qgis/Thesis/06_EcoHydro Regions/ecoHydroCentroids.csv")
    
  changeCoords <- change01 %>% 
    full_join(ecoCoords, by = 'ecoHydro') %>% 
    as.data.frame()

# Map -----
  
  # Create color palette for increasing and decreasing water ------
  
    pal <- colorFactor(palette = c("#23c9b6", "#c07c6b"),
                       levels = c("positive", "negative"))
  
  # Leaflet map -----

    leaflet(changeCoords) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%     
        setView(lng = -121.7373, lat = 41.9875, zoom = 5) %>%   
        addPolygons(data = ecoChange,
                    color = "slategray", weight = 2, smoothFactor = 0.2,
                    opacity = 1.0, fillOpacity = 0.5,
                    fillColor = 'transparent',
                    stroke = T) %>% 
          addCircleMarkers(label = ~perChange,                          
                         labelOptions = labelOptions(noHide = T, 
                                                     direction = 'center',    
                                                     textOnly = T),          
                                                     # offset=c(-10,-5)),    
                         radius = ~ abs(change * 100),
                         stroke = FALSE,
                         color = ~pal(type),
                         fillOpacity = 0.8) 
