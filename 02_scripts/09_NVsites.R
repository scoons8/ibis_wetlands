################################################################################
# Task: Extract NV sites, summarize, map, and send to Patrick
# 
# Dec 29 2020
################################################################################




NVsites <- c('Alkali Lake WMA', 'Canvasback Club', 'Carson Lake',
             'Harmon Reservoir', 'Humboldt WMA', 'Mason Valley WMA',
             'Stillwater NWR', 'Swan Lake', 'Washoe Lake', 'Humboldt River',
             'Quinn River Lakes', 'Quinn River', 'Rye Patch Reservoir', 
             'Rock Creek', 'Franklin Lake', 'Ruby Lake NWR', 'Secret Soldier',
             'Spring Valley', 'Kirch WMA', 'Railroad Valley', 'Ash Meadows NWR')

siteChangeNV <- siteChange %>% 
  filter(siteName %in% NVsites)

fwrite(siteChangeNV, '01_data/13_NVsites/NVsites.csv')


 siteCat <- siteChangeNV %>% 
        mutate(cat = case_when(change < 0 & p.value < 0.05 ~ "significant Decrease",
                               change < 0 & p.value > 0.05 ~ "non-significant Decrease",
                               change > 0 & p.value < 0.05 ~ "significant Increase",
                               change > 0 & p.value > 0.05 ~ "non-significant Increase"))
    
    # Define the color palette -----
      
      pal <- colorFactor(palette = c("#369499", "#98554E", "#00eaf2","#cc0a00"), 
                         levels = c("non-significant Increase","non-significant Decrease", "significant Increase", "significant Decrease"))  
    
    # Create the leaflet map with the site locations -----
    
      leaflet(data = siteCat) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%     
      setView(lng = -121.7373, lat = 41.9875, zoom = 5) %>% 
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
