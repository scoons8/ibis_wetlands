################################################################################
# Task: Map the ibis colony sites and color-code significance using ggplot
# Mar 24 2021
################################################################################



library(ggplot2)
library(maps)
library(sf)
library(sp)

# Read data -----

  change <- fread('03_output/05_Tables/table_region_wilcox.csv')
  
  eco <- st_read(
      "/Users/sc148852/Box/Qgis/Thesis/06_EcoHydro Regions/ecoHydroWest_UTM12.shp")
    ecoWGS <- st_transform(eco, CRS("+init=epsg:4326"))
    
  # Delete the ecoregions that I don't need -----
    
    eco02 <- ecoWGS %>% 
      filter(ecoHydro != 'Pacific SW') %>% 
      filter(ecoHydro != 'Madrean Mts-Chihuahuan Desert')
  
# create factor by which to color code direction of change -----
  
  change01 <- change %>% 
      mutate(wetHaDif = (T2-T1),
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
  
  colnames(changeCoords) <- c('Region', 'T1', 'T2', 'Change', 'p.value', 'Dif', 'type', 'perChange', 'eco', 'long', 'lat')

# Map -----
  
  # Create color palette for increasing and decreasing water ------
  
    pal <- colorFactor(palette = c("#23c9b6", "#c07c6b"),
                       levels = c("positive", "negative"))
  


states <- map_data("state")
st <- c('california', 'oregon', 'idaho', 'montana', 'wyoming', 'colorado',
        'utah', 'nevada', 'washington', 'arizona', 'new mexico')
states2 <- states %>% 
  filter(region %in% st)

#------
sites <- fread("01_data/09_site_names/siteNamesID.csv")
siteChange <- fread('03_output/05_Tables/table_site_wilcox.csv')
siteCat <- siteChange %>% 
        mutate(cat = case_when(change < 0 & p.value < 0.05 ~ "Sig. Decrease",
                               change < 0 & p.value > 0.05 ~ "Non-sig. Decrease",
                               change > 0 & p.value < 0.05 ~ "Sig. Increase",
                               change > 0 & p.value > 0.05 ~ "Non-sig. Increase")) %>% 
        mutate(cat = as.factor(cat))

siteTrendMap <- ggplot() +
  geom_polygon(data = states2,
               mapping = aes(x = long, y = lat, group = group),
               fill = 'gray80', colour = 'white') +
  geom_point(data = siteCat, mapping = aes(x = Longitude, y = Latitude, color = cat), size = 3, alpha = 0.7) +
  scale_color_manual(values = c('#FED179', '#265895', '#B54545', '#23C9B6'), name = 'Significance') +
  theme_void() #+
  # theme(legend.background = element_rect(fill="gray80",
                                  # size=0.5, linetype="solid", 
                                  # colour ="white"))
  # coord_quickmap()

ggsave("siteTrends.png", plot = siteTrendMap, width = 7, height = 5, units = "in", device='png', dpi=300)

fwrite(siteCat, "03_output/05_Tables/siteTrendValues.csv")






