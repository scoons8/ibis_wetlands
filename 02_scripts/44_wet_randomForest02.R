################################################################################
# Task: Random Forest Analysis of variable importance for wetland data
# # Example randomFroest analysis for landscape driver data using flooded 
# wetland area as response var.
#
# Jan 04 2020
################################################################################

# Load packages -----

  library(tidyverse)
  library(randomForestSRC)
  library(ggRandomForests)
  library(gridExtra)
  library(mgcv)
  library(forcats)
  library(zoo)
  library(ggpubr)
  library(data.table)
  options(scipen=999)

# Load data -----
  
  # AllDat <- fread('01_data/11_RF_vars_clean/RFvarstest03.csv')
  AllDat <- fread('01_data/11_RF_vars_clean/RFvars06.csv')
  
# Produce file with run mean of selected variables that will 
# be used in the ranFor analysis. -----
  
  # Can change variable list if want more from AllDat -----
    
    head(AllDat)
  
  # Change running mean years here before run -----
  
    ny = 5  # Number of years over which to run the running mean.
    
    rmDat <- AllDat %>%
      transmute(
        NAME = siteName,
        region = ecoHydro,
        Year = year,
        WET_ha = wetSum,                                    # wetland surface water area - could sub NDVI productivity value
        AG_ha = irr_ha,                                    # irrigated area
        seasonal = seasonal,                               # seasonal wetland area (possible response var)
        semiPerm = semi,                                   # semi-permanent wetland area (possible response var)
        temporary = temp,                                  # temporary wetland area (possible response var)
        # Public = Public,                                   # Public wetland area (possible response var)
        # Private = Private,                                 # private wetland area (possible response var)
        # PopDen = PopDen,# human population density
        tmin = rollapplyr(tmin, ny, mean, partial = TRUE), # avg min temp
        aet = rollapplyr(aet, ny, mean, partial = TRUE),   # climate vars
        pr = rollapplyr(pr, ny, mean, partial = TRUE),     # take a 5 year rolling mean of climate data to adjust variability (variability throws RF off)
        ro = rollapplyr(ro, ny, mean, partial = TRUE),     # normalize variance
        swe = rollapplyr(swe, ny, mean, partial = TRUE)) %>%
      na.omit()

# Filter by ecoregion - Need to run for each region (x8) -----
    
    rmDat01 <- rmDat %>% 
      filter(region == 'Southern Rockies and Basins') # <-- Change region here
    
        # Regions: 
          # Great Basin-Colorado Plateau
          # Middle Rockies
          # Mojave-Sonoran Deserts
          # Northern Plains
          # Northern Rockies
          # Pacific NW
          # Southern Plains
          # Southern Rockies and Basins

# Must make into data.frame for packages to run--some commands will not work on tibble.
# Wet_ha response var -----

  Wet_rmUsa <- rmDat01 %>% 
    dplyr::select(semiPerm, pr,aet,ro,swe,tmin,AG_ha) %>% # <-- switch out response var here 
    data.frame()

# Running randomForestSRC--can change number of trees, ntree -----
# Run the model for one portion of dataset; 
# WetHa is the response variable
# ~. is shorthand for telling it to use wetHa as response? -----

  src_Wet_rmUsa <- rfsrc(semiPerm~ ., data=Wet_rmUsa, importance = TRUE, ntree=5000) 


# Calc CI for vimp -----
  # Collect random subsample of trees (500) -----

  r_src_Wet_rmUsa <- subsample(src_Wet_rmUsa, B = 500)                


# Extracts results for plotting -----

  summary_CI_Wet_usa <- extract.subsample(r_src_Wet_rmUsa)

#  Selecting data to use for boxplot from summary data: $ci.jk.Z
#  Changing row names because they cause problems. Then gathering around rowname and spreading to
#  Get data with each part of boxplot as a column -----

  row.names(summary_CI_Wet_usa$ci.jk.Z) <- c("min","low","mid","up","max")
    
  bxp_dat_Wet_usa <- summary_CI_Wet_usa$ci.jk.Z %>%
    data.frame() %>%
    rownames_to_column() %>% 
    gather(var, value,-rowname) %>% 
    spread(rowname, value) %>% 
    mutate(var=replace(var, var=="aet", "et"),
           var = factor(var, levels = c("ro", "swe", "pr", "et", "tmin", "AG_ha"))) %>% 
    dplyr::arrange(match(var, c("ro", "swe", "pr", "et", "tmin", "AG_ha")))

# boxplot - var importance in predicting flooded wetland abundance -----
  
  # ggplot(bxp_dat_Wet_usa, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max))+
  #   geom_boxplot(stat = "identity", lwd = .2) +
  #   coord_flip() +
  #   ylim(-0, 90) +
  #   theme_classic() +
  #   ylab("Variable Importance") +
  #   xlab("") +
  #   ggtitle("Temporary") # <-- change title based on region
  
#---------------------------------------
# Boxplot aesthetics
  # GB Temp -----
  boxDat <- bxp_dat_Wet_usa %>% 
    mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
    # mutate(type = ifelse(var == 'ro', 'Highlighted', 'Normal'))
  
  gbTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                     fill = type, alpha = type)) +
    geom_boxplot(stat = "identity", lwd = .2) +
    scale_fill_manual(values = c("#FED179", "grey")) +
    scale_alpha_manual(values = c(1, 0.7)) +
    coord_flip() +
    ylim(-0, 75) +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("Variable Importance") +
    xlab("") +
    ggtitle("Temporary") # <-- change title based on region
  
  ggsave("gbTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # GB Seasonal -----
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    gbSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 90) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("gbSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # GB SemiPerm -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'ro', 'Highlighted', 'Normal'))
    
    gbSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 75) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("gbSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # MR Temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'AG_ha', 'Highlighted', 'Normal'))
    
    mrTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 80) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("mrTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300) 
    
  # MR Seasonal -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'AG_ha', 'Highlighted', 'Normal'))
    
    mrSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 90) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("mrSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # MR SemiPerm -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    mrSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 75) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("mrSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # MD Temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'pr', 'Highlighted', 'Normal'))
    
    mdTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 40) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("mdTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # MD Seas -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    mdSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-5, 40) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("mdSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # MD SemiPerm -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "tmin" |  var == "AG_ha", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    mdSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-5, 40) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("mdSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # NP Temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      # mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    npTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 70) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("npTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # NP Seas -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      # mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    npSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 60) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("npSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # NP SemiPerm -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'AG_ha', 'Highlighted', 'Normal'))
    
    npSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 60) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("npSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # NR Temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    nrTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 90) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("nrTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # NR Seasonal -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    nrSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 110) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("nrSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # NR Semi -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'et', 'Highlighted', 'Normal'))
    
    nrSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 50) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("nrSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # PNW Temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'tmin', 'Highlighted', 'Normal'))
    
    pnwTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 100) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("pnwTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # PNW Seas -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "swe" |  var == "ro", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'swe', 'Highlighted', 'Normal'))
    
    pnwSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 70) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("pnwSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # PNW Semi -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      mutate(type = ifelse(var == "AG_ha" |  var == "tmin", "Highlighted", "Normal"))
      # mutate(type = ifelse(var == 'swe', 'Highlighted', 'Normal'))
    
    pnwSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 70) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("pnwSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # SP Temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "AG-ha" |  var == "tmin", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'AG_ha', 'Highlighted', 'Normal'))
    
    spTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 75) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("spTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # SP Seasonal -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "AG-ha" |  var == "tmin", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'AG_ha', 'Highlighted', 'Normal'))
    
    spSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 75) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("spSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # SP Semi -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      # mutate(type = ifelse(var == "AG-ha" |  var == "tmin", "Highlighted", "Normal"))
      mutate(type = ifelse(var == 'swe', 'Highlighted', 'Normal'))
    
    spSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 90) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("spSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # SR temp -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      mutate(type = ifelse(var == "tmin" |  var == "swe", "Highlighted", "Normal"))
      # mutate(type = ifelse(var == 'swe', 'Highlighted', 'Normal'))
    
    srTemp <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 110) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Temporary") # <-- change title based on region
    
    ggsave("srTemp.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)
    
  # SR Seas -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      mutate(type = ifelse(var == "tmin" |  var == "swe", "Highlighted", "Normal"))
      # mutate(type = ifelse(var == 'swe', 'Highlighted', 'Normal'))
    
    srSeas <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 140) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Seasonal") # <-- change title based on region
    
    ggsave("srSeas.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)

  # SR Semi -----
    
    boxDat <- bxp_dat_Wet_usa %>% 
      mutate(type = ifelse(var == "tmin" |  var == "swe", "Highlighted", "Normal"))
      # mutate(type = ifelse(var == 'swe', 'Highlighted', 'Normal'))
    
    srSemi <- ggplot(boxDat, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max, 
                       fill = type, alpha = type)) +
      geom_boxplot(stat = "identity", lwd = .2) +
      scale_fill_manual(values = c("#FED179", "grey")) +
      scale_alpha_manual(values = c(1, 0.7)) +
      coord_flip() +
      ylim(-0, 140) +
      theme_classic() +
      theme(legend.position = "none") +
      ylab("Variable Importance") +
      xlab("") +
      ggtitle("Semi-Permanent") # <-- change title based on region
    
    ggsave("srSemi.png", plot = last_plot(), width = 4, height = 3, units = "in", device='png', dpi=300)

#-------------------------------------------------------------------------------
  
#plots

gbTemp2 <- ggpar(gbTemp,  main = "")
gbSeas2 <- ggpar(gbSeas,  main = "")
gbSemi2 <- ggpar(gbSemi,  main = "")
mrTemp2 <- ggpar(mrTemp,  main = "")
mrSeas2 <- ggpar(mrSeas,  main = "")
mrSemi2 <- ggpar(mrSemi,  main = "")
mdTemp2 <- ggpar(mdTemp,  main = "")
mdSeas2 <- ggpar(mdSeas,  main = "")
mdSemi2 <- ggpar(mdSemi,  main = "")
npTemp2 <- ggpar(npTemp,  main = "")
npSeas2 <- ggpar(npSeas,  main = "")
npSemi2 <- ggpar(npSemi,  main = "")
nrTemp2 <- ggpar(nrTemp,  main = "")
nrSeas2 <- ggpar(nrSeas,  main = "")
nrSemi2 <- ggpar(nrSemi,  main = "")
pnwTemp2 <- ggpar(pnwTemp,  main = "")
pnwSeas2 <- ggpar(pnwSeas,  main = "")
pnwSemi2 <- ggpar(pnwSemi,  main = "")
spTemp2 <- ggpar(spTemp,  main = "")
spSeas2 <- ggpar(spSeas,  main = "")
spSemi2 <- ggpar(spSemi,  main = "")    
srTemp2 <- ggpar(srTemp,  main = "")    
srSeas2 <- ggpar(srSeas,  main = "")   
srSemi2 <- ggpar(srSemi,  main = "")
    
GB <- grid.arrange(gbTemp2 + rremove("x.title") + rremove("xlab"), gbSeas2 + rremove("x.title") + rremove("y.text"), gbSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
# GB <- grid.arrange(arrangeGrob(gbTemp2 + rremove("x.title"), top = "Temporary"), arrangeGrob(gbSeas2 + rremove("x.title"), top = "Seasonal"), arrangeGrob(gbSemi2 + rremove("x.title"), top = "Semi-Permanent"), ncol = 3)
GBtext <- annotate_figure(GB,
            left = text_grob("Great Basin-CO Plateau", rot = 90, size = 9))

MR <- grid.arrange(mrTemp2 + rremove("x.title"), mrSeas2 + rremove("x.title") + rremove("y.text"), mrSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
MRtext <- annotate_figure(MR,
            left = text_grob("Middle Rockies", rot = 90, size = 9))

MD <- grid.arrange(mdTemp2 + rremove("x.title"), mdSeas2 + rremove("x.title") + rremove("y.text"), mdSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
MDtext <- annotate_figure(MD,
            left = text_grob("Mojave-Sonoran Deserts", rot = 90, size = 9))

NP <- grid.arrange(npTemp2 + rremove("x.title"), npSeas2 + rremove("x.title") + rremove("y.text"), npSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
NPtext <- annotate_figure(NP,
            left = text_grob("Northern Plains", rot = 90, size = 9))


NR <- grid.arrange(nrTemp2 + rremove("x.title"), nrSeas2 + rremove("x.title") + rremove("y.text"), nrSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
# NR <- grid.arrange(arrangeGrob(nrTemp2 + rremove("x.title"), top = "Temporary"), arrangeGrob(nrSeas2 + rremove("x.title"), top = "Seasonal"), arrangeGrob(nrSemi2 + rremove("x.title"), top = "Semi-Permanent"), ncol = 3)
NRtext <- annotate_figure(NR,
            left = text_grob("Northern Rockies", rot = 90, size = 9))


PNW <- grid.arrange(pnwTemp2 + rremove("x.title"), pnwSeas2 + rremove("x.title") + rremove("y.text"), pnwSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
PNWtext <- annotate_figure(PNW,
            left = text_grob("Pacific NW", rot = 90, size = 9))


SP <- grid.arrange(spTemp2 + rremove("x.title"), spSeas2 + rremove("x.title") + rremove("y.text"), spSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)
SPtext <- annotate_figure(SP,
            left = text_grob("Southern Plains", rot = 90, size = 9))


SR <- grid.arrange(srTemp2 + rremove("x.title"), srSeas2 + rremove("x.title") + rremove("y.text"), srSemi2 + rremove("x.title") + rremove("y.text"), ncol = 3)  
SRtext <- annotate_figure(SR,
            left = text_grob("Southern Rockies & Basins", rot = 90, size = 9))

half1 <- grid.arrange(GBtext, MRtext, MDtext, NPtext, ncol = 1)
half2 <- grid.arrange(NRtext, PNWtext, SPtext, SRtext, ncol = 1)
allVert <- grid.arrange(GBtext, MRtext, MDtext, NPtext, NRtext, PNWtext, SPtext, SRtext, ncol = 1)


 ggsave("VIMP1.png", plot = half1, width = 6, height = 7, units = "in", device='png', dpi=300)
 ggsave("VIMP2.png", plot = half2, width = 6, height = 7, units = "in", device='png', dpi=300)
 ggsave("VIMP3.png", plot = allVert, width = 6, height = 11, units = "in", device='png', dpi=300)

#------------------------------------------------------------------------------
GB <- grid.arrange(gbTemp2 + rremove("x.title") + rremove("xlab"), gbSeas2 + rremove("x.title"), gbSemi2 + rremove("x.title"), ncol = 1)
# GB <- grid.arrange(arrangeGrob(gbTemp2 + rremove("x.title"), top = "Temporary"), arrangeGrob(gbSeas2 + rremove("x.title"), top = "Seasonal"), arrangeGrob(gbSemi2 + rremove("x.title"), top = "Semi-Permanent"), ncol = 3)
GBtext <- annotate_figure(GB,
            top = text_grob("Great Basin-CO Plateau", size = 9),
            left = text_grob ("Semi-Permanent                                 Seasonal                                   Temporary            ", rot = 90, size = 9))

MR <- grid.arrange(mrTemp2 + rremove("x.title") + rremove("y.text"), mrSeas2 + rremove("x.title") + rremove("y.text"), mrSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)
MRtext <- annotate_figure(MR,
            top = text_grob("Middle Rockies", size = 9))

MD <- grid.arrange(mdTemp2 + rremove("x.title") + rremove("y.text"), mdSeas2 + rremove("x.title") + rremove("y.text"), mdSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)
MDtext <- annotate_figure(MD,
            top = text_grob("Mojave-Sonoran Deserts", size = 9))

NP <- grid.arrange(npTemp2 + rremove("x.title") + rremove("y.text"), npSeas2 + rremove("x.title") + rremove("y.text"), npSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)
NPtext <- annotate_figure(NP,
            top = text_grob("Northern Plains", size = 9))


NR <- grid.arrange(nrTemp2 + rremove("x.title") + rremove("y.text"), nrSeas2 + rremove("x.title") + rremove("y.text"), nrSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)
# NR <- grid.arrange(arrangeGrob(nrTemp2 + rremove("x.title"), top = "Temporary"), arrangeGrob(nrSeas2 + rremove("x.title"), top = "Seasonal"), arrangeGrob(nrSemi2 + rremove("x.title"), top = "Semi-Permanent"), ncol = 3)
NRtext <- annotate_figure(NR,
            top = text_grob("Northern Rockies", size = 9))


PNW <- grid.arrange(pnwTemp2 + rremove("x.title") + rremove("y.text"), pnwSeas2 + rremove("x.title") + rremove("y.text"), pnwSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)
PNWtext <- annotate_figure(PNW,
            top = text_grob("Pacific NW", size = 9))


SP <- grid.arrange(spTemp2 + rremove("x.title") + rremove("y.text"), spSeas2 + rremove("x.title") + rremove("y.text"), spSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)
SPtext <- annotate_figure(SP,
            top = text_grob("Southern Plains", size = 9))


SR <- grid.arrange(srTemp2 + rremove("x.title") + rremove("y.text"), srSeas2 + rremove("x.title") + rremove("y.text"), srSemi2 + rremove("x.title") + rremove("y.text"), ncol = 1)  
SRtext <- annotate_figure(SR,
            top = text_grob("Southern Rockies & Basins", size = 9))

allHort <- plot_grid(GBtext, MRtext, MDtext, NPtext, NRtext, PNWtext, SPtext, SRtext, align = "h", nrow = 1, rel_widths = c(3/7, 1/4, 1/4, 1/4, 1/4, 1/4, 1/4, 1/4))


ggsave("VIMP5.png", plot = allHort, width = 13.5, height = 6, units = "in", device='png', dpi=300)
