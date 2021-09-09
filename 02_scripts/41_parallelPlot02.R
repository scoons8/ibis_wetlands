################################################################################
# Task: Create nested proportions visualization
# 
# Jan 20 2021
################################################################################

# Load packages -----

  library(reshape2)
  library(tidyverse)
  library(ggforce)
  library(data.table)
  library(gridExtra)
  library(ggpubr)

# Read data -----

  data <- fread('01_data/19_hydroCleaned/hydroGraBlobFin03.csv')
  
  hydroGra <- data %>%
    filter(year > 2003) %>% 
    mutate(period = case_when(period == 'temp' ~ 'Temporary',
                             period == 'seasonal' ~ 'Seasonal',
                             period == 'semi' ~ 'Semi-permanent')) %>% 
    na.omit()
  
  # case_when(str_detect(ownAg, 'Pub') ~ 'Public',  # When the string 'Pub' is detected in the column 'ownAg', replace it with 'Public'
  #                            str_detect(ownAg, 'Pri') ~ 'Private',
  # 
# Great Basin -----

  hydro01 <- hydroGra %>% 
    filter(ecoHydro == 'Great Basin-Colorado Plateau') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro02 <- gather_set_data(hydro01, 2:4)
  hydro02$x <- factor(hydro02$x, levels = c('period', 'ownAg', 'wetType'))
  
  GB <- ggplot(hydro02, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Great Basin-Colorado Plateau") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Mojave -----
  
  hydro03 <- hydroGra %>% 
    filter(ecoHydro == 'Mojave-Sonoran Deserts') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro04 <- gather_set_data(hydro03, 2:4)
  hydro04$x <- factor(hydro04$x, levels = c('period', 'ownAg', 'wetType'))
  
  MD <- ggplot(hydro04, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Mojave-Sonoran Deserts") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Middle Rockies -----
  
  hydro05 <- hydroGra %>% 
    filter(ecoHydro == 'Middle Rockies') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro06 <- gather_set_data(hydro05, 2:4)
  hydro06$x <- factor(hydro06$x, levels = c('period', 'ownAg', 'wetType'))
  
  MR <- ggplot(hydro06, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Middle Rockies") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Northern Plains -----
  
  hydro07 <- hydroGra %>% 
    filter(ecoHydro == 'Northern Plains') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro08 <- gather_set_data(hydro07, 2:4)
  hydro08$x <- factor(hydro08$x, levels = c('period', 'ownAg', 'wetType'))
  
  NP <- ggplot(hydro08, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Northern Plains") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Northern Rockies -----
  
  hydro09 <- hydroGra %>% 
    filter(ecoHydro == 'Northern Rockies') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro10 <- gather_set_data(hydro09, 2:4)
  hydro10$x <- factor(hydro10$x, levels = c('period', 'ownAg', 'wetType'))
  
  NR <- ggplot(hydro10, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Northern Rockies") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Pacific NW -----
  
  hydro11 <- hydroGra %>% 
    filter(ecoHydro == 'Pacific NW') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro12 <- gather_set_data(hydro11, 2:4)
  hydro12$x <- factor(hydro12$x, levels = c('period', 'ownAg', 'wetType'))
  
  PNW <- ggplot(hydro12, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Pacific NW") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Southern Plains -----
  
  hydro13 <- hydroGra %>% 
    filter(ecoHydro == 'Southern Plains') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro14 <- gather_set_data(hydro13, 2:4)
  hydro14$x <- factor(hydro14$x, levels = c('period', 'ownAg', 'wetType'))
  
 SP <- ggplot(hydro14, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Southern Plains") +
    theme(plot.title = element_text(hjust = 0.5))
  
# Southern Rockies -----
  
  hydro15 <- hydroGra %>% 
    filter(ecoHydro == 'Southern Rockies and Basins') %>% 
    group_by(ecoHydro, ownAg, wetType, period, month, year) %>% 
    summarize(wetSum = sum(wetHa)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period, year) %>% 
    summarize(wetMean = mean(wetSum)) %>% 
    ungroup() %>% 
    group_by(ecoHydro, ownAg, wetType, period) %>% 
    summarise(wetMean = mean(wetMean)) %>% 
    select(period, ownAg, wetType, wetMean)
  
  hydro16 <- gather_set_data(hydro15, 2:4)
  hydro16$x <- factor(hydro16$x, levels = c('period', 'ownAg', 'wetType'))
  
  SR <- ggplot(hydro16, aes(x, id = id, split = y, value = wetMean)) +
    geom_parallel_sets(aes(fill = period), alpha = 0.6, axis.width = 0.1) +
    scale_fill_manual(values = c('#C07C6B', '#FED179', '#23C9B6'), name = 'Hydroperiod') +
    geom_parallel_sets_axes(axis.width = 0.1, fill = 'light gray', color = 'dark gray') +
    geom_parallel_sets_labels(angle = 90, colour = 'black', size = 4)+
    theme_void() + 
    ggtitle("Southern Rockies and Basins") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
# Plot everything together
  
  ggarrange(GB, NULL, MD, MR, NULL, NP, ncol = 2, nrow = 2, widths = c(1, 0.05, 1), common.legend = TRUE, legend = 'bottom')
  ggarrange(NR, PNW, SP, SR, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
  plotAll <- ggarrange(GB, MD, MR, NP, NR, PNW, SP, SR, ncol = 4, nrow = 2, common.legend = TRUE, legend="bottom")
  
  ggsave("parallelPlotsAll02.png", plot = last_plot(), width = 20, height = 10, units = "in", device='png', dpi=300)

#===============================================================================
# Save tables for proportions -----
  
  gbTab <- hydro01 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(gbTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(gbTab, '03_output/proGB.csv')
  
  mdTab <- hydro03 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(mdTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(mdTab, '03_output/proMD.csv')
  
  mrTab <- hydro05 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(mrTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(mrTab, '03_output/proMR.csv')
  
  npTab <- hydro07 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(npTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(npTab, '03_output/proNP.csv')
  
  nrTab <- hydro09 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(nrTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(nrTab, '03_output/proNR.csv')
  
  pnwTab <- hydro11 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(pnwTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(pnwTab, '03_output/proPNW.csv')

  spTab <- hydro13 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(spTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(spTab, '03_output/proSP.csv')  

  srTab <- hydro15 %>%
     mutate_if(is.numeric, ~round(.,2))
  colnames(srTab) <- c('Region', 'Hydroperiod', 'Ownership', 'Wetland Type', 'Wetland Area (ha)')
  fwrite(srTab, '03_output/proSR.csv')  
    
  
