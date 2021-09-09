library('forcats')

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
      
      
      greatBasin %>%
        filter(year == '1990') %>% 
        ggplot(aes(x = month, y = wetSum)) +
        geom_area(stat="identity")
      
      GBridge <- transform(greatBasin, variable=reorder(wetSum, -month) ) 
      
       GBridge <- greatBasin %>% 
        filter(year %in% c(1990, 1995, 2000, 2010, 2015, 2020)) %>% 
        ggplot(aes(x = wetSum, y = as.factor(year), fill = as.factor(month))) +  
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Great Basin") +
                  xlab("wet hectares")
      
      
      
      GBridge <- transform(greatBasin, variable=reorder(wetSum, -month) ) 
      
       GBridge <- greatBasin %>% 
        ggplot( aes(reorder(wetSum, month), x = wetSum, y = year, fill = as.factor(year))) +  
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Great Basin") +
                  xlab("wet hectares")
      
      
       GBridge <- greatBasin %>%                                                  # Call on greatBasin df
        arrange(month) %>%                                                     # arrange the data according to wetSum (not sure this step is necessary)
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", 
                                                "Jul", "Jun", "May", 
                                                "Apr", "Mar")),
               year = factor(year)) %>%             # change the order of month column for plotting
        ggplot( aes(x = wetSum, y = year, fill = as.factor(year))) +          # I may not need the 'as.factor' now that I've changed the months from integers to strings
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Great Basin") +
                  xlab("wet hectares")

      
      
      
            
     GBridge <- greatBasin %>%                                                  # Call on greatBasin df
        arrange(wetSum) %>%                                                     # arrange the data according to wetSum (not sure this step is necessary)
        mutate(month = factor(month, levels = c("Oct", "Sept", "Aug", 
                                                "Jul", "Jun", "May", 
                                                "Apr", "Mar"))) %>%             # change the order of month column for plotting
        ggplot( aes(x = wetSum, y = month, fill = as.factor(month))) +          # I may not need the 'as.factor' now that I've changed the months from integers to strings
                  geom_density_ridges() +
                  theme_ridges() +
                  theme(legend.position = "none") +
                  labs(title = "Great Basin") +
                  xlab("wet hectares") +
                  facet_wrap(~term, ncol = 1, scales = 'fixed')   
     
     
     
     
     
     
     
     
  ##############################################################################
     
set.seed(1)
Data <- data.frame(
  Abundance = sample(1:100),
  Organism = sample(c("organism1", "organism2"), 100, replace = TRUE)
)
Date = rep(seq(from = as.Date("2016-01-01"), to = as.Date("2016-10-01"), by = 
'month'),times=10)
Data <- cbind(Date, Data)

ggplot(Data, aes(x = Abundance, y = Organism)) + 
  geom_density_ridges(scale=1.15, alpha=0.6, color="grey90")

Data_sum <- Data %>% 
  uncount(Abundance)

ggplot(Data_sum, aes(x = Date, y = Organism)) + 
  ggridges::geom_density_ridges(scale=1, alpha=0.6, color="grey90")





GB01 <- greatBasin %>% 
  select(year, month, wetSum) %>%
  #mutate(wetSum = wetSum / 100) %>% # maybe divide wetSum by 10000 to make this table less huge
  uncount(wetSum) %>% 
  filter(year %in% c(1990, 1995, 2000, 2010, 2015, 2020))

ggplot(GB01, aes(x = month, y = year, fill = as.factor(year))) +
  ggridges::geom_density_ridges(scale=1, alpha=0.6)

GB02 <- greatBasin %>% 
  filter(year == 1990)

ggplot(GB02, aes(x = month, y = wetSum)) +
  geom_area(stat = 'identity')

