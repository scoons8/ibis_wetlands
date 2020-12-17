################################################################################
# Task: Random Forest
# 
# Dec 14 2020
################################################################################

# Packages -----

  library(tidyverse)
  library(randomForestSRC)
  library(ggRandomForests)
  library(gridExtra)
  library(mgcv)
  library(forcats)
  library(zoo)
  library(ggpubr)
  library(readr)
  options(scipen=999) # dont use scientific notations

# Read data -----
  
  clim <- read_csv('01_data/11_climate_vars_clean')
  climVar <- tbl_df(clim) %>% 
    select('year','pet','pr','ro','watDif') %>% 
    mutate(year = year+1) %>% 
    filter(year != 2020)

aveTX <- read_csv('TX_ave.csv')
aveTX <- tbl_df(aveTX) %>% 
  select('year','pdsi','tmmn','tmmx') %>% 
  mutate(year = year+1) %>% 
  filter(year != 2020)

wetTX <- read_csv('TX_36Y_Fall.csv')
wetTX <- tbl_df(wetTX) %>% 
  select('1984':'2019','wetType') %>% 
  gather(year, wetHa,'1984':'2019') %>%
  group_by(year) %>%
  summarise(wetHa = sum(wetHa)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>% 
  full_join(aveTX) %>% 
  full_join(sumTX)
  

# Change running mean years here before run:
ny = 5  # Number of years over which to run the running mean.
wetTX <- wetTX %>%
  transmute(
    year = year,
    pet = rollapplyr(pet, ny, mean, partial=TRUE),# climate vars
    pr = rollapplyr(pr, ny, mean, partial=TRUE),
    ro = rollapplyr(ro, ny, mean, partial=TRUE),
    # swe = rollapplyr(swe, ny, mean, partial=TRUE), # no snow in Texas
    pdsi = rollapplyr(pdsi, ny, mean, partial=TRUE),                              # take a 5 year rolling mean of climate data because of variability (variability throws RF off)
    tmmn = rollapplyr(tmmn, ny, mean, partial=TRUE),                              # normalize variance
    tmmx = rollapplyr(tmmx, ny, mean, partial=TRUE),                              # partial = TRUE 
    watDif = rollapplyr(watDif, ny, mean, partial=TRUE),
    wetHa = wetHa) %>% # wetland surface water (think about adding irrigated ag area and human pop density)
    na.omit()

#filter(individual ecohydroregion) <- need to do this 9 times

                # copying from Patrick's presentation (not actually apart of og code)   
                wet_rmUsa <- rmDat %>% 
                  dplyr::select(wetha, pr, pet, ro, swe, Ag_ha, popDen) %>% 
                  data.frame()

# Must make into data.frame for packages to run--some commands will not work on tibble.
# WetC response var.

wetData <- wetTX %>% 
  select(-year) %>%
  data.frame()

# Running randomForestSRC--can change number of trees, ntree:
wetDataSrc <- rfsrc(wetHa~ ., data=wetData, importance = TRUE, ntree=5000) # running the model for one portion of dataset, wetHa is the response variable ~. is shorthand for telling it to use wetHa as response? 
# ntree = number of iterations (5000)

# Calc CI for vimp
wetDataSrcCi <- subsample(wetDataSrc, B = 500)  # collect random subsample of trees (500)

# extracts results for plotting
srcSum <- extract.subsample(wetDataSrcCi)       # 
                                      #summary_CI_Wet_usa <- extract.subsample(r_src_Wet_rmUSA)
                                      #head(summary_CI_Wet_usa)
#  Selecting data to use for boxplot from summary data: $ci.jk.Z (column names in subsample from above)
#  Changing row names because they cause problems. Then gathering around rowname and spreading to
#  Get data with each part of boxplot as a column.

row.names(srcSum$ci.jk.Z) <- c("min","low","mid","up","max")
wetBxp <- srcSum$ci.jk.Z %>%
  data.frame() %>%
  rownames_to_column() %>% 
  gather(var, value,-rowname) %>% 
  spread(rowname, value)

# boxplot - var importance in predicting wetland surface water area
ggplot(wetBxp, aes(x=var, lower=low, upper=up, middle=mid, ymin=min, ymax=max))+
  geom_boxplot(stat = "identity", fill='light gray', lwd = .2) +
  coord_flip() +
  theme_classic() +
  ylab("variable importance") +
  xlab("") 