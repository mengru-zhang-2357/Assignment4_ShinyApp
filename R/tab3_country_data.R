#####################################################
### This script is used to create the data frames ###   
### used in Tab 3.                                ###
#####################################################

library(tidyverse)

country_data_df <- read.csv("data/processed/tab1_data.csv")

#####################################################
### Process WHO data: produce summary statistics
### by country. Indicators include:     
### % Population with safely managed water                          
### % Population with basic water                        
#####################################################

# Filter out the data corresponding to each indicator, remove unnecessary columns
diarrhoea_disease <- country_data_df %>% 
  filter(IndicatorName == "Diarrhoea disease attributable to inadequate water") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

unsafe_water_mortality <- country_data_df %>% 
  filter(IndicatorName == "Mortality rate attributed to unsafe water or sanitation") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na()