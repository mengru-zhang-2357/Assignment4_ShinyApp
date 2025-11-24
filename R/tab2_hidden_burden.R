#####################################################
### This script is used to create the data frames ###   
### used in Tab 2.                                ###
#####################################################

library(tidyverse)

hidden_burden_df <- read.csv("data/processed/tab2_data.csv")

#####################################################
### Process DHS Program data: produce summary
### statistics by country. Indicators include:     
### Time to obtain water                          
### Person to obtain water                        
### Physical violence  
#####################################################

# Filter out the data corresponding to each indicator, remove unnecessary columns

# Time to obtain water data
time_obtain_water <- hidden_burden_df %>% 
  filter(IndicatorName == "Time to obtain water") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na ()

# Update the Indicator to shorter descriptions so it's easier to show in legends
time_obtain_water$Indicator <- factor(time_obtain_water$Indicator, 
                                      labels = c("Water on Premise",
                                                 "Less than 30 Minutes",
                                                 "More than 30 Minutes",
                                                 "Missing Info"),
                                      levels = c("Population with water on the premises",
                                                 "Population with water 30 minutes or less away round trip",
                                                 "Population with water more than 30 minutes away round trip",
                                                 "Population with unknown or missing information on round trip time to water"
                                                 ))

# Person to obtain water data
person_obtain_water <- hidden_burden_df %>% 
  filter(IndicatorName == "Person to obtain water",
         Indicator != "Population with no drinking water on premises") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na()
# Update the Indicator to shorter descriptions so it's easier to show in legends
person_obtain_water$Indicator <- factor(person_obtain_water$Indicator, 
                                      labels = c("Adult Female",
                                                 "Adult Male",
                                                 "Child Female",
                                                 "Child Male"),
                                      levels = c("Adult female age 15 or older collects the drinking water",
                                                 "Adult male age 15 or older collects the drinking water",
                                                 "Female child under age 15 collects the drinking water",
                                                 "Male child under age 15 collects the drinking water"))

# Physical violence against women data
physical_violence <- hidden_burden_df %>% 
  filter(IndicatorName == "% Women experience physical violence") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

#####################################################
### Process WHO data: produce summary statistics
### by country. Indicators include:     
### % Population with safely managed water                          
### % Population with basic water                        
#####################################################

# Filter out the data corresponding to each indicator, remove unnecessary columns
# % Population with safely managed water access
safe_water <- hidden_burden_df %>% 
  filter(IndicatorName == "% Population using safely managed water") %>% 
  select(-X, -Indicator, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

# % Population with basic water access
basic_water <- hidden_burden_df %>% 
  filter(IndicatorName == "% Population using basic water") %>% 
  select(-X, -Indicator, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

#####################################################
### Process World Bank data: produce summary statistics
### by country. Indicators include:     
### % female adolescents out of secondary school                          
### % male adolescents out of secondary school
#####################################################

# Filter out the data corresponding to each indicator, remove unnecessary columns

# Female adolescent out of school data
female_une <- hidden_burden_df %>% 
  filter(IndicatorName == "% female adolescent out of secondary school") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source, -Indicator) %>% 
  drop_na(Value)

# Male adolescent out of school data
male_une <- hidden_burden_df %>% 
  filter(IndicatorName == "% male adolescent out of secondary school") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source, -Indicator) %>% 
  drop_na(Value)

#####################################################
# To investigate the association between water access and other factors, we create joins between water access and several other data frames

# Combine female and male unenrollment data
unenrollment <- bind_rows(female_une %>% mutate(Gender = "Female"),
                          male_une %>% mutate(Gender = "Male"))

# Join basic water with unrollment data for both sex
water_une <- unenrollment %>% 
  left_join(basic_water, by = c("Year", "Country", "Region")) %>% 
  rename (unenrollment = Value.x,
          access_to_basic_water = Value.y) %>% 
  filter(Residence_Area == "Total")

# Join basic water with person fetching water
water_access_fetch <- person_obtain_water %>% 
  left_join(basic_water, by = c("Year", "Country", "Region")) %>% 
  rename (person_obtain_water = Indicator,
          population_pct = Value.x,
          access_to_basic_water = Value.y) %>% 
  filter(Residence_Area == "Total") %>% 
  drop_na()

# Join basic water with physical violence
water_violence <- physical_violence %>% 
  left_join(basic_water, by = c("Year", "Country", "Region")) %>% 
  rename (pct_violence = Value.x,
          access_to_basic_water = Value.y) %>% 
  filter(Residence_Area == "Total") %>% 
  drop_na()


