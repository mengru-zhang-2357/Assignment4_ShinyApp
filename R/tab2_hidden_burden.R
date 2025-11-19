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
time_obtain_water <- hidden_burden_df %>% 
  filter(IndicatorName == "Time to obtain water") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na ()

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

person_obtain_water <- hidden_burden_df %>% 
  filter(IndicatorName == "Person to obtain water",
         Indicator != "Population with no drinking water on premises") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

person_obtain_water$Indicator <- factor(person_obtain_water$Indicator, 
                                      labels = c("Adult Female",
                                                 "Adult Male",
                                                 "Child Female",
                                                 "Child Male"),
                                      levels = c("Adult female age 15 or older collects the drinking water",
                                                 "Adult male age 15 or older collects the drinking water",
                                                 "Female child under age 15 collects the drinking water",
                                                 "Male child under age 15 collects the drinking water"))

physical_violence <- hidden_burden_df %>% 
  filter(IndicatorName == "% Women experience physical violence") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

# Create a list of data frames so we can process below via a loop
list_A <- list(
  time_obtain_water = time_obtain_water, 
  person_obtain_water = person_obtain_water, 
  physical_violence = physical_violence)
# Create an empty list to store processed data frames
list_Merged_A <- list()

# Loop through DHS data frames
for (i in 1: length(list_A)){
  # Read in the data
  df = list_A[[i]]
  
  # Calculate the average over time by country and indicator
  df_avg <- df %>% 
    group_by(Region, Country, Indicator) %>% 
    summarize(
      Value = mean(Value, na.rm = TRUE)
    ) %>% 
    mutate (Year = "Average") %>%           # Store this data as the "Average" over time
    group_by(Region, Indicator) %>% 
    mutate(regions_avg = mean(Value)) %>% 
    ungroup()
  
  # Filter the data points from the most recent year by country and indicator
  df_latest <- df %>% 
    group_by(Country, Indicator) %>%
    filter(Year == max(Year)) %>% 
    group_by(Region, Indicator) %>% 
    mutate(regions_avg = mean(Value)) %>% 
    ungroup()
  
  # Change the data type of "Year" to string so we can combine it with the average data frame in the next step
  df_latest$Year <- as.character(df_latest$Year)
    
  # Combine the average over time and the most recent reads
  df_merged <- bind_rows(df_avg, df_latest)
  
  # Store this merged df in list_Merged_A and rename this data frame
  list_Merged_A[[i]] <- df_merged
  names(list_Merged_A)[i] <- paste0(names(list_A)[i],"_merged")
}

# Convert the list back to individual data frames
list2env(list_Merged_A, envir = .GlobalEnv)

#####################################################
### Process WHO data: produce summary statistics
### by country. Indicators include:     
### % Population with safely managed water                          
### % Population with basic water                        
#####################################################

# Filter out the data corresponding to each indicator, remove unnecessary columns
safe_water <- hidden_burden_df %>% 
  filter(IndicatorName == "% Population using safely managed water") %>% 
  select(-X, -Indicator, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

basic_water <- hidden_burden_df %>% 
  filter(IndicatorName == "% Population using basic water") %>% 
  select(-X, -Indicator, -IndicatorName, -Unit, -Source) %>% 
  drop_na()

# Create a list of data frames so we can process below via a loop
list_B <- list(
  safe_water = safe_water, 
  basic_water = basic_water)
# Create an empty list to store processed data frames
list_Merged_B <- list()

# Loop through WHO data frames
for (i in 1: length(list_B)){
  # Read in the data
  df = list_B[[i]]
  
  # Calculate the average over time by country and indicator
  df_avg <- df %>% 
    mutate(Indicator = Residence_Area) %>% 
    group_by(Region, Country, Indicator) %>% 
    summarize(
      Value = mean(Value, na.rm = TRUE)
    ) %>% 
    mutate (Year = "Average") %>%         # Store this data as the "Average" over time
    group_by(Region, Indicator) %>% 
    mutate(regions_avg = mean(Value)) %>% 
    ungroup()
  
  # Filter the data points from the most recent year by country and indicator
  df_latest <- df %>% 
    rename(Indicator = Residence_Area) %>% 
    group_by(Country, Indicator) %>%
    filter(Year == max(Year)) %>% 
    group_by(Region, Indicator) %>% 
    mutate(regions_avg = mean(Value)) %>% 
    ungroup()
  
  # Change the data type of "Year" to string so we can combine it with the average data frame in the next step
  df_latest$Year <- as.character(df_latest$Year)
  
  # Combine the average over time and the most recent reads
  df_merged <- bind_rows(df_avg, df_latest)
  
  # Store this merged df in list_Merged_B and rename this data frame
  list_Merged_B[[i]] <- df_merged
  names(list_Merged_B)[i] <- paste0(names(list_B)[i],"_merged")
}

# Convert the list back to individual data frames
list2env(list_Merged_B, envir = .GlobalEnv)

#####################################################
### Process World Bank data: produce summary statistics
### by country. Indicators include:     
### % female adolescents out of secondary school                          
### % male adolescents out of secondary school
#####################################################

# Filter out the data corresponding to each indicator, remove unnecessary columns
female_une <- hidden_burden_df %>% 
  filter(IndicatorName == "% female adolescent out of secondary school") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source, -Indicator) %>% 
  drop_na(Value)

male_une <- hidden_burden_df %>% 
  filter(IndicatorName == "% male adolescent out of secondary school") %>% 
  select(-X, -Residence_Area, -IndicatorName, -Unit, -Source, -Indicator) %>% 
  drop_na(Value)

# Create a list of data frames so we can process below via a loop
list_C <- list(
  female_une = female_une, 
  male_une = male_une)

# Create an empty list to store processed data frames
list_Merged_C <- list()

# Loop through WDI data frames
for (i in 1: length(list_C)){
  # Read in the data  
  df = list_C[[i]]
  
  # Calculate the average over time by country
  df_avg <- df %>% 
    group_by(Region, Country) %>% 
    summarize(
      Value = mean(Value, na.rm = TRUE)
    ) %>% 
    mutate (Year = "Average") %>%         # Store this data as the "Average" over time
    group_by(Region) %>% 
    mutate(regions_avg = mean(Value)) %>% 
    ungroup()
  
  # Filter the data points from the most recent year by country
  df_latest <- df %>% 
    group_by(Country) %>%
    filter(Year == max(Year)) %>% 
    group_by(Region) %>% 
    mutate(regions_avg = mean(Value)) %>% 
    ungroup()
  
  # Change the data type of "Year" to string so we can combine it with the average data frame in the next step
  df_latest$Year <- as.character(df_latest$Year)
  
  # Combine the average over time and the most recent reads
  df_merged <- bind_rows(df_avg, df_latest)
  
  # Store this merged df in list_Merged_C and rename this data frame
  list_Merged_C[[i]] <- df_merged
  names(list_Merged_C)[i] <- paste0(names(list_C)[i],"_merged")
}

# Convert the list back to individual dataframes
list2env(list_Merged_C, envir = .GlobalEnv)

# To research the association between water access and school enrollment, we create joins between water access and enrollment data frames

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


