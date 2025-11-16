#####################################################
### This script is used to scrub data downloaded  ###   
### and create data frames ready for building     ###
### Shiny.                                        ###
#####################################################

# Set working directory
setwd("~/Personal/Brown/BHDS2010/Assignment4_ShinyApp/data")

# Install libraries used for data wrangling
library (tidyverse)
library (countrycode)

# Set up indicator lists
indicator_codes <- c("WSH_WATER_SAFELY_MANAGED",
                     "WSH_WATER_BASIC",
                     "WSH_20_WAT",
                     "SDGWSHBOD",
                     "WHOSIS_000001",
                     "SE.ENR.PRIM.FM.ZS",
                     "SE.SEC.UNER.LO.FE.ZS",
                     "WS_TIME_P",
                     "WS_PCDW_P",
                     "DV_EXPV_W")

# Name of the indicators
indicator_names <- c("% Population using safely managed water",
                     "% Population using basic water",
                     "Diarrhoea disease attributable to inadequate water",
                     "Mortality rate attributed to unsafe water or sanitation",
                     "Life expectancy at birth",
                     "Gender parity index, primary school enrollment",
                     "% female adolescent out of secondary school",
                     "Time to obtain water",
                     "Person to obtain water",
                     "% Women experience physical violence")

# Create indicator dataframe with codes and names
indicator_df <- data.frame(indicator_codes, indicator_names)

############################
# Countries used for this analysis
countries_used <- c(
  # Developed Economies (3)
  "Germany", "Japan", "United States",
  # Sub-Saharan Africa (4)
  "Ethiopia", "Tanzania", "Sierra Leone", "Chad",
  # Asia (4)
  "India", "Bangladesh", "Nepal", "Cambodia",
  # Latin America & MENA (4)
  "Peru", "Guatemala", "Morocco", "Tunisia"
)

# Create country map
regions_map <- c(
  rep ("Developed Economies", times = 3),
  rep ("Sub-Saharan Africa", times = 4),
  rep ("Asia", times = 4),
  rep ("Latam & MENA", times = 4)
)

# Find country codes
country_codes_iso3c <- countrycode(countries_used, origin = "country.name", destination = "iso3c")
country_codes_iso2c <- countrycode(countries_used, origin = "country.name", destination = "iso2c")

# Create a country dictionary with country name, region name, and country codes
countries_df <- data.frame(countries_used, regions_map, country_codes_iso2c, country_codes_iso3c)

############################
# Data wrangling
tab1_data_df <- data.frame() # Initialize an empty dataframe for tab1: Health Impact
tab2_data_df <- data.frame() # Initialize an empty dataframe for tab2: Hidden Burden
who_data <- list() # Initialize an empty list for WHO data
wdi_data <- list() # Initialize an empty list for World Bank data
dhs_data <- list() # Initialize an empty list for DHS survey data

############################
# Processing WHO Data
for (i in 1:5){
  # Pull in the data file
  filename <- paste0("raw/",indicator_df[i, 1],".csv")
  df_raw <- read.csv(filename)
  
  # Process data: 
  # First branch: If the extra dimension is residence area
  if (str_detect(df_raw$Dim1Type[1], "RESIDENCE")){
    # Filter the countries in our target list, select the target field
    # Rename Dim1 to ResidenceArea and change it into a factor variable later
    # Add a few columns about the data, such as indicator name, source, and unit
    df_processed <- df_raw %>% 
      filter(Country %in% country_codes_iso3c) %>% 
      select(Country, Year, Dim1, NumericValue) %>% 
      rename(
        Country_code = Country,
        Residence_Area = Dim1,
        Value = NumericValue
      ) %>% 
      mutate(
        IndicatorName = as.character(indicator_df[i,2]),
        Unit = "% of Population",
        Source = "WHO"
      )
    
    df_processed$Residence_Area <- factor(df_processed$Residence_Area, 
                                          levels = c("RESIDENCEAREATYPE_TOTL",
                                                     "RESIDENCEAREATYPE_RUR",
                                                     "RESIDENCEAREATYPE_URB"),
                                          labels = c("Total", "Rural", "Urban"))
    # Branch 2: If the extra dimension is sex
  } else if (str_detect(df_raw$Dim1Type[1], "SEX")){
    # Filter the countries in our target list, select the target field
    # Rename Dim1 to ResidenceArea and change it into a factor variable later
    # Add a few columns about the data, such as indicator name, source, and unit
    df_processed <- df_raw %>% 
      filter(Country %in% country_codes_iso3c) %>% 
      select(Country, Year, Dim1, NumericValue) %>% 
      rename(
        Country_code = Country,
        Gender = Dim1,
        Value = NumericValue
      ) %>% 
      mutate(
        IndicatorName = as.character(indicator_df[i,2]),
        Unit = case_when(
          IndicatorName == "Life expectancy at birth" ~ "Years", 
          IndicatorName == "Mortality rate attributed to unsafe water or sanitation" ~ "Per 100k Population",
          TRUE ~ "% of Population"),
        Source = "WHO"
      )
    
    df_processed$Gender <- factor(df_processed$Gender, 
                                  levels = c("SEX_BTSX",
                                             "SEX_MLE",
                                             "SEX_FMLE"),
                                  labels = c("All", "Male", "Female"))
  }
  # Add region info and country name
  df_processed <- left_join(df_processed, countries_df, by = c("Country_code" = "country_codes_iso3c"))
  # Drop the Country_code column
  df_processed <- df_processed %>% select(-Country_code, -country_codes_iso2c)
  # Append df_processed to who_data
  who_data[[i]] = df_processed
}

############################
# Processing World Bank Data
for (i in 6:7){
  # Pull in the data file
  filename <- paste0("raw/",indicator_df[i, 1],".csv")
  df_raw <- read.csv(filename)
  
  # Process data: 
  # Filter the countries in our target list, select the target field
  # Rename Dim1 to ResidenceArea and change it into a factor variable later
  # Add a few columns about the data, such as indicator name, source, and unit
  df_processed <- df_raw %>% 
    filter(iso3c %in% country_codes_iso3c) %>% 
    select(iso3c, year, indicator_df[i,1]) %>% 
    rename(
      Country_code = iso3c,
      Year = year,
      Value = indicator_df[i,1]
    ) %>% 
    mutate(
      IndicatorName = indicator_df[i,2],
      Unit = case_when(
        IndicatorName == "% female children out of school (primary)" ~ "% of Population", 
        TRUE ~ "Ratio (Girls to Boys)"),
      Source = "World Bank"
    )
  # Add region info and country name
  df_processed <- left_join(df_processed, countries_df, by = c("Country_code" = "country_codes_iso3c"))
  # Drop the Country_code column
  df_processed <- df_processed %>% select(-Country_code, -country_codes_iso2c)
  # Append df_processed to wdi_data
  wdi_data[[i]] = df_processed
}

############################
# Processing DHS Data
for (i in 8:10){
  # Pull in the data file
  filename <- paste0("raw/",indicator_df[i, 1],".csv")
  df_raw <- read.csv(filename)
  
  # Process data: 
  # Filter the countries in our target list, select the target field
  # Rename Dim1 to ResidenceArea and change it into a factor variable later
  # Add a few columns about the data, such as indicator name, source, and unit
  df_processed <- df_raw %>% 
    filter(DHS_CountryCode %in% country_codes_iso2c) %>% 
    select(DHS_CountryCode, SurveyYear, Value, Indicator) %>% 
    rename(
      Country_code = DHS_CountryCode,
      Year = SurveyYear
    ) %>% 
    mutate(
      IndicatorName = indicator_df[i,2],
      Unit = "% of Population",
      Source = "DHS Program"
    )
  # Add region info and country name
  df_processed <- left_join(df_processed, countries_df, by = c("Country_code" = "country_codes_iso2c"))
  # Drop the Country_code column
  df_processed <- df_processed %>% select(-Country_code, -country_codes_iso3c)
  # Append df_processed to dhs_data
  dhs_data[[i]] = df_processed
}

############################
# Create Master Dataframes for Shiny
# Tab 1: Health Impact contains all the WHO data
for (i in 1:5){
  tab1_data_df <- bind_rows(tab1_data_df, who_data[[i]])
}
write.csv(tab1_data_df, "processed/tab1_data.csv")

# Tab 2: Hidden Impact contains WHO data % population using basic water and safely managed water, WDI data, and DHS data
for (i in 1:2){
  tab2_data_df <- bind_rows(tab2_data_df, who_data[[i]])
}
for (i in 6:7){
  tab2_data_df <- bind_rows(tab2_data_df, wdi_data[[i]])
}
for (i in 8:10){
  tab2_data_df <- bind_rows(tab2_data_df, dhs_data[[i]])
}
write.csv(tab2_data_df, "processed/tab2_data.csv")
