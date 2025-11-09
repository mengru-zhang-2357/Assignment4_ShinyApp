#################################################
### This script is used to download data      ###
### available in various databases. This is   ###
### used after the indicators have been       ###
### identified.                               ###
#################################################

# Set working directory
setwd("~/Personal/Brown/BHDS2010/Assignment4_ShinyApp")

# 1. Download data from WHO GHO database
library(jsonlite)
library (tidyverse)
library ()

# Connect to database API
gho_base <- "https://ghoapi.azureedge.net/api"

# List of Indicators we are interested in
indicator_codes <- c("WSH_WATER_SAFELY_MANAGED",
                     "WSH_WATER_BASIC",
                     "WSH_20_WAT",
                     "NLIS_NU_CA_041",
                     "WHOSIS_000001",
                     "SDGWSHBOD",
                     "WSH_10_WAT",
                     "MORT_100",
                     "WHS2_167",
                     "RHR_IPV")


# Create an empty list to write to
data_list <- list()

# Download data
for (i in 1 : length(indicator_codes)){
  # Create URL for each indicator
  url <- paste0(url <- paste0(gho_base, "/", URLencode(indicator_codes[i], reserved = TRUE)))
  # Retrieve data
  all_data <- fromJSON(url)$value 
  # If data is available, write to the big data list
  if (!is.null(all_data) && length(all_data) > 0) {
    data_list[[length(data_list) + 1]] <- all_data  %>% 
      as_tibble() %>% 
      select(any_of(c(
        "IndicatorCode",
        "SpatialDim",
        "TimeDim",
        "NumericValue"
      ))) %>% 
      rename(
        Country = SpatialDim,
        Year    = TimeDim,
        Value  = NumericValue
      )
  }
}

# Export to csv
# Set up a function so we can loop through the data list
export_data <- function (df, df_name){
  path = "~/Personal/Brown/BHDS2010/Assignment4_ShinyApp/data/"
  filename = paste0(path, df_name,".csv")
  write.csv (df, filename)
}

# Use lapply() to export the data
lapply(data_list, export_data(df_name = names(data_list)))

# 2. Explore indicator list from World Bank WDI database
library(WDI)

WDIsearch("life expectancy")
WDIsearch("School enrollment")
WDIsearch("out of school")

# 3. Explore indicator list from DHS database
dhs_base <- "https://api.dhsprogram.com/rest/dhs"

key_terms <- c("obtain drinking",
               "collecting water")

url <- paste0(dhs_base, "/indicators?",
              "returnFields=IndicatorId,Label,Definition,Level2&",
              "perPage=10000&page=1")

json_page  <- fromJSON(url)           # DHS returns paged JSON
all_indicators <- as_tibble(json_page$Data) 

indicators_list <- list()
for (i in 1:length(key_terms)) {
  hit_indicators  <- all_indicators %>% 
    filter(str_detect(Level2, key_terms[i]) | str_detect(Definition, key_terms[i]))
  if (nrow(hit_indicators) > 0) {
    indicators_list[[length(indicators_list)+1]] <- hit_indicators
  }
}

indicators_list

