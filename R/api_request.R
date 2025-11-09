#################################################
### This script is used to explore indicators ###
### available in various databases. Another   ###
### script is used to download and process    ###
### the data selected.                        ###
#################################################

# Set working directory
setwd("~/Personal/Brown/BHDS2010/Assignment4_ShinyApp")

# 1. Explore indicator list from WHO GHO database
library(jsonlite)
library (tidyverse)

# Connect to database API
gho_base <- "https://ghoapi.azureedge.net/api"

# List of Key words to search for
key_terms <- c("safely managed",
               "basic drinking",
               "diarrhoea", 
               "intimate partner violence",
               "life expectancy",
               "by cause",
               "WASH services")

# Start an empty list to store the indicator search results
indicators_list <- list()

# For each element in key terms, create its search URL. If any results return, add the Indicator Code and Indicator Name to the big list
for (i in 1 : length(key_terms)){
  url <- paste0(gho_base, "/Indicator?",
                "$select=IndicatorCode,IndicatorName&",
                "$filter=",
                URLencode(sprintf("contains(tolower(IndicatorName), '%s')",
                                  tolower(key_terms[i])), reserved = TRUE))
  
  all_indicators <- fromJSON(url)$value 
  
  if (!is.null(all_indicators) && length(all_indicators) > 0) {
    indicators_list[[length(indicators_list) + 1]] <- all_indicators  %>% 
      as_tibble() %>% 
      select(IndicatorCode, IndicatorName) 
  }
  
}

# Print the indicator list for selection
indicators_list

# 2. Explore indicator list from World Bank WDI database
library(WDI)

# Search for key words, return a list of indicator code and names
WDIsearch("life expectancy")


# 3. Explore indicator list from DHS database
library(jsonlite)
library (tidyverse)

# Connect to database API
dhs_base <- "https://api.dhsprogram.com/rest/dhs"

# List of Key words to search for
key_terms <- c("obtain drinking",
               "collecting water")

# For the DHS database, first return the big list of all indicators, then filter by key words
# Build the URL
url <- paste0(dhs_base, "/indicators?",
              "returnFields=IndicatorId,Label,Definition,Level2&",
              "perPage=5000&page=1")

# Retrieve the full list of indicators
json_page  <- fromJSON(url)

# Convert the data to a dataframe
all_indicators <- as_tibble(json_page$Data) 

# Start an empty list to store the indicator search results
indicators_list <- list()

# Loop through the key words, if we find any results in the definition or level 2 description, add them to the big indicator list
for (i in 1:length(key_terms)) {
    hit_indicators  <- all_indicators %>% 
      filter(str_detect(Level2, key_terms[i]) | str_detect(Definition, key_terms[i]))
    if (nrow(hit_indicators) > 0) {
      indicators_list[[length(indicators_list) + 1]] <- hit_indicators
    }
}

# Print the indicator list for selection
indicators_list

