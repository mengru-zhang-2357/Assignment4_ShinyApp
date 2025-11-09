#################################################
### This script is used to explore indicators ###
### available in various databases. Another   ###
### script is used to download and process    ###
### the data selected.                        ###
#################################################

# Set working directory
setwd("~/Personal/Brown/BHDS2010/Assignment4_ShinyApp")

# 1. Explore indicator list from WHO GHO database
library(glue)
library(httr)
library(jsonlite)
library (tidyverse)

# Connect to database API
gho_base <- "https://ghoapi.azureedge.net/api"

key_terms <- c("safely managed",
               "basic drinking",
               "diarrhoeal diseases", 
               "intimate partner violence",
               "life expectancy",
               "Child deaths",
               "causes of deaths",
               "WASH services")

indicators_list <- list()
for (i in 1 : length(key_terms)){
  url <- paste0(gho_base, "/Indicator?",
                "$select=IndicatorCode,IndicatorName&",
                "$filter=",
                URLencode(sprintf("contains(tolower(IndicatorName), '%s')",
                                  tolower(key_terms[i])), reserved = TRUE))
  
  all_indicators <- fromJSON(url)$value 
  
  if (!is.null(all_indicators) && length(all_indicators) > 0) {
    indicators_list[[length(indicators_list)+1]] <- all_indicators  %>% 
      as_tibble() %>% 
      select(IndicatorCode, IndicatorName) 
  }
  
}

indicators_list

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

