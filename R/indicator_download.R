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

# File path to write data
path = "~/Personal/Brown/BHDS2010/Assignment4_ShinyApp/data/"

# Connect to database API
gho_base <- "https://ghoapi.azureedge.net/api"

# List of Indicators we are interested in
indicator_codes <- c("WSH_WATER_SAFELY_MANAGED",
                     "WSH_WATER_BASIC",
                     "WSH_10_WAT",
                     "WSH_20_WAT",
                     "SDGWSHBOD",
                     "WHOSIS_000001"
                     )

# Create a concatenate of indicator codes, separated by "or"
url_codes <- paste0(paste0("IndicatorCode eq '", indicator_codes, "'"), collapse = " or ")
# Create the URL to retrieve Indicator names
url  <- paste0(
  gho_base,
  "/Indicator?$select=IndicatorCode,IndicatorName&$filter=",
  URLencode(url_codes, reserved = TRUE)
)
# Retrieve indicator names
GHO_indicator_names <- fromJSON(url)$value %>% as_tibble()

# Now download the data
# Loop through each Indicator Code, if there's data, save to CSV
for (i in 1 : length(indicator_codes)){
  code = indicator_codes[i]
  # Create URL for each indicator
  url <- paste0(url <- paste0(gho_base, "/", URLencode(code, reserved = TRUE)))
  # Retrieve data
  all_data <- fromJSON(url)$value 
  # If data is available, write to the big data list
  if (!is.null(all_data) && length(all_data) > 0) {
    filename <- paste0(code, ".csv")
    data_to_write <- all_data  %>% 
      as_tibble() %>% 
      rename(
        Country = SpatialDim,
        Year    = TimeDim
      )
    write.csv(data_to_write, paste0(path, filename))
  }
}

# 2. Explore indicator list from World Bank WDI database
library(WDI)

# File path to write data
path = "~/Personal/Brown/BHDS2010/Assignment4_ShinyApp/data/"

# List of Indicators we are interested in
indicator_codes <- c("SE.ENR.PRIM.FM.ZS",
                     "SE.PRM.UNER.FE.ZS")

# Retrieve indicator names
all_indicator_names <- WDIsearch(string = "", field = "indicator") 
WDI_indicator_names <- all_indicator_names %>% 
  filter(indicator %in% indicator_codes)

# Now download the data
# Loop through each Indicator Code, if there's data, save to CSV
for (i in 1 : length(indicator_codes)){
  code = indicator_codes [i]
  wdi_data <- WDI(
    indicator = code,
    start = 2000,
    end = 2024,
    extra = TRUE
  )
  # If data is available, write to the big data list
  if (!is.null(wdi_data) && length(wdi_data) > 0) {
    filename <- paste0(code, ".csv")
    write.csv(wdi_data, paste0(path, filename))
  }
}

# 3. Explore indicator list from DHS database
library(jsonlite)
library (tidyverse)

# File path to write data
path = "~/Personal/Brown/BHDS2010/Assignment4_ShinyApp/data/"

# Connect to database API
dhs_base <- "https://api.dhsprogram.com/rest/dhs"

# Time to Obtain Water
url <- "https://api.dhsprogram.com/rest/dhs/data/WS_TIME_P_ONP,WS_TIME_P_L30,WS_TIME_P_M30,WS_TIME_P_DKM?perPage=10000&f=csv"
time_df <- read_csv(url)
write_csv(time_df, paste0(path, "WS_TIME_P.csv"))

# Person to collect water
url <- "https://api.dhsprogram.com/rest/dhs/data/WS_PCDW_P_NDW,WS_PCDW_P_AFM,WS_PCDW_P_AML,WS_PCDW_P_CFM,WS_PCDW_P_CML,WS_PCDW_P_NHH?f=html"
person_df <- read_csv(url)
write_csv(person_df, paste0(path, "WS_PCDW_P.csv"))

