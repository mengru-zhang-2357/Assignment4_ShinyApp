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
                     "WHOSIS_000001",
                     "SDGWSHBOD",
                     "MORT_100",
                     "RHR_IPV",
                     "WSH_10_WAT",
                     "SE.ENR.PRIM.FM.ZS",
                     "SE.PRM.UNER.FE.ZS",
                     "WS_TIME_P",
                     "WS_PCDW_P")

indicator_names <- c("% Population using safely managed water",
                     "% Population using basic water",
                     "Diarrhoea attributable to inadequate water",
                     "Life expectancy at birth",
                     "Mortality rate attributed to unsafe water or sanitation",
                     "Number of diarrhoea deaths from inadequate water",
                     "Number of deaths in children aged <5 years, by cause",
                     "Intimate partner violence prevalence",
                     "Gender parity index, primary school enrollment",
                     "% female children out of school (primary)",
                     "Time to obtain water",
                     "Person to obtain water")

indicator_df = data.frame(indicator_codes, indicator_names)

# Countries used for this analysis
countries_used <- c(
  # Developed Economies (3)
  "Germany", "Japan", "United States",
  # Sub-Saharan Africa (4)
  "Ethiopia", "Tanzania", "Uganda", "Democratic Republic of the Congo",
  # Asia (4)
  "India", "Bangladesh", "Nepal", "Cambodia",
  # Latin America & MENA (4)
  "Peru", "Guatemala", "Morocco", "Tunisia"
)

regions_map <- c(
  rep ("Developed Economies", times = 3),
  rep ("Sub-Saharan Africa", times = 4),
  rep ("Asia", times = 4),
  rep ("Latam & MENA", times = 4)
)

country_codes <- countrycode(countries_used, origin = "country.name", destination = "iso3c")


