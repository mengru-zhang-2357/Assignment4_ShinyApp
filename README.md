# Assignment 4: Shiny App - Water Insecurity
In this project, team members worked together using R and Github to develop a Shiny App which explores the intersection between water insecurity and public health, focusing on two themes: (1) the health impact of limited access to safe drinking water, and (2) the hidden gender burden, as women and girls often bear the responsibility for water collection. 

## Repository Contents
- `README.md` - Documentation and collaboration details 
- `data/` - Raw and processed data
- `R/` - R codes for modules  
- `app.R` - Code for analysis including visualization and summary statistics
- `Helpers/` - Data download and processing codes
- `www/` - style CSS file
- `rsconnect/` - shinyapps.io deployment

## Description of Data
-	**WHO Global Health Observatory:** Includes indicators on access to basic and safely managed drinking water, mortality from unsafe water and poor sanitation, diarrheal disease burden, and life expectancy.
-	**World Bank Databank:** Provides education-related indicators such as the percentage of adolescent girls and boys out of secondary school, highlighting gender disparities linked to water access.
-	**Demographic and Health Surveys (DHS) Program:** Contains household-level data on time to obtain drinking water, the primary person responsible for water collection, and women’s experiences of physical violence.

## Setup and Required Packages
To reproduce the analysis, install and load the following R package:

```r
library(shiny)       # for shiny
library(rsconnect)   # for deployment of the app online
library(tidyverse)   # for dplyr, stringr, and ggplot2 functions
library(leaflet)     # for leaflet map
library(sf)          # for handling spatial data
library(maps)        # for world map data
library(countrycode) # for mapping countries from ISO codes to country names
library(rnaturalearth) # for world map data
library(htmltools)   # Used for label formatting
library(viridis)     # Added for color palettes in ggplot
library(bslib)       # for modern Shiny UI toolkit
library(plotly)      # for creation of plotly charts
library(shinyWidgets) # for pretty switch for regression line
library(ggthemes)    # for plot themes
library(paletteer)   # for plot themes
library(thematic)    # To carry over the Shiny app theme into the server

```

## Key Features and User Interaction
### Description of Inputs
Each tab in the app includes user inputs that filter, animate, and customize the visualizations.
### Health Impact Tab
-	Water Access Indicator: A selectInput that lets users choose between basic and safely managed drinking water.
-	Year: A slider that allows users to select a specific year. An animation button automatically cycles through years, dynamically updating both the world map and the correlation plot.
### Hidden Burden Tab
-	Region: A selectInput to select a region or view all regions. When “All” is selected, the x-axis displays regions; otherwise, it shows countries within the chosen region.
-	Year Range: A range slider to set the period for exploration, used instead of a single-year slider due to uneven data availability.
-	Color Palette: A selectInput that allows users to change the color scheme of all plots on this tab.
-	Additional panel-specific inputs include:
   -	Show Data: A button that opens a modal displaying the data table behind the selected chart.
   -	Color Group Variable: Lets users color scatter plots by region or country.
   -	Show Regression Line: A toggle switch that overlays a linear regression line on scatter plots when enabled.
### Explore Country Data Tab
-	Select a Country: A selectInput for choosing a country to view its historical water access and related indicators. Changing the selection triggers a modal with plots and data tables.
-	Clickable Map: An interactive leaflet map where clicking a country automatically updates the country selection and opens the same modal.

### Description of outputs
Each tab produces dynamic visualizations and data tables that respond to user selections in real time.
### Health Impact Tab
-	Global Water Access Map: A leaflet output showing the percentage of the population with basic or safely managed water, colored by value and annotated with tooltips.
-	Correlation Plot: A scatter plot comparing water access and life expectancy, optionally including a regression line.
-	Trend Over Time: A time-series plot illustrating global progress in water access since 2000.
### Hidden Burden Tab
-	Time to Obtain Water: A stacked bar chart displaying the share of the population with onsite, <30-minute, or >30-minute access.
-	Person Responsible for Water Collection: A grouped bar chart comparing gender and age categories among water collectors.
-	Education Impact: A line chart showing gender disparities in secondary school enrollment in relation to water access.
-	Violence and Water Access: A scatter plot examining the relationship between women’s experience of physical violence and access to basic water.
-	Each chart includes a corresponding modal data table, accessible via the “Show Data” button.
### Explore Country Data Tab
-	Country Profile Charts: When a country is selected or clicked on the map, a modal displays:
   -	Line chart of water access over time.
   -	Stacked bar chart of time to obtain water.
   -	Summary table of key indicators (water access, disease burden, life expectancy).

All outputs are rendered using ggplot2, plotly, and DT, enabling zoom, hover, and filtering capabilities for intuitive exploration.

## Server Logic
The server logic connects user inputs with the underlying datasets and reactive visualizations. 
### Health Impact Tab
-	Data Preparation: Reactive expressions filter and merge WHO indicators based on user-selected year and water access type.
-	Scatter and Line Charts: renderPlot functions generate ggplot visualizations that automatically adjust to the selected year. If no overlapping data exists, a message “No data available for this year” appears instead of a blank plot.
-	Map Rendering: The leaflet output dynamically updates when inputs change, recoloring polygons and refreshing legends accordingly.
### Hidden Burden Tab
-	Reactive Filtering: Each panel filters DHS or World Bank data by region and year range, precomputing mean values for aggregation.
-	Plot Creation: Charts are built using ggplot2 and converted to interactive format via ggplotly(). 
-	Modal Tables: renderDT creates detailed data tables that appear only when users click “Show Data.” Observers track these clicks and trigger the modal display.
### Explore Country Data Tab
-	Map Interaction: A leaflet map displays countries with clickable polygons linked to country ISO codes and names. When clicked, an observer updates the selected country input.
-	Modal Generation: Observers open a modal containing three outputs, two charts and one table, filtered by the selected country.

## Examples of Insights
- The animated global map demonstrates steady improvements in access to safe drinking water over the past two decades, yet significant disparities persist. Sub-Saharan Africa remains the most affected region, with many countries showing less than 50% access to safely managed water.
-	The scatter plots show a clear positive association between safe water access and life expectancy, highlighting how clean water is linked to improved population health outcomes.
-	In Sub-Saharan Africa, over two-thirds of households rely on women or girls to collect water. In many countries, they spend more than 30 minutes per trip, reducing time for education and work while increasing risks of injury and gender-based violence.
-	Countries with higher access to safe water also show lower rates of adolescent girls out of school, suggesting that improving water infrastructure can have cascading benefits for the social determinants of health including education status.
   
## Collaboration and Roles
This project was completed collaboratively using GitHub branching and pull requests.
| Team Member | Role |Key Contributions |
|------------------|------|-------------------|
| **Mengru&nbsp;Zhang** | Tab 2, Tab 3 Modal, Documentation, Final report | Created Tab 2, Modal in Tab 3, wrote README, drafted final report |
| **Chris Xu** | Tab 1, Tab 3 Map | Created Tab 1, Map in Tab 3 |

## Reproducibility
To reproduce this analysis:
1. Open RStudio. Go to File → New Project → Version Control → Git
2. Paste the repo URL:  
   `https://github.com/mengru-zhang-2357/Assignment4_ShinyApp.git`
3. Click "Create Project".
4. Run the `app.R` script in RStudio using the Run App button.
5. Review the app deployed in browser.
