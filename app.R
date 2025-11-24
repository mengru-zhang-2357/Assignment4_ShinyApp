library(shiny)
library(bslib)
library(rsconnect)
library(paletteer)
library(thematic)

# In this app, we explore the impact of water insecurity on life expectancy, disease burden, and gender inequality. All the data used and papers quoted are listed on the Bibliography tab.
# This is the main app which calls the tab UI and Servers. The app uses a navbarPage layout, with the sandstone theme.

# Define UI for the application
ui <- navbarPage(
  # Main title of the app
  title = "Water Insecurity and Public Health",
  
  # Use bootswatch sandstone theme. I have downloaded the style CSS file from the bootwatch website and saved in the www folder.
  theme = bs_theme(bootswatch = "sandstone"),
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
  # The four tabs in the order they appear on the app
  health_impactUI("tab1", "Health Impact"),
  hidden_burdenUI("tab2", "Hidden Burden"),
  explore_countryUI("tab3", "Explore Country Data"),
  bibUI("tab4", "Bibliography")
)

# Define server logic required to render the UI above
server <- function(input, output, session) {
  # The server functions corresponding to the four tabs
  health_impactServer("tab1")
  hidden_burdenServer("tab2") 
  explore_countryServer("tab3")
  bibServer("tab4")
}

# Run the application 
shinyApp(ui = ui, server = server)
