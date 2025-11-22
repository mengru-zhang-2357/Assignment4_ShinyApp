library(shiny)
library(bslib)
library(rsconnect)
library(paletteer)
library(thematic)

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Water Insecurity and Public Health",
      
    theme = bs_theme(bootswatch = "sandstone"),
    
    header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
    hidden_burdenUI("tab2", "Hidden Burden"),
    
    explore_countryUI("tab3", "Explore Country Data"),
    
    bibUI("tab4", "Bibliography")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  hidden_burdenServer("tab2") 
  
  explore_countryServer("tab3")
  
  bibServer("tab4")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
