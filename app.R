library(shiny)
library(bslib)
library(rsconnect)
library(paletteer)
library(thematic)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Water Insecurity and Public Health",
    
  theme = bs_theme(bootswatch = "yeti"),
  
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

  hidden_burdenUI("tab2", "Hidden Burden")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  hidden_burdenServer("tab2") 
  # By country shift over time
  # output$time_to_obtain_water_over_years <- renderPlot(
  #   time_obtain_water %>% 
  #   filter (Country == "Tanzania") %>% 
  #   ggplot(aes(x = Year, y = Value, fill = Indicator)) + 
  #   geom_col(position = "stack") 
  # )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
