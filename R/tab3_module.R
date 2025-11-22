#library(shiny)
library(ggthemes)
library(maps)
library(countrycode)

# Load country list from world map
world_map <- map_data("world") %>% 
  filter (long <= 180, region != "Antarctica")    # removes weird line due to Russia and US crossing 180 longitude line, removes Antarctica
  

countries <- world_map %>% distinct(region) %>% 
  mutate(iso3c = countrycode(region, origin = 'country.name', destination = 'iso3c'))

# Merge with % population with safely managed water
safe_water_total <- safe_water %>% 
  filter(Year == max(Year), Residence_Area == "Total") %>% 
  mutate(iso3c = countrycode(Country, origin = 'country.name', destination = 'iso3c'))

countries_water <- countries %>% 
  left_join(safe_water_total, by = "iso3c") 

# Set up the UI
explore_countryUI <- function(id, title){
  tabPanel(
    title,
    plotOutput(NS(id, "countryMap"), height = "90vh"),
    absolutePanel(
      wellPanel(
        selectInput(NS(id, "country_selected"),
        "Select a country to view data",
        choices = c(unique(time_obtain_water$Country)),
        selected = "All"
        )
      ),
      width = "300px", 
      left = "50px", 
      top = "75px", 
      draggable = TRUE,
    )
  )
}

explore_countryServer <- function(id){
  moduleServer(id, function(input, output, session){
    thematic::thematic_shiny()
    
    countries_water_selected <- reactive(countries_water %>% 
                                           mutate(Selected = (Country == input$country_selected)) %>% 
                                           mutate(Selected = replace_na(Selected, FALSE)))
    
    output$countryMap <- renderPlot({
      countries_water_selected() %>% 
        ggplot(aes(fill = Selected, map_id = region)) +
        geom_map (map = world_map) +
        expand_limits(x = world_map$long, y = world_map$lat) +
        scale_fill_paletteer_d("PrettyCols::Autumn") +
        theme_map(base_family = "Roboto") +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              legend.position = c(0, 90))
      
    }) 
    
    # Card 1: By country time to obtain water shift over time
    output$time_to_obtain_water_over_years <- renderPlotly({
      req(input$country_selected)
      base<- time_obtain_water %>%
        filter (Country == input$country_selected) %>%
        ggplot(aes(x = Year, y = Value, fill = Indicator)) +
        geom_col(position = "stack") +
        theme_classic() +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              legend.title = element_blank(),
              legend.position = "top") 
      ggplotly(base) 
      })
    
    observe({
      showModal(
        modalDialog(
          title = "Time to Obtain Water",
          easy_close = TRUE,
          size = "xl",
          plotlyOutput(NS(id, "time_to_obtain_water_over_years"))
        )
      )
    }) %>%
      bindEvent(input$country_selected, ignoreInit = TRUE)
    
  })
}