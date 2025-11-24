#library(shiny)
library(ggthemes)
library(maps)
library(countrycode)

# Load country list from world map
world_map_tab3 <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>% # Get the base world map data (an sf object with country boundaries)
  dplyr::select(iso_a3, name, geometry)

# Merge with % population with safely managed water
safe_water_total <- water_basic_cleaned %>%  
  mutate(Country = countrycode(iso_a3, origin = 'iso3c', destination = 'country.name'))

safe_water_latest <- safe_water_total %>% group_by(Country) %>% 
  filter(Year == max(Year)) %>% 
  ungroup()

countries_water <- world_map_tab3 %>% 
  left_join(safe_water_latest, by = "iso_a3") %>%
  st_as_sf() # Convert back to sf object after join

# Set up the UI
explore_countryUI <- function(id, title){
  tabPanel(
    title,
    div(class = "plot-container", leafletOutput(NS(id,"water_access_map"), height = 900)),
    absolutePanel(
      wellPanel(
        selectInput(NS(id, "country_selected"),
        "Select a country to view data",
        choices = c(sort(unique(safe_water_total$Country))),
        selected = "Tanzania"
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
    
    # Color palette & constants
    pal_map <- colorNumeric(palette = "YlGnBu", domain = c(0, 100))
    na_color <- "#CCCCCC"
    
    
    
    # Create the base map initially
    output$water_access_map <- renderLeaflet({
      
      # Define popups/labels
      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %s",
        countries_water$name, 
        "% Population with Basic Water Access",
        ifelse(is.na(countries_water$PercentWaterAccess), "No Data", paste0(round(countries_water$PercentWaterAccess, 1), "%%"))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(countries_water) %>%
        setView(lng = 0, lat = 30, zoom = 2) %>% # Center the map
        addProviderTiles(providers$CartoDB.Positron) %>% # Clean, simple base map
        addPolygons(
          fillColor = ~ifelse(is.na(PercentWaterAccess), na_color, pal_map(PercentWaterAccess)),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 3,
            color = "#e65100", # Highlight color (Deep Orange)
            dashArray = "",
            fillOpacity = 1,
            bringToFront = TRUE),
          layerId = ~iso_a3,           # <-- enables click -> country name
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "bold", padding = "5px 10px"),
            textsize = "14px",
            direction = "auto")
        ) %>%
        
        # Add legend for the percentage scale
        addLegend(pal = pal_map, values = c(0, 100), opacity = 0.7, title = paste0("% Population with Basic Water"),
                  position = "bottomright") %>%
        
        # Add a custom legend entry for 'No Data'
        addLegend(colors = na_color, labels = "No Data", opacity = 0.7, 
                  position = "bottomright", title = "")
    })
    
    # Observer to update country selected
    observe({
        # Get the details of the clicked shape
        click <- input$water_access_map_shape_click
        
        # Check if a shape was actually clicked (not just a background click)
        if (is.null(click$id))
          return()
        
        country_name <- countrycode(sourcevar = click$id, origin = "iso3c", destination = "country.name")
        
        # Update the selectInput's selected value to match the country clicked
        updateSelectInput(session, "country_selected", selected = country_name)
        
      }) %>% 
      bindEvent(input$water_access_map_shape_click)
    
    
    # Observer to open modal
    observe({
      showModal(
        modalDialog(
          title = paste0("Reviewing: Data from ", input$country_selected),
          div(
            style = "max-height = 500px; overflow-y: auto; padding: 15px;",
            h4("% Population with Basic Water"),
            plotlyOutput(session$ns("water_access_over_years")),
            br(),
            
            h4("Time to Obtain Water"),
            plotlyOutput(session$ns("time_to_obtain_water_over_years")),
            br(),
            
            h4("Key Public Health Statistics from 2019"),
            DT::DTOutput(session$ns("countrydata"))
          ),
          easy_close = TRUE,
          size = "xl"
        )
      )
    }) %>%
      bindEvent(input$country_selected, ignoreInit = TRUE)
    
    # Chart 1: By country % basic water access over time
    output$water_access_over_years <- renderPlotly({
      req(input$country_selected)
      base <- safe_water_total %>%
        filter (Country == input$country_selected) %>%
        ggplot(aes(x = factor(Year), y = PercentWaterAccess, group = 1,
                   text = paste0("Year: ", as.integer(Year), "<br>",
                                 "% Population with Basic Water: ", round(PercentWaterAccess,1)))) +
        geom_line(color = 'dodgerblue3', linewidth = 1) +
        geom_point(color = 'dodgerblue3', size = 1.5) +
        labs(x = "Year", y = "% Population with Basic Water Access") +
        theme_classic() +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              legend.title = element_blank(),
              legend.position = "top") 
      ggplotly(base, tooltip = "text") 
    })
    
    # Chart 2: By country time to obtain water shift over time
    output$time_to_obtain_water_over_years <- renderPlotly({
      req(input$country_selected)
      base <- time_obtain_water %>%
        filter (Country == input$country_selected)
      
      if (nrow(base) == 0) {
        p <- ggplot() + 
          geom_text(aes(x=0, y=0, label="No data available."), 
                    size=6) + 
          theme_void() +
          theme(text = element_text(size = 10, family = "Roboto"))
        
        return(ggplotly(p))
      }
      
      base <- base %>% rename (Percent_Population = Value) %>% 
        ggplot(aes(x = factor(Year), y = Percent_Population, fill = Indicator,
                   text = paste0("Year: ", as.integer(Year), "<br>",
                                 "Time to Obtain Water: ", Indicator, "<br>",
                                 "% Population: ", round(Percent_Population,1)) )) +
        geom_col(position = "stack") +
        labs(x = "Year", y = "% Population") +
        theme_classic() +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              legend.title = element_blank(),
              legend.position = "top") 
      ggplotly(base, tooltip = "text") 
    })
  
  
    # DataTable 3: By country % access to basic water, diarrhoea disease attributable to unsafe water, and mortality rate due to unsafe water
    output$countrydata <- DT::renderDT({
      req(input$country_selected)
      base <- country_data_df %>%
        filter (Country == input$country_selected, 
                Year == 2019,
                Residence_Area == "Total"| is.na(Residence_Area),
                Gender == "All" | is.na(Gender)) %>% 
        mutate(across(where(is.numeric), round, 2)) %>% 
        select(IndicatorName, Value, Unit) 
    })
  })
}