library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(htmltools) # Used for label formatting
library(ggplot2) # Added for scatter plot
library(viridis) # Added for color palettes in ggplot

# --- Data Loading and Initial Preparation ---
# NOTE: Ensure all necessary CSV files are in the same directory as this app.R file.

# Load Water Access Data
tryCatch({
  basic_water_data <- read.csv("data/raw/WSH_WATER_BASIC.csv", stringsAsFactors = FALSE)
  safely_managed_data <- read.csv("data/raw/WSH_WATER_SAFELY_MANAGED.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  stop("Error loading water access CSVs. Please ensure they are in the same directory as app.R: ", e$message)
})

# Load Life Expectancy Data (WHOSIS_000001)
tryCatch({
  life_expectancy_data <- read.csv("data/raw/WHOSIS_000001.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  stop("Error loading WHOSIS_000001.csv. Please ensure the file is in the same directory as app.R: ", e$message)
})

# --- Data Cleaning Functions ---

# Function to clean water data
clean_water_data <- function(data, indicator_name) {
  data %>%
    filter(Dim1 == "RESIDENCEAREATYPE_TOTL") %>%
    select(Country, Year, NumericValue) %>%
    rename(iso_a3 = Country, PercentWaterAccess = NumericValue) %>%
    mutate(Year = as.integer(Year),
           PercentWaterAccess = as.numeric(PercentWaterAccess),
           Indicator = indicator_name) %>%
    filter(!is.na(PercentWaterAccess))
}

# Function to clean life expectancy data (Averages Male and Female values)
clean_life_expectancy_data <- function(data) {
  # 1. Select relevant columns and clean data types
  cleaned <- data %>%
    select(Country, Year, Dim1, NumericValue) %>%
    rename(iso_a3 = Country, Sex = Dim1, Value = NumericValue) %>%
    mutate(Year = as.integer(Year),
           Value = as.numeric(Value)) %>%
    filter(!is.na(Value))
  
  # 2. Filter for Male and Female data points
  sex_data <- cleaned %>%
    filter(Sex %in% c("SEX_MLE", "SEX_FMLE"))
  
  # 3. Calculate mean Life Expectancy (LifeExpectancy) for each country/year
  life_expectancy_combined <- sex_data %>%
    group_by(iso_a3, Year) %>%
    summarise(LifeExpectancy = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    filter(!is.na(LifeExpectancy))
  
  return(life_expectancy_combined)
}


# Clean datasets
water_basic_cleaned <- clean_water_data(basic_water_data, "Basic Water Access")
water_safely_managed_cleaned <- clean_water_data(safely_managed_data, "Safely Managed Drinking Water")
life_expectancy_cleaned <- clean_life_expectancy_data(life_expectancy_data)


# Combine datasets and get the full year range
all_water_data <- bind_rows(water_basic_cleaned, water_safely_managed_cleaned)

# Get the base world map data (an sf object with country boundaries)
world_map <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
  dplyr::select(iso_a3, name, geometry)

# Define year range for the slider (only using water and life expectancy data)
available_years <- sort(unique(c(all_water_data$Year, life_expectancy_cleaned$Year)))
min_year <- ifelse(length(available_years) > 0, min(available_years), 2000)
max_year <- ifelse(length(available_years) > 0, max(available_years), 2020)


# --- UI Definition ---
health_impactUI <- function(id, title){
  tabPanel(
    # --- Tab 1: World Map, Scatter Plot, and Trend Line ---
    title = "Health Impact",
    page_sidebar(
       sidebar = sidebar(
          p("Water insecurity reduces life expectancy as the lack of safe and reliable water increases disease, malnutrition, and poor sanitation, leading to higher mortality and weakened overall health."),
          
          # 1. Indicator Selection (Affects BOTH map and scatter plot)
          selectInput(NS(id,"selected_indicator"),
                      "Select Water Access Indicator:",
                      choices = c("Basic Water Access", "Safely Managed Drinking Water"),
                      selected = "Basic Water Access"
          ),
          
          # 2. Slider for year selection (Affects map and scatter plot)
          sliderInput(NS(id,"selected_year"),
                      "Select Year:",
                      min = min_year,
                      max = max_year,
                      value = (max_year + min_year) / 2,
                      step = 1,
                      sep = "",
                      animate = animationOptions(interval = 1000, loop = FALSE)
          ),
          
          # Footer information
          p(HTML("<hr>Water Access Data: WHO/UNICEF Joint Monitoring Programme (JMP). Filtered for total (TOTL) population estimates.")),
          p(HTML("Data Source: <a href='https://www.who.int/data/gho/indicator-metadata-registry/mreg-details/136' target='_blank' style='color: #1e88e5;'>WHO/UNICEF JMP</a>")),
       ),
       
       
       # Output 1: Leaflet map
       h4("Global Water Access Distribution"),
       div(class = "plot-container", leafletOutput(NS(id,"water_access_map"), height = 500)),
       
       br(),
       
       # Output 2: Scatter plot (Dynamic based on sidebar selection)
       h4(uiOutput(NS(id,"scatter_plot_title"))), # Dynamic title for the plot
       div(class = "plot-container", plotOutput(NS(id,"life_expectancy_scatter_plot"), height = 500)),
       
       br(),
       
       # Output 3: Global Time Series Trend (Contextual, static data)
       h4("Global Water Access Trend (2000 - Present)"),
       div(class = "plot-container", plotOutput(NS(id,"global_time_series_plot"), height = 500)),
       
     )
    )
}

# --- Server Logic ---
health_impactServer <- function(id){
  moduleServer(id, function(input, output, session) {
    
    thematic::thematic_shiny()
    # 1 Dynamic Title for the scatter plot
    output$scatter_plot_title <- renderUI({
      h4(paste("Correlation:", input$selected_indicator, "vs. Life Expectancy"))
    })
    
    # Reactive expression to get the merged data for the SCATTER PLOT
    scatter_data <- reactive({
      req(input$selected_year, input$selected_indicator)

      # Filter water data: Uses the selected indicator from the sidebar
      water_subset <- all_water_data %>%
        filter(Year == input$selected_year,
               Indicator == input$selected_indicator) %>% 
        select(iso_a3, PercentWaterAccess)
      
      # Filter life expectancy data for the selected year
      life_expectancy_subset <- life_expectancy_cleaned %>%
        filter(Year == input$selected_year) %>%
        select(iso_a3, LifeExpectancy)
      
      # Merge the data
      merged_data <- water_subset %>%
        inner_join(life_expectancy_subset, by = "iso_a3") 
      
      merged_data <- merged_data %>% 
        left_join(world_map %>% st_drop_geometry() %>% select(iso_a3, name), by = "iso_a3") %>%
        mutate(CountryName = ifelse(is.na(name), iso_a3, name)) %>%
        select(-name)
      
      return(merged_data)
    })
    
    # Reactive expression for Global Time Series Data
    global_time_series_data <- reactive({
      all_water_data %>%
        group_by(Year, Indicator) %>%
        summarise(GlobalAverage = mean(PercentWaterAccess, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(GlobalAverage))
    })
    
    # 2. Scatter Plot Output: Life Expectancy (Dynamic X-Axis)
    output$life_expectancy_scatter_plot <- renderPlot({
      plot_data <- scatter_data()
      water_label <- input$selected_indicator # Dynamic X-axis label
      
      if (nrow(plot_data) == 0) {
        return(ggplot() + 
                 geom_text(aes(x=0, y=0, label="No overlapping data available for this year."), 
                           size=6) + 
                 theme_void())
      }
      
      ggplot(plot_data, aes(x = PercentWaterAccess, y = LifeExpectancy)) + 
        geom_point(aes(color = LifeExpectancy, label = CountryName), size = 3, alpha = 0.8) + 
        geom_smooth(method = "lm", se = FALSE, color = "#0d47a1", linetype = "dashed") +
        scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
        scale_y_continuous(limits = c(40, 90), breaks = seq(40, 90, 10)) +
        scale_color_viridis_c(name = "Life Expectancy (Years)", option = "plasma", direction = 1) +
        labs(
          x = paste0(water_label, " (% Population Access)"), # Dynamic label
          y = "Life Expectancy (Years, Average of Male/Female)",
        ) +
        theme_classic() +
        theme(
          text = element_text(size = 12, family = "Roboto"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.position = "right",
          panel.grid.minor = element_blank(),
          axis.title = element_text(face = "bold")
        )
    })
    
    # 3. Global Time Series Line Plot
    output$global_time_series_plot <- renderPlot({
      plot_data <- global_time_series_data() # Get the aggregated time series data
      
      # Check if the aggregated data is empty
      if (nrow(plot_data) == 0) {
        return(ggplot() + 
                 geom_text(aes(x=0, y=0, label="No global average data available for the time series."), 
                           size=12) + 
                 theme_void())
      }
      
      ggplot(plot_data, aes(x = Year, y = GlobalAverage, color = Indicator)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Basic Water Access" = "#4CAF50", "Safely Managed Drinking Water" = "#1E88E5")) +
        # Use min_year and max_year for robust x-axis limits
        scale_x_continuous(limits = c(min_year, max_year), 
                           breaks = seq(min_year, max_year, by = 5)) + 
        scale_y_continuous(limits = c(40, 100), breaks = seq(0, 100, 10)) +
        labs(
          x = "Year",
          y = "Global Average Water Access (%)",
          color = "Indicator"
        ) +
        theme_classic() +
        theme(
          text = element_text(size = 12, family = "Roboto"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = "top",
          panel.grid.minor = element_blank()
        )
    })
    
    
    # 4. Reactive expression to filter data for the MAP
    data_for_map <- reactive({
      req(input$selected_year, input$selected_indicator) # Ensure inputs are available
      
      filtered_water_data <- all_water_data %>%
        filter(Year == input$selected_year,
               Indicator == input$selected_indicator)
        
        # Join the filtered data with the spatial data using the 3-letter ISO code
      world_map %>% 
        left_join(filtered_water_data, by = "iso_a3") %>%
        st_as_sf() # Convert back to sf object after join
    })
    
    # Color Palette for the map (fixed domain 0-100 for percentages)
    color_pal <- reactive({
      colorNumeric(
        # Use a different color palette for Safely Managed to distinguish easily
        palette = if (input$selected_indicator == "Basic Water Access") "YlGnBu" else "BuPu", 
        domain = c(0, 100)
      )
    })
    
    # Create the base map initially
    output$water_access_map <- renderLeaflet({
      leaflet(world_map) %>%
        setView(lng = 0, lat = 30, zoom = 2) %>% # Center the map
        addProviderTiles(providers$CartoDB.Positron) # Clean, simple base map
    })
    
    # 5. Observer to update the map polygons and legend when the year or indicator changes
    observe({
      map_data <- data_for_map()
      pal <- color_pal()
      indicator_label <- input$selected_indicator
      
      # Countries with no data will have NA for PercentAccess
      na_color <- "#CCCCCC"
      
      # Calculate colors, assigning a specific color for NA values
      map_colors <- ifelse(is.na(map_data$PercentWaterAccess), na_color, pal(map_data$PercentWaterAccess))
      
      # Define popups/labels
      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %s",
        map_data$name, 
        indicator_label,
        ifelse(is.na(map_data$PercentWaterAccess), "No Data", paste0(round(map_data$PercentWaterAccess, 1), "%%"))
      ) %>% lapply(htmltools::HTML)
      
      # Update the map polygons
      leafletProxy("water_access_map", data = map_data) %>%
        # Clear existing shapes (polygons) and legend
        clearShapes() %>%
        clearControls() %>%
        
        # Add new colored polygons (choropleth layer)
        addPolygons(
          fillColor = map_colors,
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
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "bold", padding = "5px 10px"),
            textsize = "14px",
            direction = "auto")
        ) %>%
        
        # Add legend for the percentage scale
        addLegend(pal = pal, values = c(0, 100), opacity = 0.7, title = paste0("% ", indicator_label),
                  position = "bottomright") %>%
        
        # Add a custom legend entry for 'No Data'
        addLegend(colors = na_color, labels = "No Data", opacity = 0.7, 
                  position = "bottomright", title = "")
    })
  })
}
