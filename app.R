library(shiny)
library(bslib)
library(rsconnect)
library(paletteer)
library(thematic)

# Define UI for application that draws a histogram
ui <- navbarPage("Clean Water and Public Health",
    
    theme = bs_theme(
      bg = "black",
      fg = "#FFC125",
      primary = "#DAA520",
      bootswatch = "darkly",
      base_font = font_google("Roboto"),
      heading_font = font_google("Roboto")) %>% bs_add_variables(
        "navbar-dark-brand-color" = "#FFC125"
      ),
  
      tabPanel("Hidden Burden",
        # FluidRow layout
        fluidRow(
          h3("Fetching water is a demanding task"),
          sidebarLayout(
            sidebarPanel(
              selectInput("region", 
                          "Region",
                          choices = c(unique(time_obtain_water$Region), "All"),
                          selected = "All"
              ),
              selectInput("time_period", 
                          "Time",
                          choices = c("Average", "Most Recent"),
                          selected = "Average"
              )
            ),
            mainPanel (
              plotOutput("time_to_obtain_water_chart"),
              DT::DTOutput("time_to_obtain_water_table")
            )
          )
        )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
  # Average time to fetch water chart
  output$time_to_obtain_water_chart <- renderPlot(
    if (input$region == "All"){
      time_obtain_water_merged %>% group_by(Region, Indicator) %>% 
        filter (case_when(
          input$time_period == "Average" ~ Year == "Average",
          TRUE ~ Year != "Average"))%>% 
        summarize (regions_avg = mean(regions_avg)) %>% 
        ggplot(aes(x = Region, y = regions_avg, fill = Indicator)) +
        geom_col(position = "stack") +
        labs(x = "Region", y = "% of Population",
             title = "Distribution of Population by Time to Obtain Water") +
        scale_color_paletteer_d("MetBrewer::Klimt") +
        scale_fill_paletteer_d("MetBrewer::Klimt") 
    } else {
      time_obtain_water_merged %>% filter (Region == input$region) %>% 
        group_by(Country, Indicator) %>% 
        filter (case_when(
          input$time_period == "Average" ~ Year == "Average",
          TRUE ~ Year != "Average")) %>% 
        ggplot(aes(x = Country, y = Value, fill = Indicator)) +
        geom_col(position = "stack") +
        labs(x = "Country", y = "% of Population",
             title = "Distribution of Population by Time to Obtain Water") +
        scale_color_paletteer_d("MetBrewer::Klimt") +
        scale_fill_paletteer_d("MetBrewer::Klimt") 
    },
    bg = "transparent"
  )
  
  # Data table to display
  output$time_to_obtain_water_table <- DT::renderDT(
    if (input$region == "All"){
      time_obtain_water_merged %>% group_by(Region, Indicator) %>% 
        filter (case_when(
          input$time_period == "Average" ~ Year == "Average",
          TRUE ~ Year != "Average")) %>%
        summarize(regions_avg = mean(regions_avg)) %>%
        pivot_wider(names_from = Indicator,
                    values_from = regions_avg) 
    } else {
      time_obtain_water_merged %>% filter (Region == input$region) %>% 
        group_by(Country, Indicator) %>% 
        filter (case_when(
          input$time_period == "Average" ~ Year == "Average",
          TRUE ~ Year != "Average")) %>%
        select (-regions_avg) %>% 
        pivot_wider(names_from = Indicator,
                    values_from = Value) 
    }
      
  )
  
  # By country shift over time
  output$time_to_obtain_water_over_years <- renderPlot(
    time_obtain_water %>% 
    filter (Country == "Tanzania") %>% 
    ggplot(aes(x = Year, y = Value, fill = Indicator)) + 
    geom_col(position = "stack") 
  )
  
  output$person_to_obtain_water_over_years <- renderPlot(
    person_obtain_water %>% 
    filter (Country == "Tanzania") %>% 
    ggplot(aes(x = Year, y = Value, fill = Indicator)) +
    geom_col(position = "stack") 
  )
  
  ### Association between access to basic water and female unenrollment
  
  output$water_and_female_unenroll_chart <- renderPlot(
    water_female_une %>%
      filter (Year < 2005) %>% 
      ggplot (aes(x = access_to_basic_water, y = female_unenroll,
                  fill = Region)) +
      geom_point()
  )
  
  ### Association between access to basic water and male unenrollment
  
  output$water_and_male_unenroll_chart <- renderPlot(
    water_male_une %>%
      filter (Year < 2005) %>% 
      ggplot (aes(x = access_to_basic_water, y = male_unenroll,
                  fill = Region)) +
      geom_point()
  )
  
  ### Association between access to basic water and gender inequality in school enrollment
  water_une %>%
    filter (Year < 2005) %>% 
    ggplot (aes(x = access_to_basic_water, y = unenrollment,
                color = Gender)) +
    geom_point()
  
  ## Average line
  water_une %>%
    filter(Year < 2005) %>%
    mutate(water_bin = cut(access_to_basic_water, breaks = seq(0, 100, by = 10))) %>%
    group_by(water_bin, Gender) %>%
    summarize(mean_unenroll = mean(unenrollment, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = water_bin, y = mean_unenroll, color = Gender, group = Gender)) +
    geom_line(size = 1.2) +
    geom_point() +
    theme_minimal() +
    labs(x = "Access to basic water (%)", y = "Mean unenrollment",
         title = "Gender gap in unenrollment vs. water access (2005 and earlier)")
  
  ## Box plot (I like this more)
  output$water_school_inequality_chart <- renderPlot(
    water_une %>%
      filter(Year < 2005) %>%
      mutate(water_bin = cut(access_to_basic_water, breaks = seq(0, 100, by = 20))) %>%
      ggplot(aes(x = Gender, y = unenrollment, fill = Gender)) +
      geom_boxplot(outlier.size = 0.5) +
      facet_wrap(~ water_bin, scales = "free_y") +
      theme_minimal() +
      labs(title = "Unenrollment distribution by gender across water access levels",
           x = "Gender", y = "Unenrollment (%)")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
