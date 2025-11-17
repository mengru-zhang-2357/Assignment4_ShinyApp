library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Clean Water and Public Health"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("time_to_obtain_water_chart"),
           dataTableOutput("time_to_obtain_water_table")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Average time to fetch water chart
    output$time_to_obtain_water_chart <- renderPlot(
      time_obtain_water_merged %>% group_by(Region, Indicator) %>% 
      filter (Year == "Average") %>% 
      summarize (regions_avg = mean(regions_avg)) %>% 
      ggplot(aes(x = Region, y = regions_avg, fill = Indicator)) +
      geom_col(position = "stack") 
    )
    
    # Data table to display
    output$time_to_obtain_water_table <- renderDataTable(
      time_obtain_water_merged %>% group_by(Region, Indicator) %>% 
      filter (Year == "Average") %>% 
      summarize(regions_avg = mean(regions_avg)) %>%
      pivot_wider(names_from = Indicator,
                  values_from = regions_avg) %>% 
      rename(
        Missing_Info = "Population with unknown or missing information on round trip time to water",
        Over_30_Min = "Population with water more than 30 minutes away round trip",
        Less_than_30_Min = "Population with water 30 minutes or less away round trip",
        On_Premise = "Population with water on the premises"
      )
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
    
    unenrollment_merged <- bind_rows(female_une_merged %>% mutate(Gender = "female"),
                                     male_une_merged %>% mutate(Gender = "male"))
    
    unenroll_summary <- unenrollment_merged %>% 
      group_by(Region, Gender) %>% 
      mutate (regions_avg = mean(regions_avg, na.rm = TRUE))
    
    # Average % of children out of school
    output$unenroll_compare <- renderPlot(
      ggplot(unenroll_summary, aes(x = Region, y = regions_avg, fill = Gender)) +
      geom_col(position = "dodge", width = 0.7) 
    )
    
    # Data table to display
    output$female_unenroll_table_region <- renderDataTable(
      female_une_merged %>% group_by(Country) %>% 
        filter (Year == "Average",
              Region == "South Asia") %>% 
        select(-regions_avg)
    )
    
    # Data table to display
    output$female_unenroll_table_country <- renderDataTable(
      female_une %>% 
        filter (Country == "India") %>% 
        pivot_wider (names_from = "Year", values_from = "Value")
    )
    
    ### Association between access to basic water and female unenrollment
    water_female_une <- female_une %>% 
      left_join(basic_water, by = c("Year", "Country", "Region")) %>% 
      rename (female_unenroll = Value.x,
              access_to_basic_water = Value.y) %>% 
      filter(Residence_Area == "Total")
    
    output$water_and_female_unenroll_chart <- renderPlot(
      water_female_une %>%
        filter (Year < 2005) %>% 
        ggplot (aes(x = access_to_basic_water, y = female_unenroll,
                    fill = Region)) +
        geom_point()
    )
    
    ### Association between access to basic water and male unenrollment
    water_male_une <- male_une %>% 
      left_join(basic_water, by = c("Year", "Country", "Region")) %>% 
      rename (male_unenroll = Value.x,
              access_to_basic_water = Value.y) %>% 
      filter(Residence_Area == "Total")
    
    output$water_and_male_unenroll_chart <- renderPlot(
      water_male_une %>%
        filter (Year < 2005) %>% 
        ggplot (aes(x = access_to_basic_water, y = male_unenroll,
                    fill = Region)) +
        geom_point()
    )
    
    ### Association between access to basic water and gender inequality in school enrollment
    unenrollment <- bind_rows(female_une %>% mutate(Gender = "Female"),
                              male_une %>% mutate(Gender = "Male"))
    water_une <- unenrollment %>% 
      left_join(basic_water, by = c("Year", "Country", "Region")) %>% 
      rename (unenrollment = Value.x,
              access_to_basic_water = Value.y) %>% 
      filter(Residence_Area == "Total")
    
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
