library(shiny)
library(bslib)
library(plotly)
library(shinyWidgets) # To use pretty switch for regression line

# --- Data Loading and Initial Preparation ---
# Takes place in tab2_hidden_burden.R

# --- UI Definition ---
hidden_burdenUI <- function(id, title){
  tabPanel(
    # --- Tab 2: Hidden Burden of Water Insecurity on Women and Girls: Time to Fetch Water, Person to Fetch Water, School Enrollment and Physical Violence ---
    title,
    
    # Set up the sidebar layout
    page_sidebar(
      sidebar = sidebar(
        # Overview of the tab - summary of the impact of water insecurity on gender inequality
        p("The public health burden of water insecurity on women and girls is amplified because their traditional role as water collectors and household caregivers exposes them to injuries from carrying heavy loads, increased risk of waterborne infections due to poor hygiene and unsafe sources, and higher rates of gender-based violence while performing these essential, yet hazardous, duties."),
        br(),
        
        # 1. Hierarchical Grouping: User can select all regions or one region. If All regions, then the charts are grouped by regions; otherwise, the charts are grouped by countries.
        selectInput(NS(id, "region"),
                    "Region",
                    choices = c(unique(time_obtain_water$Region), "All"),
                    selected = "All"
        ),
        br(),
        
        # 2. Slider input to allow the user select a data range
        sliderInput(NS(id, "time_period"),
                    "Year Range",
                    min = 2000,
                    max = 2024,
                    value = c(2000, 2024),
                    sep = ""
        ),
        br(),
        
        # 3. Select input that allows the user to choose a color palette
        selectInput (NS(id,"color"), "Choose a color palette",
                    list ("PrettyCols::Autumn", "colorBlindness::Brown2Blue12Steps", "rcartocolor::Prism", "ggthemes::calc"),
                    selected = "PrettyCols::Autumn"),
        
      ),
      
      # The accordion layout is chosen given its flexibility to open and close charts
      accordion(
        # 1. Time to obtain water chart: define the key message, quote from literature, plotly chart, and a button that opens a modal for the user to review the data
        accordion_panel(
          icon = bsicons::bs_icon("water"),
          title = "Fetching water is a demanding task...",
          p(em("\"Over two-thirds of the population in sub-Saharan Africa report leaving their homes to collect water, and many rural water systems are often non-functional, exacerbating the difficulty of water collection and augmenting health problems.\"")),
          p(style = "text-align: right;", "-- Graham et al., 2016"),
          br(),
          p("Distribution of Population by Time to Obtain Water"),
          plotlyOutput(NS(id, "time_to_obtain_water_chart")),
          actionButton(NS(id, "modal_card_1"), "Show Data")
        ),
        # 2. Person to obtain water chart: define the key message, quote from literature, plotly chart, and a button that opens a modal for the user to review the data
        accordion_panel(
          value = "person_water",
          title = "... and the burden tends to fall on women and girls",
          icon = bsicons::bs_icon("droplet-fill"),
          p(em("\"When inquired about the reason of carrying out the chore, the most common answer would be, 'because it is a woman’s job!'\"")),
          p(style = "text-align: right;", "-- Mungekar, 2022"),
          br(),
          p("Distribution of Population by Person to Obtain Water"),
          plotlyOutput(NS(id, "person_to_obtain_water_chart")),
          actionButton(NS(id, "modal_card_2"), "Show Data")
        ),
        # 3. School unenrollment chart: define the key message, quote from literature, plotly chart, and a button that opens a modal for the user to review the data
        accordion_panel(
          value = "school",
          title = "This is associated with more girls out of school...",
          icon = bsicons::bs_icon("droplet-half"),
          p(em("\"A standard deviation increase in water insecurity resulted in 0.30 more missed school days in the last week.\"")),
          p(style = "text-align: right;", "-- Cooper-Vince et al., 2017"),
          br(),
          p("% of Adolescents out of Secondary School"),
          plotlyOutput(NS(id, "water_school_inequality_chart")),
          actionButton(NS(id, "modal_card_3"), "Show Data")
        ),
        # 4. Violence against women chart: define the key message, quote from literature, plotly chart, and a button that opens a modal for the user to review the data
        accordion_panel(
          value = "water_violence",
          title = "... and more women suffering from physical violence",
          icon = bsicons::bs_icon("droplet"),
          p(em("\"[H]ousehold water insecurity could increase women's exposure to emotional and physical forms of intimate partner violence.\"")),
          p(style = "text-align: right;", "-- Choudhary et al., 2020"),
          br(),
          selectInput (NS(id,"color_var4"), "Choose color group variable",
                       list ("Region", "Country"),
                       selected = "Region"),
          prettySwitch (NS(id,"reg_4"), "Show Regression Line"),
          br(),
          p("% of Women Who Have Experienced Physical Violence since 15"),
          plotlyOutput(NS(id, "women_violence_chart")),
          actionButton(NS(id, "modal_card_4"), "Show Data")
        ),
        id = NS(id, "acc"),
        open = "time_water",   # Define the tab open at launch
        multiple = FALSE       # Only one panel is open at a time
      )
    )
  )
}

# --- Server Logic ---
hidden_burdenServer <- function(id){
  moduleServer(id, function(input, output, session){
    
    # Apply app theme to the outputs
    thematic::thematic_shiny()
    
    # Panel 1: Time to fetch water chart
    # First create the reactive data frame used in charting. The reactive data frame filters to the right region and time period
    time_to_obtain_water_card1 <- reactive({
      df <- time_obtain_water %>%
        mutate(across(where(is.numeric), round, 2)) %>% 
        filter (between (Year, input$time_period[1], input$time_period[2]))
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })
    
    # Create the chart that shows cross-sectional comparison of time to obtain water
    output$time_to_obtain_water_chart <- renderPlotly({
      # Define the column to use for x axis
      x <- if (input$region == "All") "Region" else "Country"
      
      # Calculate the mean by region over the time period, also create the text for tooltip
      ### Note: Ideally, we would weight each country/region by population. For this app, we use a simple average as a proof of concept.
      data_summary <- time_to_obtain_water_card1() %>% 
        mutate(xvar = .data[[x]]) %>% 
        group_by(xvar, Indicator) %>% 
        summarize(Value = mean(Value, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate (text = paste0(
          "Region / Country: ", xvar, "<br>",
          "Time to Obtain Water: ", Indicator, "<br>",
          "% Population: ", round(Value, 1)
          ))
      
      # Draw the stacked column chart, using the user-defined color palette. Adjust text size for readability
      base <- ggplot(data_summary, aes(x = xvar,
                                       y = Value,
                                       fill = Indicator, 
                                       text = text)) +
        geom_col(position = "stack") +
        labs(y = "% of Population",
             x = x) +
        scale_fill_paletteer_d(input$color) +
        theme_classic() +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
              axis.text.y = element_text(size = 12),
              legend.title = element_blank(),
              legend.position = "top") 
      
      # Convert to plotly chart for interaction
      ggplotly(base, tooltip = "text")
    })

    # Data table to display in the modal: If there are multiple data points in the same country, same indicator, same year, take the mean.
    # Pivot to wide data for readability
    output$time_to_obtain_water_table <- DT::renderDT(
      time_to_obtain_water_card1() %>%
        group_by(Region, Country, Indicator, Year) %>%
        summarize(Value = mean(Value, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = Indicator,
                    values_from = Value) 
    )
    
    # Use an observer to open the modal and display the data table, only if the user clicks on "show data"
    observe({
      showModal(
        modalDialog(
          title = "Time to Obtain Water",
          easy_close = TRUE,
          size = "xl",
          div(
            DT::DTOutput(NS(id, "time_to_obtain_water_table")),
            style = "font-size: 80%;"
          )
        )
      )
    }) %>%
      bindEvent(input$modal_card_1)


    # Panel 2: Person to fetch water chart
    # First create the reactive data frame used in charting. The reactive data frame filters to the right region and time period
    person_to_obtain_water_card2 <- reactive({
      df <- person_obtain_water %>%
        mutate(across(where(is.numeric), round, 2)) %>% 
        filter (Indicator != "NA") %>%
        filter (between (Year, input$time_period[1], input$time_period[2]))
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })

    # Create the chart that shows cross-sectional comparison by person responsible for obtaining water
    output$person_to_obtain_water_chart <- renderPlotly({
      
      # Calculate the mean by indicator over the time period, also create the text for tooltip
      ### Note: Ideally, we would weight each country/region by population. For this app, we use a simple average as a proof of concept.
      data_summary <- person_to_obtain_water_card2() %>% 
        group_by(Indicator) %>% 
        summarize(Value = mean(Value, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate (text = paste0("Person to Obtain Water: ", Indicator, "<br>",
                              "% Population: ", round(Value, 1)))
      
      # Draw the bar chart, using the user-defined color palette. Adjust text size for readability
      base <- ggplot(data_summary, aes(x = Value,
                                       y = Indicator,
                                       fill = Indicator,
                                       text = text)) +
        geom_col() +
        scale_fill_paletteer_d(input$color) +
        labs (x = "% Population") +
        theme_classic() +
        theme(text = element_text(size = 12, family = "Roboto"),
              axis.text.y = element_text(size = 12),
              axis.text.x = element_text(size = 12),
              axis.title.y = element_blank())
      
      # Convert to plotly chart for interaction
      ggplotly(base, tooltip = "text")
    })

    # Data table to display in the modal: If there are multiple data points in the same country, same indicator, same year, take the mean.
    # Pivot to wide data for readability
    output$person_to_obtain_water_table <- DT::renderDT(
      person_to_obtain_water_card2() %>%
        group_by(Region, Country, Indicator, Year) %>%
        summarize(Value = mean(Value, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = Indicator,
                    values_from = Value))
    
    # Use an observer to open the modal and display the data table, only if the user clicks on "show data"
    observe({
      showModal(
        modalDialog(
          title = "Person to Obtain Water",
          easy_close = TRUE,
          size = "xl",
          div(
            DT::DTOutput(NS(id, "person_to_obtain_water_table")),
            style = "font-size: 80%;"
          )
        )
      )
    }) %>%
      bindEvent(input$modal_card_2)

    # Panel 3: Association between access to basic water and school unenrollment
    # First create the reactive data frame used in charting. The reactive data frame filters to the right region and time period
    water_school_inequality_card3 <- reactive({
      df <- water_une %>%
        mutate(across(where(is.numeric), round, 2)) %>% 
        
        # Convert the continuous variable access_to_basic_water to five bins
        mutate(water_bin = cut(access_to_basic_water, breaks = seq(0, 100, by = 20))) %>%
        filter (between (Year, input$time_period[1], input$time_period[2]))
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })
    
    # Create the chart that shows female vs male enrollment by access to basic water
    output$water_school_inequality_chart <- renderPlotly({
      # Calculate the mean by water bin and gender over the time period, also create the text for tooltip
      ### Note: Ideally, we would weight each country/region by population. For this app, we use a simple average as a proof of concept.
      data_summary <- water_school_inequality_card3() %>% 
        group_by(water_bin, Gender) %>% 
        summarize(unenrollment = mean(unenrollment, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate (text = paste0("% Population with Basic Water Access: ", water_bin, "<br>",
                              "% Adolescent out of Secondary School: ", round(unenrollment, 1)))
      
      # Draw the line chart, using the user-defined color palette. Adjust text size for readability
      base <- ggplot(data_summary,aes(x = water_bin, y = unenrollment, color = Gender, group = Gender, text = text)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        labs(x = "% Population with Access to Basic Water", y = "Unenrollment (%)") +
        scale_color_paletteer_d(input$color) +
        theme_classic() +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.position = "top")
      
      # Convert to plotly chart for interaction
      ggplotly(base, tooltip = "text")
    })

    # Data table to display in the modal: If there are multiple data points in the same country, same gender, same year, take the mean.
    output$water_school_inequality_table <- DT::renderDT(
      water_school_inequality_card3() %>%
        group_by(Region, Country, Gender, Year) %>%
        summarize(unenrollment = mean(unenrollment, na.rm = TRUE),
                  access_to_basic_water = mean(access_to_basic_water, na.rm = TRUE)) %>%
        ungroup() %>% 
        rename(Percent_Population_with_Basic_Water_Access = access_to_basic_water,
               Percent_Adolescents_out_of_Secondary_School = unenrollment)
      )
    
    # Use an observer to open the modal and display the data table, only if the user clicks on "show data"
    observe({
      showModal(
        modalDialog(
          title = "Access to Water Impact on School Enrollment",
          easy_close = TRUE,
          size = "xl",
          div(
            DT::DTOutput(NS(id, "water_school_inequality_table")),
            style = "font-size: 80%;"
          )
        )
      )
    }) %>%
      bindEvent(input$modal_card_3)

    # Panel 4: Association between access to basic water and violence toward women
    # First create the reactive data frame used in charting. The reactive data frame filters to the right region and time period
    water_violence_card4 <- reactive({
      df <- water_violence %>%
        mutate(across(where(is.numeric), round, 2)) %>% 
        filter (between (Year, input$time_period[1], input$time_period[2]))
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })

    # Create the scatter plot that shows violence against women by access to basic water
    output$women_violence_chart <- renderPlotly({
      
      # Draw the scatter plot, using the user-defined color palette. Adjust text size for readability
      # This panel allows the user to choose to color code the chart by region or country. This is input$color_var4.
      base <- ggplot(water_violence_card4(), aes(x = access_to_basic_water, y = pct_violence, color = .data[[input$color_var4]],
                                                 text = paste0("% Population with Basic Water Access: ", access_to_basic_water, "<br>",
                                                               "% Women who Have Experienced Physical Violence since 15: ", round(pct_violence,1)))) +
        geom_point(size = 3) +
        labs(x = "% of Population with Access to Basic Water", y = "% of Women Who Have Experienced Physical Violence") +
        scale_color_paletteer_d(input$color) +
        theme_classic() +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              legend.position = "top")
      
      # If the user switches on "Show Regression Line", then show regression line.
      if (input$reg_4){
        base <- base + geom_smooth(aes(group = 1),
                                   method = "lm", se = TRUE)
      }
      
      # Convert to plotly chart for interaction
      ggplotly(base, tooltip = "text") 
    })

    # Data table to display in the modal: If there are multiple data points in the same country, same year, take the mean.
    output$women_violence_table <- DT::renderDT(
      water_violence_card4() %>%
        group_by(Region, Country, Year) %>%
        summarize(access_to_basic_water = mean(access_to_basic_water, na.rm = TRUE),
                  pct_violence = mean(pct_violence, na.rm = TRUE)) %>%
        ungroup() %>% 
        rename(Percent_Population_with_Basic_Water_Access = access_to_basic_water,
               Percent_Women_Experienced_Physical_Violence = pct_violence)
        )

    # Use an observer to open the modal and display the data table, only if the user clicks on "show data"
    observe({
      showModal(
        modalDialog(
          title = "Access to Water Impact on Violence Against Women",
          easy_close = TRUE,
          size = "xl",
          div(
            DT::DTOutput(NS(id, "women_violence_table")),
            style = "font-size: 80%;"
          )
        )
      )
    }) %>%
      bindEvent(input$modal_card_4)
      
  })
}