library(shiny)
library(bslib)

hidden_burdenUI <- function(id, title){
  tabPanel(
    title,

    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "region"), 
                    "Region",
                    choices = c(unique(time_obtain_water$Region), "All"),
                    selected = "All"
        ),
        sliderInput(NS(id, "time_period"), 
                    "Year Range",
                    min = 2000,
                    max = 2024,
                    value = c(2000, 2024)
        ),
        checkboxInput(NS(id, "show_data"),
                      "Show data table", value = TRUE),
        width = 4
      ),
      mainPanel(
        width = 8,
        layout_columns(
          col_widths = c (6, 6),
          card(
            full_screen = TRUE,
            height = "420px",
            card_header(h4("Fetching water is a demanding task...")),
            card_body(
              fill = TRUE,
              p("Distribution of Population by Time to Obtain Water"),
              plotOutput(NS(id, "time_to_obtain_water_chart")),
              DT::DTOutput(NS(id, "time_to_obtain_water_table"))
              )
          ),
          card(
            full_screen = TRUE,
            height = "420px",
            card_header("... and the burden tends to fall on women and girls"),
            card_body(
              p("Distribution of Population by Person to Obtain Water"),
              plotOutput(NS(id, "person_to_obtain_water_chart")),
              br(),
              DT::DTOutput(NS(id, "person_to_obtain_water_table"))
            )
          )
        ),
        layout_columns(
          col_widths = c (6, 6),
          card(
            full_screen = TRUE,
            height = "420px",
            card_header("This leads to more female children out of school..."),
            card_body(
              p("% of Adolescents out of Secondary School"),
              plotOutput(NS(id, "water_school_inequality_chart")),
              DT::DTOutput(NS(id, "water_school_inequality_table"))
            )
          ),
          card(
            full_screen = TRUE,
            height = "420px",
            card_header("... and more women suffering from violence"),
            card_body(
              p("% of Women Who Have Experienced Physical Violence since 15"),
              plotOutput(NS(id, "women_violence_chart")),
              DT::DTOutput(NS(id, "women_violence_table"))
              )
            )
          )
        )
      )
    )
}

hidden_burdenServer <- function(id){
  moduleServer(id, function(input, output, session){
    thematic::thematic_shiny()
    
    # Card 1: Time to fetch water chart
    time_to_obtain_water_card1 <- reactive({
      df <- time_obtain_water %>%
        filter (between (Year, input$time_period[1], input$time_period[2])) 
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })
    
    output$time_to_obtain_water_chart <- renderPlot({
      x <- if (input$region == "All") "Region" else "Country"
      ggplot(time_to_obtain_water_card1(), aes(x = .data[[x]], 
                                             y = Value, 
                                             fill = Indicator)) +
        geom_bar(stat = "summary", fun = "mean", position = "stack") +
        labs(y = "% of Population") +
        scale_fill_paletteer_d("MetBrewer::Egypt") +
        theme(text = element_text(family = "Helvetica"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              legend.title = element_blank(),
              legend.position = "top")
    }, bg = "transparent"
    )
    
    # Data table to display
    output$time_to_obtain_water_table <- DT::renderDT(
      if (input$show_data){
        time_to_obtain_water_card1() %>%
          group_by(Region, Country, Indicator, Year) %>% 
          summarize(Value = mean(Value, na.rm = TRUE)) %>% 
          ungroup() %>% 
          pivot_wider(names_from = Indicator,
                      values_from = Value) %>% 
          mutate(across(where(is.numeric), round, 2))
      }
    )
    
    # Card 2: Person to fetch water chart
    person_to_obtain_water_card2 <- reactive({
      df <- person_obtain_water %>%
        filter (between (Year, input$time_period[1], input$time_period[2])) 
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })
    
    output$person_to_obtain_water_chart <- renderPlot({
      ggplot(person_to_obtain_water_card2(), aes(x = Value,
                                                 y = Indicator,
                                                 fill = Indicator)) +
        geom_col() +
        scale_color_paletteer_d("MetBrewer::Egypt") +
        scale_fill_paletteer_d("MetBrewer::Egypt") +
        theme(text = element_text(size = 16, family = "Helvetica"))
    }, bg = "transparent"
    )
    
    # Data table to display
    output$person_to_obtain_water_table <- DT::renderDT(
      if (input$show_data){
        person_to_obtain_water_card2() %>%
          group_by(Region, Country, Indicator, Year) %>% 
          summarize(Value = mean(Value, na.rm = TRUE)) %>% 
          ungroup() %>% 
          pivot_wider(names_from = Indicator,
                      values_from = Value) %>% 
          mutate(across(where(is.numeric), round, 2))
      }
    )
    
    # Card 3: Association between access to basic water and female unenrollment
    water_school_inequality_card3 <- reactive({
      df <- water_une %>%
        mutate(water_bin = cut(access_to_basic_water, breaks = seq(0, 100, by = 20))) %>% 
        filter (between (Year, input$time_period[1], input$time_period[2])) 
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })
    
    output$water_school_inequality_chart <- renderPlot({
      ggplot(water_school_inequality_card3(), aes(x = Gender, y = unenrollment, fill = Gender)) +
        geom_boxplot() +
        facet_wrap(~ water_bin, scales = "free") +
        labs(x = "Gender", y = "Unenrollment (%)") +
        scale_color_paletteer_d("MetBrewer::Egypt") +
        scale_fill_paletteer_d("MetBrewer::Egypt") +
        theme(text = element_text(size = 16, family = "Helvetica"),
              legend.position = "top")
    }, bg = "transparent"
    )
    
    # Data table to display
    output$water_school_inequality_table <- DT::renderDT(
      if (input$show_data){
        water_school_inequality_card3() %>%
          group_by(Region, Country, Gender, Year) %>% 
          summarize(unenrollment = mean(unenrollment, na.rm = TRUE),
                    access_to_basic_water = mean(access_to_basic_water, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(across(where(is.numeric), round, 2))
      }
    )
    
    # Card 4: Association between access to basic water and violence toward women
    water_violence_card4 <- reactive({
      df <- water_violence %>%
        filter (between (Year, input$time_period[1], input$time_period[2])) 
      if (input$region != "All"){
        df <- df %>% filter (Region == input$region)
      }
      df
    })
    
    output$women_violence_chart <- renderPlot({
      ggplot(water_violence_card4(), aes(x = access_to_basic_water, y = pct_violence, color = Region)) +
        geom_point(size = 5) +
        labs(x = "% of Population with Access to Basic Water", y = "% of Women Who Have Experience Violence") +
        scale_color_paletteer_d("MetBrewer::Egypt") +
        scale_fill_paletteer_d("MetBrewer::Egypt") +
        theme(text = element_text(size = 16, family = "Helvetica"),
              legend.position = "top")
    }, bg = "transparent"
    )
    
    # Data table to display
    output$women_violence_table <- DT::renderDT(
      if (input$show_data){
        water_violence_card4() %>%
          group_by(Region, Country, Year) %>% 
          summarize(access_to_basic_water = mean(access_to_basic_water, na.rm = TRUE),
                    pct_violence = mean(pct_violence, na.rm = TRUE)) %>% 
          ungroup() %>% 
          mutate(across(where(is.numeric), round, 2))
      }
    )
      
  })
}