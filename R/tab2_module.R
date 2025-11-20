library(shiny)
library(bslib)

hidden_burdenUI <- function(id, title){
  tabPanel(
    title,
    page_sidebar(
      sidebar = sidebar(
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
        )
      ),
      accordion(
        accordion_panel(
          title = "Fetching water is a demanding task...",
          icon = bsicons::bs_icon("water"),
          p("Distribution of Population by Time to Obtain Water"),
          plotOutput(NS(id, "time_to_obtain_water_chart")),
          actionButton(NS(id, "modal_card_1"), "Show Data")
        ),
        accordion_panel(
          title = "... and the burden tends to fall on women and girls",
          icon = bsicons::bs_icon("droplet-fill"),
          p("Distribution of Population by Person to Obtain Water"),
          plotOutput(NS(id, "person_to_obtain_water_chart")),
          actionButton(NS(id, "modal_card_2"), "Show Data")
        ),
        accordion_panel(
          title = "This leads to more female children out of school...",
          icon = bsicons::bs_icon("droplet-half"),
          p("% of Adolescents out of Secondary School"),
          plotOutput(NS(id, "water_school_inequality_chart")),
          actionButton(NS(id, "modal_card_3"), "Show Data")
        ),
        accordion_panel(
          title = "... and more women suffering from physical violence",
          icon = bsicons::bs_icon("droplet"),
          p("% of Women Who Have Experienced Physical Violence since 15"),
          plotOutput(NS(id, "women_violence_chart")),
          actionButton(NS(id, "modal_card_4"), "Show Data")
        ),
        id = "acc",
        open = "Fetching water is a demanding task..."
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
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              legend.title = element_blank(),
              legend.position = "top")
    }, bg = "transparent"
    )

    # Data table to display
    output$time_to_obtain_water_table <- DT::renderDT(
      time_to_obtain_water_card1() %>%
        group_by(Region, Country, Indicator, Year) %>%
        summarize(Value = mean(Value, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = Indicator,
                    values_from = Value) %>%
        mutate(across(where(is.numeric), round, 2))
    )
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


    # Card 2: Person to fetch water chart
    person_to_obtain_water_card2 <- reactive({
      df <- person_obtain_water %>%
        filter (Indicator != "NA") %>%
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
        scale_fill_paletteer_d("MetBrewer::Egypt") +
        labs (x = "% Population") +
        theme(text = element_text(size = 10, family = "Roboto"),
              axis.title.y = element_blank())
    }, bg = "transparent"
    )

    # Data table to display
    output$person_to_obtain_water_table <- DT::renderDT(
      person_to_obtain_water_card2() %>%
        group_by(Region, Country, Indicator, Year) %>%
        summarize(Value = mean(Value, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = Indicator,
                    values_from = Value) %>%
        mutate(across(where(is.numeric), round, 2))
    )
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
      ggplot(water_school_inequality_card3(),
             aes(x = water_bin, y = unenrollment, color = Gender, group = Gender)
             ) +
        stat_summary(fun = mean, geom = "line", linewidth = 1, na.rm = TRUE) +
        stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
        labs(x = "% Population with Access to Basic Water", y = "Unenrollment (%)") +
        scale_color_paletteer_d("MetBrewer::Egypt") +
        theme(text = element_text(size = 10, family = "Roboto"),
              legend.position = "top")
    }, bg = "transparent"
    )

    # Data table to display
    output$water_school_inequality_table <- DT::renderDT(
      water_school_inequality_card3() %>%
        group_by(Region, Country, Gender, Year) %>%
        summarize(unenrollment = mean(unenrollment, na.rm = TRUE),
                  access_to_basic_water = mean(access_to_basic_water, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 2))

    )
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
        theme(text = element_text(size = 10, family = "Roboto"),
              legend.position = "top")
    }, bg = "transparent"
    )

    # Data table to display
    output$women_violence_table <- DT::renderDT(
      water_violence_card4() %>%
        group_by(Region, Country, Year) %>%
        summarize(access_to_basic_water = mean(access_to_basic_water, na.rm = TRUE),
                  pct_violence = mean(pct_violence, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 2))
    )

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