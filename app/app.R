# =============================================================================
# This Shiny app is used to be deployed on the html website
# =============================================================================

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, ggplot2, lubridate, dplyr, kableExtra, sf, 
               rnaturalearth, janitor, countrycode, stringr, shiny, bslib, 
               shinythemes, DT, ggrepel, viridis, shinyWidgets, shinydashboard, 
               plotly)

# --------------- import data 
climate <- readRDS("Data/climate.rds")
data <- readRDS("Data/data.rds")
#get only countries with at least one non NA value for any indicator
valid_countries <- climate|>
  pivot_longer(cols = matches("^(19|20)\\d{2}$"), names_to = "year", values_to = "value") |>
  group_by(`Country Name`)|>
  summarise(non_na_count = sum(!is.na(value)))|>
  filter(non_na_count > 0)|>
  pull(`Country Name`)

valid_medians <- data|>
  pivot_longer(cols = matches("^(19|20)\\d{2}_median$"), names_to = "year", values_to = "value") |>
  group_by(`region_un`)|>
  summarise(non_na_count = sum(!is.na(value)))|>
  filter(non_na_count > 0)|>
  pull(`region_un`)

# =============================================================================
# User interface 
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Yearly changes in sustainability indicators", titleWidth = 500),
  
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Line Graph", tabName = "line", 
                              icon = icon("chart-line")),
                     menuItem("Line Graph by Continent", tabName = "line-region", 
                              icon = icon("chart-line"))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "line",
              fluidRow(
                column(12, h2("Regional Trends by Country and Indicator"), align = "center")
              ),
              fluidRow(
                column(12,
                       p("Explore regional trends based on country selection and indicator of interest.",
                         style = "font-size: 12px"),
                       align = "center")
              ),
              fluidRow(
                column(4,
                       selectizeInput("country_select", "Choose a Country:",
                                      choices = sort(valid_countries))
                ),
                column(4,
                       selectizeInput(
                         "indicator_select",
                         "Choose an Indicator:",
                         choices = sort(unique(climate$`Indicator Name`))
                       )
                )
              ),
              fluidRow(
                column(12,
                       plotlyOutput("regionPlot")
                )
              )
      ),
      tabItem(tabName = "line-region",
              fluidRow(
                column(12, h2("Regional Trends by Continent and Indicator"), align = "center")
              ),
              fluidRow(
                column(12,
                       p("Explore regional trends based on continent selection and indicator of interest.",
                         style = "font-size: 12px"),
                       align = "center")
              ),
              fluidRow(
                column(4,
                       selectizeInput("country_select_con", "Choose a Continent:",
                                      choices = sort(valid_medians))
                ),
                column(4,
                       selectizeInput(
                         "indicator_select_con",
                         "Choose an Indicator:",
                         choices = sort(unique(data$`Indicator Name`))
                       )
                )
              ),
              fluidRow(
                column(12,
                       plotlyOutput("continentPlot")
                )
              )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {
  
  output$continentPlot <- renderPlotly({
    req(input$country_select_con, input$indicator_select_con)
    
    #filter data by selected country and indicator
    filtered_data1 <- data |>
      filter(`region_un` == input$country_select_con,
             `Indicator Name` == input$indicator_select_con)
    
    #convert year columns to long format
    data_long <- filtered_data1 |>
      pivot_longer(
        cols = matches("^(19|20)\\d{2}_median$"),
        names_to = "year",
        values_to = "value"
      )
    
    #generate plot
    p1 <- ggplot(data_long, aes(x = year, y = value)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = paste("Variable:", input$indicator_select_con),
        subtitle = paste("Continent:", input$country_select_con),
        x = "Year",
        y = input$indicator_select_con
      ) +
      theme_minimal()
    
    ggplotly(p1)
  })
  
  observe({
    req(input$country_select)
    
    #filter data for the selected country and get valid indicators
    valid_indicators <- climate |>
      filter(`Country Name` == input$country_select) |>
      pivot_longer(cols = matches("^(19|20)\\d{2}$"), names_to = "year", values_to = "value") |>
      group_by(`Indicator Name`) |>
      summarise(non_na_count = sum(!is.na(value))) |>
      filter(non_na_count > 0) |>
      pull(`Indicator Name`)  # Extract valid indicators
    
    #update the indicator choices
    updateSelectizeInput(session, "indicator_select", choices = valid_indicators, server = TRUE)
  })

  
  output$regionPlot <- renderPlotly({
    req(input$country_select, input$indicator_select)
    
    #filter data by selected country and indicator
    filtered_data <- climate |>
      filter(`Country Name` == input$country_select,
             `Indicator Name` == input$indicator_select)
    
    #convert year columns to long format
    data_long <- filtered_data |>
      pivot_longer(
        cols = matches("^(19|20)\\d{2}$"),
        names_to = "year",
        values_to = "value"
      ) |>
      mutate(year = as.integer(year))
    
    #generate plot
    p <- ggplot(data_long, aes(x = year, y = value)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = paste("Variable:", input$indicator_select),
        subtitle = paste("Country:", input$country_select),
        x = "Year",
        y = input$indicator_select
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
 
}

# =============================================================================
# Run the app
# =============================================================================
shinyApp(ui = ui, server = server)
