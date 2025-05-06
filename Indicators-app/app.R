# =============================================================================
# This Shiny app is used to be deployed on the html website
# =============================================================================

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, ggplot2, lubridate, dplyr, kableExtra, sf, 
               rnaturalearth, janitor, countrycode, stringr, shiny, bslib, 
               shinythemes, DT, ggrepel, viridis, shinyWidgets, shinydashboard, 
               plotly)

# =============== import data =================================================
data <- readRDS("Data/data.rds")
# =============================================================================
  

# =============== prepare data ================================================
# Filters groups (countries or continents) with at least one non-NA value
get_valid_groups <- function(df, group_col, year_pattern) {
  df |>
    pivot_longer(cols = matches(year_pattern), names_to = "year", 
                 values_to = "value") |>
    group_by(across(all_of(group_col))) |>
    summarise(non_na_count = sum(!is.na(value)), .groups = "drop") |>
    filter(non_na_count > 0) |>
    pull(!!sym(group_col)) # Return names of valid groups
}

# Get countries and continent medians with valid data
valid_countries <- get_valid_groups(data, "Country Name", "^(19|20)\\d{2}$")
valid_medians <- get_valid_groups(data, "region_un", "^(19|20)\\d{2}_median$")
# =============================================================================



# =============== User interface ==============================================

ui <- dashboardPage(
  dashboardHeader(title = "Yearly changes in sustainability indicators", 
                  titleWidth = 500),
  
  dashboardSidebar(collapsed = FALSE,
                   sidebarMenu(
                     menuItem("Line Graph", tabName = "line", 
                              icon = icon("chart-line")),
                     menuItem("Line Graph by Continent", 
                              tabName = "line-region", 
                              icon = icon("chart-line"))
  )),
  
  dashboardBody(
    tabItems(
# ========== Country tab UI ==========
      tabItem(tabName = "line",
              fluidRow(
                column(12, h2("Regional Trends by Country and Indicator"), 
                       align = "center")
              ),
              fluidRow(
                column(12,
p("Explore trends based on country selection and indicator of interest.",
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
                         choices = sort(unique(data$`Indicator Name`))
                       )
                )
              ),
              fluidRow(
                column(12,
                       plotlyOutput("regionPlot")
                )
              )
      ),
# ========== Continent tab UI ==========
      tabItem(tabName = "line-region",
              fluidRow(
                column(12, h2("Regional Trends by Continent and Indicator"), 
                       align = "center")
              ),
              fluidRow(
                column(12,
p("Explore trends based on continent selection and indicator of interest.",
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



# =============== Server ======================================================
server <- function(input, output, session) {

# ======== Continent tab ======================================================
  output$continentPlot <- renderPlotly({
    req(input$country_select_con, input$indicator_select_con)
    
    # Prepare filtered and reshaped data
    continent_data <- reactive({
      data |>
        filter(`region_un` == input$country_select_con,
               `Indicator Name` == input$indicator_select_con) |>
        pivot_longer(
          cols = matches("^(19|20)\\d{2}_median$"),
          names_to = "year",
          values_to = "value"
        ) |>
        mutate(year = as.integer(str_remove(year, "_median")))
    })
      
    
    # Generate plot
    p1 <- ggplot(continent_data(), aes(x = year, y = value)) +
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
# =============================================================================  
  

# ======== Country tab ========================================================
# Dynamically show indicators for which data is available based on country 
  observe({
    req(input$country_select)
    
    # Filter data for the selected country and get valid indicators
    valid_indicators <- data |>
      filter(`Country Name` == input$country_select) |>
      pivot_longer(cols = matches("^(19|20)\\d{2}$"), names_to = "year", 
                   values_to = "value") |>
      group_by(`Indicator Name`) |>
      summarise(non_na_count = sum(!is.na(value))) |>
      filter(non_na_count > 0) |>
      pull(`Indicator Name`)  # Extract valid indicators
    
    # Update the indicator choices
    updateSelectizeInput(session, "indicator_select", 
                         choices = valid_indicators, server = TRUE)
  })

  
  output$regionPlot <- renderPlotly({
    req(input$country_select, input$indicator_select)
    
    # Filter data by selected country and indicator
      country_data <- reactive({
        data |>
          filter(`Country Name` == input$country_select,
                 `Indicator Name` == input$indicator_select) |>
          pivot_longer(
            cols = matches("^(19|20)\\d{2}$"),
            names_to = "year",
            values_to = "value"
          ) |>
          mutate(year = as.integer(year))
      })
    
    # Generate plot
    p <- ggplot(country_data(), aes(x = year, y = value)) +
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


# ======= Run the app =========================================================
shinyApp(ui = ui, server = server)
