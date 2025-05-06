# =============================================================================
# This Shiny app is used to be deployed on the html website
# =============================================================================

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, ggplot2, lubridate, dplyr, kableExtra, sf, 
               rnaturalearth, janitor, countrycode, stringr, shiny, bslib, 
               shinythemes, DT, ggrepel, viridis, shinyWidgets, shinydashboard, 
               plotly)

# =============== import data =================================================
load("Data/data.RData")

#rename variables to fit the plot box
data <- data |>
  mutate(`Indicator Name` = case_when(
    `Indicator Name` == 
      "Renewable energy consumption (% of total final energy consumption)" ~ 
      "Renewable energy consumption (% of total)",
    `Indicator Name` == 
      "Renewable electricity output (% of total electricity output)" 
    ~ "Renewable electricity output (% of total)",
    `Indicator Name` == 
      "Electricity production from oil sources (% of total)" 
    ~ "Electricity production from oil (% of total)",
    `Indicator Name` == 
      "Electricity production from natural gas sources (% of total)" 
    ~ "Electricity from natural gas (% of total)",
    `Indicator Name` == 
      "Electricity production from coal sources (% of total)" ~ 
      "Electricity production from coal (% of total)",
    `Indicator Name` == 
"Carbon dioxide (CO2) emissions (total) excluding LULUCF (% change from 1990)" ~ 
      "CO2 emissions (% change from 1990)",
    #keep other values unchanged
    TRUE ~ `Indicator Name`  
  ))
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
          column(12, h2("Regional trends by country"), 
                 align = "center")
        ),
        fluidRow(
          column(12,
                 p("Explore trends based on country, indicator and year.",
                   style = "font-size: 12px"),
                 align = "center"
          )
        ),
        #output country, variable and year in one row
        fluidRow(
          column(4,
                 selectizeInput("country_select", "Choose a Country:",
                                choices = sort(valid_countries))
          ),
          column(4,
                 selectizeInput("indicator_select", "Choose an Indicator:",
                                choices = sort(unique(data$`Indicator Name`)))
          ),
          column(4,
                 selectInput("year_select", "Choose a Year:",
                             choices = sort(
                               unique(
                                 as.integer(
                                   str_extract(names(data), "^(19|20)\\d{2}$")  # Extracts years like 1990, 2001, etc.
                                 )
                               )
                             ),
                             selected = 1991))
        ),
        fluidRow(
          column(12,
                 plotlyOutput("regionPlot")
          )
        ),
        br(),
        fluidRow(
          column(12,
                 plotOutput("mapPlot", height = "450px")
          )
        )
),
# ========== Continent tab UI ==========
      tabItem(tabName = "line-region",
              fluidRow(
                column(12, h2("Regional trends by continent"), 
                       align = "center")
              ),
              fluidRow(
                column(12,
p("Explore trends based on continent and indicator.",
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
    
    # Prepare filtered median data
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
      
    
    #generate plot
    p1 <- ggplot(continent_data(), aes(x = year, y = value)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = paste(input$indicator_select_con),
        subtitle = paste("Continent:", input$country_select_con),
        x = "Year",
        y = input$indicator_select_con
      ) +
      theme_minimal()+
      theme(
        axis.text.y = element_text(size = 8) 
      )
    
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
      #Output only those variables, that have at least one non zero value
      summarise(non_na_count = sum(!is.na(value))) |>
      filter(non_na_count > 0) |>
      pull(`Indicator Name`)  # Extract valid indicators
    
    # Update the indicator choices
    updateSelectizeInput(session, "indicator_select", 
                         choices = valid_indicators, server = TRUE)
  })

  # Generate plotly output
  output$regionPlot <- renderPlotly({
    req(input$country_select, input$indicator_select)
    
    # Filter data by selected country and indicator
      country_data <- data |>
        filter(`Country Name` == input$country_select,
               `Indicator Name` == input$indicator_select) |>
        pivot_longer(
          cols = matches("^(19|20)\\d{2}$"),
          names_to = "year",
          values_to = "value"
        ) |>
        mutate(year = as.integer(year))
    
    # Generate plot
    p <- ggplot(country_data, aes(x = year, y = value)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = paste(input$indicator_select),
        subtitle = paste("Country:", input$country_select),
        x = "Year",
        y = input$indicator_select
      ) +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 14, hjust = 0.5)
      )
    
    ggplotly(p)
  })
  
  # Map plot generation
  output$mapPlot <- renderPlot({
    req(input$indicator_select, input$year_select)
    # Transform the selected year as a character value to use as column name
    year_col <- as.character(input$year_select)
    # Filter data based on the selected indicator and year
    map_data <- data |>
      filter(`Indicator Name` == input$indicator_select) |>
      select(`Country Name`, geometry, !!year_col) |>
      rename(value = !!year_col)
   
    # Plot the map
    ggplot(map_data) +
      geom_sf(aes(geometry = geometry, fill = value),
              color = "grey", linewidth = 0.2) +
      scale_fill_viridis_c(option = "viridis", na.value = "grey90",
                           name = input$indicator_select) +
      theme(legend.position = "top",
            legend.text = element_text(size = 8),
            legend.key.width = unit(2.5, "cm"),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      labs(title = paste(input$indicator_select, "in", input$year_select))+
      theme(
        plot.title = element_text(size = 20, hjust = 0.5)
      )
  })
  
}
# ============================================================================= 


# ======= Run the app =========================================================
shinyApp(ui = ui, server = server)
