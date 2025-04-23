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

# =============================================================================
# User interface 
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Race for Human Capital", titleWidth = 250),
  
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Line Graph", tabName = "line", 
                              icon = icon("chart-line"))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "line",
              fluidRow(
                column(12, h2("Regional Trends by Country and Indicator"), 
                       align = "center")
              )))
    )
  )

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {
  
 
}

# =============================================================================
# Run the app
# =============================================================================
shinyApp(ui = ui, server = server)
