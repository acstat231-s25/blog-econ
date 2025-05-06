# Load the required libraries 
library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)

#-----------------------------------------------------------------------------#

## Data Wrangling


#-----------------------------------------------------------------------------#





data <- readRDS("Data/data.rds")
# Create a long CO2 df version:
co2_map <- data |> 
  filter(`Indicator Name` == 
"Carbon dioxide (CO2) emissions (total) excluding LULUCF (% change from 1990)")|> 
  select(1:36, geometry) |> 
  mutate(across(matches("^(19|20)\\d{2}$"), as.numeric)) |>
  #Pivot longer: create one row per Year and Value
  pivot_longer(!c(`Country Name`, `Country Code`, `Indicator Name`, geometry), 
               names_to  = "Year",
               values_to = "co2conc" ) |> 
  # Cap co2conc at 500 % to avoid outliers
  mutate(co2conc = pmin(co2conc, 500)) |> 
  mutate(Year = as.numeric(Year)) 


#-----------------------------------------------------------------------------#

## User Interface


#-----------------------------------------------------------------------------#


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:",
                  #Allow the user to choose a year between 1994 and 2022 
                  min = 1991, max = 2022, value = 1991,
                  step = 1, sep = "", 
                  # Allow the user to animate graph
                  animate = animationOptions(interval = 1000))
    ),
    mainPanel(
      plotOutput("mapPlot", width = "800px", height = "400px")
    )
  )
)


#-----------------------------------------------------------------------------#

## Function


#-----------------------------------------------------------------------------#


server <- function(input, output) {
  filtered <- reactive(co2_map |> filter(Year == input$year))
  output$mapPlot <- renderPlot({
    ggplot(filtered()) +
      geom_sf(aes(geometry = geometry, fill = co2conc),
              colour = "grey40", linewidth = .2) +
      scale_fill_viridis_c(option = "viridis", na.value = "grey90",
                           name = "% change vs 1990",
                           limits = c(-100, 500)) +
      coord_sf(crs = "+proj=robin", expand = FALSE) +
      theme_void(base_size = 16) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8)) +
      labs(title = paste0("CO₂ emissions relative to 1990: ", input$year))
  })
}

shinyApp(ui, server)