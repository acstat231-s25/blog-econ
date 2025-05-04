# Load required libraries inside the shinylive chunk

library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)

co2_map <- readRDS("Data/co2_map.rds")
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


