# Load libraries
library(shiny)
library(randomForest)
library(tidyr)    
library(dplyr)     
library(tibble)      
library(broom)   
library(janitor)

# Load the data
co2_map <- readRDS("Data/co2_map.rds")
climate_long <- readRDS("Data/climate_long.rds")

# Remove geometry and delete NA values
co2_cleaned <- co2_map |> 
  sf::st_drop_geometry(data_all) |> 
  drop_na() |> 
  select(`Country Code`, `Country Name`, `Year`, `co2conc`)
  

co2_numeric <- co2_cleaned |> 
  select(co2conc)

# Normalize the data
co2_scaled <- scale(co2_numeric) 

# Clustering

# Seed for Reproducibility
set.seed(631)

kmeans_result <- kmeans(co2_scaled, centers = 4, nstart = 25)

co2_cleaned$Cluster <- as.factor(kmeans_result$cluster)

co2_final <- co2_cleaned |> 
  right_join(climate_long, by =  c("Country Name", "Country Code", "Year")) |> 
  drop_na() 

co2_final <- co2_final |> 
  mutate(Cluster = case_when(
    Cluster == "1" ~ "Extreme Increase",
    Cluster == "2" ~ "Decrease",
    Cluster == "3" ~ "Moderate Increase",
    Cluster == "4" ~ "High Increase"
  )) 

# Set Clusters as factor
co2_final <- co2_final |> 
  mutate(Cluster = as.factor(Cluster))

# Set Seed fed for data training
set.seed(150)

# Train data on values from 1990 to 2014
train_data <- co2_final[co2_final$Year != 2015, ]


# Test data based on 2015
test_data <- co2_final[co2_final$Year == 2015,] 

# Build a Forest learning algorithm
rf_model <- randomForest(Cluster ~ Renew_Cons + Renew_Elec + Oil_Elec + Gas_Elec
                         + Coal_Elec,
                         data = train_data, ntree = 100)

# Draw predictions based on the previous table
predictions <- predict(rf_model, test_data)

# Confusion Matrix
confusion_matrix <- table(Actual = test_data$Cluster, Predicted = predictions)
print(confusion_matrix)

# Calculate Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Prediction Accuracy:", round(accuracy, 3)))

# ShinyApp

library(shiny)
library(randomForest)

# UI
ui <- fluidPage(
  titlePanel("Predict Energy Cluster"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("renew_cons", "Renewable Consumption (%)",
                  min = 0, max = 100, value = 50),
      sliderInput("renew_elec", "Electricity production from Renewable sources (%)",
                  min = 0, max = 100, value = 50),
      sliderInput("oil_elec", "Electricity production from Oil (%)", min = 0,
                  max = 100, value = 30),
      sliderInput("coal_elec", "Electricity production from Coal (%)", min = 0,
                  max = 100, value = 10),
      sliderInput("gas_elec", "Electricity production from gas (%)", min = 0,
                  max = 100, value = 10),
      actionButton("predictBtn", "Predict Cluster")
    ),
    
    mainPanel(
      verbatimTextOutput("prediction")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predictBtn, {
    # Create new data point
    new_data <- data.frame(
      Renew_Cons = input$renew_cons,
      Renew_Elec = input$renew_elec,
      Oil_Elec   = input$oil_elec,
      Coal_Elec  = input$coal_elec,
      Gas_Elec  = input$gas_elec
    )
    
    # Predict cluster using trained rf_model
    predicted_cluster <- predict(rf_model, new_data)
    
    output$prediction <- renderText({
      if (input$coal_elec +  input$oil_elec + input$renew_elec 
          + input$gas_elec > 100 ) {
        paste("The electricity production shouldn't add up to more than 100")
      } else{
      paste("The model predicts the following CO2 change:", predicted_cluster)}
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)

