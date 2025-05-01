# Load libraries
library(shiny)
library(randomForest)
library(tidyr)    
library(dplyr)     
library(tibble)      
library(broom)   
library(janitor)

co2_map <- readRDS("Data/co2_map.rds")
climate1 <- read.csv("Data/climate-change.csv", skip = 3)
climate1 <- climate1 |>
  row_to_names(row_number = 1)|>
  select(-"Indicator Code") 

# Seed for Reproducibility
set.seed(631)

climate_long <- climate1 |> 
  # 1) Filter for the four indicators in the image
  filter(
    `Indicator Name` %in% c(
      "Renewable energy consumption (% of total final energy consumption)",
      "Renewable electricity output (% of total electricity output)",
      "Electricity production from oil sources (% of total)",
      "Electricity production from coal sources (% of total)"
    )
  ) |> 
  # 2) Keep only the essential columns
  select(
    "Country Code",
    "Country Name",
    "Indicator Name",
    matches("^(19|20)\\d{2}$")
  ) |> 
  
  mutate(across(matches("^(19|20)\\d{2}$"), as.numeric)) |> 
  # 5) Pivot longer to get Year and Value
  pivot_longer(
    cols      = matches("^(19|20)\\d{2}$"),
    names_to  = "Year",
    values_to = "Value"
  ) |> 
  # 6) Pivot wider to separate each indicator into its own column
  pivot_wider(
    names_from  = `Indicator Name`,
    values_from = Value
  ) |> 
  drop_na() |> 
  mutate(Year = as.numeric(Year))

names(climate_long) <- c("Country Code", "Country Name", "Year",
                         "Renew_Cons", "Renew_Elec",   "Oil_Elec",
                         "Coal_Elec")



# Remove geometry and delete NA values
co2_cleaned <- co2_map |> 
  sf::st_drop_geometry(data_all) |> 
  drop_na() |> 
  select(`Country Code`, `Country Name`, `Year`, `co2conc`)
  



co2_numeric <- co2_cleaned |> 
  select(co2conc)

# Scale the data
co2_scaled <- scale(co2_numeric) 

# Clustering
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





# Choose a training sample of 70% 
sample_index <- sample(1:nrow(co2_final), 0.7 * nrow(co2_final))

train_data <- co2_final[sample_index, ]
#Choose a testing sample of the remaining 30%
test_data <- co2_final[-sample_index, ]

# Build a Forest learning algorithm
rf_model <- randomForest(Cluster ~ Renew_Cons + Renew_Elec + Oil_Elec 
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





library(shiny)
library(randomForest)

# UI
ui <- fluidPage(
  titlePanel("Predict Energy Cluster"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("renew_cons", "Renewable Consumption (%)",
                  min = 0, max = 100, value = 50),
      sliderInput("renew_elec", "Renewable Electricity Output (%)",
                  min = 0, max = 100, value = 50),
      sliderInput("oil_elec", "Electricity from Oil (%)", min = 0,
                  max = 100, value = 30),
      sliderInput("coal_elec", "Electricity from Coal (%)", min = 0,
                  max = 100, value = 30),
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
      Coal_Elec  = input$coal_elec
    )
    
    # Predict cluster using trained rf_model
    predicted_cluster <- predict(rf_model, new_data)
    
    output$prediction <- renderText({
      paste("Predicted Cluster:", predicted_cluster)
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)

