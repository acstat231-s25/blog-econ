library(shiny)
library(randomForest)
library(tidyr)    
library(dplyr)     
library(tibble)      
library(broom)   
library(janitor)

# See for Reproducibility
set.seed(123)

climate1 <- read.csv("raw-data/climate-change.csv", skip = 3)
climate1 <- climate1 |>
  row_to_names(row_number = 1)|>
  select(-"Indicator Code") 

# vector for selected variables of interest
sustain_indicators <- c(
  "Renewable energy consumption (% of total final energy consumption)",
  "Renewable electricity output (% of total electricity output)",
  "Electricity production from oil sources (% of total)",
  "Electricity production from coal sources (% of total)")

# Filter 2015
sustainability2015 <- climate1 |>
  filter(`Indicator Name` %in% sustain_indicators) |>
  select(`Country Name`, `Country Code`, `Indicator Name`, `2015`) |>
  pivot_wider(names_from = `Indicator Name`, 
              values_from = `2015`) |>
  drop_na()

sustain_2015_std <- sustainability2015 |>
  mutate(across(where(is.numeric)
                , ~ (.x - mean(.x)) / sd(.x)
                , .names = "{.col}_z")) |>
  select(ends_with("_z"))

sustain_2015_kmeans4 <- sustain_2015_std |>
  kmeans(centers = 4, nstart = 20)

sustain_2015_c4 <- augment(sustain_2015_kmeans4, sustainability2015) |>
  rename(cluster_2015 = .cluster)

# Rename

names(sustain_2015_c4) <- c("Country_Name", "Country Code",
                            "Renew_Cons", "Renew_Elec",   "Oil_Elec", "Coal_Elec", "Cluster")

# Choose a training sample of 70% 
sample_index <- sample(1:nrow(sustain_2015_c4), 0.7 * nrow(sustain_2015_c4))

train_data <- sustain_2015_c4[sample_index, ]

# Choose a testing sample of the remianing 30%

test_data <- sustain_2015_c4[-sample_index, ]

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

