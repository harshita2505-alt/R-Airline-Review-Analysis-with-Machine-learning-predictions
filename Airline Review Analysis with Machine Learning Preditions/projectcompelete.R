# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(DT)
library(plotly)

# Load the data
airlines_reviews <- read.csv("C:\\Users\\harsh\\OneDrive\\Desktop\\airlines_reviews.csv")

# Preprocess data
airlines_reviews$Recommended <- as.factor(ifelse(airlines_reviews$Recommended == "yes", 1, 0))

# Train the Random Forest model with the full dataset
model_data <- airlines_reviews %>%
  select(Seat.Comfort, Staff.Service, Food...Beverages, Inflight.Entertainment, Value.For.Money, Overall.Rating, Recommended) %>%
  na.omit()

set.seed(123)
trainIndex <- createDataPartition(model_data$Recommended, p = 0.8, list = FALSE)
trainData <- model_data[trainIndex, ]
testData <- model_data[-trainIndex, ]
rf_model <- randomForest(Recommended ~ ., data = trainData, ntree = 100, mtry = 3, importance = TRUE)

# Model performance on test data
rf_predictions <- predict(rf_model, testData, type = "response")
conf_matrix <- confusionMatrix(rf_predictions, testData$Recommended)
feature_importance <- importance(rf_model)

# Define UI for the Shiny app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e6f9e6;
        font-family: Arial, sans-serif;
      }
      h1 {
        color: #2e8b57;
        font-size: 28px;
        font-weight: bold;
        text-align: center;
        background-color: #3cb371;
        padding: 12px;
        border-radius: 10px;
        color: white;
      }
      .container {
        padding: 20px;
        border: 1px solid #ddd;
        border-radius: 10px;
        background-color: #d4edda;
      }
      .sidebarPanel {
        background-color: #98fb98;
        padding: 20px;
        border-radius: 8px;
        margin-right: 10px;
      }
      .summary-title {
        color: #3c763d;
        font-size: 20px;
        font-weight: bold;
        margin-top: 15px;
      }
      .summary-content {
        color: #333;
        margin-bottom: 20px;
      }
      .highlight {
        color: #2e8b57;
        font-weight: bold;
      }
      .btn {
        background-color: #32cd32;
        color: white;
        border-radius: 8px;
        font-weight: bold;
      }
      .tab-pane {
        padding: 20px;
        background-color: #f0f9f0;
        border-radius: 8px;
      }
    "))
  ),
  
  titlePanel("Airlines Reviews Analysis with Machine Learning Predictions"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Analysis Options"),
      selectInput("airline", "Choose an Airline:", choices = unique(airlines_reviews$Airline)),
      selectInput("x_var", "Choose X-axis Variable:", choices = c("Seat.Comfort", "Staff.Service", "Food...Beverages")),
      selectInput("fill_var", "Choose Fill Variable:", choices = c("Recommended", "Class")),
      selectInput("chart_type", "Select Chart Type:", 
                  choices = c("Bar Chart", "Scatter Plot", "Pie Chart", "Line Chart", "Heatmap")),
      actionButton("analyze", "Analyze", class = "btn"),
      
      hr(),
      h4("Individual Prediction Inputs"),
      sliderInput("seat_comfort", "Seat Comfort:", min = 1, max = 5, value = 3),
      sliderInput("staff_service", "Staff Service:", min = 1, max = 5, value = 3),
      sliderInput("food_quality", "Food & Beverages Quality:", min = 1, max = 5, value = 3),
      sliderInput("value_for_money", "Value for Money:", min = 1, max = 5, value = 3),
      sliderInput("overall_rating", "Overall Rating:", min = 1, max = 5, value = 3),
      actionButton("predict", "Generate Individual Prediction", class = "btn"),
      
      hr(),
      actionButton("collective_predict", "Generate Collective Prediction Summary", class = "btn")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", uiOutput("summary")),
        tabPanel("Charts", plotlyOutput("charts")),
        tabPanel("Machine Learning Summary", uiOutput("ml_results")),
        tabPanel("Individual Prediction", uiOutput("prediction_output")),
        tabPanel("Collective Predictions", uiOutput("collective_predictions_output"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive data filtering based on user-selected airline
  filtered_data <- reactive({
    req(input$analyze)
    airlines_reviews[airlines_reviews$Airline == input$airline, ]
  })
  
  # Enhanced Descriptive Analysis: Summary statistics with detailed breakdowns
  output$summary <- renderUI({
    req(input$analyze)
    data <- filtered_data()
    
    # Calculate overall average ratings
    avg_ratings <- data %>% 
      summarise(
        Seat_Comfort = mean(as.numeric(Seat.Comfort), na.rm = TRUE),
        Staff_Service = mean(as.numeric(Staff.Service), na.rm = TRUE),
        Value_For_Money = mean(as.numeric(Value.For.Money), na.rm = TRUE)
      )
    
    # Segmented summary by class
    segment_summary <- data %>% 
      group_by(Class) %>% 
      summarise(
        Avg_Seat_Comfort = mean(as.numeric(Seat.Comfort), na.rm = TRUE),
        Avg_Staff_Service = mean(as.numeric(Staff.Service), na.rm = TRUE),
        Avg_Food_Quality = mean(as.numeric(Food...Beverages), na.rm = TRUE)
      )
    
    # Descriptive summary
    HTML(paste0(
      "<div class='summary-title'>Overall Average Ratings:</div>",
      "<div class='summary-content'>",
      "Seat Comfort: <span class='highlight'>", round(avg_ratings$Seat_Comfort, 2), "</span><br>",
      "Staff Service: <span class='highlight'>", round(avg_ratings$Staff_Service, 2), "</span><br>",
      "Value for Money: <span class='highlight'>", round(avg_ratings$Value_For_Money, 2), "</span>",
      "</div>",
      
      "<div class='summary-title'>Total Reviews for Selected Airline:</div>",
      "<div class='summary-content'><span class='highlight'>", nrow(data), "</span> reviews available for analysis.</div>",
      
      "<div class='summary-title'>Segmented Ratings by Class:</div>",
      "<div class='summary-content'>",
      paste(
        segment_summary %>% 
          mutate(Formatted = paste0("<div><strong>Class: ", Class, "</strong><br>",
                                    "Seat Comfort: ", round(Avg_Seat_Comfort, 2), "<br>",
                                    "Staff Service: ", round(Avg_Staff_Service, 2), "<br>",
                                    "Food Quality: ", round(Avg_Food_Quality, 2), "</div>")
          ) %>% pull(Formatted),
        collapse = ""
      ),
      "</div>"
    ))
  })
  
  # Machine Learning Model Output: Display accuracy and top 3 feature importance variables
  output$ml_results <- renderUI({
    HTML(paste0(
      "<div class='summary-title'>Model Performance Summary:</div>",
      "<div class='summary-content'>",
      "Accuracy: <span class='highlight'>", round(conf_matrix$overall['Accuracy'] * 100, 2), "%</span><br>",
      "</div>",
      
      "<div class='summary-title'>Top Feature Importance:</div>",
      "<div class='summary-content'>",
      paste(
        names(sort(feature_importance[,1], decreasing = TRUE))[1:3], ": ", 
        round(sort(feature_importance[,1], decreasing = TRUE)[1:3], 2), 
        collapse = "<br>"
      ),
      "</div>"
    ))
  })
  
  # Individual Prediction Output
  output$prediction_output <- renderUI({
    req(input$predict)
    
    # Use the slider inputs as features for prediction
    prediction_data <- data.frame(
      Seat.Comfort = input$seat_comfort,
      Staff.Service = input$staff_service,
      Food...Beverages = input$food_quality,
      Value.For.Money = input$value_for_money,
      Overall.Rating = input$overall_rating,
      Inflight.Entertainment = mean(model_data$Inflight.Entertainment, na.rm = TRUE)
    )
    
    # Generate prediction probability
    recommendation_prob <- predict(rf_model, prediction_data, type = "prob")[,2]
    
    # Display the result in color-coded format
    HTML(paste0(
      "<div class='summary-title'>Prediction Result:</div>",
      "<div class='summary-content'>",
      "Based on the selected ratings:<br>",
      "<span class='highlight'>Likelihood of Recommendation: ", 
      round(recommendation_prob * 100, 2), "%</span>",
      "</div>"
    ))
  })
  
  # Collective Prediction Summary and Simplified Output
  output$collective_predictions_output <- renderUI({
    req(input$collective_predict)
    
    # Filter and predict probabilities for the selected airline's reviews
    data_for_prediction <- filtered_data()
    data_for_prediction$Prediction_Prob <- predict(rf_model, data_for_prediction, type = "prob")[,2]
    
    # Summary of collective predictions
    avg_prob <- mean(data_for_prediction$Prediction_Prob, na.rm = TRUE)
    recommendation_count <- sum(data_for_prediction$Prediction_Prob > 0.5)
    total_reviews <- nrow(data_for_prediction)
    
    HTML(paste0(
      "<div class='summary-title'>Collective Prediction Summary:</div>",
      "<div class='summary-content'>",
      "Average Recommendation Probability: <span class='highlight'>", round(avg_prob * 100, 2), "%</span><br>",
      "Number of Recommended Reviews: <span class='highlight'>", recommendation_count, "</span> out of ", total_reviews,
      "</div>"
    ))
  })
  
  # Render Charts in the Charts Tab
  output$charts <- renderPlotly({
    req(input$analyze)
    data <- filtered_data()
    
    # Generate chart based on the selected chart type
    if (input$chart_type == "Bar Chart") {
      plot <- ggplot(data, aes_string(x = input$x_var, fill = input$fill_var)) +
        geom_bar(position = "dodge") +
        labs(title = "Bar Chart", x = input$x_var, fill = input$fill_var) +
        theme_minimal()
      
    } else if (input$chart_type == "Scatter Plot") {
      plot <- ggplot(data, aes_string(x = input$x_var, y = "Overall.Rating", color = input$fill_var)) +
        geom_point(size = 2, alpha = 0.7) +
        labs(title = "Scatter Plot", x = input$x_var, y = "Overall Rating") +
        theme_minimal()
      
    } else if (input$chart_type == "Line Chart") {
      plot <- ggplot(data, aes_string(x = input$x_var, y = "Overall.Rating", color = input$fill_var, group = input$fill_var)) +
        geom_line(size = 1) +
        labs(title = "Line Chart", x = input$x_var, y = "Overall Rating") +
        theme_minimal()
      
    } else if (input$chart_type == "Pie Chart") {
      pie_data <- data %>%
        group_by(.data[[input$fill_var]]) %>%
        summarise(Count = n())
      
      plot <- plot_ly(pie_data, labels = ~.data[[input$fill_var]], values = ~Count, type = 'pie') %>%
        layout(title = "Pie Chart")
      
    } else if (input$chart_type == "Heatmap") {
      plot <- ggplot(data, aes_string(x = input$x_var, y = "Overall.Rating", fill = input$fill_var)) +
        geom_tile() +
        labs(title = "Heatmap", x = input$x_var, y = "Overall Rating") +
        theme_minimal()
    }
    
    # Convert ggplot object to a Plotly object for interactivity
    ggplotly(plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
