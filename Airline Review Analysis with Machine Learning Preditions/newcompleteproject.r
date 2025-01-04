library(shiny)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(DT)
library(plotly)
library(shinythemes)

# Sample user credentials
valid_users <- data.frame(
  username = c("admin", "user1"),
  password = c("password123", "mypassword")
)

# Load the data
airlines_reviews <- read.csv("C:\\Users\\harsh\\OneDrive\\Desktop\\airlines_reviews.csv")

# Preprocess data
airlines_reviews$Recommended <- as.factor(ifelse(airlines_reviews$Recommended == "yes", 1, 0))
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

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"), # Lighter color scheme for the theme
  uiOutput("main_ui") # Dynamic UI for login or app content
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to track login status
  logged_in <- reactiveVal(FALSE)
  
  
  # Reactive data filtering based on user-selected airline
  filtered_data <- reactive({
    req(input$analyze)
    airlines_reviews[airlines_reviews$Airline == input$airline, ]
  })
  
  
  # Render the main UI based on login status
  output$main_ui <- renderUI({
    if (logged_in()) {
      # Home page UI with all functionalities
      fluidPage(
        titlePanel(
          div(
            tags$img(src = "https://i.pinimg.com/736x/8b/91/39/8b9139ada178009954fb16c5244b7c3a.jpg", height = "50px", style = "margin-right: 10px;"),
            "Airlines Reviews Analysis App"
          )
        ),
        navbarPage(
          title = "Explore Airlines Data",
          tabPanel("Home",
                   div(
                     style = "text-align: center; margin: 20px;",
                     tags$img(
                       src = "https://i.pinimg.com/736x/f9/42/89/f942892a194995d3e3a38175156d269e.jpg",
                       height = "300px",
                       style = "margin-bottom: 20px; border-radius: 10px;"
                     ),
                     h1("Welcome to the Airlines Reviews Analysis App"),
                     p("Analyze customer reviews, generate predictions, and explore insights.")
                   )),
          tabPanel("Data Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       h4("Select Analysis Options"),
                       selectInput("airline", "Choose an Airline:", choices = unique(airlines_reviews$Airline)),
                       selectInput("x_var", "Choose X-axis Variable:", choices = c("Seat.Comfort", "Staff.Service", "Food...Beverages")),
                       selectInput("fill_var", "Choose Fill Variable:", choices = c("Recommended", "Class")),
                       selectInput("chart_type", "Select Chart Type:", choices = c("Bar Chart", "Scatter Plot", "Pie Chart", "Line Chart", "Heatmap")),
                       actionButton("analyze", "Analyze", class = "btn btn-primary"),
                       hr(),
                       h4("Individual Prediction Inputs"),
                       sliderInput("seat_comfort", "Seat Comfort:", min = 1, max = 5, value = 3),
                       sliderInput("staff_service", "Staff Service:", min = 1, max = 5, value = 3),
                       sliderInput("food_quality", "Food & Beverages Quality:", min = 1, max = 5, value = 3),
                       sliderInput("value_for_money", "Value for Money:", min = 1, max = 5, value = 3),
                       sliderInput("overall_rating", "Overall Rating:", min = 1, max = 5, value = 3),
                       actionButton("predict", "Generate Prediction", class = "btn btn-success"),
                       hr(),
                       actionButton("collective_predict", "Generate Collective Predictions", class = "btn btn-warning")
                     ),
                     mainPanel(
                       tabsetPanel(
                         tabPanel("Summary", uiOutput("summary")),
                         tabPanel("Charts", plotlyOutput("charts")),
                         tabPanel("Machine Learning Summary", uiOutput("ml_results")),
                         tabPanel("Predictions", uiOutput("prediction_output")),
                         tabPanel("Collective Predictions", uiOutput("collective_predictions_output"))
                       )
                     )
                   )),
          tabPanel("About",
                   div(
                     style = "text-align: center; padding: 20px;",
                     h2("About This Project"),
                     p("This app is designed to provide insights into customer reviews of airlines"),
                     tags$ul(
                       tags$li("Understand customer satisfaction metrics."),
                       tags$li("Visualize trends in reviews based on different variables."),
                       tags$li("Predict the likelihood of a customer recommending an airline."),
                       tags$li("Explore the performance of airlines across various parameters.")
                     ),
                     tags$img(
                       src = "https://i.pinimg.com/736x/30/db/39/30db3919b34cb4e094cd5511684cda80.jpg",
                       height = "300px",
                       style = "margin-top: 20px; border-radius: 10px;"
                     )
                   )),
          tabPanel("Logout", actionButton("logout", "Logout", class = "btn btn-danger"))
        )
      )
    } else {
      # Login page UI
      fluidPage(
        tags$head(
          tags$style(HTML("
            body {
              background: #f8f9fa;
              color: #343a40;
              font-family: Arial, sans-serif;
            }
            .login-box {
              max-width: 400px;
              margin: 100px auto;
              padding: 20px;
              background-color: #ffffff;
              border: 1px solid #ced4da;
              border-radius: 10px;
              box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            }
          "))
        ),
        div(
          class = "login-box",
          h2("Login", style = "text-align: center; color: #343a40;"),
          textInput("username", "Username", placeholder = "Enter your username"),
          passwordInput("password", "Password", placeholder = "Enter your password"),
          actionButton("login", "Login", class = "btn btn-primary btn-block"),
          tags$p("Enter valid credentials to continue.", style = "text-align: center; margin-top: 10px; color: #666;")
        )
      )
    }
  })
  
  # Login logic
  observeEvent(input$login, {
    req(input$username, input$password)
    if (any(valid_users$username == input$username & valid_users$password == input$password)) {
      logged_in(TRUE)
    } else {
      showNotification("Invalid username or password.", type = "error")
    }
  })
  
  # Logout logic
  observeEvent(input$logout, {
    logged_in(FALSE)
  })
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
