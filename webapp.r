# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(GGally)
library(broom)

# Data Loading
salary_data <- read.table("salary.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)

# Data Processing
salary_data <- salary_data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(salary_increment = salary - lag(salary)) %>%
  mutate(avg_increment = mean(salary_increment, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(salary_increment), year >= 90, year <= 95)

cleaned_data <- salary_data %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup()

salary_data <- cleaned_data %>%
  filter(!is.nan(avg_increment)) %>%
  mutate(years_experience = year - startyr)

lm_w_gender_only <- lm(avg_increment~sex, data=salary_data)
summary(lm_w_gender_only)
print("###############################################################")

# Define predictors
predictors <- c("sex", "yrdeg", "rank", "admin", "deg", "field")

# Initialize results data frame
results_int <- data.frame(Model = character(), 
                          Predictors = character(), 
                          R2 = numeric(), 
                          Adj_R2 = numeric(), 
                          stringsAsFactors = FALSE)

# Compute linear models (including all interactions up to 6-way)
for (k in 1:length(predictors)) {
  combs <- combn(predictors, k, simplify = FALSE)
  for (comb in combs) {
    formula_str <- paste("avg_increment ~ (", paste(comb, collapse = " + "), ")^6")
    formula <- as.formula(formula_str)
    model <- lm(formula, data = salary_data)
    model_summary <- summary(model)
    r2 <- model_summary$r.squared
    adj_r2 <- model_summary$adj.r.squared
    results_int <- rbind(results_int, 
                         data.frame(Model = formula_str, 
                                    Predictors = paste(comb, collapse = ", "), 
                                    R2 = r2, 
                                    Adj_R2 = adj_r2,
                                    stringsAsFactors = FALSE))
  }
}

results_int <- results_int[order(-results_int$Adj_R2), ]
print(results_int)
print("-----------------------------------------------------------------")

# Extract the top 5 models with the highest Adjusted R-squared.
top5 <- head(results_int, 5)
top5_vars <- top5[, c("Model", "Predictors", "R2", "Adj_R2")]
print(top5_vars)

################# Shiny App creation begins below ###########################

ui <- dashboardPage(
  dashboardHeader(title = "Gender Bias in Salary Increments"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Model", tabName = "interactive", icon = icon("chart-line")),
      menuItem("Static Visualizations", tabName = "static", icon = icon("chart-bar")),
      checkboxGroupInput(
        inputId = "predictors",
        label   = "Select additional predictors to include:",
        choices = c("yrdeg", "rank", "admin", "deg", "field"),
        selected = NULL
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Interactive Model Tab
      tabItem(tabName = "interactive",
              fluidRow(
                box(title = "Salary Increment vs Sex", width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("model_plot")),
                box(title = "Model Summary", width = 6, status = "info", solidHeader = TRUE,
                    verbatimTextOutput("model_summary"),
                    br(),
                    textOutput("r2_text")
                )
              )
      ),
      # Static Visualizations Tab
      tabItem(tabName = "static",
              fluidRow(
                box(width = 12, status = "warning", solidHeader = TRUE,
                    h3("Salary Increment by Degree - Means are roughly the same"),
                    fluidRow(
                      column(4, h4("PhD Holders"), plotOutput("static_plot_phd")),
                      column(4, h4("Prof Degree Holders"), plotOutput("static_plot_prof")),
                      column(4, h4("Other Degree Holders"), plotOutput("static_plot_other"))
                    ),
                    br(),
                    h3("Years of Experience by Gender"),
                    fluidRow(
                      column(12, plotOutput("experience_by_sex_plot"))
                    ),
                    h3("Years of Experience vs Salary Increment"),
                    fluidRow(
                      column(12, plotOutput("experience_plot"))
                    ),
                    br(),
                    h3("Salary Increment by Field - Means roughly the same"),
                    fluidRow(
                      column(4, h4("Field: Other"), plotOutput("field_other_plot")),
                      column(4, h4("Field: Arts"), plotOutput("field_arts_plot")),
                      column(4, h4("Field: Prof"), plotOutput("field_prof_plot"))
                    )
                )
              )
      )
    )
  )
)  # End of dashboardPage

server <- function(input, output, session) {
  # Reactive expression: Build the model formula based on user-selected predictors.
  model_formula <- reactive({
    additional <- input$predictors
    if (length(additional) > 0) {
      formula_str <- paste("avg_increment ~ (sex +", paste(additional, collapse = " + "), ")^6")
    } else {
      formula_str <- "avg_increment ~ sex"
    }
    as.formula(formula_str)
  })
  
  # Reactive expression: Fit the linear model using the dynamic formula.
  model_fit <- reactive({
    lm(model_formula(), data = salary_data)
  })
  
  # Plot for the Interactive Model tab with an annotation showing the model formula.
  output$model_plot <- renderPlot({
    p <- ggplot(salary_data, aes(x = sex, y = salary_increment, fill = sex)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(title = "Salary Increment by Sex", x = "Sex", y = "Salary Increment") +
      theme_minimal()
    
    # Convert the reactive formula to text and add it as an annotation.
    formula_text <- paste("Model:", deparse(model_formula()))
    p + annotate("text", 
                 x = 1.5, 
                 y = max(salary_data$salary_increment, na.rm = TRUE) * 0.95, 
                 label = formula_text, 
                 size = 4, 
                 color = "black")
  })
  
  # Render the full model summary with the model formula printed above.
  output$model_summary <- renderPrint({
    cat("Model Formula:\n")
    print(model_formula())
    cat("\nModel Summary:\n")
    print(summary(model_fit()))
  })
  
  # Display the R-squared value below the interactive plot.
  output$r2_text <- renderText({
    mod_sum <- summary(model_fit())
    r2_val <- round(mod_sum$r.squared, 3)
    paste("R²:", r2_val)
  })
  
  # Static Visualizations

  # Years of Experience by Gender plot
  output$experience_by_sex_plot <- renderPlot({
    ggplot(salary_data, aes(x = sex, y = years_experience, fill = sex)) +
      geom_boxplot() +
      labs(title = "Years of Experience by Gender - Men have over 6 years more than women", 
           x = "Sex", y = "Years of Experience") +
      theme_minimal()
  })

  # Years of Experience vs Salary Increment plot
  output$experience_plot <- renderPlot({
    ggplot(salary_data, aes(x = years_experience, y = salary_increment, color = sex)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Years of Experience vs Salary Increment by Gender - Salary increment and experience is uncorrelated", 
           x = "Years of Experience", y = "Salary Increment") +
      theme_minimal()
  })
  

  
  # Function to create a degree-based static plot
  create_degree_plot <- function(degree) {
    renderPlot({
      salary_data %>% filter(deg == degree) %>%
        ggplot(aes(x = sex, y = salary_increment, fill = sex)) +
        geom_boxplot() +
        labs(title = paste("Salary Increment for", degree), 
             x = "Sex", y = "Salary Increment") +
        theme_minimal()
    })
  }
  
  output$static_plot_phd <- create_degree_plot("PhD")
  output$static_plot_prof <- create_degree_plot("Prof")
  output$static_plot_other <- create_degree_plot("Other")
  
  # Function to create a field-based static plot
  create_field_plot <- function(field_name) {
    renderPlot({
      salary_data %>% filter(field == field_name) %>%
        ggplot(aes(x = sex, y = salary_increment, fill = sex)) +
        geom_boxplot() +
        labs(title = paste("Salary Increment for Field:", field_name), 
             x = "Sex", y = "Salary Increment") +
        theme_minimal()
    })
  }
  
  output$field_other_plot <- create_field_plot("Other")
  output$field_arts_plot <- create_field_plot("Arts")
  output$field_prof_plot <- create_field_plot("Prof")
}

# Run the Shiny App
shinyApp(ui, server)
