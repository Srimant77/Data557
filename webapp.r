# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(GGally)
library(broom)

####################################
# Data Loading & Preprocessing     #
####################################
salary_data <- read.table("salary.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)

salary_data <- salary_data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(salary_increment = salary - lag(salary)) %>%
  mutate(avg_increment = mean(salary_increment, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(salary_increment), year >= 90, year <= 95)

# Generate statistics
cleaned_data <- salary_data %>%
  group_by(id) %>%
  slice_tail(n=1) %>%
  ungroup()

cleaned_data_again <- cleaned_data[!is.nan(cleaned_data$avg_increment),]
salary_data <- cleaned_data_again
print("###########################################################")

# Converting sex from a character vector to a factor
# This will allow us to see which is the reference gender in the linear model
# salary_data$sex <- as.factor(salary_data$sex)
# levels(salary_data$sex)

lm_w_sex_only <- lm(avg_increment ~ salary_data$sex, data = salary_data)
summary(lm_w_sex_only)

print("-----------------------------------------------------------")

# Define your predictors
predictors <- c("sex", "yrdeg", "rank", "admin", "deg", "field")

# Data frame to store the results with interactions
results_int <- data.frame(Model = character(), 
                          Predictors = character(), 
                          R2 = numeric(), 
                          Adj_R2 = numeric(), 
                          stringsAsFactors = FALSE)

# Loop over all non-empty combinations of predictors
for (k in 1:length(predictors)) {
  combs <- combn(predictors, k, simplify = FALSE)
  for (comb in combs) {
    # Construct the formula with interactions: (predictors)^2 expands to main effects + all 2-way interactions
    formula_str <- paste("avg_increment ~ (", paste(comb, collapse = " + "), ")^6")
    formula <- as.formula(formula_str)
    
    # Fit the linear model with interaction terms
    model <- lm(formula, data = salary_data)
    model_summary <- summary(model)
    
    # Extract R-squared and Adjusted R-squared
    r2 <- model_summary$r.squared
    adj_r2 <- model_summary$adj.r.squared
    
    # Append the result to our results data frame
    results_int <- rbind(results_int, 
                         data.frame(Model = formula_str, 
                                    Predictors = paste(comb, collapse = ", "), 
                                    R2 = r2, 
                                    Adj_R2 = adj_r2,
                                    stringsAsFactors = FALSE))
  }
}

# Sort results by the highest Adjusted R-squared
results_int <- results_int[order(-results_int$Adj_R2), ]
print(results_int)

print("-----------------------------------------------------------------")

top5 <- head(results_int, 5)
top5_vars <- top5[, c("Model", "Predictors", "R2", "Adj_R2")]

# Print the table in the console
print(top5_vars)



# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Gender Bias in Salary Increments"),
  dashboardSidebar(
    sidebarMenu(
      checkboxGroupInput(
        inputId = "predictors",
        label   = "Select additional predictors to include:",
        choices = c("yrdeg", "rank", "admin", "deg", "field"),
        selected = NULL
      )
    )
  ),
  dashboardBody(
    fluidRow(
      box(title = "Salary Increment vs Sex", width = 6, status = "primary", solidHeader = TRUE,
          plotOutput("model_plot")),
      box(title = "Model Summary", width = 6, status = "info", solidHeader = TRUE,
          verbatimTextOutput("model_summary"),
          br(),
          textOutput("r2_text")
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive: Build model formula based on user selection.
  model_formula <- reactive({
    additional <- input$predictors
    if (length(additional) > 0) {
      # Build a formula that includes sex plus the additional predictors with all interactions (up to ^6)
      formula_str <- paste("avg_increment ~ (sex +", paste(additional, collapse = " + "), ")^6")
    } else {
      formula_str <- "avg_increment ~ sex"
    }
    as.formula(formula_str)
  })
  
  # Reactive: Fit the model
  model_fit <- reactive({
    lm(model_formula(), data = salary_data)
  })
  
  # Plot: Display salary_increment vs sex along with model details for sex
  output$model_plot <- renderPlot({
    p <- ggplot(salary_data, aes(x = sex, y = salary_increment, fill = sex)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(title = "Salary Increment by Sex", x = "Sex", y = "Salary Increment") +
      theme_minimal()
    
    # Get model summary to extract coefficients for sex.
    mod_sum <- summary(model_fit())
    # For the sex variable, R creates a dummy coefficient (usually "sexM" if levels are "F" and "M").
    if ("sexM" %in% rownames(mod_sum$coefficients)) {
      intercept <- round(mod_sum$coefficients["(Intercept)", "Estimate"], 2)
      slope <- round(mod_sum$coefficients["sexM", "Estimate"], 2)
      p_val <- mod_sum$coefficients["sexM", "Pr(>|t|)"]
      p_val <- ifelse(p_val < 0.001, "<0.001", round(p_val, 3))
      ann_text <- paste("Intercept:", intercept, "\nSlope (sexM):", slope, "\np-value:", p_val)
      p <- p + annotate("text", x = 1.5, y = max(salary_data$salary_increment, na.rm = TRUE) * 0.9, 
                        label = ann_text, hjust = 0)
    }
    
    print(p)
  })
  
  # Show full model summary along with the actual formula
  output$model_summary <- renderPrint({
    cat("Model Formula:\n")
    print(model_formula())
    cat("\nModel Summary:\n")
    print(summary(model_fit()))
  })
  
  # Display the R^2 value below the plot
  output$r2_text <- renderText({
    mod_sum <- summary(model_fit())
    r2_val <- round(mod_sum$r.squared, 3)
    paste("R²:", r2_val)
  })
  
}

# Run the Shiny app
shinyApp(ui, server)