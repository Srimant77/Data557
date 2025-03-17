library(tidyverse)
library(dplyr)
library(GGally)
library(broom)

# Data Loading
salary_data <- read.table("salary.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)

# Data Processing
salary_data <- salary_data %>%
  filter(between(year, 90, 95)) %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(salary_increment = salary - lag(salary)) %>%
  mutate(avg_increment = mean(salary_increment, na.rm = TRUE)) %>%
  mutate(percentraise = (salary - lag(salary)) / lag(salary) * 100) %>%
  mutate(avgraise = mean(percentraise, na.rm = TRUE)) %>%
  mutate(promoted = ifelse(rank == first(rank) & rank == last(rank), 0, 1)) %>%
  ungroup()

cleaned_data <- salary_data %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup()

salary_data_static <- cleaned_data %>%
  filter(!is.nan(avgraise)) %>%
  mutate(years_experience = year - startyr)

# Helper functions
interactive_section <- function(input, output) {
  # Get data
  final_data <- reactive({
    min_years <- input$years
    ranks <- input$ranks
    fields <- input$fields
    degrees <- input$deg
    req(input$promo)
    promo_filter <- case_when(
      input$promo == "Include Promoted" ~ c(0, 1),
      input$promo == "Exclude Promoted" ~ c(0),
      input$promo == "Only Promoted" ~ c(1)
    )
    
    # Collapse data
    collapsed_data <- salary_data %>%
      group_by(id) %>%
      filter(n() >= min_years) %>%
      slice_tail(n = 1) %>%
      ungroup()
    
    # Apply remaining filters
    data <- collapsed_data %>%
      filter(
        rank %in% ranks,
        field %in% fields,
        deg %in% degrees,
        promoted %in% promo_filter,
        !is.nan(avgraise)
      )
    
    data
  })
  
  # Fit model
  model_formula <- reactive({
    req(input$predictors)
    predictors <- input$predictors
    interactions <- input$interactions
    if (interactions > 1 && length(predictors) > 1) {
      capped_interactions = min(interactions, length(predictors))
      formula_str <- paste("avg_increment ~ (", paste(predictors, collapse = " + "), ")^", capped_interactions)
    } else {
      formula_str <- paste("avgraise ~", paste(predictors, collapse = " + "))
    }
    as.formula(formula_str)
  })
  
  model_fit <- reactive({
    lm(model_formula(), data = final_data())
  })
  
  # Outputs
  output$rows <- renderPrint({
    counts <- final_data() %>%
      group_by(sex) %>%
      summarise(count = n()) %>%
      ungroup
    print(as.data.frame(counts))
    cat("Total:", nrow(final_data()))
  })
  
  output$model_plot <- renderPlot({
    p <- ggplot(final_data(), aes(x = sex, y = avgraise, fill = sex)) +
      geom_boxplot() +
      labs(title = "Average Percentage Salary Increase by Sex", x = "Sex", y = "Average Percentage Salary Increase per Year") +
      theme_minimal()
    p
  })
  
  output$model_formula <- renderPrint(
    cat(format(model_formula()))
  )
  
  output$model_summary <- renderPrint({
    tryCatch({
      # Print summary with the "Call" line removed
      model_summary <- summary(model_fit())
      model_summary_text <- capture.output(print(model_summary))
      
      residuals_index <- grep("^Residuals:", model_summary_text)
      model_summary_text <- model_summary_text[residuals_index: length(model_summary_text)]
      
      cat(model_summary_text, sep = "\n")
    }, error = function(e) {
      # In case of an error, print the error message
      cat("Error: ", e$message)
    })
  })
  
  output$conf_int <- renderPrint({
    tryCatch({
      # Try to get the model formula and print it
      cat("95% confidence interval for 'sexM': [", confint(model_fit())['sexM', ][1], ", ", confint(model_fit())['sexM', ][2], "]\n")
    }, error = function(e) {
      # In case of an error, print the error message
      cat("Error: ", e$message)
    })
  })
  
  output$model_residuals <- renderPlot({
    tryCatch({
      # Try to get the model formula and print it
      model <- model_fit()
      p <- plot(model$fitted.values, model$residuals)
      p
    }, error = function(e) {
      # In case of an error, print the error message
      cat("Error: ", e$message)
    })
  })
}

static_section <- function(input,output) {
  # Years of Experience by Gender plot
  output$experience_by_sex_plot <- renderPlot({
    ggplot(salary_data_static, aes(x = sex, y = years_experience, fill = sex)) +
      geom_boxplot() +
      labs(title = "Years of Experience by Gender - Men have over 6 years more than women", 
           x = "Sex", y = "Years of Experience") +
      theme_minimal()
  })
  
  # Years of Experience vs Salary Increment plot
  output$experience_plot <- renderPlot({
    ggplot(salary_data_static, aes(x = years_experience, y = avg_increment, color = sex)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Years of Experience vs Average Salary Increment by Gender", 
           x = "Years of Experience", y = "Average Salary Increment") +
      theme_minimal()
  })
  
  output$experience_plot_perc <- renderPlot({
    ggplot(salary_data_static, aes(x = years_experience, y = avgraise, color = sex)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Years of Experience vs Average Percentage Salary Increment by Gender", 
           x = "Years of Experience", y = "Average Percentage Salary Increment") +
      theme_minimal()
  })
  
  
  
  # Function to create a degree-based static plot
  create_degree_plot <- function(degree, usePerc = FALSE) {
    y_axis <- ifelse(usePerc, "avgraise", "avg_increment")
    y_label <- ifelse(usePerc, "Average Percentage Salary Increment", "Average Salary Increment")
    renderPlot({
      salary_data_static %>% filter(deg == degree) %>%
        ggplot(aes(x = sex, y = !!sym(y_axis), fill = sex)) +
        geom_boxplot() +
        labs(title = paste(y_label, "for Degree:", degree), 
             x = "Sex", y = y_label) +
        theme_minimal()
    })
  }
  
  output$static_plot_phd <- create_degree_plot("PhD")
  output$static_plot_prof <- create_degree_plot("Prof")
  output$static_plot_other <- create_degree_plot("Other")
  
  output$static_plot_phd_perc <- create_degree_plot("PhD", TRUE)
  output$static_plot_prof_perc <- create_degree_plot("Prof", TRUE)
  output$static_plot_other_perc <- create_degree_plot("Other", TRUE)
  
  # Function to create a field-based static plot
  create_field_plot <- function(field_name, usePerc = FALSE) {
    y_axis <- ifelse(usePerc, "avgraise", "avg_increment")
    y_label <- ifelse(usePerc, "Average Percentage Salary Increment", "Average Salary Increment")
    renderPlot({
      salary_data_static %>% filter(field == field_name) %>%
        ggplot(aes(x = sex, y = !!sym(y_axis), fill = sex)) +
        geom_boxplot() +
        labs(title = paste(y_label, "for Field:", field_name), 
             x = "Sex", y = y_label) +
        theme_minimal()
    })
  }
  
  output$field_other_plot <- create_field_plot("Other")
  output$field_arts_plot <- create_field_plot("Arts")
  output$field_prof_plot <- create_field_plot("Prof")

  output$field_other_plot_perc <- create_field_plot("Other", TRUE)
  output$field_arts_plot_perc <- create_field_plot("Arts", TRUE)
  output$field_prof_plot_perc <- create_field_plot("Prof", TRUE)
}

server <- function(input, output, session) {
  output$options <- renderUI({
    # Check which tab is selected from the sidebar
    if (input$tabs == "interactive") {
      model <- div(class = "sidebar-rect",
       sidebarMenu(
         div(class = "rectangle-header", "Model"),
         checkboxGroupInput(
           inputId = "responses",
           label   = "Response",
           choiceNames = list(
             HTML("avgraise <i class='fa fa-info-circle info-icon'
         data-toggle='tooltip'
         title='The average percentage salary increase per year for an individual. Individuals with only one year of data are excluded.'>
       </i>"
             )
           ),
           choiceValues = list("avgraise"),
           selected = list("avgraise")
         ),
         checkboxGroupInput(
           inputId = "predictors",
           label   = "Predictors",
           choices = c("sex", "deg", "yrdeg", "field", "startyr", "rank", "admin"),
           selected = c("sex")
         ),
         sliderInput("interactions", "Number of Interaction Terms:", 1, 7, 1),
         tags$script(HTML("
          $(document).ready(function(){
            $('input[value=\"avgraise\"]').prop('disabled', true);
            $('input[value=\"sex\"]').prop('disabled', true);
          });
        "))
       )
      )

      # Change the interaction terms slider value to the nearest valid value
      observeEvent(input$interactions, {
        if (input$interactions > length(input$predictors)) {
          updateSliderInput(session, "interactions", value = length(input$predictors))
        }
      })
      
      observeEvent(input$predictors, {
        if (input$interactions > length(input$predictors)) {
          updateSliderInput(session, "interactions", value = length(input$predictors))
        }
      })
      
      data <- div(class = "sidebar-rect2",
        sidebarMenu(
          div(class = "rectangle-header", "Data"),
          sliderInput("years", "Minimum years worked:", 2, 6, 2),
          checkboxGroupInput(
            inputId = "ranks",
            label   = "Filter by rank:",
            choices = c("Assoc", "Assist", "Full"),
            selected = c("Assoc", "Assist", "Full"),
          ),
          checkboxGroupInput(
            inputId = "fields",
            label   = "Filter by field:",
            choices = c("Arts", "Prof", "Other"),
            selected = c("Arts", "Prof", "Other"),
          ),
          checkboxGroupInput(
            inputId = "deg",
            label   = "Filter by degree:",
            choices = c("PhD", "Prof", "Other"),
            selected = c("PhD", "Prof", "Other"),
          ),
          selectInput("promo",
          label = tagList(
            "Filter by promotion status: ",
            HTML("<i class='fa fa-info-circle info-icon'
               data-toggle='tooltip'
               title='Accounts for individuals who received a promotion during years 90-95'>
             </i>"
            )
          ),
          c("Include Promoted", "Exclude Promoted", "Only Promoted"))
        )
      )
      
      tagList(model, data)
    }
  })
  
  # Interactive Model
  interactive_section(input, output)
  
  # Static Visualizations
  static_section(input, output)
}