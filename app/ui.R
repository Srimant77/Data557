ui <- dashboardPage(
  dashboardHeader(
    title = "Gender Bias in Granting Salary Increases (1990-1995)",
    titleWidth = 500
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Static Visualizations", tabName = "static", icon = icon("chart-bar")),
      menuItem("Interactive Model", tabName = "interactive", icon = icon("chart-line")),
      uiOutput("options")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      # Interactive Model Tab
      tabItem(tabName = "interactive",
              fluidRow(
                column(
                  width = 6,
                  box(
                    title = "Row count",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput("rows")
                  ),
                  box(
                    title = "Boxplot",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    plotOutput("model_plot")
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Model Formula",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput("model_formula")
                  ),
                  box(
                    title = "Confidence Interval",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput("conf_int")
                  ),
                  box(
                    title = "Model Summary",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    verbatimTextOutput("model_summary")
                  ),
                  box(
                    title = "Residual Plot",
                    status = "primary",
                    solidHeader = TRUE,
                    width = NULL,
                    plotOutput("model_residuals")
                  )
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
                    fluidRow(
                      column(4, h4("PhD Holders"), plotOutput("static_plot_phd_perc")),
                      column(4, h4("Prof Degree Holders"), plotOutput("static_plot_prof_perc")),
                      column(4, h4("Other Degree Holders"), plotOutput("static_plot_other_perc"))
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
                    fluidRow(
                      column(12, plotOutput("experience_plot_perc"))
                    ),
                    br(),
                    h3("Salary Increment by Field - Means roughly the same"),
                    fluidRow(
                      column(4, h4("Field: Other"), plotOutput("field_other_plot")),
                      column(4, h4("Field: Arts"), plotOutput("field_arts_plot")),
                      column(4, h4("Field: Prof"), plotOutput("field_prof_plot"))
                    ),
                    fluidRow(
                      column(4, h4("Field: Other"), plotOutput("field_other_plot_perc")),
                      column(4, h4("Field: Arts"), plotOutput("field_arts_plot_perc")),
                      column(4, h4("Field: Prof"), plotOutput("field_prof_plot_perc"))
                    )
                )
              )
      )
    )
  )
)