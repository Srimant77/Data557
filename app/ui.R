library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title = "Gender Bias in Granting Salary Increases (1990-1995)",
    titleWidth = 600
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
                  width = 12,
                  box (
                    status = "info",
                    width = NULL,
                    HTML('<div style="word-wrap: break-word; max-width: 100%;">An interactive playground that aims to explore the question "Has sex bias existed in granting salary increases between 1990 - 1995?"
                    by fitting various linear regression models to the data.
                    Select additional predictor variables and the number of interaction terms you want to add to the base model in the Model menu.
                    Filter your data with the options in the Data menu.</div>')
                  )
                )
              ),
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
                column(
                  width = 12,
                  box (
                    status = "info",
                    solidHeader = TRUE,
                    width = NULL,
                    HTML('<div style="word-wrap: break-word; max-width: 100%;">An exploration of the differences observed in granting salary increases between men and women in the years 1990 - 1995. </div>')
                  )
                )
              ),
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