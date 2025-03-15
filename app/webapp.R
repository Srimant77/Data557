library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(GGally)
library(broom)
source("ui.R")
source("server.R")

# Run the Shiny App
shinyApp(ui, server)