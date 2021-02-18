library(dygraphs)
library(dplyr)
library(xts)
library(shiny)
library(shinyBS)
library(dualR)

PYTHON_DEPENDENCIES = c('sweat')

source("utils.R")
source("server.R")
source("ui.R")

app_env <- reactiveValues(rmd_params = list(),
                          ready = FALSE)

shinyApp(ui, server)
