library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(rstatix)
library(patchwork)
library(wesanderson)
library(rhandsontable)
library(dplyr)
library(ggplot2)
library(here)
library(openxlsx)
library(data.table)
library(lubridate)
library(plotly)

options(shiny.maxRequestSize = 500*1024^2)
cores <- parallel::detectCores()
cl <- parallel::makeCluster(cores[1] - 1)
doParallel::registerDoParallel(cl)

on.exit({
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
})

dataSheet <- shiny::reactiveValues()

ui <- shiny::fluidPage(
  shiny::titlePanel("UrinatoR: a tool for visualizing mouse urine data from DVC"),
  shiny::tabsetPanel(type = "tabs",
                     id = "inTabset",
                     shiny::tabPanel(title = "Data Upload",
                                     uploadUI("upload"),
                                     value = "uploadPage"),
                     shiny::tabPanel(title = "Summary figure",
                                     summaryFigUI("summary"),
                                     value = "summaryFig")
                     )
)

server <-function(input, output, session){
  parentSession <- session
  upload("upload",dataSheet, parentSession)
  summary("summary", dataSheet, parentSession)
}

shinyApp(ui = ui, server = server)
