library(shiny)

library(rhandsontable)

options(shiny.maxRequestSize = 500*1024^2)
# cores <- parallel::detectCores()
# cl <- parallel::makeCluster(cores[1] - 1)
# doParallel::registerDoParallel(cl)
#
# on.exit({
#   parallel::stopCluster(cl)
#   foreach::registerDoSEQ()
# })

dataSheet <- shiny::reactiveValues()

ui <- shiny::fluidPage(
  theme = bslib::bs_theme(version = 5,
                          bootswatch = "united"),
  shiny::titlePanel("UrinatoR: a tool for visualizing mouse urine data from DVC"),
  shiny::tabsetPanel(type = "tabs",
                     id = "inTabset",
                     shiny::tabPanel(title = "Data Upload",
                                     uploadUI("upload"),
                                     value = "uploadPage"),
                     shiny::tabPanel(title = "Preprocessing",
                                     preprocessingUI("preprocess"),
                                     value = "preprocess"),
                     shiny::tabPanel(title = "Summary figure",
                                     summaryFigUI("summary"),
                                     value = "summaryFig"),
                     shiny::tabPanel(title = "Circadian plot",
                                     circadianUI("circadian"),
                                     value = "circadian"),
                     shiny::tabPanel(title = "Download Data",
                                     downloadUI("download"),
                                     value = "download")
                     )
)

server <-function(input, output, session){
  parentSession <- session
  upload("upload",dataSheet, parentSession)
  preprocessing("preprocess", dataSheet, parentSession)
  summary("summary", dataSheet, parentSession)
  circadian("circadian",dataSheet,parentSession)
  download("download", dataSheet, parentSession)
}

shinyApp(ui = ui, server = server)
