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



ui <-
  bslib::page_navbar(title = "UrinatoR: a tool for visualizing mouse urine data from DVC",
                     id = "inTabset",
                     theme = bslib::bs_theme(version = 5,
                                              bootswatch = "united"),
                     bslib::nav_panel(title = "Welcome",
                                     welcomeui("welcome"),
                                     value = "welcomePage"),
                     bslib::nav_panel(title = "Data Upload",
                                     uploadUI("upload"),
                                     value = "uploadPage"),
                     bslib::nav_panel(title = "Preprocessing",
                                     preprocessingUI("preprocess"),
                                     value = "preprocess"),
                     bslib::nav_panel(title = "Summary figure",
                                     summaryFigUI("summary"),
                                     value = "summaryFig"),
                     bslib::nav_panel(title = "Circadian plot",
                                     circadianUI("circadian"),
                                     value = "circadian"),
                     bslib::nav_panel(title = "Download Data",
                                     downloadUI("download"),
                                     value = "download"),
                     bslib::nav_panel(title = "About UrinatoR",
                                     aboutui("about"),
                                     value = "aboutpage")

)

server <-function(input, output, session){
  dataSheet <- shiny::reactiveValues()
  colorscheme <- shiny::reactiveValues()
  parentSession <- session
  welcome("welcome",dataSheet, parentSession)
  upload("upload",dataSheet, colorscheme, parentSession)
  preprocessing("preprocess", dataSheet, colorscheme, parentSession)
  summary("summary", dataSheet, colorscheme, parentSession)
  circadian("circadian",dataSheet, colorscheme, parentSession)
  download("download", dataSheet, parentSession)
  about("about",dataSheet, parentSession)
}

shinyApp(ui = ui, server = server)
