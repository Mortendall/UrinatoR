summaryFigUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow()
  )
}

summary <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(output, input, session){
      ns <- session$ns
    }
  )
}
