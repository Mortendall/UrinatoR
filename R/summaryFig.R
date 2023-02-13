summaryFigUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(width = 12,
                    plotly::plotlyOutput(outputId = ns("rawdataPlot")),
                    plotly::plotlyOutput(outputId = ns("summaryPlot"))
                    )
    )
  )
}

summary <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
       output$rawdataPlot <- plotly::renderPlotly({
         req(data$longData)
         plotly::plot_ly(data = data$longData,
                          x = ~TimeElapsed,
                          y = ~Rawdata,
                          color = ~ID,
                         type = "scatter",
                         mode = "lines"
          )
       })
       output$summaryPlot <- plotly::renderPlotly({
         req(data$longData)
         plotly::plot_ly(data = data$longData,
                          x = ~TimeElapsed,
                          y = ~AccumulatedValue,
                          color = ~ID,type = "scatter",
                          mode = "markers"
          ) |> plotly::add_trace(x = ~TimeElapsed,
                              y = ~RollingMeanAcc,
                              type = "scatter",
                              mode = "lines")
       })
    }
  )
}
