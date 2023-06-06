circadianUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(inputId = ns("plotcircadian"),
                                        label = "Plot circadian plot")
                    ,
                    plotly::plotlyOutput(outputId = ns("circadiansummary"))
                    )
    )
  )
}

circadian <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      shiny::observeEvent(input$plotcircadian,{
        req(data$groupeddata)
        yaxismaxdata <- data$circadiandata |>
          dplyr::filter(meanIncremental == max(data$circadiandata$meanIncremental))
        yaxismax <- yaxismaxdata$meanIncremental+yaxismaxdata$sdIncremental

        output$circadiansummary <- plotly::renderPlotly({
          plotly::plot_ly(data = data$circadiandata,
                          x = ~hour,
                          y = ~meanIncremental,
                          color = ~Group,
                          type = "bar",
                          error_y =~list(array = sdIncremental,
                                          color = "black",
                                          thickness = 1)
          ) |>
            plotly::layout(title = "Circadian Plot of Incremental Data",
                           yaxis = list(title = "Incremental Signal Increase"),
                           xaxis = list(title = "ZT time"),
                           shapes = list(
                             list(type = "rect",
                                  fillcolor = "black",
                                  line = list(color  ="black"),
                                  opacity = 0.3,
                                  x0 = 0,
                                  x1 = 6,
                                  xref  = "x",
                                   y0 = 0,
                                   y1 = yaxismax,
                                  yref = "y"),
                             list(type = "rect",
                                  fillcolor = "black",
                                  line = list(color  ="black"),
                                  opacity = 0.3,
                                  x0 = 18,
                                  x1 = 24,
                                  xref  = "x",
                                  y0 = 0,
                                  y1 = yaxismax,
                                  yref = "y")
                           ))
        })
      })
    }
  )
}
