circadianUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
                    shiny::actionButton(inputId = ns("plotcircadian"),
                                        label = "Plot circadian plot")
                    ,
                    shiny::selectInput(inputId = ns("grouptoggle"),
                                       label = "Display as grouped or single cage?",
                                       choices = c("Individual", "Group"),
                                       selected = "Individual"),
                    shiny::uiOutput(outputId = ns("circadiansummary"))
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
        # yaxismaxdata <- data$circadiandata |>
        #   dplyr::filter(meanIncremental == max(data$circadiandata$meanIncremental))
        # yaxismax <- yaxismaxdata$meanIncremental
        # #+yaxismaxdata$sdIncremental

        #code for individualplot
        output$circadiansummaryindividual <- plotly::renderPlotly({
          plotly::plot_ly(data = data$circadiandata,
                          x = ~ZT,
                          y = ~meanNormalized,
                          color = ~Individual,
                          type = "scatter",
                          mode = "lines"
                          #,
                          # error_y =~list(array = sdIncremental,
                          #                 color = "black",
                          #                 thickness = 1)
          ) |>
            plotly::layout(title = "Circadian Plot of Incremental Data",
                           yaxis = list(title = "Incremental Signal Increase"),
                           xaxis = list(title = "Time"),
                           shapes = list(
                             list(type = "rect",
                                  fillcolor = "black",
                                  line = list(color  ="black"),
                                  opacity = 0.3,
                                  x0 = 12,
                                  x1 = 23,
                                  xref  = "x",
                                   y0 = 0,
                                   y1 = max(data$circadiandata$meanNormalized),
                                  yref = "y")
                           ))
        })
        #code for grouped plot
        output$plotsummarygroup <- plotly::renderPlotly({
          plotly::plot_ly(data = data$circadiandatagroup,
                          x = ~ZT,
                          y = ~meanNormalizedGroup,
                          color = ~Group,
                          type = "scatter",
                          mode = "lines"
                          #,
                          # error_y =~list(array = sdIncremental,
                          #                 color = "black",
                          #                 thickness = 1)
          ) |>
            plotly::layout(title = "Circadian Plot of Incremental Data",
                           yaxis = list(title = "Incremental Signal Increase"),
                           xaxis = list(title = "Time"),
                           shapes = list(
                             list(type = "rect",
                                  fillcolor = "black",
                                  line = list(color  ="black"),
                                  opacity = 0.3,
                                  x0 = 12,
                                  x1 = 23,
                                  xref  = "x",
                                  y0 = 0,
                                  y1 = max(data$circadiandatagroup$meanNormalizedGroup),
                                  yref = "y")
                           ))
        })
        output$circadiansummary <- shiny::renderUI({
          if(input$grouptoggle == "Individual"){
            plotly::plotlyOutput(ns("circadiansummaryindividual"))
          }
          else if(input$grouptoggle == "Group"){
            plotly::plotlyOutput(ns("plotsummarygroup"))
          }
        })
      })
    }
  )
}
