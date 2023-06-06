summaryFigUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(width = 6,
                    shiny::selectInput(inputId = ns("grouptoggle"),
                                       label = "Plot by Group or Individual?",
                                       choices = c("Individual", "Group"),
                                       selected = "Group"),
                    shiny::selectInput(inputId = ns("plottoggle"),
                                       label = "Select plot to display",
                                       choices = c("Cumulative Urination Plot",
                                                   "Incremental Plot",
                                                   "Raw Data Plot"))),
      shiny::column(width = 6,
                    shiny::h6("Press button to draw selected plot"),
                    shiny::actionButton(inputId = ns("updateplot"),
                                        label = "Draw!")
      ))
    ,
    shiny::fluidRow(
      shiny::column(width = 12,
                    shiny::uiOutput(outputId = ns("selectedplot"))
                    # plotly::plotlyOutput(outputId = ns("rawdataPlot")),
                    # plotly::plotlyOutput(outputId = ns("summaryPlot")),
                    # plotly::plotlyOutput(outputId = ns("incrementalPlot"))
                    )
    )
  )
}

summary <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      shiny::observeEvent(input$updateplot,{
        #Define the code for the plots that can be rendered

          output$rawdataPlotIndividual <- plotly::renderPlotly({
            req(data$longData)
            plotly::plot_ly(data = data$longData,
                            x = ~TimeElapsed,
                            y = ~Rawdata,
                            color = ~Individual,
                            type = "scatter",
                            mode = "lines"
            ) |>
              plotly::layout(title = "Bedding Status Index",
                             yaxis = list(title = "Raw DVC Signal"),
                             xaxis = list(title = "Elapsed Time (hours)"))
          })
          output$summaryPlotIndividual <- plotly::renderPlotly({
            req(data$longData)
            plotly::plot_ly(data = data$longData,
                            x = ~TimeElapsed,
                            y = ~AccumulatedValue,
                            color = ~Individual,
                            type = "scatter",
                            mode = "markers"
            ) |> plotly::add_trace(x = ~TimeElapsed,
                                   y = ~RollingMeanAcc,
                                   type = "scatter",
                                   mode = "lines")|>
              plotly::layout(title = "Cumulative Urination",
                             yaxis = list(title = "Accumulated Signal"),
                             xaxis = list(title = "Elapsed Time (hours)"))
          })

          output$incrementalPlotIndividual <- plotly::renderPlotly({
            req(data$longData)
            plotly::plot_ly(data = data$longData,
                            x = ~TimeElapsed,
                            y = ~DeltaRollingMean,
                            color = ~Individual,
                            type = "scatter",
                            mode = "lines"
            ) |>
              plotly::layout(title = "Incremental Change w. Bedding Change Removed",
                             yaxis = list(title = "Incremental Signal"),
                             xaxis = list(title = "Elapsed Time (hours)"))
          })

          output$rawdataPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~TimeElapsed,
                            y = ~meanRawdata,
                            color = ~Group,
                            type = "scatter",
                            mode = "lines"
                            # Excluded error bars as it looks very ugly
                            # ,
                            # error_y =~list(array = sdRawdata,
                            #                color = "black",
                            #                thickness = 0.1)
            ) |>
              plotly::layout(title = "Bedding Status Index",
                             yaxis = list(title = "Raw DVC Signal"),
                             xaxis = list(title = "Elapsed Time (hours)"))
          })
          output$summaryPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~TimeElapsed,
                            y = ~meanAccumulated,
                            color = ~Group,
                            type = "scatter",
                            mode = "markers"
            ) |> plotly::add_trace(x = ~TimeElapsed,
                                   y = ~meanRolling,
                                   type = "scatter",
                                   mode = "lines"
                                   # ,
                                   # error_y = ~list(array = sdRolling,
                                   #                 color = "black",
                                   #                 thickness = 0.1)
                                   )|>
              plotly::layout(title = "Cumulative Urination",
                             yaxis = list(title = "Accumulated Signal"),
                             xaxis = list(title = "Elapsed Time (hours)"))
          })

          output$incrementalPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~TimeElapsed,
                            y = ~meanIncremental,
                            color = ~Group,
                            type = "scatter",
                            mode = "lines"
                            # ,
                            # error_y = ~list(array = sdIncremental,
                            #                 color = "black",
                            #                 thickness = 0.1)
            ) |>
              plotly::layout(title = "Incremental Change w. Bedding Change Removed",
                             yaxis = list(title = "Incremental Signal"),
                             xaxis = list(title = "Elapsed Time (hours)"))
          })

          #Define the switch that determines what is being displayed

          output$selectedplot <- shiny::renderUI({
            if(input$grouptoggle == "Individual"&input$plottoggle == "Cumulative Urination Plot"){
              plotly::plotlyOutput(outputId = ns("summaryPlotIndividual"))
            }
            else if(input$grouptoggle == "Individual"&
               input$plottoggle == "Incremental Plot"){
              plotly::plotlyOutput(outputId = ns("incrementalPlotIndividual"))
            }
            else if(input$grouptoggle == "Individual"&
               input$plottoggle == "Raw Data Plot"){
              plotly::plotlyOutput(outputId = ns("rawdataPlotIndividual"))
            }
            else if(input$grouptoggle == "Group"&
               input$plottoggle == "Incremental Plot"){
              plotly::plotlyOutput(outputId = ns("incrementalPlotGroup"))
            }
            else if(input$grouptoggle == "Group"&
               input$plottoggle == "Cumulative Urination Plot"){
              plotly::plotlyOutput(outputId = ns("summaryPlotGroup"))
            }
            else if(input$grouptoggle == "Group"&
               input$plottoggle == "Raw Data Plot"){
              plotly::plotlyOutput(outputId = ns("rawdataPlotGroup"))
            }
          })

      })

    }
  )
}
