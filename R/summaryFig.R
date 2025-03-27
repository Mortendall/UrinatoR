summaryFigUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
    col_widths = c(6,6),
    shiny::column(12,
                  bslib::card(
                    shiny::selectInput(inputId = ns("grouptoggle"),
                                      label = "Plot by Group or Individual?",
                                      choices = c("Individual", "Group"),
                                      selected = "Group"),
                    shiny::selectInput(inputId = ns("plottoggle"),
                                       label = "Select plot to display",
                                       choices = c("Cumulative Urination Plot",
                                                   "Incremental Plot",
                                                   "Raw Data Plot")),
                    shiny::actionButton(inputId = ns("updateplot"),
                                        label = "Press button to draw selected plot")
                  )),
    shiny::column(12,
                  )),
  bslib::layout_column_wrap(
    shiny::uiOutput(outputId = ns("selectedplot")
  )))
}

summary <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      #####Plot code####
      shiny::observeEvent(input$updateplot,{

        #Rawdata for individuals
          output$rawdataPlotIndividual <- plotly::renderPlotly({
            req(data$joinedData)
            plotly::plot_ly(data = data$joinedData,
                            x = ~(TimeElapsed/24),
                            y = ~Rawdata,
                            color = ~ID,
                            type = "scatter",
                            mode = "lines"
            ) |>
              plotly::layout(title = "Bedding Status Index",
                             yaxis = list(title = "BSI"),
                             xaxis = list(title = "Elapsed Time (days)"))
          })

          #I
          output$summaryPlotIndividual <- plotly::renderPlotly({
            req(data$joinedData)
            plotly::plot_ly(data = data$joinedData,
                            x = ~(TimeElapsed/24),
                            y = ~CumulativeNormalized,
                            color = ~ID,
                            type = "scatter",
                            mode = "lines"
            ) |>
              plotly::layout(title = "Urination Index (UI)",
                             yaxis = list(title = "UI / mouse"),
                             xaxis = list(title = "Elapsed Time (days)"))
          })

          #Incremental Plot
          output$incrementalPlotIndividual <- plotly::renderPlotly({
            req(data$joinedData)
            plotly::plot_ly(data = data$joinedData,
                            x = ~(TimeElapsed/24),
                            y = ~NormalizedValue,
                            color = ~ID,
                            type = "scatter",
                            mode = "lines"
            ) |>
              plotly::layout(title = "Incremental Change w. Bedding Change Removed",
                             yaxis = list(title = "ΔBSI / timepoint"),
                             xaxis = list(title = "Elapsed Time (days)"))
          })

          #Same figures but grouped data
          #Raw data grouped
          output$rawdataPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~(TimeElapsed/24),
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
                             yaxis = list(title = "BSI"),
                             xaxis = list(title = "Elapsed Time (days)"))
          })

          #Grouped cumulative urination
          output$summaryPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~(TimeElapsed/24),
                            y = ~meanCumulativeNorm,
                            color = ~Group,
                            type = "scatter",
                            mode = "lines"
            ) |>
              plotly::layout(title = "Average Urination Index (UI)",
                             yaxis = list(title = "UI / mouse"),
                             xaxis = list(title = "Elapsed Time (days)"))
          })

          #Incremental plot
          output$incrementalPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~(TimeElapsed/24),
                            y = ~meanNormalized,
                            color = ~Group,
                            type = "scatter",
                            mode = "lines"
                            # ,
                            # error_y = ~list(array = sdIncremental,
                            #                 color = "black",
                            #                 thickness = 0.1)
            ) |>
              plotly::layout(title = "Incremental Change w. Bedding Change Removed",
                             yaxis = list(title = "ΔBSI / timepoint"),
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
