summaryFigUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
    col_widths = c(8,4),
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
                    shinyWidgets::actionBttn(inputId = ns("updateplot"),
                                        label = "Press button to draw selected plot"),
                    shiny::uiOutput(outputId = ns("selectedplot")
                  ))),
    shiny::column(12,
                  bslib::card(
                    shiny::numericInput(inputId = ns("linethickness"),
                                        label = "Line thickness",
                                        value = 3),
                    shiny::numericInput(inputId = ns("titlesize"),
                                        label = "Title size",
                                        value = 16),
                    shiny::numericInput(inputId = ns("axistitle"),
                                        label = "Axis title size",
                                        value = 14),
                    shiny::uiOutput(outputId = ns("color_picker"))
                  )
                  )))
}

summary <- function(id, data, colorpalette, parentSession){
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
                            color = ~factor(ID),
                            type = "scatter",
                            mode = "lines",
                            line = list(width = ~input$linethickness),
                            colors = colorpalette$individual
            ) |>
              plotly::layout(
                title = list(text = "Bedding Status Index",
                             font = list(size = input$titlesize)),
                yaxis = list(title = list(
                  text = "BSI",
                  font = list(size = input$axistitle)
                )),
                xaxis = list(title = list(
                  text = "Elapsed Time (days)",
                  font = list(size = input$axistitle)
                ))
              )
          })

          #I
          output$summaryPlotIndividual <- plotly::renderPlotly({
            req(data$joinedData)
            plotly::plot_ly(data = data$joinedData,
                            x = ~(TimeElapsed/24),
                            y = ~CumulativeNormalized,
                            color = ~ID,
                            type = "scatter",
                            mode = "lines",
                            line = list(width = ~input$linethickness),
                            colors = colorpalette$individual
            ) |>
              plotly::layout(
              title = list(text = "Urination Index (UI)",
                           font = list(size = input$titlesize)),
              yaxis = list(title = list(
                text = "UI / mouse",
                font = list(size = input$axistitle)
              )),
              xaxis = list(title = list(
                text = "Elapsed Time (days)",
                font = list(size = input$axistitle)
              ))
            )
          })

          #Incremental Plot
          output$incrementalPlotIndividual <- plotly::renderPlotly({
            req(data$joinedData)
            plotly::plot_ly(data = data$joinedData,
                            x = ~(TimeElapsed/24),
                            y = ~NormalizedValue,
                            color = ~ID,
                            type = "scatter",
                            mode = "lines",
                            line = list(width = ~input$linethickness),
                            colors = colorpalette$individual
            ) |>
            plotly::layout(
              title = list(text = "Incremental Change w. Bedding Change Removed",
                           font = list(size = input$titlesize)),
              yaxis = list(title = list(
                text = "ΔBSI / timepoint",
                font = list(size = input$axistitle)
              )),
              xaxis = list(title = list(
                text = "Elapsed Time (days)",
                font = list(size = input$axistitle)
              ))
            )
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
                            mode = "lines",
                            line = list(width = ~input$linethickness),
                            colors = colorpalette$group

            ) |>
              plotly::layout(
                title = list(text = "Bedding Status Index",
                             font = list(size = input$titlesize)),
                yaxis = list(title = list(
                  text = "BSI",
                  font = list(size = input$axistitle)
                )),
                xaxis = list(title = list(
                  text = "Elapsed Time (days)",
                  font = list(size = input$axistitle)
                ))
              )
          })

          #Grouped cumulative urination
          output$summaryPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~(TimeElapsed/24),
                            y = ~meanCumulativeNorm,
                            color = ~Group,
                            type = "scatter",
                            mode = "lines",
                            line = list(width = ~input$linethickness),
                            colors = colorpalette$group
            ) |>
            plotly::layout(
              title = list(text = "Average Urination Index (UI)",
                           font = list(size = input$titlesize)),
              yaxis = list(title = list(
                text = "UI / mouse",
                font = list(size = input$axistitle)
              )),
              xaxis = list(title = list(
                text = "Elapsed Time (days)",
                font = list(size = input$axistitle)
              ))
            )
          })

          #Incremental plot
          output$incrementalPlotGroup <- plotly::renderPlotly({
            req(data$groupeddata)
            plotly::plot_ly(data = data$groupeddata,
                            x = ~(TimeElapsed/24),
                            y = ~meanNormalized,
                            color = ~Group,
                            type = "scatter",
                            mode = "lines",
                            line = list(width = ~input$linethickness),
                            colors = colorpalette$group

            ) |>
              plotly::layout(
                title = list(text = "Incremental Change w. Bedding Change Removed",
                             font = list(size = input$titlesize)),
                yaxis = list(title = list(
                  text = "ΔBSI / timepoint",
                  font = list(size = input$axistitle)
                )),
                xaxis = list(title = list(
                  text = "Elapsed Time (days)",
                  font = list(size = input$axistitle)
                ))
              )
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

          #####color picker####

          output$color_picker <- shiny::renderUI({
            shiny::tagList(
              shiny::selectizeInput(inputId = ns("colorselect"),
                                    label  = "Pick colors for graph",
                                    multiple  =T,
                                    choices = grDevices::colors()),
            shinyWidgets::actionBttn(inputId = ns("colorapply"),
                                     label = "Apply selected colors"))
          })

          shiny::observeEvent(input$colorapply,{
            if(input$grouptoggle=="Individual"){
              colorpalette$individual <- input$colorselect
            }
            if(input$grouptoggle == "Group"){
              colorpalette$group <- input$colorselect
            }
          })

      })

    }
  )
}
