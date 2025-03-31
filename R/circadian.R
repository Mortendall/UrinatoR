circadianUI <- function(id){
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = c(8,4),
                        shiny::column(12,
                                      bslib::card(shinyWidgets::actionBttn(inputId = ns("plotcircadian"),
                                                          label = "Plot circadian plot")
                                      ,
                                      shiny::selectInput(inputId = ns("grouptoggle"),
                                                         label = "Display as grouped or single cage?",
                                                         choices = c("Individual", "Group"),
                                                         selected = "Individual"),
                                      #shiny::numericInput(inputId = ns("lightphase start")),
                                      shiny::uiOutput(outputId = ns("circadiansummary")))),
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
                        )

  )
}

circadian <- function(id, data,colorpalette, parentSession){
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
                          y = ~meanNormalizedcircadian,
                          color = ~ID,
                          type = "scatter",
                          mode = "lines",
                          line = list(width = ~input$linethickness),
                          colors = colorpalette$individual
                          ,
                           error_y =~list(array = semNormalized,
                                           color = "black",
                                           thickness = 1)
          ) |>
            plotly::layout(title = list(text = "Circadian Plot of Incremental Data",
                                        font = list(size = input$titlesize)),
                           yaxis = list(title = list(
                             text = "Incremental Signal Increase",
                             font = list(size = input$axistitle)
                           )),
                           xaxis = list(title = list(
                             text = "ZT",
                             font = list(size = input$axistitle)
                           ),
                           dtick = 4,
                           range = c(0,23)),
                           shapes = list(
                             list(type = "rect",
                                  fillcolor = "black",
                                  line = list(color  ="black"),
                                  opacity = 0.3,
                                  x0 = 12,
                                  x1 = 23,
                                  xref  = "x",
                                  y0 = 0,
                                  y1 = max(data$circadiandata$meanNormalizedcircadian),
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
                          mode = "lines",
                          line = list(width = ~input$linethickness),
                          colors = colorpalette$group
                          ,
                           error_y =~list(array = semNormalizedGroup,
                                           color = "black",
                                           thickness = 1)
          )|>
            plotly::layout(title = list(text = "Circadian Plot of Incremental Data",
                                        font = list(size = input$titlesize)),
                           yaxis = list(title = list(
                             text = "Incremental Signal Increase",
                             font = list(size = input$axistitle)
                           )),
                           xaxis = list(title = list(
                             text = "ZT",
                             font = list(size = input$axistitle)
                           ),
                                        dtick = 4,
                                        range = c(0,23)),
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
