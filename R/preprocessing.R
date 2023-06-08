preprocessingUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::uiOutput(outputId = ns("animalnumber"))),
      shiny::column(8,
                    shiny::uiOutput(outputId = ns("select"))
                                       ))
    )
}

preprocessing <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function( input, output, session){
      ns <- session$ns

      #Code determining what is rendered when data is loaded in
      output$select <- shiny::renderUI({
        req(data$longData)
        shiny::tagList(
          plotly::plotlyOutput(outputId = ns("excluded")),
          shiny::selectInput(inputId = ns("individuals"),
                             label = "Select a subject to visualize",
                             choices = unique(data$longData$Individual)
                             )
          )
      })

      #code for the table to change number of mice pr. cage

      output$animalnumber <- shiny::renderUI({
        req(data$longData)
        shiny::tagList(
          rhandsontable::rHandsontableOutput(outputId = ns("animaltable")),
          shiny::actionButton(inputId = ns("savedata"),
                              label = "Save changes")
        )
      })

      #Plot showing the raw data with dashed lines for exclusions
      output$excluded <- plotly::renderPlotly({
        req(data$longData)
        p <- plotly::plot_ly(data = subset(data$longData,
                                      Individual == input$individuals),
                        x = ~TimeElapsed,
                        y = ~Rawdata,
                        type = "scatter",
                        mode = "lines"
        ) |>
          plotly::layout(title = "Bedding Status Index",
                         yaxis = list(title = "Raw DVC Signal"),
                         xaxis = list(title = "Elapsed Time (hours)"))
        exclusions <- data$longData |>
          dplyr::filter(Include == 1 & Individual == input$individuals) |>
          dplyr::pull(TimeElapsed)

        for(i in 1:length(exclusions))
        {
          p <- p %>%
            plotly::add_trace(x = exclusions[i],
                              type = 'scatter',
                              mode = 'lines',
                              line = list(color = "red",
                                          dash = "dash"),
                              text = "Excluded")
        }
        p |>plotly::layout(showlegend = F)
      })

      #code for the table that allows you to input animal number
      output$animaltable <- rhandsontable::renderRHandsontable({
        req(data$groupinfo)
        rhandsontable::rhandsontable(data$groupinfo,
                                     digits = 0,
                                     readOnly = T) |>
          rhandsontable::hot_col(col= "No.ofAnimals",
                                 readOnly = F,
                                 format = "0")
      })

      #save changes made to the table. Send an error if character input in
      #number of animals

      shiny::observeEvent(input$savedata,{
        if(any(is.na(rhandsontable::hot_to_r(input$animaltable)))==T){
          shinyWidgets::sendSweetAlert(
            title = "Input Error",
            text = "Please input only numbers",
            type = "error"
          )
        }
        else{
          data$groupinfo <- rhandsontable::hot_to_r(input$animaltable)
                }

      })
    }
  )
}
