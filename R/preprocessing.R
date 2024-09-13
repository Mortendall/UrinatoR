preprocessingUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::uiOutput(outputId = ns("animalnumber"))),
      shiny::column(8,
                    shiny::uiOutput(outputId = ns("select")),
                    shiny::h6("Confirm no. of animals pr. cage and press button to normalize and continue"),
                    shiny::actionButton(inputId = ns("continue"),
                                        label = "Continue to graphs",
                                        icon = shiny::icon("arrow-right"))
                                       ))
    )
}

preprocessing <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function( input, output, session){
      ns <- session$ns

      #####Select individual to display####
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

      #####Table for no. of animals pr cage input####
      #code for the table to change number of mice pr. cage
      output$animalnumber <- shiny::renderUI({
        req(data$longData)
        shiny::tagList(
          rhandsontable::rHandsontableOutput(outputId = ns("animaltable")),
          shiny::actionButton(inputId = ns("savedata"),
                              label = "Save changes")
        )
      })

      #####Exclusion plot####
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

      #####Table code for no. of animals####
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

      #####Save changes made to the table ####
      #. Send an error if character input in number of animals
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
          shinyWidgets::sendSweetAlert(
            title = "Input Saved",
            text = "You are good to go!",
            type = "success"
          )
                }

      })

      #####Normalize + group data and continue to graphs####
      shiny::observeEvent(
        input$continue,
        {
          if(is.null(data$groupeddata)){
            data$longData <- dplyr::left_join(data$longData,
                                              data$groupinfo,
                                              by = c("Individual" = "CageID")) |>
              dplyr::mutate(NormalizedValue = dplyr::case_when(
                !is.na(CorrectedValue) ~ CorrectedValue / No.ofAnimals,
                is.na(CorrectedValue) ~ NA
              )) |>
              dplyr::group_by(Individual) |>
              dplyr::mutate(CumulativeNormalized = cumsum(ifelse(is.na(NormalizedValue),
                                                                 0,
                                                                 NormalizedValue)))




            #prepare grouped data for figure generation
            data$groupeddata  <- data$longData |>
              dplyr::group_by(Group, TimeElapsed) |>
              dplyr::summarise(meanRawdata = mean(Rawdata, na.rm = T),
                               sdRawdata = sd(Rawdata, na.rm = T),
                               meanCorrectedValue = mean(CorrectedValue, na.rm = T),
                               sdCorrectedValue = sd(CorrectedValue, na.rm = T),
                               meanNormalized = mean(NormalizedValue, na.rm = T),
                               sdNormalized = sd(NormalizedValue, na.rm = T),
                               meanCumulativeNorm = mean(CumulativeNormalized, na.rm = T),
                               sdCumulativeNorm = sd(CumulativeNormalized, na.rm = T )
              )

            #prepare data for hourly urination pr week
            data$hourly <- data$longData |>
              dplyr::mutate(week = (day - (day %% 7))/7) |>
              dplyr::group_by(Individual, hour, week) |>
              dplyr::summarise(
                meanNormalized = mean(NormalizedValue,
                                      na.rm = T),
                n = dplyr::n()
              )

            #prepare data for circadian plots
            data$circadiandata <- data$hourly |>
              dplyr::group_by(Individual, hour)|>
              dplyr::summarise(
                meanNormalizedcircadian = mean(meanNormalized, na.rm = T),
                semNormalized = sd(meanNormalized, na.rm = T),
                n = dplyr::n()) |>
              dplyr::mutate(ZT = dplyr::case_when(
                hour >= 6 ~ hour - 6,
                hour < 6 ~ hour + 18
              ),
              semNormalized = semNormalized/n)|>
              dplyr::arrange(ZT)

            #group circadian
            data$circadiandatagroup <- data$hourly|>
              dplyr::mutate(Group = stringr::str_extract(Individual,
                                                         "[:graph:]+(?=_Box)")) |>
              dplyr::group_by(Group, hour)|>
              dplyr::summarise(
                meanNormalizedGroup = mean(meanNormalized, na.rm = T),
                semNormalizedGroup = sd(meanNormalized, na.rm = T),
                n = dplyr::n())|>
              dplyr::mutate(ZT = dplyr::case_when(
                hour >= 6 ~ hour - 6,
                hour < 6 ~ hour + 18
              ),
              semNormalizedGroup = semNormalizedGroup/n) |>
              dplyr::arrange(ZT)






            # saveRDS(data$circadiandatagroup, here::here("Data/circadiangroup.rds"))
            # saveRDS(data$circadiandata, here::here("Data/circadian.rds"))
            # saveRDS(data$longData, here::here("Data/longData.rds"))
            # saveRDS(data$groupeddata, here::here("Data/groupeddata.rds"))

            #Update tabsets


            shiny::updateTabsetPanel(session = parentSession,
                                     inputId = "inTabset",
                                     selected = "summaryFig")
          }

          else{
            shinyWidgets::sendSweetAlert(title = "Pre-processing already done",
                                         text = "Pre-processing has already been completed",
                                         type = "warning")
          }

      })
    }
  )
}
