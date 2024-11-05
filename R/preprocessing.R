preprocessingUI <- function(id){
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4,8),
    shiny::column(12,
                  bslib::card(
                    shiny::uiOutput(outputId = ns("animalnumber"))
                    )),
    shiny::column(12,
                  bslib::card(
                    shiny::uiOutput(outputId = ns("select")),
                    shiny::h6("Confirm no. of animals pr. cage and press button to normalize and continue"),
                    shiny::numericInput(inputId = ns("circadian"),
                                        label = "For circadian calculations, please indicate the hour when light is switched on",
                                        value = 6,
                                        min = 0,
                                        max = 23,
                                        step = 1),
                    shiny::actionButton(inputId = ns("continue"),
                                        label = "Process data and continue to graphs",
                                        icon = shiny::icon("arrow-right"))
                  )))
}

preprocessing <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function( input, output, session){
      ns <- session$ns

      #####Select individual to display####
      #Code determining what is rendered when data is loaded in
      output$select <- shiny::renderUI({
        req(data$joinedData)
        shiny::tagList(
          plotly::plotlyOutput(outputId = ns("excluded")),
          shiny::selectInput(inputId = ns("individuals"),
                             label = "Select a subject to visualize",
                             choices = unique(data$joinedData$ID)
                             )
          )
      })

      #####Table for no. of animals pr cage input####
      #code for the table to change number of mice pr. cage
      output$animalnumber <- shiny::renderUI({
        req(data$joinedData)
        shiny::tagList(
          rhandsontable::rHandsontableOutput(outputId = ns("animaltable")),
          shiny::actionButton(inputId = ns("savedata"),
                              label = "Save changes")
        )
      })

      #####Exclusion plot####
      #Plot showing the raw data with dashed lines for exclusions
      output$excluded <- plotly::renderPlotly({
        req(data$joinedData)
        p <- plotly::plot_ly(data = subset(data$joinedData,
                                      ID == input$individuals &
                                      included == TRUE),
                        x = ~(TimeElapsed)/24,
                        #divide by 7 to get time in days
                        y = ~Rawdata,
                        type = "scatter",
                        mode = "lines"
        ) |>
          plotly::layout(title = "Bedding Status Index",
                         yaxis = list(title = "Raw DVC Signal"),
                         xaxis = list(title = "Elapsed Time (days)"))
        exclusions <- data$joinedData |>
          duckplyr::filter(event == "INSERTED" & ID == input$individuals) |>
          duckplyr::mutate(TimeElapsed = TimeElapsed/24) |>
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
            data$joinedData <- dplyr::left_join(data$joinedData,
                                              data$groupinfo,
                                              by = c("ID" = "CageID")) |>
              dplyr::mutate(NormalizedValue = dplyr::case_when(
                !is.na(CorrectedValue) ~ CorrectedValue / No.ofAnimals,
                is.na(CorrectedValue) ~ NA
              )) |>
              dplyr::group_by(ID) |>
              dplyr::mutate(CumulativeNormalized = cumsum(ifelse(is.na(NormalizedValue),
                                                                 0,
                                                                 NormalizedValue)))




            #prepare grouped data for figure generation
            data$groupeddata  <- data$joinedData |>
              dplyr::group_by(Group, TimeElapsed) |>
              dplyr::summarise(n = dplyr::n(),
                               meanRawdata = mean(Rawdata, na.rm = T),
                               sdRawdata = sd(Rawdata, na.rm = T),
                               semRawdata = sdRawdata/n,
                               meanCorrectedValue = mean(CorrectedValue, na.rm = T),
                               sdCorrectedValue = sd(CorrectedValue, na.rm = T),
                               semCorrectedValue = sdCorrectedValue/n,
                               meanNormalized = mean(NormalizedValue, na.rm = T),
                               sdNormalized = sd(NormalizedValue, na.rm = T),
                               semNormalized = sdNormalized/n,
                               meanCumulativeNorm = mean(CumulativeNormalized, na.rm = T),
                               sdCumulativeNorm = sd(CumulativeNormalized, na.rm = T ),
                               semCumulativeNorm = sdCumulativeNorm/n
              )

            #prepare data for hourly urination pr week
            data$hourly <- data$joinedData |>
              dplyr::mutate(week = (day - (day %% 7))/7) |>
              dplyr::group_by(ID, hour, week) |>
              dplyr::summarise(
                meanNormalized = mean(NormalizedValue,
                                      na.rm = T),
                n = dplyr::n()
              )

            #prepare data for circadian plots
            data$circadiandata <- data$hourly |>
              dplyr::group_by(ID, hour)|>
              dplyr::summarise(
                meanNormalizedcircadian = mean(meanNormalized, na.rm = T),
                semNormalized = sd(meanNormalized, na.rm = T),
                n = dplyr::n()) |>
              dplyr::mutate(ZT = dplyr::case_when(
                hour >= input$circadian ~ hour - input$circadian,
                hour < input$circadian ~ hour + (24-input$circadian)
              ),
              semNormalized = semNormalized/n)|>
              dplyr::arrange(ZT)

            #group circadian
            group_key <- data$joinedData |>
              duckplyr::select(ID, Group) |>
              duckplyr::distinct(ID, .keep_all = T)

            data$circadiandatagroup <- duckplyr::left_join(data$hourly, group_key, by = c("ID"="ID"))|>
              # dplyr::mutate(Group = stringr::str_extract(ID,
              #                                            "[:graph:]+(?=_Box)")) |>
              dplyr::group_by(Group, hour)|>
              dplyr::summarise(
                meanNormalizedGroup = mean(meanNormalized, na.rm = T),
                semNormalizedGroup = sd(meanNormalized, na.rm = T),
                n = dplyr::n())|>
              dplyr::mutate(ZT = dplyr::case_when(
                hour >= input$circadian ~ hour - input$circadian,
                hour < input$circadian ~ hour + (24-input$circadian)
              ),
              semNormalizedGroup = semNormalizedGroup/n) |>
              dplyr::arrange(ZT)






            # saveRDS(data$circadiandatagroup, here::here("Data/circadiangroup.rds"))
            # saveRDS(data$circadiandata, here::here("Data/circadian.rds"))
            # saveRDS(data$joinedData, here::here("Data/longData.rds"))
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
