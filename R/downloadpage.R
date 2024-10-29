downloadUI <- function(id){
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(2,8,2),
    shiny::column(12),
    shiny::column(12,
                  bslib::card(shiny::selectInput(inputId = ns("outputtype"),
                                     choices = c("Individual",
                                                 "Grouped"),
                                     selected = "Individual",
                                     label = "Select the data you want to download"),
                  shiny::downloadButton(outputId = ns("downloadbutton"),
                                        label = "Press to download your data as an  Excel file")),
                  bslib::card(shiny::downloadButton(outputId = ns("sessionDownload"),
                                                    label = "Download your processed data as RDS object"))
                  )
  )
}

download <-function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      output$downloadbutton <-  shiny::downloadHandler(

        filename = function(){
          if(input$outputtype == "Individual"){
            paste0(Sys.Date(),"_urinatordata_individual.xlsx")
          }
          else if(input$outputtype == "Grouped"){
            paste0(Sys.Date(),"_urinatordata_grouped.xlsx")
          }
        },
        content = function(file){
          urinatordata <- list(Rawdata = NA,
                               Cumulative = NA,
                               Incremental = NA,
                               Circadian = NA)
          if(input$outputtype == "Individual"){
            urinatordata$Rawdata <- data$joinedData |>
              dplyr::select(ID, hour, TimeElapsed, Rawdata,event)
            # |>
            #   tidyr::pivot_wider(names_from = ID,
            #                      values_from = Rawdata)
            urinatordata$Cumulative <- data$joinedData |>
              dplyr::select(ID, hour, TimeElapsed, CumulativeNormalized, event)
            # |>
            #   tidyr::pivot_wider(names_from = ID,
            #                      values_from = CumulativeNormalized)
            urinatordata$Incremental <-  data$joinedData |>
              dplyr::select(ID, hour, TimeElapsed, NormalizedValue, event)
            # |>
            #   tidyr::pivot_wider(names_from = ID,
            #                      values_from = NormalizedValue)
             circadian1 <- data$circadiandata |>
              dplyr::select(ID, ZT, meanNormalizedcircadian) |>
               tidyr::pivot_wider(names_from = ID,
                                  names_prefix = "Mean_",
                                  values_from = meanNormalizedcircadian)
             circadian2 <- data$circadiandata |>
               dplyr::select(ID, ZT, semNormalized)|>
                tidyr::pivot_wider(names_from = ID,
                                   names_prefix = "SEM_",
                                   values_from = semNormalized)
            urinatordata$Circadian <- dplyr::left_join(circadian1,
                                                       circadian2,
                                                       by =c("ZT"="ZT"))

          }

          else if(input$outputtype == "Grouped"){
            urinatordata$Rawdata <- data$groupeddata |>
              dplyr::select(Group,  TimeElapsed, meanRawdata) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = meanRawdata)
            urinatordata$Cumulative <- data$groupeddata |>
              dplyr::select(Group,  TimeElapsed, meanCumulativeNorm) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = meanCumulativeNorm)
            urinatordata$Incremental <-  data$groupeddata|>
              dplyr::select(Group, TimeElapsed, meanNormalized) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = meanNormalized)
            circadian1 <- data$circadiandatagroup |>
              dplyr::select(Group, ZT, meanNormalizedGroup) |>
              tidyr::pivot_wider(names_from = Group,
                                 names_prefix = "Mean_",
                                 values_from = meanNormalizedGroup)

            circadian2 <- data$circadiandatagroup |>
              dplyr::select(Group, ZT, semNormalizedGroup) |>
              tidyr::pivot_wider(names_from = Group,
                                 names_prefix = "SEM_",
                                 values_from = semNormalizedGroup)

            urinatordata$Circadian <-dplyr::left_join(circadian1,
                                                      circadian2,
                                                      by =c("ZT"="ZT"))
          }

          openxlsx::write.xlsx(urinatordata, file = file)
        })

      output$sessionDownload <- shiny::downloadHandler(
        filename = function(){
          paste0(Sys.Date(),"_urinatordata.rds")
        },

        content = function(file){
          export_data <- list(joinedData = data$joinedData,
                              circadiandatagroup = data$circadiandatagroup,
                              circadiandata = data$circadiandata,
                              events = data$events,
                              groupeddata = data$groupeddata,
                              groupinfo = data$groupinfo,
                              groups = data$groups,
                              hourly = data$hourly,
                              rawData = data$rawData,
                              trimmedData = data$trimmedData)

          readr::write_rds(export_data, file= file)
        }
      )

      })
    }


