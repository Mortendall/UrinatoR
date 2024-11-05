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

          #extract time stamp from rawdata
          TimeZero <- data$rawData$relativeTime[2]/3600

          timestamp <- data$rawData |>
            duckplyr::slice(-1) |>
            #remove summary columns
            #THIS IS WHERE WE START
            duckplyr::select(duckplyr::first(tidyselect::ends_with("TIMESTAMP")),
                             relativeTime,
                             day,
                             hour,
                             minute) |>
            #order data by calculating TimeElapsed
            duckplyr::mutate(TimeElapsed = relativeTime / 3600 - TimeZero) |>
            duckplyr::select(tidyselect::ends_with("TIMESTAMP"),
                             TimeElapsed,
                             day,
                             hour,
                             minute)
          stamp_name <- colnames(timestamp)[1]
          timestamp <- timestamp |>
            duckplyr::rename(TIMESTAMP=stamp_name)

          if(input$outputtype == "Individual"){
            timestamp <- timestamp |>
              duckplyr::select(TIMESTAMP, TimeElapsed)

           data_export <- duckplyr::left_join(data$joinedData,
                                               timestamp,
                                               by = c("TimeElapsed"="TimeElapsed"))

            #sometimes multiple events will occur which will cause trouble in matching for export.
            #this is only a problem for making it wider. Hence it should be filtered out
            data_export <- data_export |>
              duckplyr::mutate(unique_ID = paste(ID, TimeElapsed)) |>
              duckplyr::distinct(unique_ID, .keep_all = T) |>
              duckplyr::select(-unique_ID)


            event_export <- data_export |>
              duckplyr::select(TimeElapsed, ID, event) |>
              tidyr::pivot_wider(names_from = ID,
                                 values_from = event,
                                 values_fill = NA)

            urinatordata$Rawdata <- data_export  |>
              dplyr::select(ID, day, hour, minute, TIMESTAMP, TimeElapsed, Rawdata) |>
               tidyr::pivot_wider(names_from = ID,
                                  values_from = Rawdata,
                                  values_fill = NA) |>
              duckplyr::left_join(event_export, by = c("TimeElapsed"="TimeElapsed"))


            urinatordata$Cumulative <- data_export  |>
              dplyr::select(ID, day, hour, minute, TimeElapsed, CumulativeNormalized)|>
               tidyr::pivot_wider(names_from = ID,
                                  values_from = CumulativeNormalized,
                                  values_fill = NA )|>
              duckplyr::left_join(event_export, by = c("TimeElapsed"="TimeElapsed"))

            urinatordata$Incremental <-  data_export  |>
              dplyr::select(ID, day, hour, minute, TIMESTAMP, TimeElapsed, NormalizedValue)|>
               tidyr::pivot_wider(names_from = ID,
                                  values_from = NormalizedValue,
                                  values_fill = NA) |>
               duckplyr::left_join(event_export, by = c("TimeElapsed"="TimeElapsed"))

            circadian1 <- data$circadiandata |>
              dplyr::select(ID, ZT, meanNormalizedcircadian) |>
              tidyr::pivot_wider(names_from = ID,
                                 names_prefix = "Mean_",
                                 values_from = meanNormalizedcircadian)
            circadian2 <- data$circadiandata |>
              dplyr::select(ID, ZT, semNormalized) |>
              tidyr::pivot_wider(names_from = ID,
                                 names_prefix = "SEM_",
                                 values_from = semNormalized)
            urinatordata$Circadian <- dplyr::left_join(circadian1,
                                                       circadian2,
                                                       by = c("ZT" = "ZT"))

          }

          else if(input$outputtype == "Grouped"){
            data_export <- duckplyr::left_join(timestamp,
                                               data$groupeddata,
                                               by = c("TimeElapsed"="TimeElapsed"))

            #sometimes multiple events will occur which will cause trouble in matching for export.
            #this is only a problem for making it wider. Hence it should be filtered out
            # data_export <- data_export |>
            #   duckplyr::mutate(unique_ID = paste(ID, TimeElapsed)) |>
            #   duckplyr::distinct(unique_ID, .keep_all = T) |>
            #   duckplyr::select(-unique_ID)

            #raw data
            urinatordata$Rawdata <- data_export |>
              dplyr::select(day, hour, minute,Group,  TimeElapsed, meanRawdata) |>
              duckplyr::mutate(Group = paste(Group, "_Mean")) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = meanRawdata)
            SEM_data_raw <- data_export |>
              duckplyr::select(semRawdata,TimeElapsed, Group) |>
              duckplyr::mutate(Group = paste(Group, "_SEM")) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = semRawdata)
            urinatordata$Rawdata <- duckplyr::left_join(urinatordata$Rawdata,
                                                        SEM_data_raw,
                                                        by = c("TimeElapsed"="TimeElapsed"))

            #Cumulative data
            urinatordata$Cumulative <- data_export |>
              dplyr::select(Group,  TimeElapsed, meanCumulativeNorm) |>
              duckplyr::mutate(Group = paste(Group, "_Mean")) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = meanCumulativeNorm)
            SEM_cumulative <- data_export |>
              duckplyr::select(semCumulativeNorm,TimeElapsed, Group) |>
              duckplyr::mutate(Group = paste(Group, "_SEM")) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = semCumulativeNorm)
            urinatordata$Cumulative <- duckplyr::left_join(urinatordata$Cumulative,
                                                        SEM_cumulative,
                                                        by = c("TimeElapsed"="TimeElapsed"))

            #Incremental
            urinatordata$Incremental <-  data_export|>
              dplyr::select(Group, TimeElapsed, meanNormalized) |>
              duckplyr::mutate(Group = paste(Group, "_Mean")) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = meanNormalized)
            SEM_Normalized <- data_export |>
              duckplyr::select(semNormalized,TimeElapsed, Group) |>
              duckplyr::mutate(Group = paste(Group, "_SEM")) |>
              tidyr::pivot_wider(names_from = Group,
                                 values_from = semNormalized)
            urinatordata$Incremental <- duckplyr::left_join(urinatordata$Incremental,
                                                        SEM_Normalized,
                                                        by = c("TimeElapsed"="TimeElapsed"))


            #circadian
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


