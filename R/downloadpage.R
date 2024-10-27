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
                                        label = "Press to download your data as an  Excel file"))
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
            urinatordata$Rawdata <- data$longData |>
              dplyr::select(Individual, hour, TimeElapsed, Rawdata) |>
              tidyr::pivot_wider(names_from = Individual,
                                 values_from = Rawdata)
            urinatordata$Cumulative <- data$longData |>
              dplyr::select(Individual, hour, TimeElapsed, CumulativeNormalized) |>
              tidyr::pivot_wider(names_from = Individual,
                                 values_from = CumulativeNormalized)
            urinatordata$Incremental <-  data$longData |>
              dplyr::select(Individual, hour, TimeElapsed, NormalizedValue) |>
              tidyr::pivot_wider(names_from = Individual,
                                 values_from = NormalizedValue)
             circadian1 <- data$circadiandata |>
              dplyr::select(Individual, ZT, meanNormalizedcircadian) |>
              tidyr::pivot_wider(names_from = Individual,
                                 names_prefix = "Mean_",
                                 values_from = meanNormalizedcircadian)
             circadian2 <- data$circadiandata |>
               dplyr::select(Individual, ZT, semNormalized) |>
               tidyr::pivot_wider(names_from = Individual,
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

      })
    }


