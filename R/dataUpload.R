uploadUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
                    shiny::h4("Upload a DVC file"),
                     shiny::fileInput(inputId = ns("fileUpload"),
                                       label = "Upload a DVC file as csv",
                                       buttonLabel = "Upload",
                                       accept = ".csv"),
                    shiny::uiOutput(outputId = ns("Table"))
                    )
                    )
  )
}

upload <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
       ns <- shiny::NS(id)

       #Register user input. Check they have submitted a csv file.
       #If CSV file is submitted, display data in table below.
       #Press 'process data' to proceed

       shiny::observeEvent(input$fileUpload,{
           datafile <- input$fileUpload
           ext <- tools::file_ext(datafile$datapath)
           req(datafile)
           validate(need(ext == "csv", "Please upload a csv file"))
           data$rawData <- vroom::vroom(datafile$datapath,
                                        show_col_types = F,
                                        delim = ";",
                                        locale = vroom::locale(decimal_mark = ","))

           #extract group names from column headers
           testdataCols <- colnames(data$rawData)
           testGroups <- stringr::str_subset(testdataCols,
                                             pattern = "_TIMESTAMP$")
           testGroups <- stringr::str_replace(testGroups,
                                              pattern = "_TIMESTAMP",
                                              replacement = "")
           data$groups <- testGroups
           #remove irrelevant columns
           data$trimmedData<- data$rawData |>
             dplyr::select(!tidyselect::ends_with(c("QRT",
                                                    "AVG",
                                                    "SEM",
                                                    "SAMPLES")
             )
             )
         }
         )
      output$Table<-  shiny::renderUI({
        req(data$trimmedData)
        shiny::tagList(
                    shiny::actionButton(inputId = ns("ProcessData"),
                              label = "Process data",
                              icon = shiny::icon("arrow-right")
                              ),
                    rhandsontable::rHandsontableOutput(outputId = ns("groupOverview")),
                    rhandsontable::rHandsontableOutput(outputId = ns("dataScreen"))
          )
      })
      output$dataScreen <- rhandsontable::renderRHandsontable({
        req(data$trimmedData)
        rhandsontable::rhandsontable(data$trimmedData)
      })

      #Group overview
      output$groupScreen <- rhandsontable::renderRHandsontable({
        req(data$groups)
        rhandsontable::rhandsontable(data$groups)
      })

      shiny::observeEvent(input$ProcessData,{
        #prepare data in long format for plotting

        TimeZero <- data$trimmedData$relativeTime[2]/3600
        testdataLong <- data$trimmedData |>
          #we remove the first row as this one does not represent a full hour,
          #but when the animals were placed in the system
          dplyr::slice(-1) |>
          dplyr::select(- tidyselect::ends_with("TIMESTAMP"),
                        -day,
                        -minute,
                        - tidyselect::ends_with("SAMPLES")) |>
          dplyr::mutate(TimeElapsed = relativeTime/3600-TimeZero) |>
          dplyr::select(-relativeTime) |>
          tidyr::pivot_longer(cols = - c(TimeElapsed, hour),
                              names_to = "ID",
                              values_to = "Rawdata") |>
          dplyr::group_by(ID) |>
          dplyr::arrange(TimeElapsed) |>
          dplyr::mutate(Rawdata = as.numeric(stringr::str_replace_all(Rawdata, ",", ".")),
                        Value = Rawdata - dplyr::lead(Rawdata,
                                                      default = dplyr::last(Rawdata)))

        #calculate rolling mean and rolling sd
        testdataLong <- testdataLong |>
          dplyr::mutate(RollingSDpoint = zoo::rollapply(Value,
                                                        50,
                                                        sd,
                                                        fill = NA,
                                                        partial = T),
                        RollingMeanpoint = zoo::rollapply(Value,
                                                          50,
                                                          mean,
                                                          fill = NA,
                                                          partial = T),
                        #exclude outliers when RollingMean + 2*RollingSD is larger than value
                        Include = dplyr::case_when(
                          abs(RollingMeanpoint) + 2*abs(RollingSDpoint) < abs(Value) ~ FALSE,
                          abs(RollingMeanpoint) + 2*abs(RollingSDpoint) > abs(Value) ~ TRUE
                        )) |>
          dplyr::filter(Include == TRUE) |>
          dplyr::mutate(AccumulatedValue = cumsum(Value),
                        RollingSDAcc = zoo::rollapply(AccumulatedValue,
                                                      50,
                                                      sd,
                                                      fill = NA,
                                                      partial = T),
                        RollingMeanAcc = zoo::rollapply(AccumulatedValue,
                                                        50,
                                                        mean,
                                                        fill = NA,
                                                        partial = T),
                        DeltaRollingMean = RollingMeanAcc - lag(RollingMeanAcc)
          ) |>
          dplyr::rename(Individual= ID)

        #function to add group based on ID
        testdataLong$Group <- NA
        testdataLong <- purrr::map_dfr(data$groups,
                                               ~ dplyr::mutate(
                                                 testdataLong,
                                                 Group = dplyr::case_when(
                                                   stringr::str_detect(Individual, .x) == TRUE ~ .x,
                                                   TRUE ~ as.character(Group)
                                                 )
                                               )
        ) |>
          dplyr::filter(!is.na(Group)) |>
          dplyr::arrange(TimeElapsed)

        data$longData <- testdataLong

        testdataGroup <- testdataLong |>
          dplyr::group_by(Group, TimeElapsed) |>
          dplyr::summarise(meanRawdata = mean(Rawdata),
                           sdRawdata = sd(Rawdata),
                           meanAccumulated = mean(AccumulatedValue),
                           sdAccumulated = sd(AccumulatedValue),
                           meanIncremental = mean(DeltaRollingMean),
                           sdIncremental= sd(DeltaRollingMean),
                           meanRolling = mean(RollingMeanAcc),
                           sdRolling = sd(RollingMeanAcc))
        data$groupeddata <- testdataGroup



        #switch to next tab
        shiny::updateTabsetPanel(session = parentSession,
                                 inputId = "inTabset",
                                 selected = "summaryFig")

      })

      })
     }

