uploadUI <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
                    shiny::h4("Upload a DVC file or load demo data"),
                     shiny::fileInput(inputId = ns("fileUpload"),
                                       label = "Upload a DVC file as csv",
                                       buttonLabel = "Upload",
                                       accept = ".csv"),
                    shiny::actionButton(inputId = ns("demodata"),
                                        label = "Load demodata"),
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
                    shiny::textOutput(outputId = ns("groups"))
                    #,
                    #rhandsontable::rHandsontableOutput(outputId = ns("dataScreen"))
          )
      })
      output$groups <- shiny::renderText({
        paste("The following groups have been detected in the dataset: ",
              paste(data$groups, collapse = ", "))
      })
#original version included table of data, but this did not contribute much.
      #I have therefore commented it out from being rendered
      output$dataScreen <- rhandsontable::renderRHandsontable({
        req(data$trimmedData)
        rhandsontable::rhandsontable(data$trimmedData)
      })

      shiny::observeEvent(input$demodata,{
        data$longData <- readRDS(here::here("Data/testData.rds"))
        data$circadiandata <- readRDS(here::here("Data/testCircadian.rds"))
        data$groupeddata <- readRDS(here::here("Data/testGrouped.rds"))
        shiny::updateTabsetPanel(session = parentSession,
                                 inputId = "inTabset",
                                 selected = "summaryFig")
      }
      )

      #This code controls what happens when data process button is selected.
      #Data is processed and user is sent to next page
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
          dplyr::mutate(Rawdata = as.numeric(stringr::str_replace_all(Rawdata, ",", "."))
                        ,
                         Value = Rawdata - dplyr::lead(Rawdata,
                                            default = dplyr::last(Rawdata))
                        )

        #calculate rolling mean and rolling sd
        testdataLong <- testdataLong |>
          dplyr::mutate(RollingSDpoint = zoo::rollapply(Rawdata,
                                                        48,
                                                        sd,
                                                        fill = 0,
                                                        partial = T,
                                                        align = "left"),
                         Include = dplyr::case_when(
                           RollingSDpoint < abs(Value) ~ 1,
                           RollingSDpoint > abs(Value) ~ 0
                         ),
                        CorrectedValue = dplyr::case_when(
                          Include == 0 ~ Value,
                          Include == 1 ~ NA
                        )
                        )|>
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
        #prepare grouped data for figure generation
        testdataGroup <- testdataLong |>
          dplyr::group_by(Group, TimeElapsed) |>
           dplyr::summarise(meanRawdata = mean(Rawdata, na.rm = T),
                            sdRawdata = sd(Rawdata, na.rm = T),
                            meanCorrectedValue = mean(CorrectedValue, na.rm = T),
                            sdCorrectedValue = sd(CorrectedValue, na.rm = T)
           )
        data$groupeddata <- testdataGroup

        #prepare data for circadian plots
                testdataCircadian <- testdataLong |>
                  dplyr::group_by(Individual, hour)|>
                    dplyr::summarise(
                       meanIncremental = mean(CorrectedValue, na.rm = T)
                       ,sdIncremental= sd(CorrectedValue, na.rm = T))
        data$circadiandata <- testdataCircadian

        #prepare group data
        data$groupinfo <- data.frame("CageID"= unique(data$longData$Individual),
                                     "No.ofAnimals" = 1)

        #switch to next tab
        shiny::updateTabsetPanel(session = parentSession,
                                 inputId = "inTabset",
                                 selected = "preprocess")


      })

      })
     }

