
uploadUI <- function(id){
  ns <- shiny::NS(id)
  bslib::layout_columns(
   col_widths =  c(6,6),
    shiny::column(12,
                  bslib::card(bslib::card_title("Step 1: Upload a DVC data file or load demo data"),
                  shiny::selectInput(inputId = ns("seperator"),
                                     label = "Select seperator in CSV",
                                     choices = c(",", ";"),
                                     selected = ";"),
                  shiny::selectInput(inputId = ns("decimal"),
                                     label = "Select regional decimal mark",
                                     choices = c(",", "."),
                                     selected = "."),
                  shiny::fileInput(inputId = ns("fileUpload"),
                                   label = "Upload a DVC file as csv - be sure to select correct decimal and seperator!",
                                   buttonLabel = "Upload",
                                   accept = ".csv"),
                  shiny::uiOutput(outputId = ns("Table")),
                  shiny::actionButton(inputId = ns("demodata"),
                                      label = "Load demodata"))
                  ),
    shiny::column(12,
                  bslib::card(
                    bslib::card_title("Step 2: Upload an event file"),
                    bslib::card_body(
                      shiny::h6("Event file is necessary to remove cage changes from data file.
                                This is only possible after you upload a data file."),
                      shiny::uiOutput(ns("eventUI"))
                    )
                  ))
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
                                        delim = input$seperator,
                                        locale = vroom::locale(decimal_mark = input$decimal)
                                        )

           #write in check of columns if to check if right format has been submitted

           #extract group names from column headers

           testGroups <- stringr::str_subset(colnames(data$rawData),
                                             pattern = "_TIMESTAMP$")
           data$groups <- stringr::str_replace(testGroups,
                                              pattern = "_TIMESTAMP",
                                              replacement = "")


           #remove irrelevant columns
           data$trimmedData<- data$rawData |>
             dplyr::select(!tidyselect::ends_with(c("QRT",
                                                    "AVG",
                                                    "SEM",
                                                    "SAMPLES")
             )
             )



           #Data are time-stamped and vroom cannot handle that. We therefore
           #import the data again for the first time stamp and extract the info
           #we need

           headername <- stringr::str_subset(colnames(data$rawData),
                                             pattern = "_TIMESTAMP$")[1]

           firsttimestamp <- vroom::vroom(datafile$datapath,
                                          show_col_types = F,
                                          delim = input$seperator,
                                          locale = vroom::locale(decimal_mark = input$decimal),
                                          col_select = headername,
                                          col_types = vroom::cols(.default = "c"))

           timeadjust <- stringr::str_extract(firsttimestamp[1,1],
                                              pattern = "[+-]?[:digit:]{4}$")



           timeadjustTime <- lubridate::hm(paste0(stringr::str_extract(timeadjust,
                                                                      pattern = "(?<=[+-])[:digit:]{2}"),
                                                  ":",
                                                 stringr::str_extract(timeadjust,
                                                                      pattern = "[:digit:]{2}$")))

           timeadjustFactor <- as.numeric(timeadjustTime)/3600
           #check if the timestamp factor is positive or negative
           if(isTRUE(stringr::str_extract(timeadjust,
                                   pattern = "^[+-]{1}")=="+")){
             data$trimmedData <- data$trimmedData |>
               dplyr::mutate(hour = hour + timeadjustFactor)
           }
           else{
             data$trimmedData <- data$trimmedData |>
               dplyr::mutate(hour = hour - timeadjustFactor)
           }
         }
         )
      output$Table<-  shiny::renderUI({
        req(data$trimmedData)
        shiny::tagList(

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
        data$longData <- readRDS(here::here("Data/longData.rds"))
        data$circadiandata <- readRDS(here::here("Data/circadian.rds"))
        data$groupeddata <- readRDS(here::here("Data/groupeddata.rds"))
        data$circadiandatagroup <- readRDS(here::here("Data/circadiangroup.rds"))
        shiny::updateTabsetPanel(session = parentSession,
                                 inputId = "inTabset",
                                 selected = "summaryFig")
      }
      )
      #####Event UI####
      output$eventUI <- shiny::renderUI({
        req(data$trimmedData)
        shiny::tagList(

          shiny::selectInput(inputId = ns("eventSeparator"),
                             label = "Select separator for event file",
                             choices =  c(",", ";"),
                             selected = ";"),
          shiny::selectInput(inputId = ns("eventDecimal"),
                             label = "Select regional decimal mark",
                             choices = c(",", "."),
                             selected = "."),
          shiny::fileInput(inputId = ns("eventUpload"),
                           label = "Upload an event file as csv - be sure to select correct decimal and separator!",
                           buttonLabel = "Upload",
                           accept = ".csv"),
          shiny::uiOutput(outputId = ns("processbutton"))
          )

      })

      output$processbutton <- shiny::renderUI({
        req(data$events)
        req(data$trimmeddata)
        shinyWidgets::actionBttn(
          inputId = ns("ProcessData"),
          label = "Process data",
          icon = shiny::icon("arrow-right")
        )
      })

      #####Add event data####



      #####process data UI####
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
                        #-day,
                        -minute,
                        - tidyselect::ends_with("SAMPLES")) |>
          dplyr::mutate(TimeElapsed = relativeTime/3600-TimeZero) |>
          dplyr::select(-relativeTime) |>
          tidyr::pivot_longer(cols = - c(TimeElapsed, hour, day),
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
                        #originally include tested against abs(Value) instead of -Value
                        #but this was changed to only detect sharp increases
                         Include = dplyr::case_when(
                           RollingSDpoint < -Value ~ 1,
                           RollingSDpoint > -Value ~ 0
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

