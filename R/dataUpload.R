
uploadUI <- function(id){
  ns <- shiny::NS(id)
  bslib::layout_columns(
   col_widths =  c(6,6),
    shiny::column(12,
                  shiny::tagList(
                    bslib::card(bslib::card_title("Step 1A: Upload a DVC data file or load demo data"),
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
                  shiny::uiOutput(outputId = ns("Table"))),
                  bslib::card(bslib::card_title("Step 1B: Continue previous session"),
                              shiny::fileInput(inputId = ns("continue_session"),
                                               label = "Upload an RDS object",
                                               accept = ".rds")),
                  bslib::card(bslib::card_title("Step 1C: Load demo data"),
                              shiny::actionButton(inputId = ns("demodata"),
                                                  label = "Load demodata")
                              ))
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
            data$rawData <- load_data_file(input$fileUpload,
                                           input$separator,
                                           input$decimal)
            data$groups <- extract_group_info(data$rawData)
            data$trimmedData <- process_data(data$rawData,
                                             input$fileUpload,
                                             data$groups)
            #prepare group data
            data$groupinfo <- data.frame("CageID"= names(data$trimmedData),
                                         "No.ofAnimals" = 1)
         }
         )
      output$Table<-  shiny::renderUI({
        req(data$trimmedData)
        shiny::tagList(

                    shiny::textOutput(outputId = ns("groups"))

          )
      })
      output$groups <- shiny::renderText({
        paste("The following groups have been detected in the dataset: ",
              paste(data$groups, collapse = ", "))
      })

      #####Load demo data####
      shiny::observeEvent(input$demodata,{
        file_content <- readRDS(here::here("Data/2029-10-29_urinatordata.rds"))

        data$joinedData <- file_content$joinedData
        data$circadiandatagroup <- file_content$circadiandatagroup
        data$circadiandata <- file_content$circadiandata
        data$events <- file_content$events
        data$groupeddata <- file_content$groupeddata
        data$groupinfo <- file_content$groupinfo
        data$groups <- file_content$groups
        data$hourly <- file_content$hourly
        data$rawData <- file_content$rawData
        data$trimmedData <- file_content$trimmedData
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
          shiny::numericInput(inputId = ns("windowsetter"),
                              label = "set a range in minutes for how many data points around events that will be excluded",
                              value = 20),
          shiny::numericInput(inputId = ns("datares"),
                              label = "Resolution of data",
                              value = 10),
          shiny::uiOutput(outputId = ns("processbutton"))
          )

      })

      output$processbutton <- shiny::renderUI({
        req(data$events)
        req(data$trimmedData)
        shinyWidgets::actionBttn(
          inputId = ns("ProcessData"),
          label = "Process data",
          icon = shiny::icon("arrow-right")
        )
      })

      #####Add event data####
      shiny::observeEvent(input$eventUpload,{
        data$events <- load_event_file(input$eventUpload,
                                      input$eventSeparator,
                                      input$eventDecimal)

      })

      #####process data UI####
      #This code controls what happens when data process button is selected.
      #Data is processed and user is sent to next page
      shiny::observeEvent(input$ProcessData,{
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Processing data - this can take a while",
                     value  = 1/3)
        n <- 3
        data$joinedData <- join_event_data(data$trimmedData,
                                            data$events)


        progress$inc(1/n, detail = "Excluding data based on event window")
        data$joinedData <- exclude_cage_changes(data$joinedData,
                                                input$windowsetter,
                                                input$datares)

        data$joinedData <-data$joinedData |>
          purrr::imap_dfr(~ duckplyr::mutate(.x, ID = .y))

        progress$inc(1/n, detail = "Add groups")
        #prepare data in long format for plotting

        # TimeZero <- data$trimmedData$relativeTime[2]/3600
        # testdataLong <- data$trimmedData |>
        #   #we remove the first row as this one does not represent a full hour,
        #   #but when the animals were placed in the system
        #   dplyr::slice(-1) |>
        #   dplyr::select(- tidyselect::ends_with("TIMESTAMP"),
        #                 #-day,
        #                 -minute,
        #                 - tidyselect::ends_with("SAMPLES")) |>
        #   dplyr::mutate(TimeElapsed = relativeTime/3600-TimeZero) |>
        #   dplyr::select(-relativeTime) |>
        #   tidyr::pivot_longer(cols = - c(TimeElapsed, hour, day),
        #                       names_to = "ID",
        #                       values_to = "Rawdata") |>
        #   dplyr::group_by(ID) |>
        #   dplyr::arrange(TimeElapsed) |>
        #   dplyr::mutate(Rawdata = as.numeric(stringr::str_replace_all(Rawdata, ",", "."))
        #                 ,
        #                  Value = Rawdata - dplyr::lead(Rawdata,
        #                                     default = dplyr::last(Rawdata))
        #                 )

        #calculate rolling mean and rolling sd
        # testdataLong <- testdataLong |>
        #   dplyr::mutate(RollingSDpoint = zoo::rollapply(Rawdata,
        #                                                 48,
        #                                                 sd,
        #                                                 fill = 0,
        #                                                 partial = T,
        #                                                 align = "left"),
        #                 #originally include tested against abs(Value) instead of -Value
        #                 #but this was changed to only detect sharp increases
        #                  Include = dplyr::case_when(
        #                    RollingSDpoint < -Value ~ 1,
        #                    RollingSDpoint > -Value ~ 0
        #                  ),
        #                 CorrectedValue = dplyr::case_when(
        #                   Include == 0 ~ Value,
        #                   Include == 1 ~ NA
        #                 )
        #                 )|>
        #   dplyr::rename(Individual= ID)


        #function to add group based on ID

        data$joinedData$Group <- NA

        data$joinedData <- purrr::map_dfr(data$groups,
                                               ~ dplyr::mutate(
                                                 data$joinedData,
                                                 Group = dplyr::case_when(
                                                   stringr::str_detect(ID, .x) == TRUE ~ .x,
                                                   TRUE ~ as.character(Group)
                                                 )
                                               )
        ) |>
          dplyr::filter(!is.na(Group)) |>
          dplyr::arrange(TimeElapsed)



        #prepare group data
        data$groupinfo <- data.frame("CageID"= unique(data$joinedData$ID),
                                     "No.ofAnimals" = 1)



        #switch to next tab
        shiny::updateTabsetPanel(session = parentSession,
                                 inputId = "inTabset",
                                 selected = "preprocess")

      })

      shiny::observeEvent(input$continue_session,{
        ext <- tools::file_ext(input$continue_session$datapath)
        req(input$continue_session)
        validate(need(ext == "rds", "Please upload an rds file"))

        file_content <- readr::read_rds(input$continue_session$datapath)
          data$joinedData <- file_content$joinedData
          data$circadiandatagroup <- file_content$circadiandatagroup
          data$circadiandata <- file_content$circadiandata
          data$events <- file_content$events
          data$groupeddata <- file_content$groupeddata
          data$groupinfo <- file_content$groupinfo
          data$groups <- file_content$groups
          data$hourly <- file_content$hourly
          data$rawData <- file_content$rawData
          data$trimmedData <- file_content$trimmedData

          shiny::updateTabsetPanel(session = parentSession,
                                   inputId = "inTabset",
                                   selected = "summaryFig")
      })

      })
     }

