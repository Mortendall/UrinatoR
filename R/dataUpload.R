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
         shiny::observeEvent(input$fileUpload,{
           datafile <- input$fileUpload
           ext <- tools::file_ext(datafile$datapath)
           req(datafile)
           validate(need(ext == "csv", "Please upload a csv file"))
           data$rawData <- data.table::fread(datafile$datapath)
         }
         )
      output$Table<-  shiny::renderUI({
        req(data$rawData)
        shiny::tagList(
                    shiny::actionButton(inputId = ns("ProcessData"),
                              label = "Process data",
                              icon = shiny::icon("arrow-right")
                              ),
                    rhandsontable::rHandsontableOutput(outputId = ns("dataScreen"))
          )
      })
      output$dataScreen <- rhandsontable::renderRHandsontable({
        req(data$rawData)
        rhandsontable::rhandsontable(data$rawData)
      })

      shiny::observeEvent()

      })
     }

