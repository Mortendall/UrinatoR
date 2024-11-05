welcomeui <- function(id) {
  ns <- shiny::NS(id)
   bslib::page_fluid(
     bslib::layout_columns(
       col_widths = c(3, 6, 3),
       row_heights = 12,
       shiny::column(12),
      bslib::layout_columns(
        col_widths = 12,
        row_heights = c(6,6,6),
         shiny::column(12),
           bslib::card(fill = F,
                       height = "300px",
           bslib::card_body(

             shiny::h4("Welcome to Urinator!"),
             shiny::h5(
               "To begin data analysis, press the button below.
                                 To learn more about Urinator please see our paper: [paper URL]"
             ),
             shiny::br(),
             shiny::br(),
             shinyWidgets::actionBttn(inputId = ns("start"),
                                      label = "press button to begin")
           ),
           align = "center",
         ),
         shiny::column(12)),
       shiny::column(12)
     )
   )

}

welcome <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- shiny::NS(id)

      shiny::observeEvent(input$start,{
        updateTabsetPanel(session = parentSession,
                          inputId = "inTabset",
                          selected = "uploadPage")
      })
    }
      )
  }
