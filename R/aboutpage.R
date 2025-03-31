aboutui <- function(id){
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths =  c(3,6,3),
    shiny::column(12)
    ,
    shiny::column(12,
                  shiny::tagList(
                    shiny::h5(
                      "About UrinatoR"
                    ),
                    bslib::accordion(
                      multiple = F,
                      open = F,

                      bslib::accordion_panel(
                        title = "UrinatoR",
                        shiny::tagList(
                          "Idea and calculations for UrinatoR
                                    were developed by Thomas S. Nielsen, PhD",
                          shiny::a(href = "https://www.tsnscientific.com/",
                                   "TSN Scientific."),
                                    "UrinatoR is designed and maintained by Morten Dall, PhD ",
                          shiny::a(href="https://cbmr.ku.dk/",
                                   "(Novo Nordisk Foundation Center for Basic Metabolic Research, University of Copenhagen (CBMR))"),
                          "."
                        )

                      ),
                      bslib::accordion_panel(
                        title = "Submit feedback",
                        shiny::tagList("If you encounter bugs or have feedback, please submit it ",
                                       shiny::a(href = "mailto:dall@sund.ku.dk",
                                                "here"))
                      ),
                      bslib::accordion_panel(
                        title = "Cite UrinatoR",
                        "[Insert URL here]"
                      ),
                      bslib::accordion_panel(
                        title = "Code availability",
                        shiny::tagList(
                          "UrinatoR code is available ",
                          shiny::a(href = "https://github.com/Mortendall/UrinatoR",
                                   "here"),
                          ". ")

                      ),
                      bslib::accordion_panel(
                        title = "Funding",
                        "Development of UrinatoR was supported by grants from the Novo Nordisk Foundation NNF23SA0084103 and NNF18CC0034900"
                      ),
                      bslib::accordion_panel(
                        title = "Version notes",
                        shiny::h5("current version: 1.0.1"),
                        shiny::tagList(
                          "March 28th 2025 ",
                          tags$br(),
                          "Added features: ",
                          tags$br(),
                          "- Added custom color selection and graph customization ,and added data load safeguards",
                          tags$br(),
                          "- Fixed errors in download excel function and layout for circadian graph"),
                        tags$br(),
                        "March 12th 2025 ",
                          tags$br(),
                          "- Added function that removes events within the same time window to prevent double entries")
                  )
                  ),
    shiny::column(12)
  ))
}

about <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- shiny::NS(id)
    }
  )
}
