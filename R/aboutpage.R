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
                                    were developed by Thomas S. Nielsen, PhD (TSN Scientific).
                                    UrinatoR is designed and maintained by Morten Dall, PhD ",
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
                      )
                    )
                  )
                  ),
    shiny::column(12)
  )
}

about <- function(id, data, parentSession){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- shiny::NS(id)
    }
  )
}
