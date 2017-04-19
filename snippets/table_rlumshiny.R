info_table_rlumshiny <- function() {
  tags$div(class = "table-responsive",
           tags$table(class = "table table-hover table-condensed",
                      tags$thead(tags$th("QR Code"),
                                 tags$th("Link"),
                                 tags$th("Description")
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_rlumshiny-github.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "https://github.com/tzerk/RLumShiny")),
                          tags$td("GitHub repository of the R package 'RLumShiny'")
                        ),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_rlumshiny-article.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "http://www.ecu.edu/cs-cas/physics/ancient-timeline/upload/ATL_34-2_Burow_p22-32.pdf")),
                          tags$td("'Ancient TL' journal article")
                        ),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_rlumshiny-server.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "http://rlum.geographie.uni-koeln.de")),
                          tags$td("Freely accessible Shiny Server of the Insitute of Geography (University of Cologne)")
                        )
                      )
                      
           ) 
  )
}