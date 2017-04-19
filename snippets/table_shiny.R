info_table_shiny <- function() {
  tags$div(class = "table-responsive",
           tags$table(class = "table table-hover table-condensed",
                      tags$thead(tags$th("QR Code"),
                                 tags$th("Link"),
                                 tags$th("Description")
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_shiny-gallery.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "https://shiny.rstudio.com/gallery/")),
                          tags$td(tags$p("Gallery of example apps"))),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_shiny-tutorial.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "https://shiny.rstudio.com/tutorial/")),
                          tags$td("Tutorial on how to build a Shiny app")
                        ),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_shiny-cheatsheet.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "http://shiny.rstudio.com/images/shiny-cheatsheet.pdf")),
                          tags$td("A cheat sheet for building Shiny apps")
                        ),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_shiny-dashboard.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "https://rstudio.github.io/shinydashboard/")),
                          tags$td("Building a shiny dashboard (like this presentation)")
                        ),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_shiny-server.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "https://www.rstudio.com/products/shiny/shiny-server/")),
                          tags$td("Shiny Server product information")
                        ) 
                      )
                      
           ) 
  )
}