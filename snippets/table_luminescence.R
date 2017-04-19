info_table_lum <- function() {
  tags$div(class = "table-responsive",
           tags$table(class = "table table-hover table-condensed",
                      tags$thead(tags$th("QR Code"),
                                 tags$th("Link"),
                                 tags$th("Description")
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_lum_homepage.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "http://www.r-luminescence.de/")),
                          tags$td(tags$p("R.Luminescence project page"))),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_lum-github.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "https://github.com/R-Lum/Luminescence")),
                          tags$td("GitHub repository of the R package 'Luminescence'")
                        ),
                        tags$tr(
                          tags$td(tags$img(src = "img/qr_lum-manuals.png", class = "qr-table", border = 0)),
                          tags$td(tags$a(href = "#", "http://www.r-luminescence.de/en/manuals_en.html")),
                          tags$td("Manuals for the R package 'Luminescence'")
                        )
                      )
                      
           ) 
  )
}