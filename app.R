# Dependencies
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(zoo)
library(rworldmap)
source("global.R")


################################################################################
####                          SERVER                                          ##
################################################################################
ui <- dashboardPage(
  
  ## ------------------------------------------------------------------------ ##
  ##                             HEADER
  ## ------------------------------------------------------------------------ ##
  dashboardHeader(title = "EGU 2017 - PICO",
                  tags$li(class = "dropdown",
                          actionLink("aboutBtn", " About", icon = icon("info-circle"))),
                  tags$li(class = "dropdown", 
                          tags$a(id = "btnFullscreen", icon("arrows-alt"), " Toggle Fullscreen")),
                  tags$li(class = "dropdown",
                          tags$a(href = "", icon("refresh"), " Reload"))
                  
  ),## EndOf::HEADER
  
  ## ------------------------------------------------------------------------ ##
  ##                             SIDEBAR
  ## ------------------------------------------------------------------------ ##
  dashboardSidebar(
    
    # Talk title
    tags$p(id = "talk-title", align = "center",
           HTML("Easing access to R using ‘shiny’ to create graphical user interfaces:</br>",
                "An example for the R package ‘Luminescence’")),
    tags$hr(id = "title_hr"),
    
    # Sidebar navigation items
    sidebarMenu(id = "sidebar",
                
                # Menus
                menuItem("Introduction", icon = icon("play"), tabName = "intro",
                         menuSubItem(HTML("The p<b>R</b>oblem"), icon = icon("question-circle-o"), tabName = "intro_1"),
                         menuSubItem("A solution?", icon = icon("lightbulb-o"), tabName = "intro_2")),
                
                menuItem("The 'shiny' framework", icon = icon("star"), tabName = "shiny",
                         menuSubItem("Hello Shiny!", icon = icon(""), tabName = "shiny_1"),
                         menuSubItem("Widgets", icon = icon(""), tabName = "shiny_2"),
                         menuSubItem("Deploying apps", icon = icon(""), tabName = "shiny_3")),
                
                menuItem("From CLI...", icon = icon("terminal"), tabName = "lum",
                         menuSubItem("'Luminescence' package", icon = icon(""), tabName = "lum_1"),
                         menuSubItem("How it all started", icon = icon(""), tabName = "lum_2"),
                         menuSubItem("Current content", icon = icon(""), tabName = "lum_3"),
                         menuSubItem("Reception", icon = icon(""), tabName = "lum_4")),
                
                menuItem("...to GUI", icon = icon("television"), tabName = "shinylum",
                         menuSubItem("'RLumShiny' package", icon = icon(""), tabName = "shinylum_1"),
                         menuSubItem("Examples", icon = icon(""), tabName = "shinylum_2")),
                
                menuItem("Get started!", icon = icon("rocket"), tabName = "getstarted")
    ),#EndOf::SideBarMenu
    
    # QR Code to presentation
    tags$hr(id = "qr_hr"),
    tags$p(id = "qr_presentation_text", align = "center",
           "Watch this PICO presentation on you smartphone or tablet!"),
    tags$p(id = "qr_presentation", align = "center",
           tags$img(src = "img/qr_presentation_tablet.png", style = "width:100%;", border = 0)
    )
    
    ### Uncomment this to show a slider used for CSS zooming (see JQuery code below)
    # tags$hr(id = "qr_hr"),
    # sliderInput("zoom", "Zoom", min = 0.1, max = 2.0, value = 1.0, step = 0.1)
    
    # tags$hr(id = "qr_hr"),
    # tags$p(align = "center", valueBoxOutput("approval", width = 12))
    
  ),#EndOf::SIDEBAR
  
  ## ------------------------------------------------------------------------ ##
  ##                             BODY
  ## ------------------------------------------------------------------------ ##
  dashboardBody(
    
    ## -----
    ## Include (javascript, css)
    tags$header(
      tags$script(src = "js/fullscreen.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
      
      ### Uncomment this code to include JQuery zoom function (buggy on IE)
      #       tags$script("
      # $(document).on('shiny:inputchanged', function(event) {
      #   if (event.name === 'zoom') {
      #     $('body').css('zoom', event.value);
      #   }
      # });")
    ),
    
    tabItems(
      ## Introduction ----
      tabItem("intro_1",
              fluidRow(
                box(width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
                    title = "",
                    div(align = "center",
                        tags$p(id = "title", title),
                        tags$p(id = "authors", authors),
                        tags$hr()
                    ),
                    fluidRow(
                      box(width = 8, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                          title = "Abstract",
                          tabBox(width = 12,
                                 tabPanel("I", HTML(abstract[[1]])),
                                 tabPanel("II", HTML(abstract[[2]])),
                                 tabPanel("III", HTML(abstract[[3]]))
                          )),
                      box(width = 4, status = "warning", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
                          title = "Affiliations",
                          tabBox(width = 12,
                                 tabPanel("[1]", HTML(affils[[1]])),
                                 tabPanel("[2]", HTML(affils[[2]])),
                                 tabPanel("[3]", HTML(affils[[3]])),
                                 tabPanel("[4]", HTML(affils[[4]])),
                                 tabPanel("[5]", HTML(affils[[5]]))
                          ))
                    )
                )
              ),
              fluidRow(
                box(width = 12, solidHeader = FALSE,
                    title = NULL,
                    fluidRow(width = 12,
                             column(width = 1,
                                    actionButton("problem", "", icon = icon("info-circle"), class = "info-btn")),
                             column(width = 11,
                                    div(align = "left", 
                                        HTML("<blockquote class = 'blockquote-reverse'>"), 
                                        tags$p(id = "reality", 
                                               HTML("&laquo; Working with the command-line interface (CLI) of R can be tedious at best and overwhelming at worst. &raquo;")),
                                        HTML("<footer>An anonymous user<b>R</b></footer>"),
                                        HTML("</blockquote>"))
                             )
                    ),
                    box(width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                        title = "Expectation",
                        tags$img(src = "img/expectation.gif", style = "width:100%;", border = 0)),
                    box(width = 6, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                        title = "Reality",
                        tags$img(src = "img/reality.gif", style = "width:100%;", border = 0))
                    
                )
              )
      ),
      tabItem("intro_2",
              fluidRow(
                box(width = 12, solidHeader = FALSE,
                    title = NULL,
                    fluidRow(width = 12,
                             column(width = 1,
                                    actionButton("solution", "", icon = icon("info-circle"), class = "info-btn")),
                             column(width = 11,
                                    div(align = "left", 
                                        HTML("<blockquote class = 'blockquote-reverse'>"), 
                                        tags$p(id = "reality", 
                                               HTML("&laquo; For users with little or no experience with command-lines (CLI) a 
                                                    graphical user interface (GUI) offers
                                                    intuitive access that counteracts the perceived steep learning
                                                    curve of a CLI &raquo;")),
                                        HTML("<footer>Burow et al. (2016)</footer>"),
                                        HTML("</blockquote>"))
                             )
                    ),
                    box(width = 6, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
                        title = "The desired outcome produced via...",
                        tags$p(HTML("<b>Consider the following situation:</b></br> You, as a scientist, are given the task
                                    to visualise your data in a more complex chart using <b>R</b>. Let us further
                                    assume that someone else already provided a custom <b>R</b> function that
                                    produces this kind of non-standard plot (here: <code>plot_AbanicoPlot()</code>). The desired plot may look like the one below.")),
                        plotOutput("solution_plot")),
                    tabBox(title = NULL, side = "left",
                           tabPanel(title = tagList(icon("terminal"), HTML("&nbsp;&nbsp;CLI")), width = 6,
                                    HTML("For the plot to look like it does right now the user would be required to write the
                                    following <b>R</b> code:"),
                                    htmlOutput("solution_plot_code")),
                           tabPanel(title = tagList(icon("television"), HTML("&nbsp;&nbsp;GUI")), width = 6,
                                    tags$p(HTML(
                                      "Alternatively, you could be provided a graphical user interface with input
                                      widgets that control the numerous arguments of this particular function.
                                      You would no longer be required to look up all the arguments and their
                                      required input type and/or structure."
                                    )),
                                    fluidRow(width = 12,
                                             column(width = 3,
                                                    checkboxInput("solution_rotate", "Rotate", FALSE)),
                                             column(width = 3, 
                                                    checkboxInput("solution_kde", "KDE", TRUE)),
                                             column(width = 3,
                                                    checkboxInput("solution_hist", "Histogram", FALSE)),
                                             column(width = 3, 
                                                    checkboxInput("solution_dots", "Dots", FALSE))
                                    ),
                                    sliderInput("solution_ratio", "Plot ratio", min = 0, max = 1, value = 0.75),
                                    sliderInput("solution_cex", "Scaling", min = 0, max = 1, value = 0.95),
                                    tags$p(HTML(HTML(
                                      "To be fair, this is of course only a small subset of input widgets required
                                      to control all the options listed in the CLI code. But for a user new to <b>R</b>
                                      we could safely assume that a GUI is the much more comofortable alternative.</br>
                                      In the next section <code>\"The 'shiny' framework\"</code> you will learn about the basic
                                      structure of a 'shiny' application, the different input widgets and how to share
                                      the applications with others."
                                    )))))
                )
              )
      ),
      
      ## Shiny framework ----
      # Hello Shiny app
      tabItem("shiny_1",
              fluidRow(
                tabBox(title = actionButton("hello_shiny_modal", "", icon = icon("info-circle"), class = "info-btn"),
                       width = 6,
                       tabPanel("Ui.R", htmlOutput("helloshiny_code_ui")),
                       tabPanel("Server.R", htmlOutput("helloshiny_code_server"))
                ),
                box(width = 6, status = "primary", solidHeader = TRUE,
                    title = "Hello Shiny!",
                    plotOutput("helloshiny"),
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
                )
              )
      ),
      # Input widgets
      tabItem("shiny_2",
              
              fluidRow(
                column(width = 6,
                       
                       ## 
                       box(width = 12, solidHeader = TRUE, collapsible = TRUE,
                           title = tags$b("Input Widgets"),
                           tags$p(HTML("This is a non-exhaustive collection of input widgets available in the
                                       <code>shiny</code> framework. Most of the widgets here are bound to a parameter of the
                                       plot on the right-hand side. Feel free to test them all and observe its effect
                                       on the plot. You can also check the <b>R</b> console output to see the
                                       value returned by each widget.")),
                           fluidRow(width = 12,
                                    box(title = "Action button",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE,
                                        width = 6,
                                        actionButton("actionBtn", "Do NOT press!", class = "btn btn-danger",
                                                     style = "color: white;", width = "100%")
                                    ),
                                    box(title = "Checkbox",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE,
                                        width = 6,
                                        checkboxInput("checkbox", "Check this out!", value = FALSE)
                                    )
                           ),
                           fluidRow(width = 12,
                                    box(title = "Checkbox group", 
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        checkboxGroupInput("checkboxGroup", "Check us out!", 
                                                           selected = c("Blue", "Green", "Red"),
                                                           choices = list("Choice 1" = "Blue", 
                                                                          "Choice 2" = "Green",
                                                                          "Choice 3" = "Red"))
                                    ),
                                    box(title = "Date input",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        dateInput("dateInput", "Date input"))
                           ),
                           fluidRow(width = 12,
                                    box(title = "Date range", 
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        dateRangeInput("dateRange", "Date range") 
                                    ),
                                    box(title = "File input",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        fileInput("fileInput", "File input"))
                           ),
                           fluidRow(width = 12,
                                    box(title = "Numeric input", 
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        numericInput("numericInput", "Numeric input", value = 42) 
                                    ),
                                    box(title = "Radio buttons",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        radioButtons("radioButtons", "Radio buttons", 
                                                     choices = list("Choice 1" = 1,
                                                                    "Choice 2" = 2,
                                                                    "Choice 3" = 3)))
                           ),
                           fluidRow(width = 12,
                                    box(title = "Select box", 
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        selectInput("selectBox", "Select box", 
                                                    choices = list("Choice 1" = 16,
                                                                   "Choice 2" = 1)) 
                                    ),
                                    box(title = "Slider",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        sliderInput("slider", "Slider", 
                                                    min = 1, max = 10, value = 2, step = 1))
                           ),
                           fluidRow(width = 12,
                                    box(title = "Slider range", 
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        sliderInput("sliderRange", "Slider range", 
                                                    min = 0, max = 100, value = c(0, 100), step = 5)
                                    ),
                                    box(title = "Text input",
                                        status = "primary",
                                        solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                        width = 6,
                                        textInput("textInput", "Text input", placeholder = "Enter text...")
                                    )
                           )
                       )
                       ##
                ),
                column(width = 6,
                       tabBox(width = 12,
                              title = NULL,
                              tabPanel(title = tagList(icon("line-chart"), HTML("&nbsp;&nbsp;Plot")),
                                       plotOutput("widget_plot")),
                              tabPanel(title = tagList(icon("terminal"), HTML("&nbsp;&nbsp;Console")),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Action button"),
                                                column(width = 10, verbatimTextOutput("widget_code_actionBtn"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Checkbox"),
                                                column(width = 10, verbatimTextOutput("widget_code_checkbox"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Checkbox group"),
                                                column(width = 10, verbatimTextOutput("widget_code_checkboxGroup"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Date input"),
                                                column(width = 10, verbatimTextOutput("widget_code_dateInput"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Date range"),
                                                column(width = 10, verbatimTextOutput("widget_code_dateRange"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "File input"),
                                                column(width = 10, verbatimTextOutput("widget_code_fileInput"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Numeric input"),
                                                column(width = 10, verbatimTextOutput("widget_code_numericInput"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Radio buttons"),
                                                column(width = 10, verbatimTextOutput("widget_code_radioButtons"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Select box"),
                                                column(width = 10, verbatimTextOutput("widget_code_selectBox"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Slider"),
                                                column(width = 10, verbatimTextOutput("widget_code_slider"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "Slider range"),
                                                column(width = 10, verbatimTextOutput("widget_code_sliderRange"))
                                       ),
                                       fluidRow(width = 12, 
                                                column(width = 2, "textInput"),
                                                column(width = 10, verbatimTextOutput("widget_code_textInput"))
                                       )
                              )
                       )
                )
              )
      ),
      # Deploying apps
      tabItem("shiny_3",
              box(title = NULL, width = 12,
                  fluidRow(width = 12,
                           column(width = 1,
                                  actionButton("info_deploy", "", icon = icon("info-circle"), class = "info-btn")),
                           column(width = 11,
                                  div(align = "left", 
                                      HTML("<blockquote class = 'blockquote-reverse'>"), 
                                      tags$p(id = "reality", 
                                             HTML("&laquo; Sharing shiny applications is simple! &raquo;")),
                                      HTML("<footer>Another anonymous user<b>R</b></footer>"),
                                      HTML("</blockquote>"))
                           )
                  )
              ),
              box(title = NULL,
                  id = "deploy_1",
                  width = 12,
                  tabBox(width = 12,
                         tabPanel(title = tagList(icon("cloud"), "Shinyapps.io"),
                                  tags$p("Placeholder")
                         ),
                         tabPanel(title = tagList(icon("cloud"), "Shiny Server"),
                                  tags$p("Placeholder")
                         ),
                         tabPanel(title = tagList(icon("desktop"), "Run locally"),
                                  tags$p("Placeholder")
                         )
                  ))
      ),
      ## Luminescence ----
      tabItem("lum_3",
              box(width = 12, status = "primary", solidHeader = FALSE,
                  title = HTML("Functions in the <b>R</b> package 'Luminescence' (v0.8.0)"),
                  dataTableOutput("lum_functions"))
      ),
      tabItem("lum_4",
              box(width = 4, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  title = HTML("Statistics"),
                  radioButtons("cran_package", "R package", 
                               choices = list("Luminescence" = "luminescence",
                                              "RLumShiny" = "rlumshiny")),
                  radioButtons("cran_category", "Statistic",
                               choices = list("Date" = "date",
                                              "Country" = "country",
                                              "OS" = "os",
                                              "R Version" = "r_version",
                                              "Architecture" = "arch"))),
              box(width = 8, status = "warning", solidheader = TRUE, collapsible = TRUE,
                  title = HTML("Plot"),
                  plotOutput("cran_plot"))
      ),
      
      ## RLumShiny ----
      tabItem("shinylum_1",
              tabBox(title = HTML("The <b>R</b> package 'RLumShiny'"),
                     width = 12,
                     tabPanel("Applications"),
                     tabPanel("Functions"))
      ),
      tabItem("shinylum_2",
              tabBox(title = "RLumShiny applications",
                     id = "examples_1",
                     width = 12,
                     tabPanel("Abanico Plot", uiOutput("abanico")),
                     tabPanel("KDE", uiOutput("kde")),
                     tabPanel("Histogram", uiOutput("hist")))
      ),
      tabItem("getstarted",
              box(title = NULL, width = 12,
                  tags$p("Here you can find a small collection of useful resources to get started."),
                  tabBox(title = NULL, width = 8,
                         tabPanel(title = HTML("<code>shiny</code>"),
                                  tags$div(class = "table-responsive",
                                           tags$table(class = "table table-hover table-condensed",
                                                      tags$thead(tags$th("QR Code"),
                                                                 tags$th("Link"),
                                                                 tags$th("Description")
                                                      ),
                                                      tags$tbody(
                                                        tags$tr(
                                                          tags$td(tags$img(src = "img/qr_shiny-gallery.png", style = "width:100%; max-width: 50px;", border = 0)),
                                                          tags$td(tags$a(href = "#", "https://shiny.rstudio.com/gallery/")),
                                                          tags$td(tags$p("Gallery of example apps"))),
                                                        tags$tr(
                                                          tags$td(tags$img(src = "img/qr_shiny-tutorial.png", style = "width:100%; max-width: 50px;", border = 0)),
                                                          tags$td(tags$a(href = "#", "https://shiny.rstudio.com/tutorial/")),
                                                          tags$td("How to build a Shiny app")
                                                        )  
                                                      )
                                                      
                                           ) 
                                  )
                         ),
                         tabPanel(title = HTML("<code>Luminescence</code>")),
                         tabPanel(title = HTML("<code>RLumShiny</code>"))
                  ),
                  box(title = HTML("This presentation"), width = 4, status = "warning",
                      solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                      tags$p("The code of this presentation is freely available on", icon("github"),"GitHub"),
                      tags$img(src = "img/qr_github.png", style = "width:100%;", border = 0),
                      div(align = "center", tags$a(id = "github", href = "#", "https://github.com/tzerk/EGU2017-PICO")))
              ))
    )#EndOf::tabItems
  )#EndOf::dashboardBody
)#EndOf::BODY

################################################################################
####                          SERVER                                          ##
################################################################################
server <- function(input, output, session) {
  
  ## Header
  observeEvent(input$aboutBtn, {
    showModal(modalDialog(title = "About this presentation", easyClose = TRUE,
                          tabBox(width = 12,
                                 tabPanel(icon("info-circle"), about_text),
                                 tabPanel("sessionInfo()", verbatimTextOutput("session_info"))
                          )
    )
    )
  })
  output$session_info <- renderPrint({ sessionInfo() })
  
  # output$approval <- renderInfoBox({ 
  #   valueBox("80 %", "Approval", 
  #            icon = icon("thumbs-up", lib = "glyphicon"), color = "light-blue") 
  #   })
  
  ## Introduction
  observeEvent(input$problem, {
    showModal(modalDialog(title = HTML("The p<b>R</b>oblem"), easyClose = TRUE,
                          problem_text))
  })
  
  output$solution_plot <- renderPlot({
    solution_plot(data, code = FALSE, 
                  cex = input$solution_cex, 
                  ratio = input$solution_ratio,
                  rotate = input$solution_rotate,
                  kde = input$solution_kde,
                  hist = input$solution_hist,
                  dots = input$solution_dots)
  })
  output$solution_plot_code <- renderUI({
    file <- tempfile()
    solution_plot(data, code = TRUE, 
                  cex = input$solution_cex,
                  ratio = input$solution_ratio,
                  rotate = input$solution_rotate,
                  kde = input$solution_kde,
                  hist = input$solution_hist,
                  dots = input$solution_dots) %>% 
      write(file)
    highlight(file, renderer = renderer_html(), output = NULL) %>% 
      paste(collapse = "") %>% 
      HTML()
  })
  
  observeEvent(input$solution, {
    showModal(modalDialog(title = HTML("A solution?"), easyClose = TRUE,
                          solution_text))
  })
  
  ## Luminescence
  output$cran_plot <- renderPlot({
    
    data <- switch(input$cran_package,
                   "luminescence" = list(cran$luminescence),
                   "rlumshiny" = list(cran$rlumshiny))
    
    fun <- switch(input$cran_category,
                  "date" = gg_Timeline(data),
                  "country" = gg_Map(data),
                  "os" = gg_ByOS(data),
                  "r_version" = gg_ByVersion(data),
                  "arch" = gg_ByArch(data))
    
    if (inherits(fun[[1]], "gg"))
      print(fun[[1]])
  })
  
  ## Shiny framework
  output$helloshiny_code_ui <- renderUI({
    helloshiny_code$ui
  })
  output$helloshiny_code_server <- renderUI({
    helloshiny_code$server
  })
  
  observeEvent(input$hello_shiny_modal, {
    showModal(modalDialog(title = HTML("What is <code>Ui.R</code> and <code>Server.R</code>?"), easyClose = TRUE,
                          tags$p("Content")))
  })
  
  output$helloshiny <- renderPlot({
    x <- faithful[ ,2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
  
  observeEvent(input$info_deploy, {
    showModal(modalDialog(title = HTML("Sharing shiny applications"), easyClose = TRUE,
                          tags$p("Placeholder")
    ))
  })
  
  ## Shiny widget code
  output$widget_code_actionBtn <- renderPrint({ print(input$actionBtn) })
  output$widget_code_checkbox <- renderPrint({ print(input$checkbox) })
  output$widget_code_checkboxGroup <- renderPrint({ print(input$checkboxGroup) })
  output$widget_code_dateInput <- renderPrint({ print(input$dateInput) })
  output$widget_code_dateRange <- renderPrint({ print(input$dateRange) })
  output$widget_code_fileInput <- renderPrint({ print(input$fileInput) })
  output$widget_code_numericInput <- renderPrint({ print(input$numericInput) })
  output$widget_code_radioButtons <- renderPrint({ print(input$radioButtons) })
  output$widget_code_selectBox <- renderPrint({ print(input$selectBox) })
  output$widget_code_slider <- renderPrint({ print(input$slider) })
  output$widget_code_sliderRange <- renderPrint({ print(input$sliderRange) })
  output$widget_code_textInput <- renderPrint({ print(input$textInput) })
  
  ## Shiny widget plot
  output$widget_plot <- renderPlot({
    
    if (is.null(input$checkboxGroup))
      return(NULL)
    
    n <- input$numericInput^1.5
    
    df <- data.frame(x = runif(n = n, 0, 100),
                     y = runif(n = n, 0, 100),
                     color = sample(c("Blue", "Green", "Red"), size = n, replace = TRUE)) %>% 
      filter(color %in% input$checkboxGroup)
    
    gg <- ggplot(df, aes(x, y, color = color)) +
      geom_point(size = input$slider, shape = as.integer(input$selectBox)) +
      xlim(input$sliderRange) + 
      ylim(input$sliderRange) +
      xlab("A random number") +
      ylab("Another random number") +
      ggtheme[[as.integer(input$radioButtons)]] +
      ggtitle(get_buttonMsg(input$actionBtn))
    
    if (input$checkbox)
      gg <- gg + facet_grid(. ~ color)
    
    gg
  })
  
  ## Luminescence functions table
  output$lum_functions <- renderDataTable({
    rlum_fun_df
  }, options = list(pageLength = 10, pagingType = "full", 
                    searching = FALSE, lengthChange = FALSE))
  
  ## RLumShiny example applications
  output$abanico <- renderUI({
    iframe$abanico
  })
  output$kde <- renderUI({
    iframe$kde
  })
  output$hist <- renderUI({
    iframe$hist
  })
  
}


# ----
shinyApp(ui, server)