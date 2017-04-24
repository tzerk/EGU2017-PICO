# Dependencies
library(shiny)
library(shinydashboard)
library(RLumShiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(rworldmap)
library(leaflet)
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
                         menuSubItem("A solution", icon = icon("lightbulb-o"), tabName = "intro_2")),
                
                menuItem("The 'shiny' framework", icon = icon("star"), tabName = "shiny",
                         menuSubItem("Hello Shiny!",  tabName = "shiny_1"),
                         menuSubItem("Widgets", tabName = "shiny_2"),
                         menuSubItem("Deploying apps", tabName = "shiny_3")),
                
                menuItem(HTML("<span class='highlight-sidebar'>CLI</span> - <span class='sidebar-small'>'Luminescence' package</span>"), icon = icon("terminal"), tabName = "lum",
                         menuSubItem("Why another R package?", tabName = "lum_1"),
                         menuSubItem("Current content", tabName = "lum_2"),
                         menuSubItem("Reception", tabName = "lum_3")),
                
                menuItem(HTML("<span class='highlight-sidebar'>GUI</span> - <span class='sidebar-small'>'RLumShiny' package</span>"), icon = icon("television"), tabName = "shinylum",
                         menuSubItem("Motivation & Content", tabName = "shinylum_1"),
                         menuSubItem("Extending 'shiny'", tabName = "shinylum_2"),
                         menuSubItem("Example applications", tabName = "shinylum_3")),
                
                menuItem("Get started!", icon = icon("rocket"), tabName = "getstarted"),
                
                menuItem("References", icon = icon("book"), tabName = "references")
    ),#EndOf::SideBarMenu
    
    # QR Code to presentation
    tags$hr(id = "qr_hr"),
    tags$p(id = "qr_presentation_text", align = "center",
           "Watch this PICO presentation on your smartphone or tablet!"),
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
                                 tabPanel(tags$b("1"), HTML(affils[[1]])),
                                 tabPanel(tags$b("2"), HTML(affils[[2]])),
                                 tabPanel(tags$b("3"), HTML(affils[[3]])),
                                 tabPanel(tags$b("4"), HTML(affils[[4]])),
                                 tabPanel(tags$b("5"), HTML(affils[[5]]))
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
                    box(width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                        title = "Expectation",
                        tags$img(src = "img/expectation.gif", style = "width:100%;", border = 0)),
                    box(width = 6, status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
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
                    box(width = 6, status = "warning", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
                        title = tagList(icon("bar-chart"), HTML("&nbsp;&nbsp;Creating a plot: a common task in <b>R</b>")),
                        tags$p(HTML("<b>Consider the following situation:</b></br> You, as a scientist, are facing the task
                                    to visualise your data in a more complex chart using <b>R</b>. Let us further
                                    assume that someone else already provided a custom <b>R</b> function that
                                    produces this kind of non-standard plot (here: <code>plot_AbanicoPlot()</code>). 
                                    The desired plot may look like the one below.")),
                        plotOutput("solution_plot")),
                    tabBox(title = "Two approaches", side = "left",
                           tabPanel(title = tagList(icon("terminal"), HTML("&nbsp;&nbsp;CLI")), width = 6,
                                    HTML("Naturally, to complete the task you would fire up the <b>R</b> command-line and 
                                        start hacking away. Assuming we have the data already in place, the essential part is 
                                        is the call of the specific plotting function.
                                        For the plot to look like it does right now the user would be required to write the
                                        <b>R</b> code below. To be fair, some or even most of the arguments do not to be explicitly
                                        specified if their default values are sufficient. But there is still an overwhelming
                                         number of arguments that need to be recognized and used to make the plot appear
                                         exactly as seen on the left-hand side."),
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
                                      we could safely assume that a GUI is the much more comfortable alternative.</br>
                                      In the next section <code>The 'shiny' framework</code> you will learn more about the basic
                                      structure of a shiny application, the different input widgets and how to share
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
                box(width = 6, status = "warning", solidHeader = TRUE,
                    title = "Hello Shiny!",
                    plotOutput("helloshiny"),
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30, animate = TRUE)
                )
              )
      ),
      # Input widgets
      tabItem("shiny_2",
              
              fluidRow(
                column(width = 6,
                       
                       ## 
                       box(width = 12, status = "warning", solidHeader = FALSE, collapsible = TRUE,
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
                  ),
                  tabBox(width = 12, title = NULL,
                         tabPanel(title = tagList(icon("cloud"), "Shinyapps.io"),
                                  tags$p(
                                    HTML(
                                      "The easist and fastest way to share your self-written shiny applications
                                          is by using the commercial service <a href = '#'>Shinyapps.io</a> by RStudio. 
                                          On this self-service platform users can upload, run and share their shiny
                                          applications. The service offers different subscription plans depending
                                          on the desired number of allowed applications, service availability and
                                          feature content. A free subscription is also available."
                                    )
                                  ),
                                  tags$img(src = "img/deploy_shiny_shinyapps.io.png", style = "width:100%;", border = 0)
                         ),
                         tabPanel(title = tagList(icon("cloud"), "Shiny Server"),
                                  tags$p(
                                    HTML(
                                      "If you do not want to rely on a third-party service to host your shiny
                                          applications there is also the option to host your own instance of 
                                          <code>Shiny-Server</code>. Naturally, apart from the software itself, which is
                                          also available in an open-source version, a server or suitable webspace
                                          is required beforehand. While setting up a server yourself requires more
                                          preparation, knowledge and potentially costs, the benefit of having full
                                          control over the service can very well be justified."
                                    )
                                  ),
                                  tags$img(src = "img/deploy_shiny_shiny-server.png", style = "width:100%;", border = 0)
                         ),
                         tabPanel(title = tagList(icon("desktop"), "Run locally"),
                                  tags$p(
                                    HTML(
                                      "Running shiny applications as a service over the internet is not a must, it is always possible
                                          to run any shiny application locally on any computer with a working instance
                                          of <b>R</b> and RStudio. Shiny applications can also be included in <b>R</b> packages (like in <code>RLumShiny</code>), which
                                          can be downloaded e.g. from GitHub or, of course, from CRAN. Running a shiny application
                                          on your local computer does require a HTML5 compliant webbrowser, but no internet connection!"
                                    )
                                  ),
                                  tags$img(src = "img/deploy_shiny_local.png", style = "width:100%;", border = 0)
                         )
                  )
              )
      ),
      ## Luminescence ----
      tabItem("lum_1",
              
              box(width = 12, solidHeader = FALSE,
                  title = NULL,
                  fluidRow(width = 12,
                           column(width = 1,
                                  actionButton("rlum_history", "", icon = icon("info-circle"), class = "info-btn")
                                  ),
                           column(width = 11,
                                  div(align = "left", 
                                      HTML("<blockquote class = 'blockquote-reverse'>"), 
                                      tags$p(id = "reality", 
                                             HTML("&laquo; <b>R</b> allows the creation of complex and transparent data analysis routines for experimental protocols that are not available in existing software. &raquo;")),
                                      HTML("<footer>Kreutzer et al. (2012)</footer>"),
                                      HTML("</blockquote>"))
                           )
                  ),
                box(width = 6, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    title = "Why?",
                    tags$p(HTML(
                      "<blockquote class = 'blockquote-reverse'>
                      For routine luminescence dating applications the commonly used [measurement devices] 
                      are bundled with analysis software, such as Viewer or Analyst. These software 
                      solutions are appropriate for most of the regular dating and publication jobs, 
                      and enable assessment of luminescence characteristics and provide basic statistical 
                      data treatment. However, for further statistical analysis and data treatments, 
                      this software may reach its limits. In such cases, open programming languages 
                      are a more appropriate approach.
                      <footer>modified after Kreutzer et al. (2012)</footer>
                      </blockquote>"
                    ))),
                    tabBox(width = 6, title = "Impressions",
                        tabPanel("Analyst #1",
                                 tags$p(HTML("Reference: Duller 2015")),
                                 tags$img(src = "img/analyst_1.png", style = "width:100%;", border = 0)
                                 ),
                        tabPanel("Analyst #2",
                                 tags$p(HTML("Reference: Duller 2015")),
                                 tags$img(src = "img/analyst_2.png", style = "width:100%;", border = 0)
                                 ),
                        tabPanel("Analyst #3",
                                 tags$p(HTML("Reference: Duller 2015")),
                                 tags$img(src = "img/analyst_3.png", style = "width:100%;", border = 0)
                                 ),
                        tabPanel("Radial Plot",
                                 tags$p(HTML("Reference: Olley & Reed 2003")),
                                 tags$img(src = "img/radial-plot.png", style = "width:100%;", border = 0)
                                 ),
                        tabPanel("RadialPlotter",
                                 tags$p(HTML("Reference: Vermeesch 2009")),
                                 tags$img(src = "img/radialplotter.png", style = "width:100%;", border = 0)
                                 )
                    )
              )
              
              ),
      tabItem("lum_2",
              box(width = 4, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  title = HTML("A potpourri of functions"),
                  tags$p(HTML("Since its release in 2012 the functionality of the <b>R</b> package <code>Luminescence</code> drastically increased.
                              What started with a handful of functions to apply a very specific type of signal
                              analysis and to plot the data is now a collection of >100 functions for all sorts
                              of (non-)specialised tasks.")),
                  plotOutput("rlum_fun_history_plot")
              ),
              box(width = 8, status = "primary", solidHeader = FALSE,
                  title = HTML("Current content of <code>Luminescence</code> (v0.8.0)"),
                  dataTableOutput("lum_functions"))
      ),
      tabItem("lum_3",
              box(width = 4, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  title = HTML("Statistics"),
                  tags$p(HTML("It is generally hard to measure and keep track of the distribution and reception of
                              the <b>R</b> package <code>Luminescence</code> (or <code>RLumShiny</code>). The official <b>CRAN</b> download statistics,
                              however, may provide at least some indication on how many and where people are using
                              our package. The data you see on the right-hand side are generated from the raw <b>CRAN</b> logs.")),
                  radioButtons("cran_package", "R package", 
                               choices = list("Luminescence" = "luminescence",
                                              "RLumShiny" = "rlumshiny")),
                  radioButtons("cran_category", "Statistic",
                               choices = list("Date" = "date",
                                              "Country" = "country",
                                              "OS" = "os",
                                              "R Version" = "r_version",
                                              "Architecture" = "arch"))),
              box(width = 8, status = "primary", solidheader = TRUE, collapsible = TRUE,
                  title = NULL,
                  plotOutput("cran_plot"))
      ),
      
      ## RLumShiny ----
      tabItem("shinylum_1",
              fluidRow(width = 12,
                       column(width = 4,
                              box(width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                  title = HTML("Motivation"),
                                  tags$p(HTML(
                                    "The general motivations have already been outlined in the introduction.
                                    In short, using the <b>R</b> command-line can be tedious, even if your
                                    experienced in <b>R</b>. If you are new to <b>R</b> and only want to quickly
                                    produce a plot, the initial experience can be overwhelming. </br></br>
                                    In the context of the <b>R</b> package <code>Luminescence</code> over the time we learned
                                    that many colleagues were struggling 
                                    to get started with the package. While writing tutorials and manuals helped
                                    to ease the access to the package, it still takes a certain amount of dedication
                                    to learn the basic functionality of the package. Quite understandably many colleagues
                                    do not feel like having to learn basic <b>R</b> programming first only to use a very small
                                    subset of functions provided by the package. By providing a GUI to at least some
                                    of the more basic and/or 'popular' functions of the <code>Luminescence</code> package
                                    we can are able to practically remove these restrictions and make both <b>R</b> and
                                    the <code>Luminescence</code> more accessible to our colleagues."
                                  ))
                              )
                       ),
                       column(width = 8,
                              tabBox(title = HTML("Current content of <code>RLumShiny</code> (v0.1.1)"),
                                     width = 12,
                                     tabPanel("Applications",
                                              tags$p(HTML("Shiny applications available in the <b>R</b> package <code>RLumShiny</code> (v0.1.1). 
                                                           Each application can be started using the function app_RLum() with a 
                                                           corresponding keyword as input for the parameter app
                                                           (e.g., <code>app_RLum(app = 'abanico')</code>). All functions are part of the <code>Luminescence</code> package.
                                                          See the 'Examples' section to test some of the listed applications yourself!")),
                                              dataTableOutput("rlumshiny_app")),
                                     tabPanel("Functions",
                                              tags$p(HTML(
                                                "Functions in the <b>R</b> package <code>RLumShiny</code> (v0.1.1). The main function is <code>app_RLum()</code>, which must be used to start
                                                any of the applications given in the other table. All other functions are used internally and extend the functionality of the <code>shiny</code>
                                                package (see 'Extending shiny' section)."
                                              )),
                                              dataTableOutput("rlumshiny_fun")
                                     ))
                       )
              )
      ),
      tabItem("shinylum_2",
              
              box(width = 12, solidHeader = FALSE,
                  title = NULL,
                  fluidRow(width = 12,
                           column(width = 1,
                                  actionButton("extending_shiny", "", icon = icon("info-circle"), class = "info-btn")),
                           column(width = 11,
                                  div(align = "left", 
                                      HTML("<blockquote class = 'blockquote-reverse'>"), 
                                      tags$p(id = "reality", 
                                             HTML("&laquo; [...] 'shiny' is based on 
                                                  modern programming and markup languages, which allows
                                                  easy integration of existing JavaScript libraries, thus greatly
                                                  increasing the capabilities of 'shiny' and <b>R</b> itself. &raquo;")),
                                      HTML("<footer>Burow et al. (2016)</footer>"),
                                      HTML("</blockquote>"))
                           )
                  ),
                  box(width = 4, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
                      title = "RLumShiny::jscolorInput()",
                      tags$p(HTML(paste(
                        "<code>jscolorInput(inputId, label, value, position = 'bottom',
                        color = 'transparent', mode = 'HSV', slider = TRUE, close = FALSE)</code>",
                        "</br></br>The <code>RLumShiny</code> package includes the JavaScript library
                        jscolor (<a href = '#'>http://jscolor.com/</a>) along with a function to
                        create a JSColor (Javascript/HTML Color Picker) widget to be used in shiny applications."
                      )
                      )),
                      jscolorInput("jscolorInput"),
                      verbatimTextOutput("jscolorInput"),
                      plotOutput("jscolor_plot")
                  ),
                  box(width = 4, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
                      title = "RLumShiny::popover()",
                      tags$p(HTML(paste(
                        "<code>popover(title, content, header = NULL, html = TRUE,
                          class = 'btn btn-default', placement = c('right', 'top', 'left',
                          'bottom'), trigger = c('click', 'hover', 'focus', 'manual'))</code>",
                        "</br></br>Create a bootstrap button with popover, i.e. a small overlays of content for housing secondary information."
                      )
                      )),
                      popover("Click me!", content = HTML("Some amazing content."), html = TRUE, header = "Great success!")
                  ),
                  box(width = 4, status = "primary", solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
                      title = "RLumShiny::tooltip()",
                      tags$p(HTML(paste(
                        "<code>tooltip(refId, text, attr = NULL, animation = TRUE, delay = 100, html = TRUE, placement = 'auto', trigger = 'hover')</code>",
                        "</br></br>Create bootstrap tooltips for any HTML element to be used in shiny applications."
                      )
                      )),
                      actionButton("tooltipBtn", "Click me too!"),
                      tooltip("tooltipBtn", text = "Even greater success!", trigger = "click")
                  )
              )
      ),
      tabItem("shinylum_3",
              tabBox(title = HTML("<code>RLumShiny</code> applications"),
                     id = "examples_1",
                     width = 12,
                     tabPanel("Abanico Plot",
                              tags$p(HTML(
                                "Application for: <code>Luminescence::plot_AbanicoPlot()</code></br></br>
                                The Abanico Plot is a combination of the classic Radial Plot and a kernel density estimate plot.
                                It allows straightforward visualisation of data precision, error scatter around
                                a user-defined central value and the combined distribution of the values, 
                                on the actual scale of the measured data (e.g. seconds, equivalent dose, years). 
                                The principle of the plot is shown in Galbraith & Green (1990)."
                              )),
                              uiOutput("abanico")),
                     tabPanel("KDE",
                              tags$p(HTML(
                                "Application for: <code>Luminescence::plot_KDE()</code></br></br>
                                Plot a kernel density estimate of measurement values in combination with the actual values
                                and associated error bars in ascending order. Optionally, statistical measures such as
                                mean, median, standard deviation, standard error and quartile range can be provided
                                visually and numerically."
                              )),
                              uiOutput("kde")),
                     tabPanel("Cosmic Dose Rate",
                              tags$p(HTML(
                                "Application for: <code>Luminescence::calc_CosmicDoseRate()</code></br></br>
                                This application calculates the cosmic dose rate taking into account the soft- and hard-component
                                of the cosmic ray flux and allows corrections for geomagnetic latitude, altitude above sea-level
                                and geomagnetic field changes."
                              )),
                              uiOutput("cosmic")))
      ),
      tabItem("getstarted",
              box(title = tags$b("A collection of useful resources"), width = 12,
                  tabBox(title = NULL, width = 8,
                         tabPanel(title = HTML("<code>shiny</code>"),
                                  info_table_shiny()
                         ),
                         tabPanel(title = HTML("<code>Luminescence</code>"),
                                  info_table_lum()
                         ),
                         tabPanel(title = HTML("<code>RLumShiny</code>"),
                                  info_table_rlumshiny())
                  ),
                  box(title = HTML("This presentation"), width = 4, status = "warning",
                      solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                      tags$p("The code of this presentation is freely available on", icon("github"),"GitHub"),
                      tags$img(src = "img/qr_github.png", style = "width:100%;", border = 0),
                      div(align = "center", tags$a(id = "github", href = "#", "https://github.com/tzerk/EGU2017-PICO")))
              )
      ),
      tabItem("references",
              box(title = NULL, width = 12,
                 dataTableOutput("references"))
      )
    ),#EndOf::tabItems
    tags$div(class = "sticky-footer",
             tags$p(align = "right",
                    HTML("Copyright &copy; Christoph Burow |  This presentation is provided 'as is' without express or implied warranty.")
                    )
             )
  )#EndOf::dashboardBody
)#EndOf::BODY

################################################################################
####                          SERVER                                          ##
################################################################################
server <- function(input, output, session) {
  
  ## General settings
  
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
    showModal(modalDialog(title = HTML("A solution"), easyClose = TRUE,
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
                          hello_shiny_text))
  })
  
  output$helloshiny <- renderPlot({
    x <- faithful[ ,2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
  
  observeEvent(input$info_deploy, {
    showModal(modalDialog(title = HTML("Sharing shiny applications"), easyClose = TRUE,
                          deploy_text
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
  output$rlum_fun_history_plot <- renderPlot({
    gg <- ggplot(rlum_fun_history, aes(dates, funs)) + 
      geom_step(lwd = 1.5, col = "red") +
      geom_point() +
      theme_light() +
      xlab("Date") +
      ylab("No. of exported functions") +
      geom_text(aes(angle = 90), label = version, vjust = "top", hjust = "left", nudge_y = 1)
    
    gg
  })
  
  observeEvent(input$rlum_history, {
    showModal(modalDialog(title = "What the heck is 'Luminescence' anyway?",
                          tags$p(HTML(
                            "See Professor Ed Rhodes explain about the basic principle of luminescence dating and the use of the TL/OSL reader.
                            Note: you can activate the subtitles if there is no sound."
                          )),
                          tags$p(HTML(
                            "<div class='embed-responsive embed-responsive-16by9'>
                            <iframe class='embed-responsive-item' src='https://www.youtube.com/embed/4g-t8jv_-Ow'></iframe>
                            </div>"
                          ))
                          ))
  })

  output$lum_functions <- renderDataTable({
    rlum_fun_df
  }, options = list(pageLength = 10, pagingType = "full", 
                    searching = FALSE, lengthChange = FALSE))
  
  ## RLumShiny application table
  output$rlumshiny_app <- renderDataTable({
    rlumshiny_app_df
  }, options = list(pageLength = 10, pagingType = "full", 
                    searching = FALSE, lengthChange = FALSE))
  
  ## RLumShiny functions table
  output$rlumshiny_fun <- renderDataTable({
    rlumshiny_fun_df
  }, options = list(pageLength = 10, pagingType = "full", 
                    searching = FALSE, lengthChange = FALSE))
  
  ## RLumShiny extending shiny
  output$jscolorInput <- renderPrint({
    input$jscolorInput
  })
  output$jscolor_plot <- renderPlot({
    ggplot(diamonds, aes(carat)) +
      geom_histogram(binwidth = 0.01, fill = input$jscolorInput)
  })
  observeEvent(input$extending_shiny, {
    showModal(modalDialog(title = "Extending Shiny", easyClose = TRUE,
                          tags$p(HTML(
                            "The shiny framework can easily be extended by either writing your own extensions or
                            by including external JavaScript code or libraries. This is possible due to how <code>shiny</code>
                            works internally. From the <b>R</b> code of a shiny application an HTML file is generated, which
                            also includes a custom JavaScript library that enables the bidirectional communication between <b>R</b>
                            and the rendered webpage. The framework is further designed in a way to be easily extensible with only little <b>R</b>
                            code and a few lines of JavaScript. Since there are many great JavaScript libraries already available
                            the potential for increasing the capabilities of <b>R</b> is tremendous. While the <b>R</b>
                            package <code>RLumShiny</code> also includes some extensions to shiny, a much more impressive
                            integration of a JavaScript library is that of, e.g., Leaflet (<a href = '#'>http://leafletjs.com/</a>), a popular open-source library
                            for interactive maps using the <b>R</b> package <code>leaflet</code> (see example below)."
                          )),
                          verbatimTextOutput("leaflet_event"),
                          leafletOutput("leaflet")
    ))
  })
  output$leaflet <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      setView(lng = 16.413686, lat = 48.234910, zoom = 17) %>% 
      addMeasure()
  })
  output$leaflet_event <- renderPrint({
    input$leaflet_click
  })
  
  ## RLumShiny example applications
  output$abanico <- renderUI({
    iframe$abanico
  })
  output$kde <- renderUI({
    iframe$kde
  })
  output$cosmic <- renderUI({
    iframe$cosmic
  })
  
  ## References
  output$references <- renderDataTable({
    references_df
  }, options = list(pageLength = 10, pagingType = "full", 
                    searching = FALSE, lengthChange = FALSE))
}


# ----
shinyApp(ui, server)